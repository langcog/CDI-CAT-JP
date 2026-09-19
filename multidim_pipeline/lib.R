## Shared functions for the noun/predicate bifactor-CAT pipeline, factored out
## after building it three times by hand (Japanese: multidim/, from local
## repo data; Cantonese: multidim_cantonese/, from Wordbank). Each language's
## own directory (multidim_<language>/) keeps thin numbered scripts
## (01-prep, 02-efa, 03-bifactor, 04-cat-simulation) that call these with
## language-specific arguments, so the underlying logic lives in one place
## and per-language scripts stay easy to read/diff against each other.
##
## Design decisions baked into these functions (see multidim/06-*.Rmd and
## multidim_cantonese/06-*.Rmd for the reasoning):
##  - item classification: category -> lexical_class -> pos2 (noun/predicate),
##    validated against Wordbank's own lexical_category coding for JP and
##    Cantonese (it collapses verbs+adjectives into "predicates" the same way
##    our category-based mapping does)
##  - confirmatory model: mirt::bfactor(), general factor G + orthogonal
##    noun-specific (S1) and predicate-specific (S2) factors
##  - CAT criterion: Drule (matched-or-beat Wrule with fewer items in both
##    languages tested so far)
##  - CAT stopping rule: min_SEM must be calibrated against *this dataset's*
##    full-item-bank SE ceiling, not reused from another language -- the
##    Cantonese run showed reusing Japanese's min_SEM=0.5 badly under-shoots
##    and truncates the test before the bias dimensions are resolved

library(tidyverse)
library(wordbankr)
library(mirt)
library(mirtCAT)
library(parallel)

# categories shared across the (near-identical) MB-CDI-derived WS category
# structure in English/Japanese/Cantonese; callers can pass additional
# language-specific categories (e.g. Cantonese's classifiers/final_particles,
# English's helping_verbs) merged on top via `extra_function_word_categories`
default_category_map <- function(extra_function_word_categories = character(0)) {
  base <- c(
    animals = "noun", body_parts = "noun", clothing = "noun", food_drink = "noun",
    furniture_rooms = "noun", household = "noun", outside = "noun", toys = "noun",
    vehicles = "noun",
    action_words = "verb",
    descriptive_words = "adjective",
    connecting_words = "function_word", locations = "function_word",
    pronouns = "function_word", quantifiers = "function_word",
    question_words = "function_word"
  )
  extra <- setNames(rep("function_word", length(extra_function_word_categories)),
                     extra_function_word_categories)
  c(base, extra)
}

## Classify a Wordbank items table into lexical_class (noun/verb/adjective/
## function_word/other) and pos2 (noun/predicate/other, the 2-way CAT split).
classify_items <- function(items_raw, category_map) {
  items_raw %>%
    mutate(
      lexical_class = coalesce(category_map[category], "other"),
      pos2 = case_when(
        lexical_class == "noun" ~ "noun",
        lexical_class %in% c("verb", "adjective") ~ "predicate",
        TRUE ~ "other"
      )
    )
}

## Pull item + response data for one Wordbank language/form, classify items,
## drop non-producers and ceiling administrations, build the full and
## content-word (noun+predicate) response matrices, and save everything to
## out_dir/noun_predicate_response_data.Rds + out_dir/item_lexical_categories.csv.
## Returns the same list structure invisibly.
prep_wordbank_data <- function(language, form, category_map, out_dir,
                                extra_admin_filter = NULL) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  items_raw <- get_item_data(language = language, form = form) %>%
    filter(item_kind == "word")
  items <- classify_items(items_raw, category_map)

  cat("Item counts by lexical_class:\n")
  print(table(items$lexical_class))
  cat("\nItem counts by pos2 (2D CAT target):\n")
  print(table(items$pos2))
  cat("\nCross-check vs Wordbank's own lexical_category (nouns/predicates should match exactly):\n")
  print(table(items$pos2, items$lexical_category, useNA = "ifany"))

  write_csv(
    items %>% select(item_id, category, english_gloss, uni_lemma, lexical_class, pos2),
    file.path(out_dir, "item_lexical_categories.csv")
  )

  admin <- get_administration_data(language = language, form = form)
  n_items_total <- nrow(items_raw)

  too_few <- which(admin$production == 0)
  ceiling <- which(admin$production == max(admin$production))
  to_remove <- unique(c(too_few, ceiling))
  if (!is.null(extra_admin_filter)) to_remove <- unique(c(to_remove, extra_admin_filter(admin)))

  cat("\nStarting N =", nrow(admin), "; removing", length(too_few), "non-producers,",
      length(ceiling), "at ceiling ->", nrow(admin) - length(to_remove), "retained.\n")
  admin <- admin[-to_remove, ]

  inst <- get_instrument_data(language = language, form = form, items = NULL,
                               administration_info = admin)

  d_wide <- inst %>%
    filter(item_id %in% items$item_id) %>%
    mutate(produces = as.integer(produces)) %>%
    select(data_id, item_id, produces) %>%
    pivot_wider(names_from = item_id, values_from = produces)

  d_mat <- d_wide %>% select(-data_id) %>% data.matrix()
  rownames(d_mat) <- d_wide$data_id

  items <- items[match(colnames(d_mat), items$item_id), ]
  stopifnot(all(items$item_id == colnames(d_mat)))

  d_demo <- admin %>% filter(data_id %in% d_wide$data_id) %>%
    arrange(match(data_id, d_wide$data_id))
  stopifnot(all(d_demo$data_id == d_wide$data_id))

  content_idx <- which(items$pos2 %in% c("noun", "predicate"))
  d_mat_content <- d_mat[, content_idx]
  items_content <- items[content_idx, ]

  cat("\nContent-word item bank (nouns + predicates):", ncol(d_mat_content), "items,",
      sum(items_content$pos2 == "noun"), "nouns,",
      sum(items_content$pos2 == "predicate"), "predicates.\n")
  cat("NA proportion in content matrix:", round(100 * mean(is.na(d_mat_content)), 2), "%\n")

  out <- list(d_mat = d_mat, items = items,
              d_mat_content = d_mat_content, items_content = items_content,
              d_demo = d_demo)
  saveRDS(out, file.path(out_dir, "noun_predicate_response_data.Rds"))
  invisible(out)
}

## Subsample administrations (rows) for the two compute-heavy model-fitting
## steps when N is large -- EFA/bfactor runtime scales with items x people x
## EM iterations, and at Wordbank's largest samples (thousands of admins)
## fitting on the full sample can take hours. Subsampling is a documented
## scope decision, not a silent shortcut -- always report n used vs available.
subsample_for_fitting <- function(d_mat_content, n, seed = 1234) {
  if (nrow(d_mat_content) <= n) return(seq_len(nrow(d_mat_content)))
  set.seed(seed)
  sort(sample(nrow(d_mat_content), n))
}

## Exploratory 2-factor IRT model (oblimin rotation) -- diagnostic check of
## whether the a priori noun/predicate split is empirically recoverable.
fit_exploratory_efa <- function(d_mat_content, items_content, out_dir, ncycles = 2000, seed = 1234) {
  set.seed(seed)
  t0 <- Sys.time()
  mod_efa2 <- mirt(data = d_mat_content, model = 2, itemtype = "2PL",
                    verbose = TRUE, technical = list(NCYCLES = ncycles))
  cat("Fit time:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
  cat("Converged:", extract.mirt(mod_efa2, "converged"), "\n")
  saveRDS(mod_efa2, file.path(out_dir, "mod_efa2.Rds"))

  s <- summary(mod_efa2, rotate = "oblimin", suppress = 0)
  loadings <- as.data.frame(s$rotF) %>%
    rownames_to_column("item_row") %>%
    mutate(item_id = colnames(d_mat_content)) %>%
    left_join(items_content %>% select(item_id, category, english_gloss, pos2), by = "item_id")

  cat("\nFactor correlation (oblimin):\n"); print(s$fcor)
  cat("\nMean rotated loadings by pos2 x factor:\n")
  print(loadings %>% group_by(pos2) %>% summarise(mean_F1 = mean(F1), mean_F2 = mean(F2), n = n()))
  cat("\nWelch t-test, F2 noun vs predicate:\n")
  print(t.test(F2 ~ pos2, data = loadings %>% filter(pos2 %in% c("noun", "predicate"))))

  write_csv(loadings, file.path(out_dir, "efa2_loadings.csv"))
  invisible(mod_efa2)
}

## Confirmatory bifactor model: G (all items) + S1 (nouns) + S2 (predicates).
fit_confirmatory_bifactor <- function(d_mat_content, items_content, d_demo, out_dir,
                                       ncycles = 2000, seed = 1234) {
  stopifnot(all(items_content$item_id == colnames(d_mat_content)))
  specific_group <- as.integer(factor(items_content$pos2, levels = c("noun", "predicate")))

  set.seed(seed)
  t0 <- Sys.time()
  mod_bifactor <- bfactor(data = d_mat_content, model = specific_group, itemtype = "2PL",
                           verbose = TRUE, technical = list(NCYCLES = ncycles))
  cat("Fit time:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
  cat("Converged:", extract.mirt(mod_bifactor, "converged"), "\n")
  saveRDS(mod_bifactor, file.path(out_dir, "mod_bifactor.Rds"))

  co <- coef(mod_bifactor, simplify = TRUE)$items %>%
    as.data.frame() %>%
    rownames_to_column("item_id") %>%
    rename(G = a1, S1_noun = a2, S2_predicate = a3) %>%
    left_join(items_content %>% select(item_id, category, english_gloss, pos2), by = "item_id")
  write_csv(co, file.path(out_dir, "bifactor_item_params.csv"))

  cat("\nMean G / own-specific loading by pos2:\n")
  print(co %>% mutate(own_specific = if_else(pos2 == "noun", S1_noun, S2_predicate)) %>%
          group_by(pos2) %>% summarise(mean_G = mean(G), mean_own_specific = mean(own_specific), n = n()))

  fs <- fscores(mod_bifactor, method = "EAP", full.scores = TRUE, full.scores.SE = TRUE)
  cat("\nEmpirical reliability (G, S1_noun, S2_predicate):\n")
  print(empirical_rxx(fs))

  colnames(fs) <- c("G", "S1_noun", "S2_predicate", "SE_G", "SE_S1_noun", "SE_S2_predicate")
  scores <- as.data.frame(fs) %>%
    mutate(noun_predicate_bias = S1_noun - S2_predicate) %>%
    bind_cols(d_demo %>% select(data_id, age, production))
  write_csv(scores, file.path(out_dir, "bifactor_scores.csv"))

  cat("\nCorrelation of bias with production/age:\n")
  print(cor(scores %>% select(noun_predicate_bias, production, age), use = "complete.obs"))
  cat("Correlation of G with production:", cor(scores$G, scores$production), "\n")
  cat("S1/S2 EAP score correlation:", cor(scores$S1_noun, scores$S2_predicate), "\n")

  invisible(mod_bifactor)
}

## One CAT simulation run (fixed criteria/design), returning per-person final
## estimates + SEs + n_items, and the multiset of items ever administered.
doCAT_multidim <- function(dat, mod, criteria = "Drule", weights = c(0, 1, 1),
                            min_items = 20, max_items = 100,
                            min_SEM = c(0.3, 0.5, 0.5), cl = NULL) {
  results <- mirtCAT(
    mo = mod, criteria = criteria, start_item = criteria, method = "MAP",
    local_pattern = dat, cl = cl,
    design = list(min_items = min_items, max_items = max_items,
                   min_SEM = min_SEM, weights = weights)
  )
  all_items <- unlist(lapply(results, function(r) r$items_answered))
  parms <- do.call(rbind, lapply(results, function(r) {
    so <- summary(r)
    c(t(so$final_estimates), n_items = length(r$items_answered))
  }))
  parms <- as.data.frame(parms)
  names(parms) <- c("G", "S1_noun", "S2_predicate",
                     "SE_G", "SE_S1_noun", "SE_S2_predicate", "n_items")
  parms$bias_cat <- parms$S1_noun - parms$S2_predicate
  list(all_items = all_items, parms = parms)
}

## Full CAT-simulation pipeline for one language: impute missing responses,
## compare Drule vs Wrule on a subsample, then run the winning criterion
## across `lengths` on `dat` (pass a subsample here too if N is very large).
## `min_SEM` should be set from *this dataset's* full-bank SE ceiling
## (see bifactor_scores.csv's SE_S1_noun/SE_S2_predicate), not copied from
## another language -- reusing Japanese's min_SEM=0.5 for Cantonese caused
## premature stopping and badly hurt recovery (r=.82 vs .97 once fixed).
## `min_items` may be a single value (applied to every length) or a vector
## the same length as `lengths` -- every language tried so far needed a
## substantial floor (150-250 items) to avoid the SE criterion stopping the
## test before the bias dimensions were actually resolved, so pass a vector
## scaled to each length's ceiling (e.g. ~75% of max_items) rather than
## rediscovering that with a follow-up "forced" run each time.
run_cat_pipeline <- function(mod, d_mat_content, d_demo, out_dir,
                              lengths = c(50, 100, 200),
                              min_items = 20, min_SEM = c(0.3, 0.5, 0.5),
                              n_cores = min(10, detectCores() - 1), seed = 123) {
  min_items_vec <- rep_len(min_items, length(lengths))
  set.seed(seed)
  fs_full <- fscores(mod)
  d_mat_imp <- imputeMissing(mod, Theta = fs_full)
  saveRDS(d_mat_imp, file.path(out_dir, "d_mat_content_imputed.Rds"))

  full_bank_scores <- read_csv(file.path(out_dir, "bifactor_scores.csv"), show_col_types = FALSE) %>%
    mutate(bias_full = S1_noun - S2_predicate)

  sub_idx <- sample(nrow(d_mat_imp), min(100, nrow(d_mat_imp)))
  dat_sub <- d_mat_imp[sub_idx, ]

  # comparison step always caps at max_items=100 regardless of `lengths`, so
  # use a scalar min_items here even when the caller passed a per-length vector
  cmp_min_items <- min(min_items_vec[1], 100)

  cl <- makeCluster(n_cores)
  cmp_drule <- doCAT_multidim(dat_sub, mod, criteria = "Drule", min_items = cmp_min_items,
                               max_items = 100, min_SEM = min_SEM, cl = cl)
  cmp_wrule <- doCAT_multidim(dat_sub, mod, criteria = "Wrule", weights = c(0, 1, 1),
                               min_items = cmp_min_items, max_items = 100, min_SEM = min_SEM, cl = cl)
  stopCluster(cl)
  cat("Drule mean SEs:\n"); print(colMeans(cmp_drule$parms[, c("SE_G", "SE_S1_noun", "SE_S2_predicate")]))
  cat("Wrule mean SEs:\n"); print(colMeans(cmp_wrule$parms[, c("SE_G", "SE_S1_noun", "SE_S2_predicate")]))
  saveRDS(list(drule = cmp_drule, wrule = cmp_wrule), file.path(out_dir, "criteria_comparison.Rds"))

  chosen <- if (mean(cmp_drule$parms$SE_S1_noun) <= mean(cmp_wrule$parms$SE_S1_noun)) "Drule" else "Wrule"
  cat("\nChosen criteria:", chosen, "\n")

  cl <- makeCluster(n_cores)
  sim_results <- list()
  for (i in seq_along(lengths)) {
    L <- lengths[i]
    t0 <- Sys.time()
    sim_results[[as.character(L)]] <- doCAT_multidim(
      d_mat_imp, mod, criteria = chosen, weights = c(0, 1, 1),
      min_items = min_items_vec[i], max_items = L, min_SEM = min_SEM, cl = cl
    )
    cat("max_items =", L, "(min_items =", min_items_vec[i], ") -",
        round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
  }
  stopCluster(cl)
  saveRDS(sim_results, file.path(out_dir, "cat_simulation_results.Rds"))

  summary_tab <- map_dfr(names(sim_results), function(L) {
    p <- sim_results[[L]]$parms
    p$data_id <- d_demo$data_id[seq_len(nrow(p))]
    p <- left_join(p, full_bank_scores %>% select(data_id, bias_full, G_full = G), by = "data_id")
    tibble(criteria = chosen, max_items = as.integer(L), mean_n_items = mean(p$n_items),
           mean_SE_G = mean(p$SE_G), mean_SE_S1 = mean(p$SE_S1_noun), mean_SE_S2 = mean(p$SE_S2_predicate),
           r_bias_vs_full = cor(p$bias_cat, p$bias_full), r_G_vs_full = cor(p$G, p$G_full),
           n_items_never_used = length(setdiff(seq_len(ncol(d_mat_content)), unique(sim_results[[L]]$all_items))))
  })
  write_csv(summary_tab, file.path(out_dir, "cat_simulation_summary.csv"))
  print(summary_tab)
  invisible(summary_tab)
}
