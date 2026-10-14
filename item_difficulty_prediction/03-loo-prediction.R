## How well can we predict a Japanese CDI item's difficulty WITHOUT its Japanese
## CDI data? Leave-one-concept-out (LOO by uni_lemma) evaluation.
##
## For each held-out Japanese item we pretend we only know:
##   (a) its lexical class,
##   (b) its Japanese word form (mora / character counts),
##   (c) concept-level norms mapped by uni_lemma (concreteness, babiness, ...),
##   (d) how early children learn the SAME concept (uni_lemma) in other
##       languages' CDIs (Japanese itself is excluded from this).
## Anything fit uses only the other Japanese items (all items sharing the held-out
## item's uni_lemma are held out together, so a near-duplicate can't leak in).
##
## Still missing (next step): Japanese-corpus frequency (CHILDES / other sources).

suppressMessages({
  library(tidyverse)
  library(glue)
  library(here)
})

aoa_repo <- "/Users/gkacherg/Documents/GitHub/aoa-pipeline"
cache_dir <- here("item_difficulty_prediction", "cache")
wb_path <- file.path(cache_dir, "wordbank")
normalize_language <- function(language) language |> str_replace(" ", "_") |> str_to_lower()
source(file.path(aoa_repo, "scripts", "[1] wordbank.R"))   # load_wb_data()
source(file.path(aoa_repo, "scripts", "[5] fit_models.R")) # fit_aoas()
pred_dir <- file.path(aoa_repo, "data", "predictors")       # used by map_predictor()
source(file.path(aoa_repo, "scripts", "[3] map_predictors.R"))
pred_dir <- file.path(aoa_repo, "data", "predictors")       # (re-set: the script above overwrites it)

# --- 1. AoAs for every language/concept in the cache (produces only) --------
aoa_file <- file.path(cache_dir, "aoas_produces.rds")
if (file.exists(aoa_file)) {
  aoas <- readRDS(aoa_file)
} else {
  langs <- list.files(wb_path, pattern = "\\.rds$") |> str_remove("\\.rds$")
  wb_data <- map_df(langs, \(l) readRDS(file.path(wb_path, paste0(l, ".rds")))) |>
    filter(measure == "produces")
  cat("Wordbank languages loaded:", n_distinct(wb_data$language), "\n")
  aoas <- fit_aoas(wb_data)
  saveRDS(aoas, aoa_file)
}
cat("AoA estimates:", nrow(aoas), "across", n_distinct(aoas$language), "languages\n")

# --- 2. Cross-linguistic summary per concept (Japanese excluded) ------------
xl_langs <- aoas |> filter(language != "Japanese")
xl_langs <- xl_langs |>
  group_by(language) |>
  mutate(aoa_clip = pmin(aoa, 48),
         aoa_z = (aoa_clip - mean(aoa_clip)) / sd(aoa_clip)) |>
  ungroup()
east_asian <- c("Cantonese", "Mandarin (Beijing)", "Mandarin (Taiwanese)", "Korean")
xl <- xl_langs |>
  group_by(uni_lemma) |>
  summarise(n_lang = n(), xl_aoa_mean = mean(aoa_clip), xl_aoa_z = mean(aoa_z),
            xl_aoa_z_ea = if (any(language %in% east_asian)) mean(aoa_z[language %in% east_asian]) else NA_real_,
            .groups = "drop")

# --- 3. Japanese items + predictors -----------------------------------------
jp <- read_csv(here("item_difficulty_prediction/jp_item_table.csv"), show_col_types = FALSE)

norm_specs <- list(
  babiness     = c(word = "word", babiness = "babyAVG"),
  valence      = c(word = "Word", valence = "V.Mean.Sum", arousal = "A.Mean.Sum"),
  concreteness = c(word = "Word", concreteness = "Conc.M"),
  sensorimotor = c(word = "Word", sensorimotor = "Minkowski3.sensorimotor"),
  emotionality = c(word = "uni_lemma", emotionality = "emotionality"))
uni_tbl <- tibble(language = "Japanese", uni_lemma = unique(na.omit(jp$uni_lemma)))
norms <- imap(norm_specs, \(spec, nm) suppressMessages(map_predictor(uni_tbl, nm, spec))) |>
  reduce(\(x, y) left_join(x, y, by = c("language", "uni_lemma"))) |>
  select(-language)

freq <- read_csv(here("item_difficulty_prediction/jp_frequency.csv"), show_col_types = FALSE) |>
  select(item_id, count_lemma, freq_lemma, ctx_div, freq_surface)

d <- jp |>
  left_join(xl, by = "uni_lemma") |>
  left_join(norms, by = "uni_lemma") |>
  left_join(freq, by = "item_id") |>
  mutate(lexical_class = factor(lexical_class),
         has_xl = !is.na(xl_aoa_z) & !is.na(uni_lemma),
         n_lang = replace_na(n_lang, 0L),
         group = coalesce(uni_lemma, item_id)) |>
  filter(!is.na(aoa_local))

norm_cols <- c("babiness", "valence", "arousal", "concreteness", "sensorimotor", "emotionality")
freq_cols <- c("freq_lemma", "ctx_div", "freq_surface")
d <- d |> mutate(across(all_of(freq_cols), \(x) if_else(count_lemma > 0, x, NA_real_)))
for (col in c(norm_cols, freq_cols, "xl_aoa_mean", "xl_aoa_z", "xl_aoa_z_ea")) {
  d[[paste0(col, "_miss")]] <- is.na(d[[col]])
  d[[col]] <- if_else(is.na(d[[col]]), mean(d[[col]], na.rm = TRUE), d[[col]])
}
d <- d |> mutate(across(c(n_mora, n_char_ja, all_of(norm_cols), all_of(freq_cols), xl_aoa_mean, xl_aoa_z, xl_aoa_z_ea),
                        \(x) as.numeric(scale(x))))

saveRDS(d, here("item_difficulty_prediction/jp_predictor_table.rds"))   # scaled/imputed predictors, reused by the CAT simulation

cat("\nJapanese items:", nrow(d), " | with cross-linguistic AoA:", sum(d$has_xl), "\n")
cat("Verbs:", sum(d$lexical_class == "verb"), " | verbs with cross-linguistic AoA:",
    sum(d$lexical_class == "verb" & d$has_xl), "\n")
cat("Languages contributing per concept (verbs): median =",
    median(d$n_lang[d$lexical_class == "verb" & d$has_xl]), "\n")

# --- 4. Leave-one-concept-out evaluation ------------------------------------
form_terms  <- c("n_mora", "n_char_ja", "is_katakana")
norm_terms  <- norm_cols
xl_terms    <- c("xl_aoa_z", "xl_aoa_mean", "has_xl")
freq_terms  <- c("freq_lemma", "ctx_div", "freq_lemma_miss")
specs <- list(
  M0_class_only       = "lexical_class",
  M1_plus_form        = c("lexical_class", form_terms),
  M2_plus_norms       = c("lexical_class", form_terms, norm_terms),
  M3_xl_only          = c("lexical_class", xl_terms),
  M4_xl_east_asian    = c("lexical_class", xl_terms, "xl_aoa_z_ea"),
  M5_all              = c("lexical_class", form_terms, norm_terms, xl_terms),
  M6_freq_only        = c("lexical_class", freq_terms),
  M7_xl_plus_freq     = c("lexical_class", xl_terms, freq_terms),
  M8_all_plus_freq    = c("lexical_class", form_terms, norm_terms, xl_terms, freq_terms)
)

loo_predict <- function(df, terms, target, train_filter = \(x) rep(TRUE, nrow(x))) {
  f <- as.formula(paste(target, "~", paste(terms, collapse = " + ")))
  pred <- rep(NA_real_, nrow(df))
  in_set <- train_filter(df)
  for (g in unique(df$group)) {
    test <- df$group == g
    if (!any(in_set[test])) next   # held-out item not in this training population
    train <- !test & in_set
    fit <- lm(f, data = df[train, ])
    pred[test & in_set] <- predict(fit, newdata = df[test & in_set, ])
  }
  pred
}

targets <- c(aoa_local = "aoa_local", b_2pl = "b_2pl", d_2pl = "d_2pl", a_2pl = "a_2pl")
train_sets <- list(all_items = \(x) rep(TRUE, nrow(x)),
                   predicates_only = \(x) x$lexical_class %in% c("verb", "adjective"))

res <- expand_grid(target = names(targets), spec = names(specs), train = names(train_sets)) |>
  mutate(pred = pmap(list(target, spec, train),
                     \(tg, sp, tr) loo_predict(d, specs[[sp]], targets[[tg]], train_sets[[tr]])))

long <- res |>
  mutate(obs = map(target, \(tg) d[[targets[[tg]]]]),
         item_id = list(d$item_id), lexical_class = list(d$lexical_class),
         has_xl = list(d$has_xl), n_lang = list(d$n_lang)) |>
  unnest(c(pred, obs, item_id, lexical_class, has_xl, n_lang))
saveRDS(long, here("item_difficulty_prediction/loo_predictions.rds"))

summ <- function(df) {
  df |> summarise(n = n(), rmse = sqrt(mean((obs - pred)^2)), mae = mean(abs(obs - pred)),
                  sd_obs = sd(obs), r = cor(obs, pred), .groups = "drop") |>
    mutate(r2 = 1 - (rmse^2) / (sd_obs^2))
}

cat("\n===== Verbs: LOO error by model (train on all items) =====\n")
verb_tab <- long |> filter(lexical_class == "verb", train == "all_items") |>
  group_by(target, spec) |> summ()
for (tg in names(targets)) {
  cat("\n--", tg, "--\n")
  print(verb_tab |> filter(target == tg) |> select(-target) |>
          mutate(across(where(is.numeric), \(x) round(x, 3))), n = 20)
}
write_csv(verb_tab, here("item_difficulty_prediction/loo_summary_verbs.csv"))

cat("\n===== Verbs with cross-linguistic info, M5, AoA: error by # of other languages =====\n")
print(long |> filter(lexical_class == "verb", train == "all_items", spec == "M5_all", target == "aoa_local", has_xl) |>
        mutate(n_lang_bin = cut(n_lang, c(0, 3, 8, 15, 40))) |>
        group_by(n_lang_bin) |> summ() |> mutate(across(where(is.numeric), \(x) round(x, 3))))

cat("\n===== All lexical classes: M5 vs M0, AoA =====\n")
print(long |> filter(train == "all_items", target == "aoa_local", spec %in% c("M0_class_only", "M5_all")) |>
        group_by(lexical_class, spec) |> summ() |>
        mutate(across(where(is.numeric), \(x) round(x, 3))), n = 20)

write_csv(long |> group_by(target, spec, train, lexical_class) |> summ(),
          here("item_difficulty_prediction/loo_summary_all.csv"))
