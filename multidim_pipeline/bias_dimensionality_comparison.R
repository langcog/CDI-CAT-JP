## Empirical comparison: current 2-group bifactor (G + S1_noun + S2_predicate,
## bias = S1-S2) vs. a genuinely 2-dimensional alternative (G + one shared
## "Bias" specific factor, with nouns and predicates both loading on it).
##
## Key trick: bfactor()'s identification (specific-factor variance fixed to 1,
## orthogonal to G and to other specific factors) doesn't require the
## "1 specific group per item type" structure we've been using -- putting
## ALL content items into a single specific group is just as well-identified,
## and mirt's 2PL slopes aren't sign-constrained, so if nouns and predicates
## are truly bipolar on this dimension the EM should recover that (nouns
## positive, predicates negative or vice versa) without needing to
## reverse-code any data or fight EFA-style rotational indeterminacy.
##
## Crucially, both models have the *same* per-item parameter count (a1 for G,
## one specific slope, one intercept -- 3 params/item either way), so their
## log-likelihoods are directly comparable: whichever is higher tells us
## whether letting nouns/predicates have separate-scale specific variances
## (current model) actually fits the data better than forcing them onto one
## shared bipolar scale (alternative), rather than that being a purely
## theoretical question.

library(tidyverse)
library(mirt)
library(mirtCAT)
library(parallel)

d <- readRDS("multidim/noun_predicate_response_data.Rds")
d_mat_content <- d$d_mat_content
items_content <- d$items_content
mod_2group <- readRDS("multidim/mod_bifactor.Rds")

# --- Fit the 1-group (single Bias dimension) alternative -------------------
set.seed(1234)
t0 <- Sys.time()
mod_1group <- bfactor(
  data = d_mat_content, model = rep(1L, ncol(d_mat_content)), itemtype = "2PL",
  verbose = TRUE, technical = list(NCYCLES = 2000)
)
cat("Fit time:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
cat("Converged:", extract.mirt(mod_1group, "converged"), "\n")
saveRDS(mod_1group, "multidim_pipeline/mod_bifactor_1group_japanese.Rds")

# --- Did the single dimension recover the noun/predicate sign split? -------
co_1group <- coef(mod_1group, simplify = TRUE)$items %>%
  as.data.frame() %>%
  rownames_to_column("item_id") %>%
  rename(G = a1, Bias = a2) %>%
  left_join(items_content %>% rename(item_id = WS) %>% select(item_id, pos2), by = "item_id")

cat("\nMean G / Bias loading by item type (1-group model):\n")
print(co_1group %>% group_by(pos2) %>% summarise(mean_G = mean(G), mean_Bias = mean(Bias), n = n()))

# orient Bias so "positive" = noun-leaning, for comparability with S1-S2
if (mean(co_1group$Bias[co_1group$pos2 == "noun"]) < 0) {
  co_1group$Bias <- -co_1group$Bias
  cat("(flipped sign so noun-leaning = positive)\n")
}
write_csv(co_1group, "multidim_pipeline/bifactor_1group_item_params_japanese.csv")

# --- Model fit comparison (same df -- both 3 params/item, directly comparable) ---
ll_2group <- extract.mirt(mod_2group, "logLik")
ll_1group <- extract.mirt(mod_1group, "logLik")
npar_2group <- extract.mirt(mod_2group, "nest")
npar_1group <- extract.mirt(mod_1group, "nest")
cat("\nLog-likelihood: 2-group (current) =", ll_2group, " | 1-group (alternative) =", ll_1group, "\n")
cat("N estimated parameters: 2-group =", npar_2group, " | 1-group =", npar_1group, "\n")
cat("AIC: 2-group =", extract.mirt(mod_2group, "AIC"), " | 1-group =", extract.mirt(mod_1group, "AIC"), "\n")
cat("BIC: 2-group =", extract.mirt(mod_2group, "BIC"), " | 1-group =", extract.mirt(mod_1group, "BIC"), "\n")

# --- Compare factor scores: does the 1-D Bias line up with S1-S2? ----------
fs_2group <- fscores(mod_2group, method = "EAP", full.scores = TRUE, full.scores.SE = TRUE)
colnames(fs_2group) <- c("G", "S1_noun", "S2_predicate", "SE_G", "SE_S1_noun", "SE_S2_predicate")
bias_2group <- fs_2group[, "S1_noun"] - fs_2group[, "S2_predicate"]

fs_1group <- fscores(mod_1group, method = "EAP", full.scores = TRUE, full.scores.SE = TRUE)
colnames(fs_1group) <- c("G", "Bias", "SE_G", "SE_Bias")
if (cor(fs_1group[, "Bias"], bias_2group) < 0) fs_1group[, "Bias"] <- -fs_1group[, "Bias"]

cat("\nCorrelation between derived S1-S2 (current) and direct Bias theta (alternative):",
    round(cor(fs_1group[, "Bias"], bias_2group), 3), "\n")

rel_2group <- empirical_rxx(fs_2group)
rel_1group <- empirical_rxx(fs_1group)
cat("\nEmpirical reliability:\n")
cat("  2-group: G =", round(rel_2group["G"], 3), " S1 =", round(rel_2group["S1"], 3),
    " S2 =", round(rel_2group["S2"], 3), "\n")
cat("  1-group: G =", round(rel_1group["G"], 3), " Bias =", round(rel_1group["Bias"], 3), "\n")
cat("  (S1/S2 reliabilities aren't directly comparable to one Bias reliability --",
    "the practical comparison is the CAT simulation below.)\n")

scores_1group <- as.data.frame(fs_1group) %>%
  mutate(data_id = d$d_demo$data_id, production = d$d_demo$production)
write_csv(scores_1group, "multidim_pipeline/bifactor_1group_scores_japanese.csv")

# --- CAT simulation comparison: same design, both models -------------------
doCAT_generic <- function(dat, mod, criteria = "Drule", min_items, max_items, min_SEM, cl = NULL) {
  results <- mirtCAT(mo = mod, criteria = criteria, start_item = criteria, method = "MAP",
                      local_pattern = dat, cl = cl,
                      design = list(min_items = min_items, max_items = max_items, min_SEM = min_SEM))
  do.call(rbind, lapply(results, function(r) {
    so <- summary(r)
    c(t(so$final_estimates), n_items = length(r$items_answered))
  }))
}

d_mat_imp_2group <- imputeMissing(mod_2group, Theta = fscores(mod_2group))
d_mat_imp_1group <- imputeMissing(mod_1group, Theta = fscores(mod_1group))

lengths <- c(100, 150, 200)
min_items_vec <- round(lengths * 0.75)

cl <- makeCluster(min(10, detectCores() - 1))
cat("\nRunning 2-group (current) CAT simulation...\n")
res_2group <- map2(lengths, min_items_vec, function(L, mi) {
  p <- doCAT_generic(d_mat_imp_2group, mod_2group, min_items = mi, max_items = L,
                      min_SEM = c(0.3, 0.42, 0.41), cl = cl)
  colnames(p)[1:6] <- c("G", "S1_noun", "S2_predicate", "SE_G", "SE_S1_noun", "SE_S2_predicate")
  as.data.frame(p) %>% mutate(bias_cat = S1_noun - S2_predicate, max_items = L)
})
cat("Running 1-group (alternative) CAT simulation...\n")
res_1group <- map2(lengths, min_items_vec, function(L, mi) {
  p <- doCAT_generic(d_mat_imp_1group, mod_1group, min_items = mi, max_items = L,
                      min_SEM = c(0.3, 0.42), cl = cl)
  colnames(p)[1:4] <- c("G", "Bias", "SE_G", "SE_Bias")
  as.data.frame(p) %>%
    mutate(Bias = if (cor(Bias, scores_1group$Bias[seq_len(nrow(p))]) < 0) -Bias else Bias,
           max_items = L)
})
stopCluster(cl)

comparison <- map2_dfr(res_2group, res_1group, function(r2, r1) {
  r2$data_id <- d$d_demo$data_id
  r1$data_id <- d$d_demo$data_id
  r2 <- left_join(r2, tibble(data_id = d$d_demo$data_id, bias_full = bias_2group), by = "data_id")
  r1 <- left_join(r1, scores_1group %>% select(data_id, bias_full = Bias), by = "data_id")
  tibble(
    max_items = unique(r2$max_items),
    model = c("2-group (current)", "1-group (alternative)"),
    mean_n_items = c(mean(r2$n_items), mean(r1$n_items)),
    mean_SE_bias_dim = c(mean((r2$SE_S1_noun + r2$SE_S2_predicate) / 2), mean(r1$SE_Bias)),
    r_bias_vs_full = c(cor(r2$bias_cat, r2$bias_full), cor(r1$Bias, r1$bias_full))
  )
})
write_csv(comparison, "multidim_pipeline/bias_dimensionality_cat_comparison_japanese.csv")
cat("\nCAT simulation comparison:\n")
print(comparison, n = 20)
