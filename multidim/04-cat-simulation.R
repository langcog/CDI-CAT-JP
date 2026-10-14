## Multidimensional CAT simulation for the noun/predicate bias bifactor model.
##
## Two things this script establishes before running the full simulation:
##
## 1) Item-selection criterion. A quick 5-person check with plain 'Drule'
##    (D-optimality across all 3 dims) left the specific-factor SEs (S1, S2)
##    stuck near the prior SD (~0.95) even after 40 items -- Drule spends its
##    budget on items that are informative for G (whose loadings are ~3-5x
##    larger) and barely touches the specific dimensions. 'Wrule' with
##    weights = c(0, 1, 1) forces item selection to optimize for the bias
##    dimensions specifically, since G is incidentally well-measured by
##    almost any content item anyway.
##
## 2) Achievable precision. Even a full 478-item non-adaptive administration
##    (see multidim/bifactor_scores.csv) only reaches median SE ~0.50-0.52 for
##    S1/S2, vs ~0.12 for G -- so a min_SEM target that's reasonable for G
##    (0.3, matching the existing unidimensional CAT) is not reachable for
##    the specific/bias dimensions for most children. We use a looser,
##    empirically-grounded min_SEM for S1/S2 and rely on max_items as the
##    practical stopping rule for those dimensions.

library(tidyverse)
library(mirt)
library(mirtCAT)
library(parallel)

mod <- readRDS("multidim/mod_bifactor.Rds")
d <- readRDS("multidim/noun_predicate_response_data.Rds")
d_mat_imp <- readRDS("multidim/d_mat_content_imputed.Rds")
full_bank_scores <- read_csv("multidim/bifactor_scores.csv", show_col_types = FALSE) %>%
  mutate(bias_full = S1_noun - S2_predicate)

doCAT_multidim <- function(dat, mod, criteria = "Wrule", weights = c(0, 1, 1),
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

# --- 1) Drule vs Wrule comparison, small subsample, fixed length --------
set.seed(2024)
sub_idx <- sample(nrow(d_mat_imp), 100)
dat_sub <- d_mat_imp[sub_idx, ]

cl <- makeCluster(min(10, detectCores() - 1))
t0 <- Sys.time()
cmp_drule <- doCAT_multidim(dat_sub, mod, criteria = "Drule", min_items = 20,
                             max_items = 100, min_SEM = c(0.3, 0.5, 0.5), cl = cl)
cmp_wrule <- doCAT_multidim(dat_sub, mod, criteria = "Wrule", weights = c(0, 1, 1),
                             min_items = 20, max_items = 100,
                             min_SEM = c(0.3, 0.5, 0.5), cl = cl)
stopCluster(cl)
cat("Drule vs Wrule comparison time:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")

cat("\nDrule: mean final SEs (G, S1, S2):\n")
print(colMeans(cmp_drule$parms[, c("SE_G", "SE_S1_noun", "SE_S2_predicate")]))
cat("Wrule: mean final SEs (G, S1, S2):\n")
print(colMeans(cmp_wrule$parms[, c("SE_G", "SE_S1_noun", "SE_S2_predicate")]))

saveRDS(list(drule = cmp_drule, wrule = cmp_wrule), "multidim/criteria_comparison.Rds")

# --- 2) Full simulation across test lengths, using Wrule -----------------
lengths <- c(50, 100, 200)
cl <- makeCluster(min(10, detectCores() - 1))
sim_results <- list()
for (L in lengths) {
  t0 <- Sys.time()
  sim_results[[as.character(L)]] <- doCAT_multidim(
    d_mat_imp, mod, criteria = "Wrule", weights = c(0, 1, 1),
    min_items = 20, max_items = L, min_SEM = c(0.3, 0.5, 0.5), cl = cl
  )
  cat("max_items =", L, "-", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
}
stopCluster(cl)

saveRDS(sim_results, "multidim/cat_simulation_results.Rds")

# --- summarize -------------------------------------------------------------
summary_tab <- map_dfr(names(sim_results), function(L) {
  p <- sim_results[[L]]$parms
  p$data_id <- d$d_demo$data_id
  p <- left_join(p, full_bank_scores %>% select(data_id, bias_full, G_full = G), by = "data_id")
  tibble(
    max_items = as.integer(L),
    mean_n_items = mean(p$n_items),
    mean_SE_G = mean(p$SE_G), mean_SE_S1 = mean(p$SE_S1_noun), mean_SE_S2 = mean(p$SE_S2_predicate),
    r_bias_vs_full = cor(p$bias_cat, p$bias_full),
    r_G_vs_full = cor(p$G, p$G_full),
    n_items_never_used = length(setdiff(seq_len(ncol(d_mat_imp)), unique(sim_results[[L]]$all_items)))
  )
})
write_csv(summary_tab, "multidim/cat_simulation_summary.csv")
print(summary_tab)
