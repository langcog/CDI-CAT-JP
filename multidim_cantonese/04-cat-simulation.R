## Multidimensional CAT simulation, Cantonese noun/predicate bifactor model.
## Mirrors multidim/04-cat-simulation.R (Japanese): compares Drule vs Wrule
## item-selection criteria, then runs the full-sample simulation across a
## few item-budget lengths using whichever criterion wins.

library(tidyverse)
library(mirt)
library(mirtCAT)
library(parallel)

mod <- readRDS("multidim_cantonese/mod_bifactor.Rds")
d <- readRDS("multidim_cantonese/noun_predicate_response_data.Rds")

set.seed(123)
fs_full <- fscores(mod)
d_mat_imp <- imputeMissing(mod, Theta = fs_full)
saveRDS(d_mat_imp, "multidim_cantonese/d_mat_content_imputed.Rds")

full_bank_scores <- read_csv("multidim_cantonese/bifactor_scores.csv", show_col_types = FALSE) %>%
  mutate(bias_full = S1_noun - S2_predicate)

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

# --- 1) Drule vs Wrule comparison, subsample, fixed length ---------------
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

saveRDS(list(drule = cmp_drule, wrule = cmp_wrule), "multidim_cantonese/criteria_comparison.Rds")
criteria_comparison_subsample <- bind_rows(
  as_tibble(as.list(colMeans(cmp_drule$parms[, c("SE_G","SE_S1_noun","SE_S2_predicate")]))) %>%
    mutate(criteria = "Drule", .before = 1),
  as_tibble(as.list(colMeans(cmp_wrule$parms[, c("SE_G","SE_S1_noun","SE_S2_predicate")]))) %>%
    mutate(criteria = "Wrule", .before = 1)
)
write_csv(criteria_comparison_subsample, "multidim_cantonese/criteria_comparison_subsample.csv")

# --- 2) Full simulation across test lengths -------------------------------
# Default to Drule per the Japanese finding (Drule matched-or-beat Wrule with
# fewer items); re-confirm on this subsample above before trusting it blindly.
chosen_criteria <- if (mean(cmp_drule$parms$SE_S1_noun) <= mean(cmp_wrule$parms$SE_S1_noun)) "Drule" else "Wrule"
cat("\nChosen criteria for full simulation:", chosen_criteria, "\n")

lengths <- c(50, 100, 200)
cl <- makeCluster(min(10, detectCores() - 1))
sim_results <- list()
for (L in lengths) {
  t0 <- Sys.time()
  sim_results[[as.character(L)]] <- doCAT_multidim(
    d_mat_imp, mod, criteria = chosen_criteria, weights = c(0, 1, 1),
    min_items = 20, max_items = L, min_SEM = c(0.3, 0.5, 0.5), cl = cl
  )
  cat("max_items =", L, "-", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
}
stopCluster(cl)

saveRDS(sim_results, "multidim_cantonese/cat_simulation_results.Rds")

summary_tab <- map_dfr(names(sim_results), function(L) {
  p <- sim_results[[L]]$parms
  p$data_id <- d$d_demo$data_id
  p <- left_join(p, full_bank_scores %>% select(data_id, bias_full, G_full = G), by = "data_id")
  tibble(
    criteria = chosen_criteria,
    max_items = as.integer(L),
    mean_n_items = mean(p$n_items),
    mean_SE_G = mean(p$SE_G), mean_SE_S1 = mean(p$SE_S1_noun), mean_SE_S2 = mean(p$SE_S2_predicate),
    r_bias_vs_full = cor(p$bias_cat, p$bias_full),
    r_G_vs_full = cor(p$G, p$G_full),
    n_items_never_used = length(setdiff(seq_len(ncol(d_mat_imp)), unique(sim_results[[L]]$all_items)))
  )
})
write_csv(summary_tab, "multidim_cantonese/cat_simulation_summary.csv")
print(summary_tab)
