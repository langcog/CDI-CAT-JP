## Follow-up: the Drule-vs-Wrule subsample check in 04-cat-simulation.R
## (n=100, max_items=100) found Drule slightly *outperforming* Wrule(0,1,1)
## on the specific/bias dimensions -- the opposite of the initial hypothesis
## (that down-weighting G in item selection would help estimate S1/S2).
## Likely explanation: once G is well-pinned-down, conditioning on it helps
## resolve S1/S2 too, so items that are simply informative overall (Drule)
## end up serving the specific dimensions better than items chosen to
## deliberately ignore G. This reruns Drule at the same lengths used for the
## full-sample Wrule run (100, 200) so the two criteria can be compared
## apples-to-apples on the full N=939 sample, to settle which to recommend.

library(tidyverse)
library(mirt)
library(mirtCAT)
library(parallel)

mod <- readRDS("multidim/mod_bifactor.Rds")
d <- readRDS("multidim/noun_predicate_response_data.Rds")
d_mat_imp <- readRDS("multidim/d_mat_content_imputed.Rds")
full_bank_scores <- read_csv("multidim/bifactor_scores.csv", show_col_types = FALSE) %>%
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

lengths <- c(100, 200)
cl <- makeCluster(min(10, detectCores() - 1))
sim_results_drule <- list()
for (L in lengths) {
  t0 <- Sys.time()
  sim_results_drule[[as.character(L)]] <- doCAT_multidim(
    d_mat_imp, mod, criteria = "Drule",
    min_items = 20, max_items = L, min_SEM = c(0.3, 0.5, 0.5), cl = cl
  )
  cat("Drule max_items =", L, "-", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
}
stopCluster(cl)
saveRDS(sim_results_drule, "multidim/cat_simulation_results_drule.Rds")

summary_tab <- map_dfr(names(sim_results_drule), function(L) {
  p <- sim_results_drule[[L]]$parms
  p$data_id <- d$d_demo$data_id
  p <- left_join(p, full_bank_scores %>% select(data_id, bias_full, G_full = G), by = "data_id")
  tibble(
    criteria = "Drule", max_items = as.integer(L),
    mean_n_items = mean(p$n_items),
    mean_SE_G = mean(p$SE_G), mean_SE_S1 = mean(p$SE_S1_noun), mean_SE_S2 = mean(p$SE_S2_predicate),
    r_bias_vs_full = cor(p$bias_cat, p$bias_full),
    r_G_vs_full = cor(p$G, p$G_full)
  )
})

wrule_tab <- read_csv("multidim/cat_simulation_summary.csv", show_col_types = FALSE) %>%
  filter(max_items %in% lengths) %>%
  mutate(criteria = "Wrule") %>%
  select(criteria, max_items, mean_n_items, mean_SE_G, mean_SE_S1, mean_SE_S2, r_bias_vs_full, r_G_vs_full)

combined <- bind_rows(summary_tab, wrule_tab) %>% arrange(max_items, criteria)
write_csv(combined, "multidim/criteria_comparison_full_sample.csv")
print(combined)
