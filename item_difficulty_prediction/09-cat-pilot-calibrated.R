## Experiment 3: CAT with verbs whose parameters were calibrated from a small PILOT.
##
## Same K = 60 held-out verb draws (same seeds) as 06/07. Per replicate:
##   * a pool of 400 real children who completed the full form (no missing data) is
##     set aside as potential pilot children; the other 539 children (the remaining
##     full-form children plus all short-form children) are the CAT test children for
##     EVERY condition, so nobody is both pilot and test and all banks are compared on
##     the same children;
##   * pilot size N = 50 / 100 / 200 / 400 uses nested subsets of the pool;
##   * the 60 verbs are calibrated from the pilot children's OBSERVED responses, every
##     other item fixed at its true parameters, with a prior centred on the predicted
##     parameters (SD = cross-validated prediction error) -- plus one no-prior (ML)
##     comparison at N = 100.
## Conditions (all fixed-length 150-item Drule CATs, real responses):
##   C_true        true parameters for every item (ceiling)
##   A_drop        the 60 verbs dropped
##   B7_pred_only  verbs kept with predicted parameters, no pilot
##   P<N>          verbs kept with pilot-calibrated parameters (prior = predicted)
##   ML100         verbs kept, calibrated from N = 100 with NO prior

source(here::here("item_difficulty_prediction/05-sim-helpers.R"))
library(parallel)

inp <- load_inputs()
raw <- readRDS(here::here("multidim/noun_predicate_response_data.Rds"))$d_mat_content
all_items <- inp$items$WS
verbs <- inp$items$WS[inp$items$category_en == "action_words"]
ws_kids <- which(rowSums(is.na(raw)) == 0)
K <- 60; n_items <- 150; Ns <- c(50, 100, 200, 400)
out_file <- here::here("item_difficulty_prediction/cat_sim_pilot.csv")
if (file.exists(out_file)) file.remove(out_file)

true_par <- inp$bif_params |> transmute(item = WS, a_G = G, a_S = if_else(pos2 == "noun", S1_noun, S2_predicate), d)
cv <- map_dfr(verbs, \(v) predict_params(inp, v, "M7") |>
                left_join(true_par, by = "item", suffix = c("", "_t")) |>
                transmute(e_aG = a_G - a_G_t, e_aS = a_S - a_S_t, e_d = d - d_t))
cv_sd <- c(a_G = sqrt(mean(cv$e_aG^2)), a_S = sqrt(mean(cv$e_aS^2)), d = sqrt(mean(cv$e_d^2)))
cat("Prior SDs (cross-validated prediction error):", round(cv_sd, 2), "\n")

set.seed(1); theta_eval <- matrix(rnorm(4000 * 3), ncol = 3)
param_err <- function(est, hold) {
  tr <- true_par |> slice(match(hold, item))
  irf <- sqrt(mean(map_dbl(seq_along(hold), \(i)
    mean((plogis(est$a_G[i] * theta_eval[, 1] + est$a_S[i] * theta_eval[, 3] + est$d[i]) -
          plogis(tr$a_G[i] * theta_eval[, 1] + tr$a_S[i] * theta_eval[, 3] + tr$d[i]))^2))))
  tibble(rmse_a_G = sqrt(mean((est$a_G - tr$a_G)^2)), rmse_a_S = sqrt(mean((est$a_S - tr$a_S)^2)),
         rmse_d = sqrt(mean((est$d - tr$d)^2)), irf_rmse = irf)
}
na_err <- tibble(rmse_a_G = NA_real_, rmse_a_S = NA_real_, rmse_d = NA_real_, irf_rmse = NA_real_)

cl <- makeCluster(12)
for (r in 1:3) {
  set.seed(1000 * K + r); hold <- sample(verbs, K)
  p7 <- predict_params(inp, hold, "M7"); p0 <- predict_params(inp, hold, "M0")
  set.seed(2000 + r); pool <- sample(ws_kids, 400)
  test <- setdiff(seq_len(nrow(raw)), pool)
  cat(sprintf("\n=== rep %d: %d test children, pilot pool %d ===\n", r, length(test), length(pool)))

  banks <- list(
    C_true       = list(model = make_model(inp, all_items),                    err = na_err, pilot_N = NA),
    A_drop       = list(model = make_model(inp, setdiff(all_items, hold)),     err = na_err, pilot_N = NA),
    B7_pred_only = list(model = make_model(inp, all_items, p7),                err = param_err(p7, hold), pilot_N = 0))
  for (N in Ns) {
    est <- calibrate_verbs(inp, raw[pool[seq_len(N)], , drop = FALSE], hold, start = p7, prior_mean = p7, prior_sd = cv_sd)
    banks[[paste0("P", N)]] <- list(model = make_model(inp, all_items, est), err = param_err(est, hold), pilot_N = N)
  }
  est_ml <- calibrate_verbs(inp, raw[pool[1:100], , drop = FALSE], hold, start = p0)
  banks[["ML100"]] <- list(model = make_model(inp, all_items, est_ml), err = param_err(est_ml, hold), pilot_N = 100)

  for (nm in names(banks)) {
    t0 <- Sys.time()
    run <- run_cat(banks[[nm]]$model, inp, n_items, cl, kids = test)
    row <- bind_cols(tibble(K = K, rep = r, condition = nm, pilot_N = banks[[nm]]$pilot_N),
                     score_run(run, inp, tracked = hold, kids = test), banks[[nm]]$err,
                     tibble(mins = as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    write_csv(row, out_file, append = file.exists(out_file))
    print(row, width = Inf)
  }
}
stopCluster(cl)
cat("\nDONE\n")
