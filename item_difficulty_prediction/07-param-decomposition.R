## Experiment 1: WHICH parameter's error stops predicted verbs from helping?
##
## Same K = 60 held-out verb draws (same seeds) as 06-cat-simulation.R, so results
## line up with the A / B7 / B0 / C rows already in cat_sim_results.csv. Two
## partial-oracle banks:
##   D1  TRUE intercept d,  PREDICTED loadings (a_G, a_S)
##   D2  PREDICTED d,       TRUE loadings
## If D1 recovers the benefit of true parameters (C), the loadings are not the
## problem and difficulty prediction is what to improve; if D1 is no better than
## dropping the verbs (A), the loadings -- which no available predictor can supply --
## are what matter and better difficulty prediction alone would not help.

source(here::here("item_difficulty_prediction/05-sim-helpers.R"))
library(parallel)

inp <- load_inputs()
all_items <- inp$items$WS
verbs <- inp$items$WS[inp$items$category_en == "action_words"]
n_items <- 150
K <- 60
out_file <- here::here("item_difficulty_prediction/cat_sim_decomp.csv")
if (file.exists(out_file)) file.remove(out_file)

cl <- makeCluster(10)
for (r in 1:3) {
  set.seed(1000 * K + r)
  hold <- sample(verbs, K)
  p7 <- predict_params(inp, hold, "M7")
  truth <- inp$bif_params |> filter(WS %in% hold) |>
    transmute(item = WS, a_G_true = G, a_S_true = S2_predicate, d_true = d)
  mix <- p7 |> left_join(truth, by = "item")
  conds <- list(
    D1_true_d_pred_a = mix |> transmute(item, a_G, a_S, d = d_true),
    D2_pred_d_true_a = mix |> transmute(item, a_G = a_G_true, a_S = a_S_true, d)
  )
  for (nm in names(conds)) {
    t0 <- Sys.time()
    run <- run_cat(make_model(inp, all_items, conds[[nm]]), inp, n_items, cl)
    row <- bind_cols(tibble(K = K, rep = r, condition = nm), score_run(run, inp, tracked = hold),
                     tibble(mins = as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    write_csv(row, out_file, append = file.exists(out_file))
    print(row, width = Inf)
  }
}
stopCluster(cl)
cat("\nDONE\n")
