## CAT simulation: how much does it hurt to use PREDICTED item parameters for
## verbs we haven't calibrated?
##
## For K of the 103 Japanese CDI verbs (drawn at random, `n_rep` times each) we
## pretend they are new items and compare four banks, all run as a fixed-length
## 150-item Drule CAT over the real children's responses:
##   C   true parameters for every item                      (reference / ceiling)
##   A   the K verbs simply dropped from the bank            (what you'd have w/o them)
##   B7  the K verbs kept, with parameters PREDICTED from lexical class +
##       other-language AoA + Japanese frequency (trained without them)
##   B0  the K verbs kept, with CLASS-MEAN parameters       (no-information version)
## The CAT selects items and scores with the bank's (believed) parameters; the
## responses are always the real children's. Gold standard = the true model's
## full-bank (all 478 items) noun-vs-predicate bias score.

source(here::here("item_difficulty_prediction/05-sim-helpers.R"))
library(parallel)

inp <- load_inputs()
all_items <- inp$items$WS
verbs <- inp$items$WS[inp$items$category_en == "action_words"]
n_items <- 150
Ks <- c(20, 40, 60)
n_rep <- 3
out_file <- here::here("item_difficulty_prediction/cat_sim_results.csv")
if (file.exists(out_file)) file.remove(out_file)

cl <- makeCluster(min(12, detectCores() - 1))
append_row <- function(row) {
  write_csv(row, out_file, append = file.exists(out_file))
  print(row, width = Inf)
}
perr_na <- tibble(rmse_a_G = NA_real_, rmse_a_S = NA_real_, rmse_d = NA_real_)
timed <- function(expr) { t0 <- Sys.time(); val <- force(expr); list(val = val, mins = as.numeric(difftime(Sys.time(), t0, units = "mins"))) }

# --- C: reference -----------------------------------------------------------
tt <- timed(run_cat(make_model(inp, all_items), inp, n_items, cl))
cat("Reference run:", round(tt$mins, 1), "min\n")
append_row(bind_cols(tibble(K = 0L, rep = 0L, condition = "C_true_params"),
                     score_run(tt$val, inp), tibble(mins = tt$mins), perr_na))

# --- A / B7 / B0 ------------------------------------------------------------
for (K in Ks) for (r in seq_len(n_rep)) {
  set.seed(1000 * K + r)
  hold <- sample(verbs, K)
  p7 <- predict_params(inp, hold, "M7")
  p0 <- predict_params(inp, hold, "M0")
  truth <- inp$bif_params |> filter(WS %in% hold) |>
    transmute(item = WS, a_G_true = G, a_S_true = S2_predicate, d_true = d)
  perr <- function(p) p |> left_join(truth, by = "item") |>
    summarise(rmse_a_G = sqrt(mean((a_G - a_G_true)^2)), rmse_a_S = sqrt(mean((a_S - a_S_true)^2)),
              rmse_d = sqrt(mean((d - d_true)^2)))
  cat(sprintf("\n=== K = %d, rep %d | param RMSE  M7: %s | M0: %s\n", K, r,
              paste(round(unlist(perr(p7)), 2), collapse = "/"), paste(round(unlist(perr(p0)), 2), collapse = "/")))

  conds <- list(
    A_drop_verbs   = list(model = make_model(inp, setdiff(all_items, hold))),
    B7_predicted   = list(model = make_model(inp, all_items, p7)),
    B0_class_means = list(model = make_model(inp, all_items, p0))
  )
  for (nm in names(conds)) {
    tt <- timed(run_cat(conds[[nm]]$model, inp, n_items, cl))
    append_row(bind_cols(tibble(K = K, rep = r, condition = nm),
                         score_run(tt$val, inp, tracked = hold),
                         tibble(mins = tt$mins),
                         switch(nm, A_drop_verbs = perr_na, B7_predicted = perr(p7), B0_class_means = perr(p0))))
  }
}
stopCluster(cl)
cat("\nDONE\n")
