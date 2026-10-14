## Experiment 2: can predicted parameters serve as PRIORS for calibrating new verbs
## from a small pilot?
##
## Pure simulation under the fitted Japanese bifactor model (so the generating
## parameters are known exactly). Each replicate:
##   * draw K new verbs (their true parameters = the fitted values; the prediction
##     model is trained WITHOUT them, as it would be for genuinely new verbs);
##   * simulate N pilot children (theta ~ N(0, I) on G / S1 / S2) who answer the K
##     new verbs plus 120 already-calibrated "anchor" items (60 nouns, 60 predicates);
##   * calibrate the new verbs by marginal MAP with ALL anchor parameters fixed at
##     their true values, under three priors:
##        ML         no prior (starts at class means; bounded)
##        Prior_cls  normal prior centred on the class mean, SD = population SD
##        Prior_pred normal prior centred on the PREDICTED value (class + other-language
##                   AoA + Japanese frequency), SD = cross-validated prediction error
##   * score each estimate against the truth: parameter RMSE and "IRF error" -- the
##     RMS difference in P(produce) across the child population, which is what
##     actually matters for scoring.
## Baselines at N = 0 use the predicted / class-mean parameters with no pilot data.

source(here::here("item_difficulty_prediction/05-sim-helpers.R"))
library(parallel)

args <- commandArgs(trailingOnly = TRUE)
N_REP   <- if (length(args) >= 1) as.integer(args[1]) else 12
N_CORES <- if (length(args) >= 2) as.integer(args[2]) else 4
Ns      <- if (length(args) >= 3) as.integer(strsplit(args[3], ",")[[1]]) else c(25, 50, 100, 200, 400, 800)
K <- 20
inp <- load_inputs()
verbs <- inp$items$WS[inp$items$category_en == "action_words"]
true_par <- inp$bif_params |>
  transmute(item = WS, pos2, a_G = G, a_S = if_else(pos2 == "noun", S1_noun, S2_predicate), d)
pv_true <- mod2values(inp$mod_true)

# --- cross-validated prediction error and population SDs (verbs) -------------
cv <- map_dfr(verbs, \(v) {
  p <- predict_params(inp, v, "M7") |> left_join(true_par |> select(item, a_G_t = a_G, a_S_t = a_S, d_t = d), by = "item")
  tibble(e_aG = p$a_G - p$a_G_t, e_aS = p$a_S - p$a_S_t, e_d = p$d - p$d_t)
})
cv_sd  <- c(a_G = sqrt(mean(cv$e_aG^2)), a_S = sqrt(mean(cv$e_aS^2)), d = sqrt(mean(cv$e_d^2)))
pop_sd <- true_par |> filter(item %in% verbs) |> summarise(a_G = sd(a_G), a_S = sd(a_S), d = sd(d)) |> unlist()
cat("Prior SDs -- cross-validated prediction error:", round(cv_sd, 2), "| population SD:", round(pop_sd, 2), "\n")

set.seed(1)
theta_eval <- matrix(rnorm(4000 * 3), ncol = 3)   # fixed evaluation sample of children
irf_rmse <- function(est, truth) {
  sqrt(mean(map_dbl(seq_len(nrow(est)), \(i) {
    pe <- plogis(est$a_G[i] * theta_eval[, 1] + est$a_S[i] * theta_eval[, 3] + est$d[i])
    pt <- plogis(truth$a_G[i] * theta_eval[, 1] + truth$a_S[i] * theta_eval[, 3] + truth$d[i])
    mean((pe - pt)^2)
  })))
}
param_rmse <- function(est, truth) c(rmse_a_G = sqrt(mean((est$a_G - truth$a_G)^2)),
                                      rmse_a_S = sqrt(mean((est$a_S - truth$a_S)^2)),
                                      rmse_d   = sqrt(mean((est$d - truth$d)^2)))

# --- one calibration ---------------------------------------------------------
calibrate <- function(dat, items, hold, start, prior_mean = NULL, prior_sd = NULL) {
  grp <- as.integer(factor(true_par$pos2[match(items, true_par$item)], levels = c("noun", "predicate")))
  pv <- bfactor(dat, model = grp, itemtype = "2PL", pars = "values")
  key_new <- paste(pv$item, pv$name); key_old <- paste(pv_true$item, pv_true$name)
  pv$value <- pv_true$value[match(key_new, key_old)]
  pv$est <- FALSE
  for (i in seq_along(hold)) {
    it <- hold[i]
    for (nm in c("a1", "a3", "d")) {
      row <- which(pv$item == it & pv$name == nm)
      v <- switch(nm, a1 = start$a_G[i], a3 = start$a_S[i], d = start$d[i])
      pv$value[row] <- v; pv$est[row] <- TRUE
      if (nm %in% c("a1", "a3")) { pv$lbound[row] <- 0.05; pv$ubound[row] <- 20 } else { pv$lbound[row] <- -30; pv$ubound[row] <- 30 }
      if (!is.null(prior_mean)) {
        pv$prior.type[row] <- "norm"
        pv$prior_1[row] <- switch(nm, a1 = prior_mean$a_G[i], a3 = prior_mean$a_S[i], d = prior_mean$d[i])
        pv$prior_2[row] <- switch(nm, a1 = prior_sd[["a_G"]], a3 = prior_sd[["a_S"]], d = prior_sd[["d"]])
      }
    }
  }
  m <- suppressWarnings(bfactor(dat, model = grp, itemtype = "2PL", pars = pv, verbose = FALSE,
                                technical = list(NCYCLES = 400)))
  out <- mod2values(m)
  tibble(item = hold,
         a_G = map_dbl(hold, \(h) out$value[out$item == h & out$name == "a1"]),
         a_S = map_dbl(hold, \(h) out$value[out$item == h & out$name == "a3"]),
         d   = map_dbl(hold, \(h) out$value[out$item == h & out$name == "d"]))
}

one_rep <- function(rep, N) {
  set.seed(5000 + rep * 10 + match(N, c(0, Ns)))
  hold <- sample(verbs, K)
  p7 <- predict_params(inp, hold, "M7"); p0 <- predict_params(inp, hold, "M0")
  truth <- true_par |> slice(match(hold, item))
  base <- tibble(rep = rep, N = N)
  # no-pilot baselines
  if (N == 0) {
    return(bind_rows(
      bind_cols(base, method = "pred_only", as_tibble_row(param_rmse(p7, truth)), irf_rmse = irf_rmse(p7, truth)),
      bind_cols(base, method = "class_mean_only", as_tibble_row(param_rmse(p0, truth)), irf_rmse = irf_rmse(p0, truth))))
  }
  non_hold_pred <- setdiff(true_par$item[true_par$pos2 == "predicate"], c(hold))
  anchors <- c(sample(true_par$item[true_par$pos2 == "noun"], 60), sample(non_hold_pred, 60))
  items <- c(anchors, hold)
  tp <- true_par |> slice(match(items, item))
  Theta <- matrix(rnorm(N * 3), ncol = 3)
  amat <- cbind(tp$a_G, if_else(tp$pos2 == "noun", tp$a_S, 0), if_else(tp$pos2 == "predicate", tp$a_S, 0))
  dat <- simdata(a = amat, d = tp$d, N = N, itemtype = "2PL", Theta = Theta)
  colnames(dat) <- items
  methods <- list(
    ML         = \() calibrate(dat, items, hold, start = p0),
    Prior_cls  = \() calibrate(dat, items, hold, start = p0, prior_mean = p0, prior_sd = pop_sd),
    Prior_pred = \() calibrate(dat, items, hold, start = p7, prior_mean = p7, prior_sd = cv_sd))
  map_dfr(names(methods), \(nm) {
    est <- tryCatch(methods[[nm]](), error = \(e) NULL)
    if (is.null(est)) return(tibble())
    bind_cols(base, method = nm, as_tibble_row(param_rmse(est, truth)), irf_rmse = irf_rmse(est, truth))
  })
}

grid <- expand_grid(rep = seq_len(N_REP), N = c(0, Ns))
cat("Running", nrow(grid), "replicate-cells on", N_CORES, "cores\n")
t0 <- Sys.time()
res <- bind_rows(mclapply(seq_len(nrow(grid)), \(i) one_rep(grid$rep[i], grid$N[i]),
                          mc.cores = N_CORES, mc.preschedule = FALSE))
cat("Elapsed:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
write_csv(res, here::here("item_difficulty_prediction/prior_calibration_results.csv"))

summ <- res |> group_by(N, method) |>
  summarise(reps = n(), across(c(rmse_a_G, rmse_a_S, rmse_d, irf_rmse), \(x) mean(x)), .groups = "drop") |>
  arrange(N, method)
print(summ, n = 40, width = Inf)
