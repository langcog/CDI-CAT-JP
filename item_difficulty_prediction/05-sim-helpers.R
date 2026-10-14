## Helpers for the "CAT with predicted item parameters" simulation.
##
## Truth: the fitted Japanese bifactor model (multidim/mod_bifactor.Rds; G +
## noun-specific S1 + predicate-specific S2) and the real (imputed) children's
## responses. A "believed" model is the same model with some items' parameters
## replaced; the CAT selects items and scores with the believed parameters while
## the responses always come from the real children.

suppressMessages({
  library(tidyverse)
  library(here)
  library(mirt)
  library(mirtCAT)
})

load_inputs <- function() {
  d <- readRDS(here("multidim/noun_predicate_response_data.Rds"))
  list(
    mod_true   = readRDS(here("multidim/mod_bifactor.Rds")),
    d_mat_imp  = readRDS(here("multidim/d_mat_content_imputed.Rds")),
    items      = d$items_content,                     # WS ids, in model column order
    d_demo     = d$d_demo,
    gold       = read_csv(here("multidim/bifactor_scores.csv"), show_col_types = FALSE),
    preds      = readRDS(here("item_difficulty_prediction/jp_predictor_table.rds")),
    bif_params = read_csv(here("multidim/bifactor_item_params.csv"), show_col_types = FALSE)
  )
}

## Rebuild a bifactor model on a subset of items (keep = WS ids) with chosen
## parameter overrides. `overrides` is a data frame with columns item, a_G, a_S, d
## (a_S is the item's OWN specific loading: S1 for nouns, S2 for predicates).
make_model <- function(inp, keep, overrides = NULL) {
  cols   <- match(keep, colnames(inp$d_mat_imp))
  grp    <- as.integer(factor(inp$items$pos2, levels = c("noun", "predicate")))[cols]
  dat    <- inp$d_mat_imp[, cols, drop = FALSE]
  pv_new <- bfactor(dat, model = grp, itemtype = "2PL", pars = "values")
  pv_old <- mod2values(inp$mod_true)

  key_new <- paste(pv_new$item, pv_new$name)
  key_old <- paste(pv_old$item, pv_old$name)
  pv_new$value <- pv_old$value[match(key_new, key_old)]
  stopifnot(!anyNA(pv_new$value))

  if (!is.null(overrides)) {
    pos2 <- setNames(inp$items$pos2, inp$items$WS)
    for (i in seq_len(nrow(overrides))) {
      it <- overrides$item[i]
      spec_name <- if (pos2[[it]] == "noun") "a2" else "a3"
      pv_new$value[pv_new$item == it & pv_new$name == "a1"]      <- overrides$a_G[i]
      pv_new$value[pv_new$item == it & pv_new$name == spec_name] <- overrides$a_S[i]
      pv_new$value[pv_new$item == it & pv_new$name == "d"]       <- overrides$d[i]
    }
  }
  pv_new$est <- FALSE
  bfactor(dat, model = grp, itemtype = "2PL", pars = pv_new)   # every est = FALSE: nothing to fit
}

## Predict bifactor parameters (a_G, own-specific a_S, intercept d) for held-out
## items from the same information a *new* verb would have: lexical class,
## other-language AoA, Japanese frequency. Training excludes every item sharing a
## concept with a held-out item. spec = "M7" (xl + freq) or "M0" (class means only).
predict_params <- function(inp, holdout, spec = c("M7", "M0")) {
  spec <- match.arg(spec)
  # the predictor table already carries each content item's bifactor a_G, a_S, d_bif
  tab <- inp$preds |> filter(item_id %in% inp$items$WS)
  terms <- if (spec == "M0") "lexical_class" else
    c("lexical_class", "xl_aoa_z", "xl_aoa_mean", "has_xl", "freq_lemma", "ctx_div", "freq_lemma_miss")
  hold_groups <- unique(tab$group[tab$item_id %in% holdout])
  train <- tab |> filter(!group %in% hold_groups)
  test  <- tab |> filter(item_id %in% holdout)
  test  <- test[match(holdout[holdout %in% test$item_id], test$item_id), ]   # keep caller's order

  pred_one <- function(target) {
    fit <- lm(as.formula(paste(target, "~", paste(terms, collapse = " + "))), data = train)
    p <- predict(fit, newdata = test)
    rng <- train |> filter(lexical_class %in% test$lexical_class) |> pull(all_of(target)) |> range()
    pmin(pmax(p, rng[1]), rng[2])   # never extrapolate beyond the observed parameter range
  }
  tibble(item = test$item_id, a_G = pred_one("a_G"), a_S = pred_one("a_S"), d = pred_one("d_bif"))
}

## Run a fixed-length CAT (every child gets exactly n_items) with a given
## believed model; return per-child final estimates and which items were given.
run_cat <- function(model, inp, n_items = 150, cl = NULL, criteria = "Drule", kids = NULL) {
  dat <- inp$d_mat_imp[, extract.mirt(model, "itemnames"), drop = FALSE]
  if (!is.null(kids)) dat <- dat[kids, , drop = FALSE]
  res <- mirtCAT(mo = model, criteria = criteria, start_item = criteria, method = "MAP",
                 local_pattern = dat, cl = cl,
                 design = list(min_items = n_items, max_items = n_items))
  est <- as.data.frame(do.call(rbind, lapply(res, \(r) c(t(summary(r)$final_estimates)))))
  names(est) <- c("G", "S1", "S2", "SE_G", "SE_S1", "SE_S2")
  items_given <- map(res, \(r) colnames(dat)[r$items_answered])
  list(est = est |> mutate(bias = S1 - S2), items_given = items_given)
}

## Compare a run against the gold standard (true model, all 478 items). `kids`
## = the row indices of the children the run was given (default: everyone).
score_run <- function(run, inp, tracked = character(0), kids = NULL) {
  if (is.null(kids)) kids <- seq_len(nrow(inp$gold))
  gold_bias <- (inp$gold$S1_noun - inp$gold$S2_predicate)[kids]
  tibble(
    r_bias    = cor(run$est$bias, gold_bias),
    rmse_bias = sqrt(mean((run$est$bias - gold_bias)^2)),
    r_G       = cor(run$est$G, inp$gold$G[kids]),
    rmse_G    = sqrt(mean((run$est$G - inp$gold$G[kids])^2)),
    share_tracked_items = if (length(tracked)) mean(map_dbl(run$items_given, \(x) mean(x %in% tracked))) else NA_real_
  )
}

## Calibrate `hold` (new verbs) from a pilot: marginal MAP with every OTHER item's
## parameters fixed at their true values. `raw` is the pilot children's OBSERVED
## responses (NA where not answered -- never imputed values, which would leak the
## truth). Prior (optional): normal, centred on prior_mean (data frame with a_G, a_S,
## d in `hold` order) with SDs prior_sd (named a_G, a_S, d).
calibrate_verbs <- function(inp, raw, hold, start, prior_mean = NULL, prior_sd = NULL) {
  # mirt cannot handle an item with a single observed response category (typical in a
  # small pilot: everyone, or no one, produced it). Such ANCHORS are simply dropped
  # (their parameters are fixed, so they carry no information about the new verbs);
  # a new VERB with no usable pilot signal keeps its starting values.
  has_var <- apply(raw, 2, \(x) length(unique(x[!is.na(x)])) > 1)
  est_verbs <- hold[has_var[hold]]
  idx <- match(est_verbs, hold)
  raw <- raw[, has_var, drop = FALSE]
  items <- colnames(raw)
  grp <- as.integer(factor(inp$items$pos2, levels = c("noun", "predicate")))[match(items, inp$items$WS)]
  pv <- bfactor(raw, model = grp, itemtype = "2PL", pars = "values")
  pv_true <- mod2values(inp$mod_true)
  pv$value <- pv_true$value[match(paste(pv$item, pv$name), paste(pv_true$item, pv_true$name))]
  pv$est <- FALSE
  for (k in seq_along(est_verbs)) for (nm in c("a1", "a3", "d")) {
    i <- idx[k]
    row <- which(pv$item == est_verbs[k] & pv$name == nm)
    pv$value[row] <- switch(nm, a1 = start$a_G[i], a3 = start$a_S[i], d = start$d[i])
    pv$est[row] <- TRUE
    if (nm == "d") { pv$lbound[row] <- -30; pv$ubound[row] <- 30 } else { pv$lbound[row] <- 0.05; pv$ubound[row] <- 20 }
    if (!is.null(prior_mean)) {
      pv$prior.type[row] <- "norm"
      pv$prior_1[row] <- switch(nm, a1 = prior_mean$a_G[i], a3 = prior_mean$a_S[i], d = prior_mean$d[i])
      pv$prior_2[row] <- switch(nm, a1 = prior_sd[["a_G"]], a3 = prior_sd[["a_S"]], d = prior_sd[["d"]])
    }
  }
  out_tab <- tibble(item = hold, a_G = start$a_G, a_S = start$a_S, d = start$d)   # fallback = start values
  if (length(est_verbs)) {
    m <- suppressWarnings(bfactor(raw, model = grp, itemtype = "2PL", pars = pv, verbose = FALSE,
                                  technical = list(NCYCLES = 400)))
    out <- mod2values(m)
    for (k in seq_along(est_verbs)) {
      i <- idx[k]
      out_tab$a_G[i] <- out$value[out$item == est_verbs[k] & out$name == "a1"]
      out_tab$a_S[i] <- out$value[out$item == est_verbs[k] & out$name == "a3"]
      out_tab$d[i]   <- out$value[out$item == est_verbs[k] & out$name == "d"]
    }
  }
  attr(out_tab, "n_unestimable_verbs") <- length(hold) - length(est_verbs)
  attr(out_tab, "n_dropped_anchors") <- sum(!has_var) - (length(hold) - length(est_verbs))
  out_tab
}
