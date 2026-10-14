## Re-fit of the 1-group ("single Bias dimension") model, using the
## already-computed exploratory 2-factor loadings (multidim/efa2_loadings.csv)
## as informed starting values, after the first attempt (default starts)
## failed to converge and landed on a degenerate solution (G loadings -1 to
## -21, both nouns and predicates loading positively on "Bias" instead of
## splitting bipolar). The EFA already found a clean, well-separated
## noun+/predicate- F2 with near-zero F1/F2 correlation, so it's a
## reasonable, principled starting point rather than mirt's defaults.
##
## Stays with bfactor() (not plain mirt() with a free G-Bias correlation) --
## bfactor's block-diagonal identification (specific factor orthogonal to G
## by construction) is what makes the 1-group specification well-identified
## without needing arbitrary zero-loading anchors; a freely-correlated dense
## 2-factor mirt() model would reintroduce rotational-indeterminacy risk.

library(tidyverse)
library(mirt)

d <- readRDS("multidim/noun_predicate_response_data.Rds")
d_mat_content <- d$d_mat_content
items_content <- d$items_content

efa <- read_csv("multidim/efa2_loadings.csv", show_col_types = FALSE)
efa <- efa[match(colnames(d_mat_content), efa$WS), ]
stopifnot(all(efa$WS == colnames(d_mat_content)))
if (mean(efa$F2[efa$pos2 == "noun"]) < 0) efa$F2 <- -efa$F2
cat("EFA starting orientation check: noun mean F2 =", round(mean(efa$F2[efa$pos2 == "noun"]), 3),
    " predicate mean F2 =", round(mean(efa$F2[efa$pos2 == "predicate"]), 3), "\n")

specific_group <- rep(1L, ncol(d_mat_content))
pv <- bfactor(d_mat_content, model = specific_group, itemtype = "2PL", pars = "values")
pv$value[pv$name == "a1"] <- pmax(efa$F1, 0.05) * 3   # rough EFA-loading -> 2PL-slope rescale
pv$value[pv$name == "a2"] <- efa$F2 * 3

set.seed(1234)
t0 <- Sys.time()
mod_1group_v2 <- bfactor(
  d_mat_content, model = specific_group, itemtype = "2PL",
  pars = pv, verbose = TRUE, technical = list(NCYCLES = 2000)
)
cat("Fit time:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
cat("Converged:", extract.mirt(mod_1group_v2, "converged"), "\n")

co <- coef(mod_1group_v2, simplify = TRUE)$items %>%
  as.data.frame() %>% rownames_to_column("item_id") %>%
  rename(G = a1, Bias = a2) %>%
  left_join(items_content %>% rename(item_id = WS) %>% select(item_id, pos2), by = "item_id")
cat("\nMean G / Bias loading by item type:\n")
print(co %>% group_by(pos2) %>% summarise(mean_G = mean(G), mean_Bias = mean(Bias), n = n()))
cat("\nG range:\n"); print(summary(co$G))
cat("logLik:", extract.mirt(mod_1group_v2, "logLik"), " AIC:", extract.mirt(mod_1group_v2, "AIC"), "\n")

saveRDS(mod_1group_v2, "multidim_pipeline/mod_bifactor_1group_japanese_v2.Rds")
write_csv(co, "multidim_pipeline/bifactor_1group_v2_item_params_japanese.csv")
