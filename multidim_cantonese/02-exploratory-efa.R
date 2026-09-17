## Exploratory 2-factor multidimensional IRT model on the Cantonese
## noun+predicate item subset (572 items, 1205 administrations) -- mirrors
## multidim/02-exploratory-efa.R (Japanese). Diagnostic check of whether the
## a priori noun/predicate split is empirically recoverable before fitting a
## confirmatory bifactor model, done separately per language rather than
## assumed to transfer from the Japanese result.

library(tidyverse)
library(mirt)

d <- readRDS("multidim_cantonese/noun_predicate_response_data.Rds")
d_mat_content <- d$d_mat_content
items_content <- d$items_content

set.seed(1234)
t0 <- Sys.time()
mod_efa2 <- mirt(
  data = d_mat_content, model = 2, itemtype = "2PL",
  verbose = TRUE, technical = list(NCYCLES = 2000)
)
cat("Fit time:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
cat("Converged:", extract.mirt(mod_efa2, "converged"), "\n")

saveRDS(mod_efa2, "multidim_cantonese/mod_efa2.Rds")

s <- summary(mod_efa2, rotate = "oblimin", suppress = 0)

loadings <- as.data.frame(s$rotF) %>%
  rownames_to_column("item_row") %>%
  mutate(item_id = colnames(d_mat_content)) %>%
  left_join(items_content %>% select(item_id, category, english_gloss, pos2), by = "item_id")

cat("\nFactor correlation (oblimin):\n")
print(s$fcor)

cat("\nMean rotated loadings by pos2 x factor:\n")
loadings %>%
  group_by(pos2) %>%
  summarise(mean_F1 = mean(F1), mean_F2 = mean(F2), n = n()) %>%
  print()

loadings <- loadings %>% mutate(dominant_factor = if_else(abs(F1) > abs(F2), "F1", "F2"))
cat("\nDominant factor x pos2 crosstab:\n")
print(table(loadings$pos2, loadings$dominant_factor))

cat("\nWelch t-test, F2 noun vs predicate:\n")
print(t.test(F2 ~ pos2, data = loadings %>% filter(pos2 %in% c("noun", "predicate"))))

write_csv(loadings, "multidim_cantonese/efa2_loadings.csv")
