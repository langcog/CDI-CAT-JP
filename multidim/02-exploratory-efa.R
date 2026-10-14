## Exploratory 2-factor multidimensional IRT model on the noun+predicate
## item subset (478 items, 939 administrations), to check empirically
## whether items separate into noun- vs. predicate-loading factors before
## committing to a confirmatory/bifactor structure for the CAT.
##
## mirt() with model = 2 and no mirt.model formula fits an *unrotated*
## exploratory solution; summary(..., rotate = "oblimin") gives the rotated
## (oblique, since we expect correlated noun/predicate abilities) loadings
## and the inter-factor correlation.

library(tidyverse)
library(mirt)

d <- readRDS("multidim/noun_predicate_response_data.Rds")
d_mat_content <- d$d_mat_content
items_content <- d$items_content

set.seed(1234)
t0 <- Sys.time()
mod_efa2 <- mirt(
  data = d_mat_content, model = 2, itemtype = "2PL",
  verbose = TRUE, technical = list(NCYCLES = 2000)
)
cat("Fit time:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")

saveRDS(mod_efa2, "multidim/mod_efa2.Rds")

s <- summary(mod_efa2, rotate = "oblimin", suppress = 0)

loadings <- as.data.frame(s$rotF) %>%
  rownames_to_column("item_row") %>%
  mutate(WS = colnames(d_mat_content)) %>%
  left_join(items_content %>% select(WS, category_en, definition_en, pos2), by = "WS")

cat("\nFactor correlation (oblimin):\n")
print(s$fcor)

cat("\nMean rotated loadings by pos2 x factor:\n")
loadings %>%
  group_by(pos2) %>%
  summarise(mean_F1 = mean(F1), mean_F2 = mean(F2), n = n()) %>%
  print()

# which factor each item loads on more strongly, cross-tabbed against its
# a priori noun/predicate tag -- the key check for whether the 2-factor
# structure recovers the lexical-category split
loadings <- loadings %>% mutate(dominant_factor = if_else(abs(F1) > abs(F2), "F1", "F2"))
cat("\nDominant factor x pos2 crosstab:\n")
print(table(loadings$pos2, loadings$dominant_factor))

write_csv(loadings, "multidim/efa2_loadings.csv")

cat("\nModel fit (M2, RMSEA, TLI, CFI) -- may take a bit:\n")
print(M2(mod_efa2, type = "M2*", calcNULL = FALSE))
