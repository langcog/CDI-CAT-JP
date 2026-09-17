## Confirmatory bifactor model for Cantonese: general factor (G) + orthogonal
## noun-specific (S1) and predicate-specific (S2) factors. Mirrors
## multidim/03-bifactor-confirmatory.R (Japanese). Run independently of (and
## concurrently with) 02-exploratory-efa.R -- the confirmatory structure here
## doesn't depend on the EFA's output, only on the data prep.

library(tidyverse)
library(mirt)

d <- readRDS("multidim_cantonese/noun_predicate_response_data.Rds")
d_mat_content <- d$d_mat_content
items_content <- d$items_content

stopifnot(all(items_content$item_id == colnames(d_mat_content)))
specific_group <- as.integer(factor(items_content$pos2, levels = c("noun", "predicate")))

set.seed(1234)
t0 <- Sys.time()
mod_bifactor <- bfactor(
  data = d_mat_content, model = specific_group, itemtype = "2PL",
  verbose = TRUE, technical = list(NCYCLES = 2000)
)
cat("Fit time:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
cat("Converged:", extract.mirt(mod_bifactor, "converged"), "\n")

saveRDS(mod_bifactor, "multidim_cantonese/mod_bifactor.Rds")

# a1 = G, a2 = S1 (noun-specific, 0 for predicates), a3 = S2 (predicate-specific, 0 for nouns)
co <- coef(mod_bifactor, simplify = TRUE)$items %>%
  as.data.frame() %>%
  rownames_to_column("item_id") %>%
  rename(G = a1, S1_noun = a2, S2_predicate = a3) %>%
  left_join(items_content %>% select(item_id, category, english_gloss, pos2), by = "item_id")
write_csv(co, "multidim_cantonese/bifactor_item_params.csv")

cat("\nMean general-factor (G) and own-specific-factor loading by pos2:\n")
co %>%
  mutate(own_specific = if_else(pos2 == "noun", S1_noun, S2_predicate)) %>%
  group_by(pos2) %>%
  summarise(mean_G = mean(G), mean_own_specific = mean(own_specific), n = n()) %>%
  print()

fs <- fscores(mod_bifactor, method = "EAP", full.scores = TRUE, full.scores.SE = TRUE)
cat("\nEmpirical reliability (G, S1_noun, S2_predicate):\n")
print(empirical_rxx(fs))

colnames(fs) <- c("G", "S1_noun", "S2_predicate", "SE_G", "SE_S1_noun", "SE_S2_predicate")
scores <- as.data.frame(fs) %>%
  mutate(noun_predicate_bias = S1_noun - S2_predicate) %>%
  bind_cols(d$d_demo %>% select(data_id, age, production))
write_csv(scores, "multidim_cantonese/bifactor_scores.csv")

cat("\nCorrelation of noun/predicate bias (S1-S2) with total production and age:\n")
print(cor(scores %>% select(noun_predicate_bias, production, age), use = "complete.obs"))

cat("\nCorrelation of general factor score with total production:\n")
print(cor(scores$G, scores$production))

cat("\nEmpirical correlation between S1_noun and S2_predicate EAP scores:\n")
print(cor(scores$S1_noun, scores$S2_predicate))
