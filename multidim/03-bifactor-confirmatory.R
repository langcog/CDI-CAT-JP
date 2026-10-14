## Confirmatory bifactor model: one general (vocabulary) factor that every
## item loads on, plus one orthogonal specific factor contrasting nouns vs.
## predicates (verbs+adjectives). The specific-factor score IS the noun/
## predicate bias measure -- unlike the exploratory 2-factor model, it's
## constructed to be independent of general ability by design, not by hoping
## rotation finds that structure (which it did only partially: F1/F2 were
## r = -.015, but F1 absorbed almost all variance in every item).
##
## mirt::bfactor() takes a group vector assigning each item to its specific
## factor; every item additionally loads on the general factor automatically.

library(tidyverse)
library(mirt)

d <- readRDS("multidim/noun_predicate_response_data.Rds")
d_mat_content <- d$d_mat_content
items_content <- d$items_content

stopifnot(all(items_content$WS == colnames(d_mat_content)))
# bfactor() requires a *numeric* group vector: 1 = noun, 2 = predicate
specific_group <- as.integer(factor(items_content$pos2, levels = c("noun", "predicate")))

set.seed(1234)
t0 <- Sys.time()
mod_bifactor <- bfactor(
  data = d_mat_content, model = specific_group, itemtype = "2PL",
  verbose = TRUE, technical = list(NCYCLES = 2000)
)
cat("Fit time:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
cat("Converged:", extract.mirt(mod_bifactor, "converged"), "\n")

saveRDS(mod_bifactor, "multidim/mod_bifactor.Rds")

# --- coefficients --------------------------------------------------------
# a1 = general-factor loading (G); a2 = specific-factor-1 loading (S1, nouns
# only, 0 for predicates); a3 = specific-factor-2 loading (S2, predicates
# only, 0 for nouns) -- bfactor()'s dimension-reduction parameterization.
co <- coef(mod_bifactor, simplify = TRUE)$items %>%
  as.data.frame() %>%
  rownames_to_column("WS") %>%
  rename(G = a1, S1_noun = a2, S2_predicate = a3) %>%
  left_join(items_content %>% select(WS, category_en, definition_en, pos2), by = "WS")
write_csv(co, "multidim/bifactor_item_params.csv")

cat("\nMean general-factor (G) and own-specific-factor loading by pos2:\n")
co %>%
  mutate(own_specific = if_else(pos2 == "noun", S1_noun, S2_predicate)) %>%
  group_by(pos2) %>%
  summarise(mean_G = mean(G), mean_own_specific = mean(own_specific), n = n()) %>%
  print()

# --- factor scores (general ability + noun/predicate bias per child) ---
# fscores() on a bfactor object returns G, S1, S2 (+ their SEs) even though
# estimation used the dimension-reduction algorithm internally.
fs <- fscores(mod_bifactor, method = "EAP", full.scores = TRUE, full.scores.SE = TRUE)
cat("\nEmpirical reliability (G, S1_noun, S2_predicate):\n")
print(empirical_rxx(fs))

colnames(fs) <- c("G", "S1_noun", "S2_predicate", "SE_G", "SE_S1_noun", "SE_S2_predicate")
scores <- as.data.frame(fs) %>%
  mutate(noun_predicate_bias = S1_noun - S2_predicate) %>%
  bind_cols(d$d_demo %>% select(data_id, age_mos, production, form))
write_csv(scores, "multidim/bifactor_scores.csv")

cat("\nCorrelation of noun/predicate bias (S1-S2) with total production and age:\n")
print(cor(scores %>% select(noun_predicate_bias, production, age_mos), use = "complete.obs"))

cat("\nCorrelation of general factor score with total production:\n")
print(cor(scores$G, scores$production))

# Note: the bifactor *model* constrains S1 and S2 to be latently orthogonal
# to each other and to G, but their EAP *score* estimates can still show a
# non-trivial empirical correlation (estimation noise shared across items) --
# worth checking rather than assuming it will be ~0.
cat("\nEmpirical correlation between S1_noun and S2_predicate EAP scores:\n")
print(cor(scores$S1_noun, scores$S2_predicate))

cat("\nEmpirical reliability:\n")
print(empirical_rxx(fs))
