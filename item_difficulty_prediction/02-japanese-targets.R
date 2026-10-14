## Japanese item table for the "can we predict item parameters for items we
## haven't calibrated?" question. One row per CDI:WS word item, with
##  - prediction TARGETS: unidimensional-2PL difficulty b / discrimination a /
##    logit intercept d (JP_production_2PL_params*.csv), bifactor G/S/d for the
##    478 noun+predicate items (multidim/bifactor_item_params.csv), and AoA in
##    months estimated from our own response data with the aoa-pipeline's own
##    fit_bglm() (so it is on the same footing as other languages' AoAs)
##  - uni_lemma (the join key to other languages' CDIs), lexical class
##  - Japanese word-FORM predictors computable from the item string alone
##    (mora count from the romanization, character count of the kana/kanji form)

suppressMessages({
  library(tidyverse)
  library(here)
})

aoa_repo <- "/Users/gkacherg/Documents/GitHub/aoa-pipeline"
source(file.path(aoa_repo, "scripts", "[5] fit_models.R"))   # fit_bglm(), aoa_se()

# --- item metadata ----------------------------------------------------------
items <- read_csv(here("WS_WG_items_combined.csv"), show_col_types = FALSE) %>%
  select(item_id = WS, category_en, definition_ja1, definition_ja2, definition_en)
uni <- read_csv(here("multidim/item_lexical_categories.csv"), show_col_types = FALSE) %>%
  select(item_id = WS, lexical_class, pos2, uni_lemma)
p2 <- read_csv(here("JP_production_2PL_params_slopeint.csv"), show_col_types = FALSE) %>%
  select(item_id, a_2pl = a1, d_2pl = d) %>%
  mutate(b_2pl = -d_2pl / a_2pl)
bif <- read_csv(here("multidim/bifactor_item_params.csv"), show_col_types = FALSE) %>%
  transmute(item_id = WS, a_G = G, a_S = if_else(pos2 == "noun", S1_noun, S2_predicate), d_bif = d)

# --- word-form predictors ---------------------------------------------------
count_morae <- function(romaji) {
  s <- romaji |> str_remove_all("[（(].*?[）)]") |> str_to_lower() |> str_replace_all("[^a-z]", "")
  str_count(s, "[aiueo]") +                 # each vowel = 1 mora (long vowels written as 2 vowels)
    str_count(s, "n(?![aiueoy])") +         # moraic n
    str_count(s, "([kstpgzdbhfjcmrw])\\1")  # sokuon (geminate consonant)
}
clean_ja <- function(x) x |> str_remove_all("[（(].*?[）)]") |> str_trim()

items <- items %>%
  mutate(n_mora = count_morae(definition_ja2),
         n_char_ja = str_length(clean_ja(definition_ja1)),
         has_kanji = str_detect(clean_ja(definition_ja1), "\\p{Han}"),
         is_katakana = str_detect(clean_ja(definition_ja1), "^[\\p{Katakana}ー]+$"))

# --- local AoA from our own data, with the pipeline's own estimator ---------
d <- readRDS(here("multidim/noun_predicate_response_data.Rds"))
resp <- as.data.frame(d$d_mat) %>%
  mutate(age = d$d_demo$age_mos) %>%
  pivot_longer(-age, names_to = "item_id", values_to = "y", values_drop_na = TRUE) %>%
  group_by(item_id, age) %>%
  summarise(num_true = sum(y), total = n(), .groups = "drop") %>%
  mutate(num_false = total - num_true)

aoa_local <- resp %>%
  nest(data = -item_id) %>%
  mutate(fit = map(data, \(df) tryCatch(fit_bglm(df), error = \(e) tibble(intercept = NA, slope = NA, aoa = NA, aoa_se = NA)))) %>%
  select(-data) %>%
  unnest(fit) %>%
  select(item_id, aoa_local = aoa, aoa_local_se = aoa_se)

jp <- items %>%
  left_join(uni, by = "item_id") %>%
  left_join(p2, by = "item_id") %>%
  left_join(bif, by = "item_id") %>%
  left_join(aoa_local, by = "item_id")

write_csv(jp, here("item_difficulty_prediction/jp_item_table.csv"))

cat("Rows:", nrow(jp), "\n")
cat("\nTarget summaries by lexical class:\n")
print(jp %>% group_by(lexical_class) %>%
        summarise(n = n(), n_uni_lemma = sum(!is.na(uni_lemma)),
                  sd_b = round(sd(b_2pl), 3), sd_d = round(sd(d_2pl), 2),
                  sd_aoa = round(sd(aoa_local, na.rm = TRUE), 2),
                  mean_aoa = round(mean(aoa_local, na.rm = TRUE), 1)))
cat("\nCorrelations among targets (all items):\n")
print(round(cor(jp %>% select(b_2pl, d_2pl, a_2pl, aoa_local, n_mora, n_char_ja), use = "pairwise"), 2))
