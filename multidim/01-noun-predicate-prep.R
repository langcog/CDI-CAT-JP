## Data prep for a multidimensional (noun vs. predicate) CAT.
##
## Goal: measure a "lexical category bias" dimension (noun-leaning vs.
## predicate[verb+adjective]-leaning vocabulary) alongside general
## vocabulary ability, so that mirtCAT can adaptively estimate both.
##
## Item classification follows the CDI semantic category -> lexical class
## mapping used by Wordbank/wordbankr (verified against
## wordbankr::get_item_data(language = "Japanese", form = "WS")$lexical_category,
## which itself collapses Japanese verbs+adjectives into "predicates" because
## Japanese i-adjectives conjugate like verbs). We keep verbs and adjectives
## separate here (lexical_class) so a 3-way split can be explored later, and
## also provide the 2-way collapse (pos2) used for the noun-vs-predicate model.
##
## Sample and exclusion criteria are identical to adaptive_cdi_production.Rmd
## so that this item bank's estimates stay comparable to the existing
## unidimensional production CAT.

library(tidyverse)

# --- Item classification --------------------------------------------------

category_to_lexical_class <- c(
  animals = "noun", body_parts = "noun", clothing = "noun", food_drink = "noun",
  furniture_rooms = "noun", household = "noun", outside = "noun", toys = "noun",
  vehicles = "noun",
  action_words = "verb",
  descriptive_words = "adjective",
  connecting_words = "function_word", locations = "function_word",
  pronouns = "function_word", quantifiers = "function_word",
  question_words = "function_word"
  # everything else (conversations, games_routines, others, people, places,
  # sounds, sounds2, time_words) is left unmapped -> "other"
)

items <- read_csv("WS_WG_items_combined.csv", show_col_types = FALSE) %>%
  mutate(
    lexical_class = coalesce(category_to_lexical_class[category_en], "other"),
    pos2 = case_when(
      lexical_class == "noun" ~ "noun",
      lexical_class %in% c("verb", "adjective") ~ "predicate",
      TRUE ~ "other"
    )
  )

stopifnot(nrow(items) == 711)

cat("Item counts by lexical_class:\n")
print(table(items$lexical_class))
cat("\nItem counts by pos2 (2D CAT target):\n")
print(table(items$pos2))

write_csv(
  items %>% select(WS, WG, category_en, definition_en, lexical_class, pos2),
  "multidim/item_lexical_categories.csv"
)

# --- Response data ----------------------------------------------------------
# same combined WG+WS production data (mapped onto the 711 WS items) and the
# same participant exclusions as adaptive_cdi_production.Rmd

load("data/Japanese-CDI-WS-WG-combined-data-disambig.Rdata")  # d_wide, d_demo

d_mat <- d_wide %>%
  data.frame() %>%
  select(-id) %>%
  data.matrix()

# reorder item metadata to match d_mat's column order (WS ids, not sorted the
# same way in the two sources)
items <- items[match(colnames(d_mat), items$WS), ]
stopifnot(all(items$WS == colnames(d_mat)))

too_young <- which(d_demo$age < 12)
too_old <- which(d_demo$age > 38)
no_words <- which(d_demo$production == 0 & d_demo$age >= 12)
ceiling <- which(d_demo$production == 711)
to_remove <- unique(c(too_young, too_old, no_words, ceiling))

d_demo <- d_demo[-to_remove, ] %>%
  mutate(age_mos = round(age), data_id = paste0(id, "-", age_mos))
d_mat <- d_mat[-to_remove, ]
rownames(d_mat) <- d_demo$data_id

cat("\nRetained", nrow(d_demo), "administrations (", sum(d_demo$form == "WS"),
    "WS,", sum(d_demo$form == "WG"), "WG ) after exclusions.\n")

# full response matrix (all 711 items, tagged) + a content-word-only matrix
# (nouns + predicates) for fitting the noun/predicate factor model directly
content_idx <- which(items$pos2 %in% c("noun", "predicate"))
d_mat_content <- d_mat[, content_idx]
items_content <- items[content_idx, ]

cat("Content-word item bank (nouns + predicates):", ncol(d_mat_content), "items,",
    sum(items_content$pos2 == "noun"), "nouns,",
    sum(items_content$pos2 == "predicate"), "predicates.\n")

saveRDS(
  list(
    d_mat = d_mat, items = items,                     # full 711-item bank, all tagged
    d_mat_content = d_mat_content, items_content = items_content,  # noun+predicate subset
    d_demo = d_demo
  ),
  "multidim/noun_predicate_response_data.Rds"
)
