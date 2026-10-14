## Data prep for a Cantonese noun/predicate bifactor CAT -- replicates
## multidim/01-noun-predicate-prep.R (Japanese) but pulls item metadata and
## administration data directly from Wordbank via wordbankr, since this repo
## has no local Cantonese response data.
##
## Cantonese Wordbank has only a WS form (16-30 months, no WG), 815 items.
## Item classification uses the same recipe as the Japanese script: Wordbank's
## own lexical_category already collapses verbs+adjectives into "predicates"
## for Cantonese too (same crosstab pattern as Japanese: action_words and
## descriptive_words are exactly the two categories mapped to "predicates"),
## so category_en lets us keep verbs and adjectives distinguishable (lexical_class)
## while also providing the 2-way noun/predicate collapse (pos2) used for the CAT.

library(tidyverse)
library(wordbankr)

# --- Item classification --------------------------------------------------

items_raw <- get_item_data(language = "Cantonese", form = "WS") %>%
  filter(item_kind == "word")

category_to_lexical_class <- c(
  animals = "noun", body_parts = "noun", clothing = "noun", food_drink = "noun",
  furniture_rooms = "noun", household = "noun", outside = "noun", toys = "noun",
  vehicles = "noun",
  action_words = "verb",
  descriptive_words = "adjective",
  connecting_words = "function_word", directions = "function_word",
  classifiers = "function_word", final_particles = "function_word",
  helping_verbs = "function_word", pronouns = "function_word",
  quantifiers = "function_word", question_words = "function_word"
  # everything else (games_routines, people, places, sounds, time_words) is
  # left unmapped -> "other"
)

items <- items_raw %>%
  mutate(
    lexical_class = coalesce(category_to_lexical_class[category], "other"),
    pos2 = case_when(
      lexical_class == "noun" ~ "noun",
      lexical_class %in% c("verb", "adjective") ~ "predicate",
      TRUE ~ "other"
    )
  )

cat("Item counts by lexical_class:\n")
print(table(items$lexical_class))
cat("\nItem counts by pos2 (2D CAT target):\n")
print(table(items$pos2))

# sanity check against Wordbank's own lexical_category coding
cat("\nCross-check vs wordbankr lexical_category (nouns/predicates should match exactly):\n")
print(table(items$pos2, items$lexical_category, useNA = "ifany"))

write_csv(
  items %>% select(item_id, category, english_gloss, uni_lemma, lexical_class, pos2),
  "multidim_cantonese/item_lexical_categories.csv"
)

# --- Response data ----------------------------------------------------------

admin <- get_administration_data(language = "Cantonese", form = "WS")
n_items_total <- nrow(items_raw)

too_few <- which(admin$production == 0)
ceiling <- which(admin$production == max(admin$production))
to_remove <- unique(c(too_few, ceiling))

cat("\nStarting N =", nrow(admin), "; removing", length(too_few), "non-producers and",
    length(ceiling), "at ceiling ->", nrow(admin) - length(to_remove), "retained.\n")

admin <- admin[-to_remove, ]

inst <- get_instrument_data(
  language = "Cantonese", form = "WS", items = NULL,
  administration_info = admin
)

# get_instrument_data() returns all item kinds (including sentence_structure /
# talk_functions / combine items beyond the 804 "word" items) -- restrict to
# the word items classified above before pivoting
d_wide <- inst %>%
  filter(item_id %in% items$item_id) %>%
  mutate(produces = as.integer(produces)) %>%
  select(data_id, item_id, produces) %>%
  pivot_wider(names_from = item_id, values_from = produces)

d_mat <- d_wide %>% select(-data_id) %>% data.matrix()
rownames(d_mat) <- d_wide$data_id

# reorder item metadata to match d_mat's column order
items <- items[match(colnames(d_mat), items$item_id), ]
stopifnot(all(items$item_id == colnames(d_mat)))

d_demo <- admin %>% filter(data_id %in% d_wide$data_id) %>%
  arrange(match(data_id, d_wide$data_id))
stopifnot(all(d_demo$data_id == d_wide$data_id))

content_idx <- which(items$pos2 %in% c("noun", "predicate"))
d_mat_content <- d_mat[, content_idx]
items_content <- items[content_idx, ]

cat("\nContent-word item bank (nouns + predicates):", ncol(d_mat_content), "items,",
    sum(items_content$pos2 == "noun"), "nouns,",
    sum(items_content$pos2 == "predicate"), "predicates.\n")
cat("NA proportion in content matrix:", round(100 * mean(is.na(d_mat_content)), 2), "%\n")

saveRDS(
  list(
    d_mat = d_mat, items = items,
    d_mat_content = d_mat_content, items_content = items_content,
    d_demo = d_demo
  ),
  "multidim_cantonese/noun_predicate_response_data.Rds"
)
