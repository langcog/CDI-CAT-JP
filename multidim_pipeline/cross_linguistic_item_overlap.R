## Cross-linguistic item overlap for the noun/predicate dimension.
##
## Question: are the same concepts (Wordbank's `uni_lemma` -- a
## language-independent identifier for a translation-equivalent item, e.g.
## "dog" links English "dog", Japanese inu, Cantonese gau2, Spanish perro)
## the most informative items for the noun-vs-predicate bias dimension
## across languages, or is "informativeness" for this dimension mostly
## language-specific?
##
## Operationalization: each language's confirmatory bifactor model gives
## every noun item a loading on S1 and every predicate item a loading on S2
## (bifactor_item_params.csv). This "own-specific-factor loading" is a
## direct, per-item measure of how much that item discriminates between
## noun-leaning and predicate-leaning children -- i.e. how useful the CAT
## would find it -- and is more directly comparable across languages than
## raw CAT exposure counts, which are path-dependent on the specific
## simulated administrations. (Item exposure counts from
## cat_simulation_results.Rds are a reasonable follow-up cross-check, since
## a Drule-selected CAT should preferentially expose high-loading items, but
## aren't used here.)
##
## Auto-discovers any multidim_<language>/ directory with a
## bifactor_item_params.csv, so re-running after adding a new language (e.g.
## Spanish) picks it up with no changes needed.

library(tidyverse)

find_language_dirs <- function(root = ".") {
  dirs <- list.dirs(root, recursive = FALSE)
  dirs <- dirs[grepl("^\\./multidim(_|$)", dirs) & !grepl("multidim_pipeline", dirs)]
  dirs[file.exists(file.path(dirs, "bifactor_item_params.csv"))]
}

## Japanese predates the shared pipeline and uses different column names
## (WS/category_en/definition_en instead of item_id/category/english_gloss)
## and lacks uni_lemma in its own item file -- both handled here rather than
## touching the already-committed multidim/ scripts.
load_language_items <- function(dir) {
  language <- basename(dir)
  params <- read_csv(file.path(dir, "bifactor_item_params.csv"), show_col_types = FALSE)

  if ("WS" %in% names(params)) {
    params <- params %>% rename(item_id = WS, category = category_en, english_gloss = definition_en)
  }

  item_meta <- read_csv(file.path(dir, "item_lexical_categories.csv"), show_col_types = FALSE)
  if (!"uni_lemma" %in% names(item_meta)) {
    stop("No uni_lemma in ", dir, "/item_lexical_categories.csv -- fetch it from wordbankr first.")
  }
  if ("WS" %in% names(item_meta)) item_meta <- item_meta %>% rename(item_id = WS)

  params %>%
    left_join(item_meta %>% select(item_id, uni_lemma), by = "item_id") %>%
    filter(pos2 %in% c("noun", "predicate"), !is.na(uni_lemma)) %>%
    mutate(
      language = language,
      own_specific_loading = if_else(pos2 == "noun", S1_noun, S2_predicate)
    ) %>%
    select(language, item_id, uni_lemma, pos2, own_specific_loading, english_gloss)
}

lang_dirs <- find_language_dirs()
cat("Languages found:", paste(basename(lang_dirs), collapse = ", "), "\n")

all_items <- map_dfr(lang_dirs, load_language_items)
write_csv(all_items, "multidim_pipeline/cross_linguistic_item_loadings.csv")

cat("\nuni_lemma coverage per language (content items with a non-missing uni_lemma):\n")
print(all_items %>% count(language, pos2))

## A few uni_lemma values map to >1 item within the same language (e.g. two
## CDI items sharing a base concept, like "fish (animal)"/"fish (food)")
## -- average their loadings before the cross-language join so each
## uni_lemma x pos2 x language cell is a single number.
dup_check <- all_items %>% count(language, uni_lemma, pos2) %>% filter(n > 1)
if (nrow(dup_check) > 0) {
  cat("\nNote:", nrow(dup_check), "uni_lemma had >1 item within a language (averaging their loadings):\n")
  print(dup_check)
}

items_dedup <- all_items %>%
  group_by(language, uni_lemma, pos2) %>%
  summarise(own_specific_loading = mean(own_specific_loading), .groups = "drop")

## wide table: one row per uni_lemma x pos2, one column per language's loading
wide <- items_dedup %>%
  pivot_wider(names_from = language, values_from = own_specific_loading)
write_csv(wide, "multidim_pipeline/cross_linguistic_item_loadings_wide.csv")

languages <- unique(all_items$language)
lang_pairs <- combn(languages, 2, simplify = FALSE)

cat("\nPairwise correlation of own-specific loadings (items with uni_lemma in both languages), by pos2:\n")
pair_cors <- map_dfr(lang_pairs, function(pair) {
  map_dfr(c("noun", "predicate"), function(p) {
    d <- wide %>% filter(pos2 == p) %>% select(uni_lemma, all_of(pair)) %>% drop_na()
    tibble(lang1 = pair[1], lang2 = pair[2], pos2 = p, n_shared = nrow(d),
           r = if (nrow(d) >= 3) cor(d[[pair[1]]], d[[pair[2]]]) else NA_real_)
  })
})
print(pair_cors, n = 50)
write_csv(pair_cors, "multidim_pipeline/cross_linguistic_pairwise_correlations.csv")

## top-N overlap: for each language pair, how many of the top 30 most
## discriminating items (by |loading|, separately for noun/predicate) share
## a uni_lemma, vs. the number expected if item informativeness were
## independent across languages (a simple hypergeometric baseline).
top_n <- 30
top_items <- items_dedup %>%
  group_by(language, pos2) %>%
  slice_max(own_specific_loading, n = top_n) %>%
  ungroup()

overlap_tab <- map_dfr(lang_pairs, function(pair) {
  map_dfr(c("noun", "predicate"), function(p) {
    pool_size <- wide %>% filter(pos2 == p) %>%
      select(uni_lemma, all_of(pair)) %>% drop_na() %>% nrow()
    l1 <- top_items %>% filter(language == pair[1], pos2 == p) %>% pull(uni_lemma)
    l2 <- top_items %>% filter(language == pair[2], pos2 == p) %>% pull(uni_lemma)
    observed <- length(intersect(l1, l2))
    # expected overlap under random draws of top_n from a shared pool of pool_size
    expected <- if (pool_size > 0) top_n * top_n / pool_size else NA_real_
    tibble(lang1 = pair[1], lang2 = pair[2], pos2 = p, top_n = top_n,
           pool_size = pool_size, observed_overlap = observed,
           expected_overlap_by_chance = round(expected, 1))
  })
})
cat("\nTop-", top_n, " most-discriminating-item overlap vs. chance expectation:\n", sep = "")
print(overlap_tab, n = 50)
write_csv(overlap_tab, "multidim_pipeline/cross_linguistic_top_item_overlap.csv")
