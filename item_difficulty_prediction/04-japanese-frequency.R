## Japanese frequency for every CDI item, from adult speech in the Japanese
## CHILDES corpora (childes-db, language "jpn", target child excluded).
##
## childes-db gives each utterance a space-aligned triple: gloss (surface tokens),
## stem (UD-style dictionary-form lemma, often kanji: surface マンガ -> lemma 漫画)
## and part_of_speech. ~74% of utterances have Japanese-script, aligned triples
## (the rest are romanized-only or misaligned and are skipped).
##
## Matching CDI items (written in kana/kanji, with optional kanji disambiguation in
## parentheses, e.g. "いる（居る）") to lemmas:
##   * if the item has a kanji form in parentheses, use that lemma only (this keeps
##     homophones like いる（居る）/いる（要る） apart);
##   * otherwise use the item's base form as a lemma AND every lemma that tokens
##     with that surface form were tagged with (kana -> kanji-lemma bridge).
## Counts are summed over all inflected forms of the lemma.
##
## Predictors produced (all would be computable for a *new* verb given only its
## Japanese dictionary form):
##   freq_lemma    log((lemma count + 1) / total tokens)
##   ctx_div       log(1 + number of distinct transcripts the lemma occurs in)
##   freq_surface  log((count of the bare citation form as a surface token + 1) / total)

suppressMessages({
  library(tidyverse)
  library(here)
})

cache_dir <- here("item_difficulty_prediction", "cache", "childes")
u <- readRDS(file.path(cache_dir, "utterances_jpn.rds"))

has_jp <- function(x) str_detect(replace_na(x, ""), "\\p{Hiragana}|\\p{Katakana}|\\p{Han}")
n_tok  <- function(x) str_count(replace_na(x, ""), "\\S+")

usable <- u |>
  mutate(n_g = n_tok(gloss), n_s = n_tok(stem), n_p = n_tok(part_of_speech)) |>
  filter(has_jp(gloss), n_g > 0, n_g == n_s, n_g == n_p)

toks <- usable |>
  transmute(transcript_id, g = str_split(gloss, "\\s+"), s = str_split(stem, "\\s+")) |>
  unnest(c(g, s))
total_tokens <- nrow(toks)
cat("Usable utterances:", nrow(usable), "of", nrow(u), "| tokens:", total_tokens, "\n")

lemma_tab <- toks |>
  group_by(s) |>
  summarise(n = n(), n_transcripts = n_distinct(transcript_id), .groups = "drop")
surface_tab <- toks |> count(g, name = "n_surface")
bridge <- toks |> distinct(g, s)   # which lemmas a given surface form was tagged with

# --- CDI items -> lemma sets -------------------------------------------------
jp <- read_csv(here("item_difficulty_prediction/jp_item_table.csv"), show_col_types = FALSE) |>
  select(item_id, definition_ja1, lexical_class)

paren_content <- function(x) str_match(x, "[（(](.*?)[）)]")[, 2]
items <- jp |>
  mutate(base = str_remove_all(definition_ja1, "[（(].*?[）)]") |> str_trim(),
         paren = paren_content(definition_ja1),
         kanji_form = if_else(str_detect(replace_na(paren, ""), "\\p{Han}"), paren, NA_character_))

lemma_set <- function(base, kanji_form) {
  if (!is.na(kanji_form)) {
    s <- kanji_form
    if (!any(lemma_tab$s %in% s)) s <- union(s, c(base, bridge$s[bridge$g == base]))  # fallback
    return(unique(s))
  }
  unique(c(base, bridge$s[bridge$g == base]))
}

freqs <- items |>
  mutate(lemmas = map2(base, kanji_form, lemma_set),
         count_lemma = map_dbl(lemmas, \(L) sum(lemma_tab$n[lemma_tab$s %in% L])),
         transcripts = map_dbl(lemmas, \(L) max(0, lemma_tab$n_transcripts[lemma_tab$s %in% L])),
         count_surface = map_dbl(base, \(b) sum(surface_tab$n_surface[surface_tab$g == b])),
         freq_lemma = log((count_lemma + 1) / total_tokens),
         ctx_div = log(1 + transcripts),
         freq_surface = log((count_surface + 1) / total_tokens),
         n_lemmas_matched = map_int(lemmas, length))

write_csv(freqs |> select(item_id, base, kanji_form, count_lemma, count_surface, transcripts,
                          freq_lemma, ctx_div, freq_surface, n_lemmas_matched),
          here("item_difficulty_prediction/jp_frequency.csv"))

cat("\nShare of items with any lemma match (count > 0), by class:\n")
print(freqs |> group_by(lexical_class) |>
        summarise(n = n(), matched = round(mean(count_lemma > 0), 2),
                  median_count = median(count_lemma), .groups = "drop"))

# sanity: does frequency relate to local AoA the way it should?
aoa <- read_csv(here("item_difficulty_prediction/jp_item_table.csv"), show_col_types = FALSE) |>
  select(item_id, aoa_local, b_2pl)
chk <- freqs |> left_join(aoa, by = "item_id") |> filter(count_lemma > 0)
cat("\nCorrelation of log lemma frequency with local AoA (items with matches):\n")
print(chk |> group_by(lexical_class) |>
        summarise(n = n(), r_freq_aoa = round(cor(freq_lemma, aoa_local), 2),
                  r_ctx_aoa = round(cor(ctx_div, aoa_local), 2),
                  r_surface_aoa = round(cor(freq_surface, aoa_local), 2), .groups = "drop"))
cat("\nMost / least frequent verbs:\n")
vv <- freqs |> filter(lexical_class == "verb") |> arrange(desc(count_lemma))
print(vv |> slice(c(1:6, (n() - 5):n())) |> select(definition_ja1, count_lemma, count_surface, n_lemmas_matched))
