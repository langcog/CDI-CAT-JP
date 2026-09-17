## American English data prep, via the shared pipeline (multidim_pipeline/lib.R).
## English (American) WS has 9093 administrations on Wordbank -- far larger
## than Japanese (939) or Cantonese (1205) -- so unlike those two languages
## this script only uses WS (no WG merge needed for adequate N), and the
## downstream EFA/bifactor fits (02/03) subsample for compute tractability
## (see those scripts for the reasoning).

source("multidim_pipeline/lib.R")

category_map <- default_category_map(extra_function_word_categories = "helping_verbs")

prep_wordbank_data(
  language = "English (American)", form = "WS",
  category_map = category_map,
  out_dir = "multidim_english"
)
