## Spanish (Mexican) data prep, via the shared pipeline (multidim_pipeline/lib.R).
## N=1682 on Wordbank -- comparable to Cantonese's scale, so (unlike English)
## no subsampling is needed; the full sample is used directly for EFA,
## bifactor, and the CAT simulation.
##
## Item bank is numerically identical to Japanese/English (478 content items:
## 312 noun, 166 predicate) -- same source CDI. Two new category names not
## seen in the other three languages ("prepositions", "states") are mapped to
## function_word.

source("multidim_pipeline/lib.R")

category_map <- default_category_map(extra_function_word_categories = c("prepositions", "states"))

prep_wordbank_data(
  language = "Spanish (Mexican)", form = "WS",
  category_map = category_map,
  out_dir = "multidim_spanish"
)
