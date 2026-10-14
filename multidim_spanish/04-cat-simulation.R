## Spanish (Mexican) CAT simulation, full N=1616 sample.
##
## Applies the lesson from Cantonese/English directly instead of
## rediscovering it: min_SEM alone under-shoots, so min_items is set to a
## substantial fraction of each length's ceiling from the start (no separate
## "forced longer" follow-up run needed this time).

source("multidim_pipeline/lib.R")

mod <- readRDS("multidim_spanish/mod_bifactor.Rds")
d <- readRDS("multidim_spanish/noun_predicate_response_data.Rds")

full_bank_scores <- read_csv("multidim_spanish/bifactor_scores.csv", show_col_types = FALSE)
med_SE_S1 <- median(full_bank_scores$SE_S1_noun)
med_SE_S2 <- median(full_bank_scores$SE_S2_predicate)
cat("Full-bank SE ceiling (median): SE_G =", median(full_bank_scores$SE_G),
    " SE_S1_noun =", med_SE_S1, " SE_S2_predicate =", med_SE_S2, "\n")

min_SEM_target <- c(0.3, round(med_SE_S1 * 1.05, 2), round(med_SE_S2 * 1.05, 2))
cat("Using min_SEM =", paste(min_SEM_target, collapse = ", "), "\n")

lengths <- c(100, 200, 300)
min_items_vec <- round(lengths * 0.75)  # ~75 / 150 / 225

run_cat_pipeline(
  mod = mod, d_mat_content = d$d_mat_content, d_demo = d$d_demo,
  out_dir = "multidim_spanish",
  lengths = lengths, min_items = min_items_vec, min_SEM = min_SEM_target
)
