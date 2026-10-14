## American English CAT simulation on the FULL sample (N=8998) bifactor
## model from 07-full-sample-bifactor.R. Applies the by-now-established
## design directly (Drule, min_items ~75% of max_items, min_SEM from this
## model's own full-bank ceiling) rather than rediscovering it.

source("multidim_pipeline/lib.R")

mod <- readRDS("multidim_english/fullsample/mod_bifactor.Rds")
d <- readRDS("multidim_english/noun_predicate_response_data.Rds")

full_bank_scores <- read_csv("multidim_english/fullsample/bifactor_scores.csv", show_col_types = FALSE)
med_SE_S1 <- median(full_bank_scores$SE_S1_noun)
med_SE_S2 <- median(full_bank_scores$SE_S2_predicate)
cat("Full-bank SE ceiling (median): SE_G =", median(full_bank_scores$SE_G),
    " SE_S1_noun =", med_SE_S1, " SE_S2_predicate =", med_SE_S2, "\n")

min_SEM_target <- c(0.3, round(med_SE_S1 * 1.05, 2), round(med_SE_S2 * 1.05, 2))
cat("Using min_SEM =", paste(min_SEM_target, collapse = ", "), "\n")

lengths <- c(100, 200, 300)
min_items_vec <- round(lengths * 0.75)

run_cat_pipeline(
  mod = mod, d_mat_content = d$d_mat_content, d_demo = d$d_demo,
  out_dir = "multidim_english/fullsample",
  lengths = lengths, min_items = min_items_vec, min_SEM = min_SEM_target
)
