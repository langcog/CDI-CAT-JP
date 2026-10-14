## American English CAT simulation, on the same n=1500 subsample used to fit
## the bifactor model (consistency required: the "full item bank" comparison
## scores in bifactor_scores.csv were computed on exactly these people).
##
## min_SEM is set from *this* subsample's full-bank SE ceiling (checked
## below, printed before the simulation runs) rather than reused from
## Japanese or Cantonese -- reusing Cantonese's target for a dataset with a
## different achievable ceiling silently truncated that CAT and hurt
## recovery (see multidim_cantonese/06-*.Rmd Section 5.2). Adjust min_SEM
## here if the printed ceiling differs meaningfully from the 0.5 default.

source("multidim_pipeline/lib.R")

mod <- readRDS("multidim_english/mod_bifactor.Rds")
d_sub <- readRDS("multidim_english/modeling_subsample.Rds")

full_bank_scores <- read_csv("multidim_english/bifactor_scores.csv", show_col_types = FALSE)
med_SE_S1 <- median(full_bank_scores$SE_S1_noun)
med_SE_S2 <- median(full_bank_scores$SE_S2_predicate)
cat("Full-bank SE ceiling on this subsample (median):\n")
cat("  SE_G:", median(full_bank_scores$SE_G), " SE_S1_noun:", med_SE_S1,
    " SE_S2_predicate:", med_SE_S2, "\n")

# min_SEM set just above this dataset's own full-bank ceiling (x1.05), so the
# target is reachable rather than copied from another language's ceiling
min_SEM_target <- c(0.3, round(med_SE_S1 * 1.05, 2), round(med_SE_S2 * 1.05, 2))
cat("Using min_SEM =", paste(min_SEM_target, collapse = ", "), "\n")

run_cat_pipeline(
  mod = mod, d_mat_content = d_sub$d_mat_content, d_demo = d_sub$d_demo,
  out_dir = "multidim_english",
  lengths = c(50, 100, 200),
  min_items = 30, min_SEM = min_SEM_target
)
