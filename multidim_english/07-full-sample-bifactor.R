## American English confirmatory bifactor model on the FULL sample
## (N=8998), not the n=1500 subsample used in 03-bifactor-confirmatory.R.
## Output goes to multidim_english/fullsample/ so the original subsampled
## results (02/03/04) stay untouched and both remain available. Runs
## independently of (and can run concurrently with) 06-full-sample-efa.R.

source("multidim_pipeline/lib.R")

dir.create("multidim_english/fullsample", showWarnings = FALSE)
d <- readRDS("multidim_english/noun_predicate_response_data.Rds")
cat("Full sample N =", nrow(d$d_mat_content), "\n")

fit_confirmatory_bifactor(d$d_mat_content, d$items_content, d$d_demo, out_dir = "multidim_english/fullsample")
