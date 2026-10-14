## Spanish (Mexican) exploratory 2-factor IRT model, full N=1616 sample
## (no subsampling needed at this scale). Runs independently of (and
## concurrently with) 03-bifactor-confirmatory.R.

source("multidim_pipeline/lib.R")

d <- readRDS("multidim_spanish/noun_predicate_response_data.Rds")
fit_exploratory_efa(d$d_mat_content, d$items_content, out_dir = "multidim_spanish")
