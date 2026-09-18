## Spanish (Mexican) confirmatory bifactor model, full N=1616 sample.
## Runs independently of (and concurrently with) 02-exploratory-efa.R.

source("multidim_pipeline/lib.R")

d <- readRDS("multidim_spanish/noun_predicate_response_data.Rds")
fit_confirmatory_bifactor(d$d_mat_content, d$items_content, d$d_demo, out_dir = "multidim_spanish")
