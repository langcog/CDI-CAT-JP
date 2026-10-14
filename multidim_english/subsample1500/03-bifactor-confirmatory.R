## American English confirmatory bifactor model, on the same n=1500
## subsample used for the EFA (see 02-subsample-and-efa.R for why). Runs
## independently of (and can run concurrently with) the EFA.

source("multidim_pipeline/lib.R")

d_sub <- readRDS("multidim_english/modeling_subsample.Rds")

fit_confirmatory_bifactor(
  d_sub$d_mat_content, d_sub$items_content, d_sub$d_demo,
  out_dir = "multidim_english"
)
