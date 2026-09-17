## American English EFA, on the n=1500 subsample drawn in 01b-subsample.R
## (see that script and multidim_english/01-prep.R for why we subsample:
## full N=8998 would take hours at this items x people x EM-iterations scale).
## Runs independently of (and can run concurrently with) the bifactor fit.

source("multidim_pipeline/lib.R")

d_sub <- readRDS("multidim_english/modeling_subsample.Rds")

fit_exploratory_efa(d_sub$d_mat_content, d_sub$items_content, out_dir = "multidim_english")
