## Draws the fixed n=1500 modeling subsample used by both 02 (EFA) and 03
## (bifactor) -- split into its own step (rather than done inside 02) so
## both of those can run concurrently against the same subsample without a
## race condition.

source("multidim_pipeline/lib.R")

d_full <- readRDS("multidim_english/noun_predicate_response_data.Rds")

sub_idx <- subsample_for_fitting(d_full$d_mat_content, n = 1500, seed = 1234)
cat("Using", length(sub_idx), "of", nrow(d_full$d_mat_content), "available administrations.\n")

d_sub <- list(
  d_mat_content = d_full$d_mat_content[sub_idx, ],
  items_content = d_full$items_content,
  d_demo = d_full$d_demo[sub_idx, ]
)
stopifnot(nrow(d_sub$d_mat_content) == nrow(d_sub$d_demo))
saveRDS(d_sub, "multidim_english/modeling_subsample.Rds")
