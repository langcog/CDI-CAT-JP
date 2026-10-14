## Fetch/cached Wordbank item-by-age summaries for the aoa-pipeline's target
## languages, using the pipeline's own functions (aoa-pipeline/scripts/[1] wordbank.R)
## so the data are built the same way as for the AoA-prediction work.
##
## Caches go in item_difficulty_prediction/cache/wordbank/ (gitignored), NOT in
## the aoa-pipeline repo. The 13 languages already cached there are copied over
## rather than refetched; the rest are pulled from Wordbank ("next" version, as
## in the pipeline's default). Languages that fail to fetch are skipped and
## logged so one bad language doesn't sink the run.

suppressMessages({
  library(tidyverse)
  library(glue)
  library(here)
  library(wordbankr)
})

aoa_repo <- "/Users/gkacherg/Documents/GitHub/aoa-pipeline"
wb_path <- here("item_difficulty_prediction", "cache", "wordbank")
dir.create(wb_path, showWarnings = FALSE, recursive = TRUE)
# same definition as aoa-pipeline/scripts/[0] setup.R (that script reads files
# relative to the aoa-pipeline root, so it isn't sourced here)
normalize_language <- function(language) {
  language |> str_replace(" ", "_") |> str_to_lower()
}
source(file.path(aoa_repo, "scripts", "[1] wordbank.R"))

# seed cache from the languages already cached in aoa-pipeline
src_files <- list.files(file.path(aoa_repo, "data", "wordbank"), pattern = "\\.rds$", full.names = TRUE)
for (f in src_files) {
  dest <- file.path(wb_path, basename(f))
  if (!file.exists(dest)) file.copy(f, dest)
}
cat("Seeded", length(src_files), "cached languages from aoa-pipeline\n")

target_langs <- c("Dutch", "English (American)", "English (Australian)", "English (British)",
                  "German (German)", "Danish", "Norwegian", "Swedish",
                  "French (French)", "French (Quebecois)", "Italian",
                  "Spanish (Mexican)", "Spanish (European)", "Spanish (Argentinian)",
                  "Catalan", "Portuguese (European)",
                  "Croatian", "Czech", "Polish", "Russian", "Turkish",
                  "Hungarian", "Estonian", "Finnish",
                  "Mandarin (Taiwanese)", "Mandarin (Beijing)", "Cantonese",
                  "Hebrew", "Korean", "Japanese")

for (lang in target_langs) {
  f <- file.path(wb_path, glue("{normalize_language(lang)}.rds"))
  if (file.exists(f)) { cat("cached:", lang, "\n"); next }
  t0 <- Sys.time()
  res <- tryCatch(create_wb_data(lang), error = function(e) {
    cat("FAILED:", lang, "-", conditionMessage(e), "\n"); NULL
  })
  cat("fetched:", lang, "-", round(difftime(Sys.time(), t0, units = "mins"), 1), "min,",
      if (is.null(res)) "no data" else paste(nrow(res), "rows"), "\n")
}

cat("\nCache contents:\n")
print(list.files(wb_path))
