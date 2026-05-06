# data-raw/add_occ_synonyms.R
#
# Add custom (occ -> occ_stand) mappings to occ_crosswalk for titles the
# existing crosswalk doesn't cover, or that get mis-standardized.
# Each entry says "treat the raw title `occ` as a synonym for `occ_stand`",
# which then flows through to HISCO / sector lookups normally.
#
# To add more entries: append rows to `new_mappings` and re-source.
#
# Run from the package root:
#   source("data-raw/add_occ_synonyms.R")
#   devtools::load_all()

suppressPackageStartupMessages({
  library(dplyr)
})

stopifnot(file.exists("data/occ_crosswalk.rda"),
          file.exists("data/hisco_crosswalk.rda"))

load("data/occ_crosswalk.rda")
load("data/hisco_crosswalk.rda")

# ---------------------------------------------------------------------------
# Mappings to add: raw title -> standardized form
# ---------------------------------------------------------------------------

new_mappings <- tibble::tribble(
  ~occ,                  ~occ_stand,
  "legitimerad läkare",  "läkare"
)

# ---------------------------------------------------------------------------
# Pad the new rows with the rest of occ_crosswalk's columns and append
# ---------------------------------------------------------------------------

extra_cols <- setdiff(names(occ_crosswalk), names(new_mappings))
for (col in extra_cols) {
  new_mappings[[col]] <- if (col == "n") 0 else NA
}
new_mappings <- new_mappings[, names(occ_crosswalk), drop = FALSE]

# Idempotency: skip exact (occ, occ_stand) pairs already present
sep <- "\037"  # ASCII unit separator -> safe delimiter
existing_pairs  <- paste0(occ_crosswalk$occ,  sep, occ_crosswalk$occ_stand)
candidate_pairs <- paste0(new_mappings$occ,   sep, new_mappings$occ_stand)
to_add <- !(candidate_pairs %in% existing_pairs)

if (any(to_add)) {
  occ_crosswalk <- rbind(occ_crosswalk, new_mappings[to_add, , drop = FALSE])
  cat(sprintf("occ_crosswalk: added %d new (occ, occ_stand) mapping(s):\n",
              sum(to_add)))
  for (i in which(to_add)) {
    cat(sprintf("  '%s'  ->  '%s'\n",
                new_mappings$occ[i], new_mappings$occ_stand[i]))
  }
} else {
  cat("occ_crosswalk: nothing to do — all mappings already present.\n")
}

# ---------------------------------------------------------------------------
# Sanity check: a synonym mapping only pays off if the target occ_stand
# actually exists in hisco_crosswalk. Flag any that don't.
# ---------------------------------------------------------------------------

cat("\n--- target occ_stand coverage in hisco_crosswalk ---\n")
for (s in unique(new_mappings$occ_stand)) {
  n_hits <- sum(hisco_crosswalk$occ_stand == s, na.rm = TRUE)
  flag <- if (n_hits == 0) "  <-- NOT FOUND, mapping won't yield a HISCO" else ""
  cat(sprintf("  '%s': %d row(s)%s\n", s, n_hits, flag))
}

save(occ_crosswalk, file = "data/occ_crosswalk.rda", compress = "bzip2")
cat("\nWrote: data/occ_crosswalk.rda\n")
cat("Now run: devtools::load_all()\n")
