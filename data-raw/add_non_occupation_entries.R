# data-raw/add_non_occupation_entries.R
#
# One-off script to add "non-occupation" entries (titles, family-relation
# words, etc.) to the package crosswalks so they get coded explicitly
# instead of falling through as NA / fuzzy-matching to a real occupation.
#
# Convention used here:
#   hisco              = -1
#   hisco_description  = "Not an occupation"
#   status / RELATION / PRODUCT  = -9 (existing missing-data sentinel)
#   hisclass_12 / inc_score_*    = NA
#
# To add more entries later: just append rows to `non_occupations` below
# and re-source.
#
# Run from the package root:
#   source("data-raw/add_non_occupation_entries.R")

suppressPackageStartupMessages({
  library(dplyr)
})

stopifnot(file.exists("data/occ_crosswalk.rda"),
          file.exists("data/hisco_crosswalk.rda"))

load("data/occ_crosswalk.rda")
load("data/hisco_crosswalk.rda")

# ---------------------------------------------------------------------------
# Entries to add. Add more lines as needed.
# ---------------------------------------------------------------------------

non_occupations <- tibble::tribble(
  ~occ_stand,    ~hisco_description,
  "fru",         "Not an occupation",
  "änkefru",     "Not an occupation"
)

# ---------------------------------------------------------------------------
# Build hisco_crosswalk rows
# ---------------------------------------------------------------------------

template_hc <- hisco_crosswalk[1, , drop = FALSE]
template_hc[] <- NA  # blank everything; we'll fill in per row

build_hc_row <- function(occ_stand, hisco_description) {
  row <- template_hc
  row$occ_stand          <- occ_stand
  row$hisco              <- -1
  row$hisco_description  <- hisco_description
  if ("status"   %in% names(row)) row$status   <- -9
  if ("RELATION" %in% names(row)) row$RELATION <- -9
  if ("PRODUCT"  %in% names(row)) row$PRODUCT  <- -9
  # hisclass_12 / inc_score_mean / inc_score_median stay NA
  row
}

new_hc_rows <- do.call(rbind, Map(build_hc_row,
                                  non_occupations$occ_stand,
                                  non_occupations$hisco_description))

# Drop existing rows with the same occ_stand so re-running is idempotent
n_removed_hc <- sum(hisco_crosswalk$occ_stand %in% non_occupations$occ_stand,
                    na.rm = TRUE)
hisco_crosswalk <- hisco_crosswalk[
  !(hisco_crosswalk$occ_stand %in% non_occupations$occ_stand), , drop = FALSE
]
hisco_crosswalk <- rbind(hisco_crosswalk, new_hc_rows)

cat(sprintf("hisco_crosswalk: removed %d existing row(s), added %d new row(s)\n",
            n_removed_hc, nrow(new_hc_rows)))

# ---------------------------------------------------------------------------
# Build occ_crosswalk rows so standardize_occupation recognizes the inputs.
# We add (occ = X, occ_stand = X) for each non-occupation, but only if that
# exact pair doesn't already exist.
# ---------------------------------------------------------------------------

new_oc_rows <- non_occupations %>%
  transmute(occ = occ_stand, occ_stand = occ_stand)

# Pad with the other columns of occ_crosswalk so rbind types line up
extra_cols <- setdiff(names(occ_crosswalk), names(new_oc_rows))
for (col in extra_cols) {
  new_oc_rows[[col]] <- if (col == "n") 0 else NA
}
new_oc_rows <- new_oc_rows[, names(occ_crosswalk), drop = FALSE]

# Idempotency: skip pairs that already exist
existing_pairs <- paste0(occ_crosswalk$occ, "", occ_crosswalk$occ_stand)
candidate_pairs <- paste0(new_oc_rows$occ, "", new_oc_rows$occ_stand)
to_add <- !(candidate_pairs %in% existing_pairs)

if (any(to_add)) {
  occ_crosswalk <- rbind(occ_crosswalk, new_oc_rows[to_add, , drop = FALSE])
  cat(sprintf("occ_crosswalk: added %d new row(s)\n", sum(to_add)))
} else {
  cat("occ_crosswalk: no new rows needed (all (occ, occ_stand) pairs already present)\n")
}

# ---------------------------------------------------------------------------
# Sanity check
# ---------------------------------------------------------------------------

cat("\n--- post-update lookups ---\n")
for (s in non_occupations$occ_stand) {
  hc_hits <- hisco_crosswalk[hisco_crosswalk$occ_stand == s, , drop = FALSE]
  cat(sprintf("  hisco_crosswalk['%s'] -> hisco=%s  desc='%s'  rows=%d\n",
              s,
              if (nrow(hc_hits)) hc_hits$hisco[1] else "<missing>",
              if (nrow(hc_hits)) hc_hits$hisco_description[1] else "<missing>",
              nrow(hc_hits)))
}

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------

save(hisco_crosswalk, file = "data/hisco_crosswalk.rda", compress = "bzip2")
save(occ_crosswalk,   file = "data/occ_crosswalk.rda",   compress = "bzip2")

cat("\nWrote: data/hisco_crosswalk.rda, data/occ_crosswalk.rda\n")
cat("Now run: devtools::load_all()\n")
