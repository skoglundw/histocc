# data-raw/add_occ_synonyms.R
#
# Add new standardized occupations as ALIASES of an existing one.
#
# For each (new_occ_stand, copy_from_occ_stand) pair below, the script:
#   1. Adds an identity mapping (occ = X, occ_stand = X) to `occ_crosswalk`
#      so `standardize_occupation()` keeps the new title as its own
#      standardized form (does NOT collapse it into the source title).
#   2. Copies every `hisco_crosswalk` row whose `occ_stand == copy_from_occ_stand`
#      into a new row with `occ_stand = new_occ_stand`, preserving HISCO,
#      description, status, hisclass_12, income scores, etc.
#
# Use this when you want a more specific Swedish title (e.g.
# "legitimerad läkare") to stay distinct in the standardized output but
# share its HISCO classification with a more general title ("läkare").
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
# Aliases to register
# ---------------------------------------------------------------------------

new_aliases <- tibble::tribble(
  ~new_occ_stand,                ~copy_from_occ_stand,
  "legitimerad läkare",          "läkare",
  "legitimerad tandläkare",      "tandläkare",
  "legitimerad veterinär",       "veterinär",
  "legitimerad apotekare",       "apotekare",
  "legitimerad sjuksköterska",   "sjuksköterska",
  "legitimerad barnmorska",      "barnmorska"
)

# ---------------------------------------------------------------------------
# 1. occ_crosswalk: identity mapping for the new title
# ---------------------------------------------------------------------------

new_oc_rows <- tibble::tibble(
  occ       = new_aliases$new_occ_stand,
  occ_stand = new_aliases$new_occ_stand
)

extra_oc_cols <- setdiff(names(occ_crosswalk), names(new_oc_rows))
for (col in extra_oc_cols) {
  new_oc_rows[[col]] <- if (col == "n") 0 else NA
}
new_oc_rows <- new_oc_rows[, names(occ_crosswalk), drop = FALSE]

sep <- "\037"
oc_existing <- paste0(occ_crosswalk$occ, sep, occ_crosswalk$occ_stand)
oc_candidate <- paste0(new_oc_rows$occ,  sep, new_oc_rows$occ_stand)
oc_to_add <- !(oc_candidate %in% oc_existing)

if (any(oc_to_add)) {
  occ_crosswalk <- rbind(occ_crosswalk, new_oc_rows[oc_to_add, , drop = FALSE])
  cat(sprintf("occ_crosswalk: added %d identity mapping(s)\n", sum(oc_to_add)))
} else {
  cat("occ_crosswalk: identity mappings already present\n")
}

# ---------------------------------------------------------------------------
# 2. hisco_crosswalk: copy source row(s), retag with new occ_stand
# ---------------------------------------------------------------------------

# Idempotency: drop any existing rows for the new_occ_stand values first
n_removed_hc <- sum(hisco_crosswalk$occ_stand %in% new_aliases$new_occ_stand,
                    na.rm = TRUE)
hisco_crosswalk <- hisco_crosswalk[
  !(hisco_crosswalk$occ_stand %in% new_aliases$new_occ_stand), , drop = FALSE
]

new_hc_chunks <- list()
for (i in seq_len(nrow(new_aliases))) {
  src <- new_aliases$copy_from_occ_stand[i]
  dst <- new_aliases$new_occ_stand[i]

  src_rows <- hisco_crosswalk[hisco_crosswalk$occ_stand == src, , drop = FALSE]
  if (nrow(src_rows) == 0L) {
    warning(sprintf("Source occ_stand '%s' not found in hisco_crosswalk; '%s' will not get a HISCO entry.",
                    src, dst))
    next
  }
  src_rows$occ_stand <- dst
  new_hc_chunks[[length(new_hc_chunks) + 1L]] <- src_rows
}

if (length(new_hc_chunks) > 0L) {
  new_hc_rows <- do.call(rbind, new_hc_chunks)
  hisco_crosswalk <- rbind(hisco_crosswalk, new_hc_rows)
  cat(sprintf("hisco_crosswalk: removed %d existing row(s), added %d new row(s)\n",
              n_removed_hc, nrow(new_hc_rows)))
} else {
  cat("hisco_crosswalk: nothing added (no source rows resolved)\n")
}

# ---------------------------------------------------------------------------
# Sanity check
# ---------------------------------------------------------------------------

cat("\n--- post-update lookups ---\n")
for (i in seq_len(nrow(new_aliases))) {
  s <- new_aliases$new_occ_stand[i]
  hc_hits <- hisco_crosswalk[hisco_crosswalk$occ_stand == s, , drop = FALSE]
  cat(sprintf("  hisco_crosswalk['%s']: %d row(s)\n", s, nrow(hc_hits)))
  if (nrow(hc_hits) > 0L) {
    print(hc_hits[, intersect(c("occ_stand", "hisco", "hisco_description",
                                "status", "hisclass_12"), names(hc_hits))])
  }
}

save(occ_crosswalk,   file = "data/occ_crosswalk.rda",   compress = "bzip2")
save(hisco_crosswalk, file = "data/hisco_crosswalk.rda", compress = "bzip2")

cat("\nWrote: data/occ_crosswalk.rda, data/hisco_crosswalk.rda\n")
cat("Now run: devtools::load_all()\n")
