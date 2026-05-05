# data-raw/cleanup_crosswalks.R
#
# One-off cleanup of homoglyph / OCR pollution in the package crosswalks.
#
# Run this from the package root (RStudio: Session > Set Working Directory >
# To Project Directory, then `source("data-raw/cleanup_crosswalks.R")`),
# or `Rscript data-raw/cleanup_crosswalks.R`.
#
# What it does:
#   1. Auto-replaces high-confidence Cyrillic homoglyphs (e.g. ӧ U+04E7 -> ö U+00F6)
#      in `occ_crosswalk$occ`, `occ_crosswalk$occ_stand`, and
#      `hisco_crosswalk$occ_stand`.
#   2. Strips non-breaking and zero-width spaces; collapses whitespace.
#   3. Applies NFC normalization.
#   4. Re-aggregates `occ_crosswalk` rows that collide after cleanup
#      (sums `n`, weighted-averages `st_sector` / `st_subsector` / `st_class`).
#   5. Writes two review CSVs to `data-raw/` listing
#      (a) every row touched by the auto-cleanup, and
#      (b) every row containing AMBIGUOUS characters (likely OCR errors
#      where the right substitution is word-dependent: ȧ ã õ â à á Ŏ Õ etc.).
#      Those rows are NOT auto-changed; eyeball them yourself.
#   6. Saves the cleaned crosswalks via `usethis::use_data(..., overwrite = TRUE)`.
#
# After running, also do:
#   devtools::document()
#   devtools::load_all()

suppressPackageStartupMessages({
  library(dplyr)
  library(stringi)
})

stopifnot(file.exists("data/occ_crosswalk.rda"),
          file.exists("data/hisco_crosswalk.rda"))

load("data/occ_crosswalk.rda")
load("data/hisco_crosswalk.rda")


# ---------------------------------------------------------------------------
# Substitution tables
# ---------------------------------------------------------------------------

# High-confidence: a non-Latin alphabet codepoint that visually duplicates a
# Latin letter and is never legitimate in Swedish occupational text.
homoglyphs <- c(
  # Cyrillic with diaeresis -> Latin with diaeresis
  "ӧ" = "ö",  # ӧ -> ö
  "ӓ" = "ä",  # ӓ -> ä
  "Ӧ" = "Ö",  # Ӧ -> Ö
  "Ӓ" = "Ä",  # Ӓ -> Ä
  # Cyrillic plain -> Latin plain (lowercase)
  "а" = "a",        # а -> a
  "е" = "e",        # е -> e
  "о" = "o",        # о -> o
  "р" = "p",        # р -> p
  "с" = "c",        # с -> c
  "у" = "y",        # у -> y (Cyrillic у looks like Latin y, not u, in print)
  "х" = "x",        # х -> x
  "і" = "i",        # і -> i (Ukrainian i)
  # Cyrillic plain -> Latin plain (uppercase)
  "А" = "A",
  "Е" = "E",
  "О" = "O",
  "Р" = "P",
  "С" = "C",
  "Х" = "X",
  "І" = "I"
)

# Ambiguous: clearly wrong, but the right substitution depends on the word.
# E.g. `ȧ` could be ä OR å; `Ŏ` is OCR garbage that's almost always Ö but not
# guaranteed. We flag these for human review rather than guessing.
ambiguous_chars <- c(
  "ȧ", # ȧ
  "Ȧ", # Ȧ
  "ã", # ã
  "Ã", # Ã
  "õ", # õ
  "Õ", # Õ
  "â", # â
  "à", # à
  "á", # á
  "ă", # ă
  "ė", # ė
  "Ŏ", # Ŏ
  "ŏ", # ŏ
  "ấ", # ấ
  "Ỗ", # Ỗ
  "ἂ", # ἂ
  "ὅ", # ὅ
  "ᾂ", # ᾂ
  # punctuation / OCR noise that may need stripping by hand
  "*", "?", ";", "…", "_", ">", "]", "[", "<"
)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

clean_string <- function(x) {
  if (is.na(x)) return(NA_character_)
  # 1. apply homoglyph substitutions
  x <- stri_replace_all_fixed(
    x,
    pattern    = names(homoglyphs),
    replacement = unname(homoglyphs),
    vectorize_all = FALSE
  )
  # 2. NFC normalize so combining marks collapse onto base letter
  x <- stri_trans_nfc(x)
  # 3. replace exotic spaces / zero-width chars with regular space
  x <- stri_replace_all_regex(x, "[\\u00A0\\u200B-\\u200D\\u2028\\u2029\\uFEFF]", " ")
  # 4. collapse runs of whitespace and trim
  x <- stri_replace_all_regex(x, "\\s+", " ")
  stri_trim_both(x)
}

flag_ambiguous <- function(x) {
  if (is.na(x)) return(FALSE)
  any(stri_detect_fixed(x, ambiguous_chars))
}


# ---------------------------------------------------------------------------
# Clean occ_crosswalk
# ---------------------------------------------------------------------------

cat("=== occ_crosswalk ===\n")
cat("rows before:", nrow(occ_crosswalk), "\n")

occ_orig <- occ_crosswalk
occ_crosswalk <- occ_crosswalk %>%
  mutate(
    occ_new       = vapply(occ,       clean_string, character(1)),
    occ_stand_new = vapply(occ_stand, clean_string, character(1))
  )

# What changed
occ_changes <- occ_crosswalk %>%
  filter(occ != occ_new | occ_stand != occ_stand_new) %>%
  transmute(occ_orig       = occ,
            occ_cleaned    = occ_new,
            occ_stand_orig = occ_stand,
            occ_stand_clean = occ_stand_new)
cat("auto-cleaned rows:", nrow(occ_changes), "\n")

# What's still suspicious (ambiguous chars present)
occ_review <- occ_orig %>%
  filter(vapply(occ,       flag_ambiguous, logical(1)) |
         vapply(occ_stand, flag_ambiguous, logical(1)))
cat("rows needing manual review (ambiguous chars):", nrow(occ_review), "\n")

occ_crosswalk <- occ_crosswalk %>%
  mutate(occ = occ_new, occ_stand = occ_stand_new) %>%
  select(-occ_new, -occ_stand_new)

# Re-aggregate: cleanup may collide previously-distinct rows
# (e.g. "ingenjӧr" + "ingenjör" both become "ingenjör").
# Sum n, weighted-mean the sector columns by original n.
agg_cols <- intersect(c("st_sector", "st_subsector", "st_class"), names(occ_crosswalk))
if ("n" %in% names(occ_crosswalk) && length(agg_cols) > 0) {
  before_dedup <- nrow(occ_crosswalk)
  occ_crosswalk <- occ_crosswalk %>%
    group_by(occ, occ_stand) %>%
    summarise(
      across(all_of(agg_cols),
             ~ if (sum(n, na.rm = TRUE) > 0)
                 stats::weighted.mean(.x, n, na.rm = TRUE)
               else mean(.x, na.rm = TRUE)),
      n = sum(n, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    select(occ, occ_stand, n, all_of(agg_cols))
  cat("rows after collision dedup:", nrow(occ_crosswalk),
      "(was", before_dedup, ")\n")
} else {
  occ_crosswalk <- distinct(occ_crosswalk)
}


# ---------------------------------------------------------------------------
# Clean hisco_crosswalk
# ---------------------------------------------------------------------------

cat("\n=== hisco_crosswalk ===\n")
cat("rows before:", nrow(hisco_crosswalk), "\n")

hisco_orig <- hisco_crosswalk
hisco_crosswalk <- hisco_crosswalk %>%
  mutate(occ_stand = vapply(occ_stand, clean_string, character(1)))

hisco_changed_n <- sum(hisco_orig$occ_stand != hisco_crosswalk$occ_stand,
                       na.rm = TRUE)
cat("auto-cleaned rows:", hisco_changed_n, "\n")

hisco_review <- hisco_orig %>%
  filter(vapply(occ_stand, flag_ambiguous, logical(1)))
cat("rows needing manual review:", nrow(hisco_review), "\n")


# ---------------------------------------------------------------------------
# Sanity check: how many of the previously-failing compounds now resolve?
# ---------------------------------------------------------------------------

probes <- c("ingenjör", "ingenjӧr", "ingenjӓr",
            "ingenjörsbiträde", "ingenjörselev")
cat("\n--- probe lookups in cleaned hisco_crosswalk$occ_stand ---\n")
for (p in probes) {
  hits <- sum(hisco_crosswalk$occ_stand == p, na.rm = TRUE)
  cat(sprintf("  %-22s %d row(s)\n", p, hits))
}


# ---------------------------------------------------------------------------
# Write review CSVs and save cleaned data
# ---------------------------------------------------------------------------

dir.create("data-raw", showWarnings = FALSE)

write.csv(occ_changes,
          "data-raw/occ_crosswalk_auto_changes.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(occ_review,
          "data-raw/occ_crosswalk_needs_review.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(hisco_review,
          "data-raw/hisco_crosswalk_needs_review.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\nReview files written to data-raw/:\n")
cat("  - occ_crosswalk_auto_changes.csv   (everything we changed)\n")
cat("  - occ_crosswalk_needs_review.csv   (rows with ambiguous chars)\n")
cat("  - hisco_crosswalk_needs_review.csv\n")

# Save .rda files directly (equivalent to what usethis::use_data() does)
save(occ_crosswalk,   file = "data/occ_crosswalk.rda",   compress = "bzip2")
save(hisco_crosswalk, file = "data/hisco_crosswalk.rda", compress = "bzip2")
cat("\nWrote: data/occ_crosswalk.rda, data/hisco_crosswalk.rda\n")

cat("\nDone. Now run:\n")
cat("  devtools::document()\n")
cat("  devtools::load_all()\n")
