#' Code occupations using HISCO
#'
#' Adds the HISCO code (always), and optionally HISCO description, status,
#' HISCLASS-12, and an income score. When hisclass = TRUE, applies advanced
#' HISCLASS coding logic that uses both HISCO codes and occupational status
#' to determine the final HISCLASS-12 values. Optionally standardizes
#' occupations first if needed.
#'
#' @param data A data frame with a column of occupational titles
#' @param occ_stand A string giving the name of the occupation column.
#'   If standardize_occupations is TRUE, this should contain raw occupation names.
#'   If FALSE, this should contain already standardized occupations.
#' @param standardize_occupations A logical value: whether to standardize occupations first.
#'   Default is FALSE.
#' @param out_col A string: name for the new column with standardized values.
#'   Only used if standardize_occupations is TRUE. Default is "standard_occupation".
#' @param fuzzy_matching A logical value: whether to perform fuzzy matching during standardization.
#'   Only used if standardize_occupations is TRUE. Default is TRUE.
#' @param fuzzy_threshold A numeric value between 0 and 1 for fuzzy matching threshold.
#'   Only used if standardize_occupations and fuzzy_matching are TRUE. Default is 0.85.
#' @param description Logical: include HISCO descriptions? Default is TRUE
#' @param status Logical: include occupational status? Default is TRUE. Required for proper HISCLASS coding.
#' @param hisclass Logical: include HISCLASS-12 codes with advanced status-based adjustments? Default is FALSE
#' @param inc_score Logical: include income score? Default is FALSE
#' @param inc_score_type Either "median" or "mean". Default is "median"
#' @param fuzzy_hisco Logical: when TRUE, rows whose `occ_stand` value does not
#'   exact-match anything in `hisco_crosswalk` are passed through a Jaro-Winkler
#'   fuzzy match against the crosswalk's unique `occ_stand` values. Useful for
#'   handling Swedish compound-word variants (e.g. `småskollärarinna` ↔
#'   `småskolelärarinna`, `ingenjörsbiträde` ↔ `ingenjörbiträde`). Default is TRUE.
#' @param fuzzy_hisco_threshold Numeric between 0 and 1. Minimum Jaro-Winkler
#'   similarity (1 - distance) required to accept a fuzzy HISCO match. Higher
#'   is stricter. Default is 0.9. Only used when `fuzzy_hisco` is TRUE.
#'
#' @details When hisclass = TRUE, the function applies the following status-based adjustments:
#' \itemize{
#'   \item Masters (status 21) become foremen (HISCLASS 6)
#'   \item Journeymen (status 22) with missing HISCLASS become medium skilled (7)
#'   \item Apprentices (status 23) get skill level reduced (HISCLASS + 2, with special handling for class 4)
#'   \item Artisans (status 24) with missing HISCLASS become medium skilled (7)
#'   \item Principals (status 31) get skill level increased (HISCLASS - 1, with special handling)
#'   \item Subordinates (status 33) get skill level reduced (HISCLASS + 2)
#'   \item Students (status 41) get NA
#'   \item Doctors (status 42) become higher professionals (2)
#'   \item Status 51/52 with missing HISCLASS become higher managers (1)
#' }
#'
#' @return The data frame with additional HISCO-related columns. The `hisco`
#'   code column is always added; other columns (`hisco_description`, `status`,
#'   `hisclass_12`, `inc_score_*`) are added only when their corresponding
#'   argument is TRUE. A `match_type` column is also added with values
#'   `"exact"`, `"fuzzy"`, or `"none"` indicating how each row was matched
#'   against `hisco_crosswalk`.
#' @export
#'
#' @examples
#' df <- data.frame(occ_stand = c("smed", "snickare", "soldat", "slöjdare", "lärling"))
#' # With already standardized occupations
#' occ_hisco(df, "occ_stand")
#' # With raw occupations that need standardization first
#' df_raw <- data.frame(occupation = c("smed", "snickare", "sold.", "sldr", "lärling"))
#' occ_hisco(df_raw, "occupation", standardize_occupations = TRUE)
#' # With advanced HISCLASS coding (requires status information)
#' occ_hisco(df, "occ_stand", hisclass = TRUE, status = TRUE)
occ_hisco <- function(data, occ_stand,
                      standardize_occupations = FALSE,
                      out_col = "standard_occupation",
                      fuzzy_matching = TRUE, fuzzy_threshold = 0.85,
                      description = TRUE,
                      status = TRUE,
                      hisclass = FALSE,
                      inc_score = FALSE,
                      inc_score_type = c("median", "mean"),
                      fuzzy_hisco = TRUE,
                      fuzzy_hisco_threshold = 0.9) {
  # Input validation
  if (!is.data.frame(data)) stop("'data' must be a data frame")
  if (!occ_stand %in% names(data)) stop(paste("Column", occ_stand, "not found in data"))
  if (!exists("hisco_crosswalk")) stop("Internal crosswalk 'hisco_crosswalk' not found")

  inc_score_type <- match.arg(inc_score_type)

  # Validate fuzzy_hisco arguments
  if (!is.logical(fuzzy_hisco) || length(fuzzy_hisco) != 1L) {
    stop("'fuzzy_hisco' must be a single TRUE/FALSE value.")
  }
  if (!is.numeric(fuzzy_hisco_threshold) || length(fuzzy_hisco_threshold) != 1L ||
      fuzzy_hisco_threshold < 0 || fuzzy_hisco_threshold > 1) {
    stop("'fuzzy_hisco_threshold' must be a single numeric value in [0, 1].")
  }

  # Check for existing columns that would be created in this call
  existing_cols <- c()
  if ("hisco" %in% names(data)) {
    existing_cols <- c(existing_cols, "hisco")
  }
  if ("match_type" %in% names(data)) {
    existing_cols <- c(existing_cols, "match_type")
  }
  if (description && "hisco_description" %in% names(data)) {
    existing_cols <- c(existing_cols, "hisco_description")
  }
  if (status && "status" %in% names(data)) {
    existing_cols <- c(existing_cols, "status")
  }
  if (hisclass && "hisclass_12" %in% names(data)) {
    existing_cols <- c(existing_cols, "hisclass_12")
  }
  if (inc_score) {
    score_col <- paste0("inc_score_", inc_score_type)
    if (score_col %in% names(data)) {
      existing_cols <- c(existing_cols, score_col)
    }
  }
  if (standardize_occupations && out_col %in% names(data)) {
    existing_cols <- c(existing_cols, out_col)
  }

  if (length(existing_cols) > 0) {
    stop(paste("Column(s) already exist in data:", paste(existing_cols, collapse = ", "),
               "\nPlease remove these columns or use different output column names."))
  }

  # Step 1: Standardize occupations if requested
  if (standardize_occupations) {
    data <- standardize_occupation(data, occ_stand, out_col,
                                   fuzzy_matching = fuzzy_matching,
                                   fuzzy_threshold = fuzzy_threshold)
    # Update occ_stand to point to the standardized column
    occ_stand_col <- out_col
  } else {
    occ_stand_col <- occ_stand
  }

  # Define columns to include in join
  # Always include the HISCO code itself
  join_cols <- c("occ_stand", "hisco")
  if (description) join_cols <- c(join_cols, "hisco_description")
  if (status) join_cols <- c(join_cols, "status")
  if (hisclass) {
    join_cols <- c(join_cols, "hisclass_12") # HISCO already included above
  }
  if (inc_score) {
    score_col <- paste0("inc_score_", inc_score_type)
    if (!score_col %in% names(hisco_crosswalk)) {
      stop(paste("Column", score_col, "not found in hisco_crosswalk"))
    }
    join_cols <- c(join_cols, score_col)
  }

  # Subset and deduplicate crosswalk
  crosswalk_subset <- unique(hisco_crosswalk[, join_cols, drop = FALSE])

  # Join
  result <- merge(data, crosswalk_subset,
                  by.x = occ_stand_col, by.y = "occ_stand",
                  all.x = TRUE, sort = FALSE)

  # Track how each row matched against hisco_crosswalk
  result$match_type <- ifelse(is.na(result$hisco), "none", "exact")

  # Optional fuzzy fallback for rows that didn't match exactly.
  # Catches Swedish compound-word variants where the linker letter (s/e) or
  # spelling differs slightly between the input and the crosswalk
  # (e.g. småskollärarinna <-> småskolelärarinna).
  if (fuzzy_hisco) {
    if (!requireNamespace("stringdist", quietly = TRUE)) {
      warning("Package 'stringdist' is required for fuzzy_hisco; skipping fuzzy fallback.")
    } else {
      unmatched_idx <- which(
        is.na(result$hisco) &
          !is.na(result[[occ_stand_col]]) &
          nzchar(as.character(result[[occ_stand_col]]))
      )
      if (length(unmatched_idx) > 0L) {
        unmatched_strs <- as.character(result[[occ_stand_col]][unmatched_idx])
        targets <- unique(crosswalk_subset$occ_stand)
        targets <- targets[!is.na(targets) & nzchar(targets)]

        if (length(targets) > 0L) {
          # Distance matrix: rows = unmatched inputs, cols = candidate targets
          sim <- 1 - stringdist::stringdistmatrix(
            unmatched_strs, targets, method = "jw", p = 0.1
          )
          best_col <- max.col(sim, ties.method = "first")
          best_sim <- sim[cbind(seq_along(unmatched_strs), best_col)]
          accept   <- !is.na(best_sim) & best_sim >= fuzzy_hisco_threshold

          if (any(accept)) {
            matched_targets <- targets[best_col[accept]]
            target_rows     <- unmatched_idx[accept]
            lookup_idx      <- match(matched_targets, crosswalk_subset$occ_stand)

            # Fill every join column except the join key itself
            fill_cols <- setdiff(join_cols, "occ_stand")
            for (col in fill_cols) {
              result[[col]][target_rows] <- crosswalk_subset[[col]][lookup_idx]
            }
            result$match_type[target_rows] <- "fuzzy"
          }
        }
      }
    }
  }

  # Apply HISCLASS coding if requested
  if (hisclass) {
    # Check if both HISCO and status columns exist for advanced coding
    if (!"hisco" %in% names(result)) {
      stop("HISCO codes are required for HISCLASS coding but not found in crosswalk")
    }

    if (!"status" %in% names(result)) {
      warning("Status codes are required for proper HISCLASS coding but not found. Using basic HISCO-to-HISCLASS mapping only.")
      # Keep the basic hisclass_12 from crosswalk as-is
    } else {
      # Apply the advanced HISCLASS coding logic with status adjustments
      # Store the basic hisclass_12 values from crosswalk
      basic_hisclass <- result$hisclass_12

      # Apply status-based adjustments
      result$hisclass_12 <- ifelse(
        # Masters (21) become "foremen" => 6
        result$status == 21, 6,
        ifelse(
          # Journeymen (22) with missing hisclass_12 => 7 (medium skilled)
          result$status == 22 & is.na(basic_hisclass), 7,
          ifelse(
            # Apprentices (23) => skill level one lower (hisclass_12 + 2) if 1 <= hisclass_12 <= 8 and not 4
            result$status == 23 & !is.na(basic_hisclass) &
              basic_hisclass >= 1 & basic_hisclass <= 8 & basic_hisclass != 4,
            basic_hisclass + 2,
            ifelse(
              # If 4, then => 5
              result$status == 23 & basic_hisclass == 4, 5,
              ifelse(
                # Artisans (24) with missing hisclass_12 => 7 (medium skilled)
                result$status == 24 & is.na(basic_hisclass), 7,
                ifelse(
                  # Principals (31): if 2 <= hisclass_12 <= 7 => hisclass_12 - 1
                  result$status == 31 & basic_hisclass >= 2 & basic_hisclass <= 7,
                  basic_hisclass - 1,
                  ifelse(
                    # Principals (31): if 9 => 6
                    result$status == 31 & basic_hisclass == 9, 6,
                    ifelse(
                      # Subordinates (33): shift skill level down one => +2
                      result$status == 33 & basic_hisclass >= 1 & basic_hisclass <= 10,
                      basic_hisclass + 2,
                      ifelse(
                        # Students (41) => NA
                        result$status == 41, NA_real_,
                        ifelse(
                          # Doctors (42) => 2 (higher professional)
                          result$status == 42, 2,
                          ifelse(
                            # 51 or 52 with missing hisclass_12 => 1 (higher managers)
                            (result$status == 51 | result$status == 52) & is.na(basic_hisclass), 1,
                            # Otherwise, keep existing hisclass_12
                            basic_hisclass
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    }
  }

  return(result)
}
