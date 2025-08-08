#' Standardize Occupation Titles
#'
#' Joins user data with the internal crosswalk to standardize occupation names.
#' Uses exact matching first, then fuzzy matching as fallback.
#'
#' @param data A data frame containing a column with dirty occupation names.
#' @param occ_col A string: the name of the column in `data` with dirty occupations.
#' @param out_col A string: name for the new column with standardized values.
#'   Default is "standard_occupation".
#' @param fuzzy_matching A logical value: whether to perform fuzzy matching. Default is TRUE.
#' @param fuzzy_threshold A numeric value between 0 and 1 for fuzzy matching threshold.
#'   Only used if fuzzy_matching is TRUE. Default is 0.85.
#'
#' @return A data frame with a new column containing standardized occupation names.
#' @export
#'
#' @examples
#' df <- data.frame(occupation = c("smed", "snickare", "sold.", "sldr", "lärling"))
#' standardize_occupation(df, "occupation")
standardize_occupation <- function(data, occ_col, out_col = "standard_occupation",
                                   fuzzy_matching = TRUE, fuzzy_threshold = 0.85) {
  # Input validation
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' is required.")
  if (!requireNamespace("stringdist", quietly = TRUE)) stop("Package 'stringdist' is required.")

  if (!occ_col %in% names(data)) {
    stop(paste("Column", occ_col, "not found in input data."))
  }

  if (!is.logical(fuzzy_matching)) {
    stop("fuzzy_matching must be TRUE or FALSE.")
  }

  if (fuzzy_matching && (fuzzy_threshold < 0 || fuzzy_threshold > 1)) {
    stop("fuzzy_threshold must be between 0 and 1.")
  }

  # Prepare data
  result <- data
  result$clean_input <- stringr::str_to_lower(stringr::str_trim(data[[occ_col]]))

  # Load and prepare crosswalk
  crosswalk <- histocc::occ_crosswalk
  crosswalk <- dplyr::select(crosswalk, occ, occ_stand)
  crosswalk <- dplyr::mutate(
    crosswalk,
    occ_clean = stringr::str_to_lower(stringr::str_trim(occ)),
    occ_stand_clean = stringr::str_to_lower(stringr::str_trim(occ_stand))
  )
  # Remove duplicates and keep unique mappings
  crosswalk <- dplyr::distinct(crosswalk, occ_clean, occ_stand_clean, .keep_all = TRUE)

  # Initialize output column
  result[[out_col]] <- NA_character_

  # Step 1: Direct match on standardized occupation names (occ_stand)
  direct_matches <- match(result$clean_input, crosswalk$occ_stand_clean)
  matched_idx <- !is.na(direct_matches)
  result[[out_col]][matched_idx] <- crosswalk$occ_stand_clean[direct_matches[matched_idx]]

  # Step 2: Exact match on original occupation names (occ)
  unmatched_idx <- is.na(result[[out_col]])
  if (any(unmatched_idx)) {
    occ_matches <- match(result$clean_input[unmatched_idx], crosswalk$occ_clean)
    occ_matched_idx <- !is.na(occ_matches)

    if (any(occ_matched_idx)) {
      global_idx <- which(unmatched_idx)[occ_matched_idx]
      result[[out_col]][global_idx] <- crosswalk$occ_stand_clean[occ_matches[occ_matched_idx]]
    }
  }

  # Step 3: Fuzzy matching on standardized occupation names
  still_unmatched_idx <- is.na(result[[out_col]])
  if (any(still_unmatched_idx) && fuzzy_matching && fuzzy_threshold > 0) {
    unmatched_values <- result$clean_input[still_unmatched_idx]

    fuzzy_matches <- vapply(
      unmatched_values,
      function(x) {
        if (is.na(x) || x == "") return(NA_character_)

        # Calculate Jaro-Winkler distances
        distances <- stringdist::stringdist(
          x,
          crosswalk$occ_stand_clean,
          method = "jw",
          p = 0.1
        )
        similarities <- 1 - distances

        # Find best match above threshold
        best_idx <- which.max(similarities)
        if (length(best_idx) > 0 && similarities[best_idx] >= fuzzy_threshold) {
          return(crosswalk$occ_stand_clean[best_idx])
        } else {
          return(NA_character_)
        }
      },
      FUN.VALUE = character(1),
      USE.NAMES = FALSE
    )

    result[[out_col]][still_unmatched_idx] <- fuzzy_matches
  }

  # Clean up temporary column
  result$clean_input <- NULL

  return(result)
}

