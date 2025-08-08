#' Code occupations by industry
#'
#' Codes occupational titles by sector, subsector, and worker type.
#' Optionally standardizes occupations first if needed.
#'
#' @param data A data frame containing a column with occupational titles
#' @param occ_stand A string: the name of the column in `data` with occupations.
#'   If standardize_occupations is TRUE, this should contain raw occupation names.
#'   If FALSE, this should contain already standardized occupations.
#' @param out_col A string: name for the new column with standardized values.
#'   Default is "standard_occupation".
#' @param standardize_occupations A logical value: whether to standardize occupations first.
#'   Default is FALSE.
#' @param fuzzy_matching A logical value: whether to perform fuzzy matching during standardization.
#'   Only used if standardize_occupations is TRUE. Default is TRUE.
#' @param fuzzy_threshold A numeric value between 0 and 1 for fuzzy matching threshold.
#'   Only used if standardize_occupations and fuzzy_matching are TRUE. Default is 0.85.
#' @param sector A logical value: whether to code by sector. Default is TRUE.
#' @param subsector A logical value: whether to code by subsector. Default is TRUE.
#' @param worker_type A logical value: whether to code by worker type Default is TRUE.
#' @param label A logical value: whether to return descriptive labels instead of
#'   numerical codes. Default is FALSE.
#'
#' @return A data frame with new columns containing sector, subsector and worker type.
#' @export
#'
#' @examples
#' df <- data.frame(occ_stand = c("smed", "snickare", "soldat", "slöjdare", "lärling"))
#' # With already standardized occupations
#' occ_sector(df, "occ_stand")
#' # With raw occupations that need standardization first
#' df_raw <- data.frame(occupation = c("smed", "snickare", "sold.", "sldr", "lärling"))
#' occ_sector(df_raw, "occupation", standardize_occupations = TRUE, label = TRUE)
occ_sector <- function(data, occ_stand, out_col = "standard_occupation",
                       standardize_occupations = FALSE,
                       fuzzy_matching = TRUE, fuzzy_threshold = 0.85,
                       sector = TRUE, subsector = TRUE, worker_type = TRUE,
                       label = TRUE) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  if (!occ_stand %in% names(data)) {
    stop(paste("Column", occ_stand, "not found in data"))
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

  # Check if st_crosswalk exists in package environment
  if (!exists("st_crosswalk")) {
    stop("Internal crosswalk 'st_crosswalk' not found")
  }

  # Check for existing columns that would be created in this call
  existing_cols <- c()
  if (sector && "sector" %in% names(data)) {
    existing_cols <- c(existing_cols, "sector")
  }
  if (subsector && "subsector" %in% names(data)) {
    existing_cols <- c(existing_cols, "subsector")
  }
  if (worker_type && "worker_type" %in% names(data)) {
    existing_cols <- c(existing_cols, "worker_type")
  }
  # Note: out_col validation is handled by standardize_occupation function itself

  if (length(existing_cols) > 0) {
    stop(paste("Column(s) already exist in data:", paste(existing_cols, collapse = ", "),
               "\nPlease remove these columns or use different output column names."))
  }

  # Define label mappings based on the actual coding scheme
  sector_labels <- c(
    "1" = "Primary",
    "2" = "Secondary",
    "3" = "Tertiary",
    "4" = "Other, undefined"
  )

  # Subsector labels organized by sector (nested structure)
  subsector_labels <- list(
    "1" = c( # Primary sector
      "0" = "",
      "1" = "Agriculture and forestry",
      "2" = "Fishing"
    ),
    "2" = c( # Secondary sector
      "0" = "",
      "1" = "Mining",
      "2" = "Metal and engineering",
      "3" = "Quarrying",
      "4" = "Wood",
      "5" = "Pulp, paper and printing",
      "6" = "Food, beverage and tobacco",
      "7" = "Textile, clothing, leather and hair",
      "8" = "Chemical and rubber",
      "9" = "Building and construction, gas and waterworks",
      "10" = "Undefined"
    ),
    "3" = c( # Tertiary sector
      "0" = "",
      "1" = "Trade",
      "2" = "Transport and communication",
      "3" = "Insurance and banking",
      "4" = "Personal and social services",
      "5" = "Education and professions",
      "6" = "Administration and military",
      "7" = "Domestic services",
      "8" = "Undefined"
    ),
    "4" = c( # Other, undefined
      "0" = "Undefined"
    )
  )

  # Worker type labels organized by sector and subsector (nested structure)
  worker_type_labels <- list(
    "1" = list( # Primary sector
      "0" = c("1" = "Landowners", "10" = "Former and retired landowners, farmers, semi-landless, landless and agricultural workers (incl. undantagsmän)"),
      "1" = c("2" = "Large-scale farmers", "3" = "Medium-scale farmers", "4" = "Small-scale farmers",
              "5" = "Semi-landless", "7" = "Salary-earners", "8" = "Wage-earners: incl. landless and agricultural workers",
              "9" = "Assistants (family)"),
      "2" = c("6" = "Working owners, artisans etc", "7" = "Salary-earners", "8" = "Wage-earners", "9" = "Assistants (family)")
    ),
    "2" = list( # Secondary sector
      "0" = c("1" = "Factory owners, CEOs", "10" = "Former and retired from secondary sector"),
      "1" = c("6" = "Working owners, artisans etc", "7" = "Salary-earners", "8" = "Wage-earners"),
      "2" = c("6" = "Working owners, artisans etc", "7" = "Salary-earners", "8" = "Wage-earners"),
      "3" = c("6" = "Working owners, artisans etc", "7" = "Salary-earners", "8" = "Wage-earners"),
      "4" = c("6" = "Working owners, artisans etc", "7" = "Salary-earners", "8" = "Wage-earners"),
      "5" = c("6" = "Working owners, artisans etc", "7" = "Salary-earners", "8" = "Wage-earners"),
      "6" = c("6" = "Working owners, artisans etc", "7" = "Salary-earners", "8" = "Wage-earners"),
      "7" = c("6" = "Working owners, artisans etc", "7" = "Salary-earners", "8" = "Wage-earners"),
      "8" = c("6" = "Working owners, artisans etc", "7" = "Salary-earners", "8" = "Wage-earners"),
      "9" = c("6" = "Working owners, artisans etc", "7" = "Salary-earners", "8" = "Wage-earners"),
      "10" = c("6" = "Working owners, artisans etc", "7" = "Salary-earners", "8" = "Wage-earners")
    ),
    "3" = list( # Tertiary sector
      "0" = c("1" = "Owners in the tertiary sector, CEOs", "10" = "Former and retired from tertiary sector"),
      "1" = c("6" = "Working owners, artisans etc", "7" = "Salary-earners", "8" = "Wage-earners"),
      "2" = c("6" = "Working owners, artisans etc", "7" = "Salary-earners", "8" = "Wage-earners"),
      "3" = c("6" = "Working owners, artisans etc", "7" = "Salary-earners", "8" = "Wage-earners"),
      "4" = c("6" = "Working owners, artisans etc", "7" = "Salary-earners", "8" = "Wage-earners"),
      "5" = c("6" = "Working owners, artisans etc", "7" = "Salary-earners", "8" = "Wage-earners"),
      "6" = c("6" = "Working owners, artisans etc", "7" = "Salary-earners", "8" = "Wage-earners"),
      "7" = c("6" = "Working owners, artisans etc", "7" = "Salary-earners", "8" = "Wage-earners"),
      "8" = c("6" = "Working owners, artisans etc", "7" = "Salary-earners", "8" = "Wage-earners")
    ),
    "4" = list( # Other, undefined
      "0" = c("1" = "Capital owners and CEOs that cannot be placed in a sector",
              "6" = "Working owners, artisans etc",
              "7" = "Salary-earners that cannot be placed in a sector",
              "8" = "Wage-earners that cannot be placed in a sector",
              "10" = "Former and retired that cannot be placed in a sector",
              "11" = "Poor, paupers, dependant tenants, prisoners, vagrants",
              "12" = "Students (excl. apprentices)",
              "13" = "Familial titles; incl. widows, wives, sons and daughters",
              "0" = "Occupation and status unknown")
    )
  )

  # Select which columns to join from the crosswalk
  join_cols <- c("occ_stand")
  if (sector) join_cols <- c(join_cols, "sector")
  if (subsector) join_cols <- c(join_cols, "subsector")
  if (worker_type) join_cols <- c(join_cols, "worker_type")

  # Create subset of crosswalk with only needed columns
  crosswalk_subset <- st_crosswalk[, join_cols, drop = FALSE]

  # Remove duplicates from crosswalk to avoid duplicate rows in result
  crosswalk_subset <- unique(crosswalk_subset)

  # Perform left join
  result <- merge(data, crosswalk_subset,
                  by.x = occ_stand_col, by.y = "occ_stand",
                  all.x = TRUE, sort = FALSE)

  # Convert to labels if requested
  if (label) {
    # Store original codes before conversion for nested lookups
    sector_codes <- NULL
    subsector_codes <- NULL

    if ("sector" %in% names(result)) {
      sector_codes <- result$sector
    }
    if ("subsector" %in% names(result)) {
      subsector_codes <- result$subsector
    }

    if (sector && "sector" %in% names(result)) {
      result$sector <- sector_labels[as.character(result$sector)]
    }

    if (subsector && "subsector" %in% names(result)) {
      if (!is.null(sector_codes)) {
        result$subsector <- mapply(function(sec, sub) {
          if (is.na(sec) || is.na(sub)) return(NA_character_)
          sec_char <- as.character(sec)
          sub_char <- as.character(sub)
          if (sec_char %in% names(subsector_labels) &&
              sub_char %in% names(subsector_labels[[sec_char]])) {
            return(subsector_labels[[sec_char]][[sub_char]])
          } else {
            return(NA_character_)
          }
        }, sector_codes, result$subsector, USE.NAMES = FALSE)
      } else {
        warning("Cannot convert subsector to labels without sector information")
      }
    }

    if (worker_type && "worker_type" %in% names(result)) {
      if (!is.null(sector_codes) && !is.null(subsector_codes)) {
        result$worker_type <- mapply(function(sec, sub, wt) {
          if (is.na(sec) || is.na(sub) || is.na(wt)) return(NA_character_)
          sec_char <- as.character(sec)
          sub_char <- as.character(sub)
          wt_char <- as.character(wt)
          if (sec_char %in% names(worker_type_labels) &&
              sub_char %in% names(worker_type_labels[[sec_char]]) &&
              wt_char %in% names(worker_type_labels[[sec_char]][[sub_char]])) {
            return(worker_type_labels[[sec_char]][[sub_char]][[wt_char]])
          } else {
            return(NA_character_)
          }
        }, sector_codes, subsector_codes, result$worker_type, USE.NAMES = FALSE)
      } else {
        warning("Cannot convert worker_type to labels without sector and subsector information")
      }
    }
  }

  # Add the standardized occupation column if requested and not already added
  if (!standardize_occupations && !is.null(out_col) && out_col != occ_stand) {
    result[[out_col]] <- result[[occ_stand]]
  }

  return(result)
}
