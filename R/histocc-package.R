#' histocc: Classification of Historical Swedish Occupations
#'
#' The histocc package provides a comprehensive toolkit for standardizing and
#' classifying historical Swedish occupational titles from parish records,
#' census data, and other historical sources.
#'
#' @description
#' This package addresses the common challenge of working with historical
#' occupation data, where titles are often spelled inconsistently, abbreviated,
#' or use archaic terminology. The package offers three main functionalities:
#'
#' \itemize{
#'   \item Standardization of messy occupation titles using fuzzy matching
#'   \item Classification using the HISCO/HISCLASS international standards
#'   \item Sector and worker type coding specific to Swedish historical contexts
#' }
#'
#' @section Main Functions:
#'
#' **Standardization:**
#' \itemize{
#'   \item \code{\link{standardize_occupation}}: Cleans and standardizes occupation
#'     titles against a comprehensive crosswalk of historical Swedish occupations.
#'     Uses a three-step matching process (direct match, exact match, fuzzy match)
#'     to handle spelling variations and abbreviations.
#' }
#'
#' **Classification Functions:**
#' \itemize{
#'   \item \code{\link{occ_hisco}}: Codes occupations using HISCO (Historical
#'     International Standard Classification of Occupations) and optionally adds:
#'     \itemize{
#'       \item HISCO descriptions
#'       \item Occupational status codes
#'       \item HISCLASS-12 social class codes with status-based adjustments
#'       \item Income scores (median or mean)
#'     }
#'
#'   \item \code{\link{occ_sector}}: Classifies occupations by economic sector
#'     (primary/secondary/tertiary), subsector, and worker type. Particularly
#'     useful for understanding the economic structure of historical populations.
#' }
#'
#' @section Workflow:
#'
#' The typical workflow for processing historical occupation data:
#'
#' 1. **Load your data** containing occupation titles (can be messy/unstandardized)
#'
#' 2. **Standardize occupations** (if needed):
#'    \preformatted{
#'    df <- standardize_occupation(df, "occupation_column")
#'    }
#'
#' 3. **Apply classifications**:
#'    \preformatted{
#'    # Add HISCO codes and HISCLASS
#'    df <- occ_hisco(df, "standard_occupation",
#'                    hisclass = TRUE, status = TRUE)
#'
#'    # Add sector classifications
#'    df <- occ_sector(df, "standard_occupation",
#'                     label = TRUE)
#'    }
#'
#' @section Integrated Processing:
#'
#' Both classification functions can standardize and classify in one step:
#' \preformatted{
#' # Direct from messy data to HISCO
#' df <- occ_hisco(df, "messy_occupation",
#'                 standardize_occupations = TRUE,
#'                 hisclass = TRUE)
#'
#' # Direct from messy data to sectors
#' df <- occ_sector(df, "messy_occupation",
#'                  standardize_occupations = TRUE,
#'                  label = TRUE)
#' }
#'
#' @section Internal Data:
#'
#' The package includes three internal crosswalks:
#' \itemize{
#'   \item \code{occ_crosswalk}: Maps occupation variants to standardized forms
#'   \item \code{hisco_crosswalk}: Links standardized occupations to HISCO codes
#'   \item \code{st_crosswalk}: Links standardized occupations to sector codes
#' }
#'
#' @section Historical Context:
#'
#' The package is optimized for Swedish occupational titles from approximately
#' 1750-1950, covering the transition from agricultural to industrial society.
#' The status-based adjustments in HISCLASS coding reflect the guild system
#' and social hierarchies of pre-industrial and early industrial Sweden.
#'
#' @section Advanced Features:
#'
#' **HISCLASS Status Adjustments:**
#'
#' The package implements sophisticated status-based adjustments for HISCLASS:
#' \itemize{
#'   \item Masters (status 21) → foremen (HISCLASS 6)
#'   \item Journeymen (status 22) → medium skilled (7) if missing
#'   \item Apprentices (status 23) → skill level reduced
#'   \item Principals (status 31) → skill level increased
#'   \item Subordinates (status 33) → skill level reduced
#' }
#'
#' **Fuzzy Matching:**
#'
#' The standardization function uses Jaro-Winkler distance for fuzzy matching,
#' which is particularly effective for:
#' \itemize{
#'   \item Historical spelling variations (e.g., "smed" vs "smith")
#'   \item Common abbreviations (e.g., "sold." for "soldat")
#'   \item Transcription errors in digitized records
#' }
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic standardization
#' df <- data.frame(
#'   occupation = c("smed", "snickare", "sold.", "lärling", "f.d. major")
#' )
#' df_clean <- standardize_occupation(df, "occupation")
#'
#' # Example 2: Complete processing pipeline
#' df <- data.frame(
#'   name = c("Anders Andersson", "Per Persson", "Lars Larsson"),
#'   occupation = c("smed", "snickare mästare", "soldat"),
#'   year = c(1850, 1875, 1900)
#' )
#'
#' # Standardize and add all classifications
#' df_coded <- df %>%
#'   occ_hisco("occupation",
#'             standardize_occupations = TRUE,
#'             hisclass = TRUE,
#'             status = TRUE,
#'             inc_score = TRUE) %>%
#'   occ_sector("standard_occupation",
#'              label = TRUE)
#'
#' # Example 3: Working with pre-standardized data
#' # If occupations are already standardized:
#' df_standardized <- data.frame(
#'   occ_stand = c("smed", "snickare", "soldat")
#' )
#'
#' df_coded <- occ_hisco(df_standardized, "occ_stand",
#'                       hisclass = TRUE)
#' }
#'
#' @references
#' van Leeuwen, M.H.D., Maas, I., & Miles, A. (2002). HISCO: Historical
#' International Standard Classification of Occupations. Leuven: Leuven
#' University Press.
#'
#' van Leeuwen, M.H.D. & Maas, I. (2011). HISCLASS: A Historical
#' International Social Class Scheme. Leuven: Leuven University Press.
#'
#' @author
#' William Skoglund \email{william.skoglund@@ekhist.uu.se}
#'
#' Department of Economic History, Uppsala University
#'
#' @seealso
#' Useful links:
#' \itemize{
#'   \item \url{https://github.com/williamskoglund/histocc}
#'   \item Report bugs at \url{https://github.com/williamskoglund/histocc/issues}
#' }
#'
#' @docType package
#' @name histocc-package
#' @aliases histocc
NULL
