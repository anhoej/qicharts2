#' Hospital infections
#'
#' A dataset containing the number and types of hospital acquired infections in
#' six hospitals in the Capital Region of Denmark 2015-2016.
#'
#' @format A data frame with 432 rows and 5 variables: \itemize{ \item
#'   \strong{hospital} Abbreviated hospital name. \item \strong{infection} Type
#'   of infection. BAC: Bacteremia, CDI: Clostridium difficile infection, UTI,
#'   Urinary tract infection. \item \strong{month} First day of month. \item
#'   \strong{n} Number of cases. \item \strong{days} Number of risk days. }
#' @source \url{http://www.haiba.dk/}
"hospital_infections"

#' CABG
#' 
#' A dataset with data on individual coronary artery bypass graft operations.
#' 
#' @format A data frame with 2205 rows and 7 variables: \itemize{ \item \strong{no}
#'   Operation number. \item \strong{data} Date of operation. \item \strong{age}
#'   Patient age in years. \item \strong{gender} Patient gender. \item
#'   \strong{los} Length og stay in days. \item \strong{death} TRUE if patient
#'   died with 30 days after surgery. \item \strong{readmission} TRUE if patient
#'   were readmitted for any reason within 30 days after surgery. }
"cabg"
