#' Hospital acquired infections
#' 
#' A dataset containing the number of hospital acquired bacteremia, Clostridium 
#' difficile infections, and urinary tract infections in six hospitals in the 
#' Capital Region of Denmark 2015-2016.
#' 
#' @format A data frame with 432 rows and 5 variables: \itemize{ \item 
#'   \strong{hospital} Abbreviated hospital name. \item \strong{infection} Type 
#'   of infection. BAC: Bacteremia, CDI: Clostridium difficile infection. UTI:
#'   Urinary tract infection. \item \strong{month} First day of month. \item
#'   \strong{n} Number of cases. \item \strong{days} Number of risk days. A risk
#'   day is a patient day without infection }
#' @source \url{http://www.haiba.dk/} (Capital Region of Denmark)
"hospital_infections"

#' Coronary artery bypass graft operations
#' 
#' A dataset with data on individual coronary artery bypass graft operations.
#' 
#' @format A data frame with 2205 rows and 6 variables: \itemize{ \item
#'   \strong{data} Date of operation. \item \strong{age} Patient age in years.
#'   \item \strong{gender} Patient gender. \item \strong{los} Length og stay in
#'   days. \item \strong{death} TRUE if patient died within 30 days after
#'   surgery. \item \strong{readmission} TRUE if patient were readmitted for any
#'   reason within 30 days after surgery. }
"cabg"

#' Clostridium difficile infections
#' 
#' A dataset with data on hospital acquired Clostridium difficile infections 
#' (CDI) before and after an intervention to reduce the risk of CDI.
#' 
#' @format A data frame with 36 rows and 5 variables: \itemize{ \item 
#'   \strong{month} Month of observation. \item \strong{n} Number of hospital 
#'   acquired CDI. \item \strong{days} Number of risk days. A risk day is a 
#'   patient day without CDI. \item \strong{period} Factor indicating the period
#'   'pre' or 'post' intervention. \item \strong{notes} Annotations. }
#' @source \url{http://www.haiba.dk/} (Amager Hvidovre Hospital)
"cdi"

#' NHS accidents
#' 
#' The number of attendances to major accident and emergency hospital 
#' departments in the NHS that were seen within 4 hours of arrival over twenty 
#' weeks.
#' 
#' @format A data frame with 20 rows and 3 variables: \itemize{ \item i Week 
#'   number. \item r Attendances seen within 4 hours. \item n Total number of 
#'   attendances. }
#' @source Mohammed MA, et al. Quality and Safety in Health Care
#'   2013;22:362–368. \url{http://dx.doi.org/10.1136/bmjqs-2012-001373}
"nhs_accidents"

#' Patient harm indentified using the Global Trigger Tool
#' 
#' A dataset with data on adverse events during hospitalisation found by the
#' Global Trigger Tool.
#' 
#' @format A data frame with 340 rows and 11 variables: \itemize{ \item 
#'   admission_id Admission ID. \item admission_dte Date of admission. \item 
#'   discharge_dte Date of discharge. \item month Month of discharge. \item days
#'   Duration of hospital stay in days. \item harms Number of adverse events
#'   \item {E-I} Type of adverse event by severity category. E-F: Temporary
#'   harm; G-H: Permanent harm; I: Fatal harm. }
#' @references \url{http://www.ihi.org/resources/Pages/Tools/IHIGlobalTriggerToolforMeasuringAEs.aspx}
"gtt"