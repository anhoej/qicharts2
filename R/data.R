#' Hospital acquired infections
#' 
#' A dataset containing the number of hospital acquired bacteremia, Clostridium 
#' difficile infections, and urinary tract infections in six hospitals in the 
#' Capital Region of Denmark 2015-2016.
#' 
#' @format A data frame with 432 rows and 5 variables: \itemize{ \item 
#'  {hospital} Abbreviated hospital name. \item{infection} Type 
#'   of infection. BAC: Bacteremia, CDI: Clostridium difficile infection. UTI:
#'   Urinary tract infection. \item{month} First day of month. \item
#'  {n} Number of cases. \item{days} Number of risk days. A risk
#'   day is a patient day without infection. }
#' @source \url{http://www.haiba.dk/} (Capital Region of Denmark).
"hospital_infections"

#' Coronary artery bypass graft operations
#' 
#' A dataset with data on individual coronary artery bypass graft operations.
#' 
#' @format A data frame with 2205 rows and 6 variables: \itemize{ \item
#'  {data} Date of operation. \item{age} Patient age in years.
#'   \item{gender} Patient gender. \item{los} Length og stay in
#'   days. \item{death} TRUE if patient died within 30 days after
#'   surgery. \item{readmission} TRUE if patient were readmitted for any
#'   reason within 30 days after surgery. }
#' @source Omitted for privacy concerns.
"cabg"

#' Clostridium difficile infections
#' 
#' A dataset with data on hospital acquired Clostridium difficile infections 
#' (CDI) before and after an intervention to reduce the risk of CDI.
#' 
#' @format A data frame with 36 rows and 5 variables: \itemize{ \item 
#'  {month} Month of observation. \item{n} Number of hospital 
#'   acquired CDI. \item{days} Number of risk days. A risk day is a 
#'   patient day without CDI. \item{period} Factor indicating the period
#'   'pre' or 'post' intervention. \item{notes} Annotations. }
#' @source \url{http://www.haiba.dk/} (Amager Hvidovre Hospital).
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
#'   2013;22:362–368. \url{https://doi.org/10.1136/bmjqs-2012-001373}.
"nhs_accidents"

#' Patient harm indentified using the Global Trigger Tool
#' 
#' A dataset with data on adverse events during hospitalisation found by the
#' Global Trigger Tool.
#' 
#' @format A data frame with 340 rows and 11 variables: \itemize{ \item 
#'   admission_id Admission ID. \item admission_dte Date of admission. \item 
#'   discharge_dte Date of discharge. \item month Month of discharge. \item days
#'   Duration of hospital stay in days. \item harms Number of adverse events.
#'   \item {E-I} Type of adverse event by severity category. E-F: Temporary
#'   harm; G-H: Permanent harm; I: Fatal harm. }
#' @source Omitted for privacy concerns.
#' @references \url{http://www.ihi.org/resources/Pages/Tools/IHIGlobalTriggerToolforMeasuringAEs.aspx}
"gtt"
