crsignal <- function(n, c, l, method = c('anhoej', 'bestbox', 'cutbox')) {
  method <- match.arg(method)
  
  if (!(method %in% c('anhoej', 'bestbox', 'cutbox')))
    stop('method should be \"anhoej\", \"bestbox\", or \"cutbox\"')
  
  if ((n < 10 | n > 100) & method != 'anhoej') {
    message('Best box and cut box computations only available for n between 10 and 100. Using method = "anhoej"')
    method <- 'anhoej'
  }
  
  if (!(c %in% c(0:n - 1)))
    stop(paste('c should be an integer between 0 and', n - 1))
  
  if (!(l %in% c(1:n)))
    stop(paste('l should be an integer between 1 and', n))
  
  message(paste('Runs analysis using', method, 'method'))
  
  bounds <- data.frame(
    n = 10:100,
    cb    = c( 2,  3,  3,  3,  3,  4,  5,  5,  5,  5,  6,  7,  6,
               6,  6,  6,  9,  9,  9, 10, 11, 11, 11, 11, 11, 12,
               13, 14, 13, 15, 15, 15, 14, 14, 17, 17, 17, 17, 19,
               19, 19, 19, 19, 21, 21, 21, 21, 23, 23, 23, 23, 23,
               25, 25, 26, 26, 27, 27, 27, 28, 29, 29, 29, 30, 30,
               31, 31, 31, 32, 33, 33, 33, 34, 33, 35, 35, 35, 35, 
               37, 37, 38, 37, 39, 39, 39, 39, 39, 41, 41, 42, 41),
    lb    = c( 6,  7,  6,  6,  6,  7,  8,  7,  7,  7,  7,  8,  7,
               7,  7,  7,  9,  8,  8,  8, 10,  9,  8,  8,  8,  8,
               9, 10,  8, 11,  9,  9,  8,  8, 10,  9,  9,  9, 12,
               10,  9,  9,  9, 11, 10,  9,  9, 12, 10, 10,  9,  9,
               11, 10, 11, 10, 12, 10, 10, 11, 14, 11, 10, 11, 10,
               12, 11, 10, 11, 13, 11, 10, 11, 10, 11, 11, 10, 10, 
               12, 11, 12, 10, 13, 11, 11, 10, 10, 12, 11, 12, 10), 
    cbord = c( 3,  4, NA, NA, NA,  6,  6, NA,  6,  6, NA, NA,  7,
               7,  7, NA, 10, 10, 11, NA, 12, 14, NA, 12, 13, NA, 
               15, NA, NA, NA, NA, 17, NA, NA, NA, NA, 19, 20, 20, 
               21, NA, 21, 21, 23, 23, NA, 23, 25, 24, 26, NA, 24,
               27, 27, 27, 27, 29, NA, 29, 29, 30, 31, 30, 31, NA, 
               32, 34, 33, 33, 37, 35, NA, 36, 36, NA, 38, 36, 38, 
               38, 39, NA, 39, 41, 40, 42, NA, 41, 42, 44, 43, 42),
    lbord = c( 5,  6, NA, NA, NA,  6,  7, NA,  6,  5, NA, NA,  6,
               6,  6, NA,  7,  7,  7, NA,  9,  8, NA,  7,  7, NA,
               8, NA, NA, NA, NA,  8, NA, NA, NA, NA,  8,  7, 11, 
               9, NA,  8,  7,  9,  8, NA,  8, 11,  9,  8, NA,  8,
               9,  9, 10,  9, 10, NA,  8,  8, 13,  9,  9, 10, NA,
               9,  8,  9,  8, 11,  9, NA, 10,  7, NA,  8,  9,  8,
              10,  9, NA,  9, 12, 10,  8, NA,  8,  9,  9, 10,  9))
  
  res <- FALSE
  
  if (method == 'anhoej') {
    can <- stats::qbinom(0.05, n - 1, 0.5)
    lan <- round(log2(n)) + 3
    if (!(c >= can & l <= lan))
      res <- TRUE
  } else if (method == 'bestbox') {
    cbn <- bounds$cb[bounds$n == n]
    lbn <- bounds$lb[bounds$n == n]
    if (!(c >= cbn & l <= lbn))
      res <- TRUE
  } else if (method == 'cutbox') {
    cbn    <- bounds$cb[bounds$n == n]
    lbn    <- bounds$lb[bounds$n == n]
    cbordn <- bounds$cbord[bounds$n == n]
    lbordn <- bounds$lbord[bounds$n == n]
    if (is.na(cbordn) & !(c >= cbn & l <= lbn))
      res <- TRUE
    else if (!is.na(cbordn)) {
      res <- TRUE
      if (c >= cbn + 1 & l <= lbn - 1)
        res <- FALSE
      if (c == cbn & l <= lbordn)
        res <- FALSE
      if (c >= cbordn & l <= lbn)
        res <- FALSE
    }
  }
  
  return(res)
}

# crsignal(11, 2, 6, 'a')
# crsignal(11, 2, 6, 'b')
# crsignal(11, 2, 6, 'c')
# crsignal(11, 3, 7, 'a')
# crsignal(11, 3, 7, 'b')
# crsignal(11, 3, 7, 'c')
# crsignal(11, 4, 7, 'a')
# crsignal(11, 4, 7, 'b')
# crsignal(11, 4, 7, 'c')
