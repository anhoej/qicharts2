runs.analysis <- function(x, method) {
  y                  <- x$y[x$include]
  cl                 <- x$cl[x$include]
  runs               <- sign(y - cl)
  runs               <- runs[runs != 0 & !is.na(runs)]
  n.useful           <- length(runs)
  n.obs <- length(y)
  
  if (n.useful) {
    run.lengths      <- rle(runs)$lengths
    n.runs           <- length(run.lengths)
    longest.run      <- max(run.lengths)
    longest.run.max  <- round(log2(n.useful)) + 3  # Schilling 2012
    n.crossings      <- max(n.runs - 1, 0)
    n.crossings.min  <- stats::qbinom(0.05,        # Chen 2010 (7)
                                      max(n.useful - 1, 0), 0.5)
    runs.signal      <- longest.run > longest.run.max ||
      n.crossings < n.crossings.min
    runs.signal <- crsignal(n.useful,
                            n.crossings,
                            longest.run,
                            method = method)
  } else {
    longest.run      <- NA
    longest.run.max  <- NA
    n.crossings      <- NA
    n.crossings.min  <- NA
    runs.signal      <- FALSE
  }

  x$n.obs           <- n.obs
  x$n.useful        <- n.useful
  x$runs.signal     <- runs.signal
  x$longest.run     <- longest.run
  x$longest.run.max <- longest.run.max
  x$n.crossings     <- n.crossings
  x$n.crossings.min <- n.crossings.min
  
  return(x)
}

crsignal <- function(n, c, l, method = c('anhoej', 'bestbox', 'cutbox')) {
  method <- match.arg(method)
  
  if (!(method %in% c('anhoej', 'bestbox', 'cutbox')))
    stop('method should be \"anhoej\", \"bestbox\", or \"cutbox\"')
  
  if ((n < 10 | n > 100) & method != 'anhoej') {
    message('Best box and cut box computations only available for n between ',
            '10 and 100. Using method = "anhoej"')
    method <- 'anhoej'
  }
  
  if(method != 'anhoej')
    message('Runs analysis using ', method, ' method')
  
  if (!(c %in% c(0:n - 1)))
    stop('c should be an integer between 0 and ', n - 1)
  
  if (!(l %in% c(1:n)))
    stop('l should be an integer between 1 and ', n)
  
  bounds <- data.frame(
    n = 10:100,
    cb    = c(  2,  3,  3,  3,  3,  4,  5,  5,  5,  5,  6,  7,  6,
                6,  6,  6,  9,  9,  9, 10, 11, 11, 11, 11, 11, 12,
               13, 14, 13, 15, 15, 15, 14, 14, 17, 17, 17, 17, 19,
               19, 19, 19, 19, 21, 21, 21, 21, 23, 23, 23, 23, 23,
               25, 25, 26, 26, 27, 27, 27, 28, 29, 29, 29, 30, 30,
               31, 31, 31, 32, 33, 33, 33, 34, 33, 35, 35, 35, 35, 
               37, 37, 38, 37, 39, 39, 39, 39, 39, 41, 41, 42, 41),
    lb    = c(  6,  7,  6,  6,  6,  7,  8,  7,  7,  7,  7,  8,  7,
                7,  7,  7,  9,  8,  8,  8, 10,  9,  8,  8,  8,  8,
                9, 10,  8, 11,  9,  9,  8,  8, 10,  9,  9,  9, 12,
               10,  9,  9,  9, 11, 10,  9,  9, 12, 10, 10,  9,  9,
               11, 10, 11, 10, 12, 10, 10, 11, 14, 11, 10, 11, 10,
               12, 11, 10, 11, 13, 11, 10, 11, 10, 11, 11, 10, 10, 
               12, 11, 12, 10, 13, 11, 11, 10, 10, 12, 11, 12, 10), 
    cbord = c(  3,  4, NA, NA, NA,  6,  6, NA,  6,  6, NA, NA,  7,
                7,  7, NA, 10, 10, 11, NA, 12, 14, NA, 12, 13, NA, 
               15, NA, NA, NA, NA, 17, NA, NA, NA, NA, 19, 20, 20, 
               21, NA, 21, 21, 23, 23, NA, 23, 25, 24, 26, NA, 24,
               27, 27, 27, 27, 29, NA, 29, 29, 30, 31, 30, 31, NA, 
               32, 34, 33, 33, 37, 35, NA, 36, 36, NA, 38, 36, 38, 
               38, 39, NA, 39, 41, 40, 42, NA, 41, 42, 44, 43, 42),
    lbord = c(  5,  6, NA, NA, NA,  6,  7, NA,  6,  5, NA, NA,  6,
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

qic.run <- function(x) {
  base  <- x$baseline & x$include
  if (anyNA(x$cl))
    x$cl  <- stats::median(x$y[base], na.rm = TRUE)
  x$ucl <- as.numeric(NA)
  x$lcl <- as.numeric(NA)
  
  return(x)
}

qic.i <- function(x) {
  base <- x$baseline & x$include
  if (anyNA(x$cl))
    x$cl <- mean(x$y[base], na.rm = TRUE)
  
  # Average moving range
  mr  <- abs(diff(x$y[base] - x$cl[base]))
  amr <- mean(mr, na.rm = TRUE)
  
  # Upper limit for moving ranges
  ulmr <- 3.267 * amr
  
  # Remove moving ranges greater than ulmr and recalculate amr, Nelson 1982
  mr  <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)
  
  # Calculate standard deviation, Montgomery, 6.33
  stdev <- amr / 1.128
  
  # Calculate control limits
  x$lcl <- x$cl - 3 * stdev
  x$ucl <- x$cl + 3 * stdev
  
  return(x)
}

qic.mr <- function(x) {
  base <- x$baseline & x$include
  x$y  <- c(NA, abs(diff(x$y)))
  
  # Calculate centre line
  if (anyNA(x$cl))
    x$cl <- mean(x$y[base], na.rm = TRUE)
  
  # Calculate upper limit for moving ranges
  x$lcl <- 0
  x$ucl <- 3.267 * x$cl
  
  return(x)
}

qic.xbar <- function(x){
  base  <- x$baseline & x$include
  var.n <- as.logical(length(unique(x$y.length)) - 1)
  
  # Calculate centre line, Montgomery 6.30
  if (anyNA(x$cl)) {
    x$cl <- sum(x$y.length[base] * x$y.mean[base], na.rm = TRUE) /
      sum(x$y.length[base], na.rm = TRUE)
  }
  
  # Calculate standard deviation and control limits, Montgomery 6.29 or 6.31
  if (var.n) {
    stdev <- sqrt(sum((x$y.length[base] - 1) * x$y.sd[base]^2, na.rm = TRUE) /
                    sum(x$y.length[base] - 1, na.rm = TRUE))
  } else {
    stdev <- mean(x$y.sd[base], na.rm = TRUE)
  }
  A3    <- a3(x$y.length)
  x$ucl <- x$cl + A3 * stdev
  x$lcl <- x$cl - A3 * stdev
  
  return(x)
}

qic.s <- function(x){
  base  <- x$baseline & x$include
  var.n <- as.logical(length(unique(x$y.length)) - 1)
  x$y   <- x$y.sd
  
  # Calculate centre line and control limits
  if (anyNA(x$cl)) {
    if (var.n) { # Variable subgroup size: Montgomery 6.31
      x$cl <- sqrt(sum((x$y.length[base] - 1) * x$y.sd[base]^2, na.rm = TRUE) /
                     sum(x$y.length[base] - 1, na.rm = TRUE))
    } else { # Constant subgroup size: Montgomery 6.29
      x$cl <- mean(x$y.sd[base], na.rm = TRUE)
    }
  }
  B3     <- b3(x$y.length)
  B4     <- b4(x$y.length)
  x$ucl  <- B4 * x$cl
  x$lcl  <- B3 * x$cl
  
  return(x)
}

qic.t <- function(x) {
  if (min(x$y, na.rm = TRUE) <= 0) {
    stop('Time between events must be greater than zero')
  }
  
  # Transform y variable and run I chart calculations
  x$y <- x$y^(1 / 3.6)
  x   <- qic.i(x)
  
  # Back transform centre line and control limits
  x$y   <- x$y^3.6
  x$cl  <- x$cl^3.6
  x$ucl <- x$ucl^3.6
  x$lcl <- x$lcl^3.6
  x$lcl[x$lcl < 0 | is.nan(x$lcl)] <- 0
  
  return(x)
}

qic.p <- function(x) {
  base <- x$baseline & x$include
  
  if (anyNA(x$cl)) {
    x$cl <- sum(x$y.sum[base], na.rm = TRUE) /
      sum(x$n[base], na.rm = TRUE)
  }
  
  # Calculate standard deviation
  stdev <- sqrt(x$cl * (1 - x$cl) / x$n)
  
  # Calculate control limits
  x$ucl          <- x$cl + 3 * stdev
  x$lcl          <- x$cl - 3 * stdev
  x$ucl[x$ucl > 1 & is.finite(x$ucl)] <- 1
  x$lcl[x$lcl < 0 & is.finite(x$lcl)] <- 0
  
  return(x)
}

qic.pp <- function(x) {
  base <- x$baseline & x$include
  
  if (anyNA(x$cl)) {
    x$cl <- sum(x$y.sum[base], na.rm = TRUE) /
      sum(x$n[base], na.rm = TRUE)
  }
  
  # Calculate standard deviation
  stdev <- sqrt(x$cl * (1 - x$cl) / x$n)
  
  # Calculate standard deviation for Laney's P prime chart, incorporating
  # between-subgroup variation.
  z_i     <- (x$y[base] - x$cl[base]) / stdev[base]
# TESTING ##############################################
  if(is.factor(x$x) || is.character(x$x))
    sigma_z <- stats::sd(z_i)
  else
    sigma_z <- mean(abs(diff(z_i)), na.rm = TRUE) / 1.128
# TESTING ##############################################  
  
  stdev   <- stdev * sigma_z
  
  x$ucl          <- x$cl + 3 * stdev
  x$lcl          <- x$cl - 3 * stdev
  x$ucl[x$ucl > 1 & is.finite(x$ucl)] <- 1
  x$lcl[x$lcl < 0 & is.finite(x$lcl)] <- 0
  
  return(x)
}

qic.c <- function(x){
  base <- x$baseline & x$include
  x$y <- x$y.sum
  
  if (anyNA(x$cl)) {
    x$cl <- mean(x$y[base], na.rm = TRUE)
  }
  
  # Calculate standard deviation, Montgomery 7.17
  stdev <- sqrt(x$cl)
  
  # Calculate control limits
  x$ucl          <- x$cl + 3 * stdev
  x$lcl          <- x$cl - 3 * stdev
  x$lcl[x$lcl < 0 & is.finite(x$lcl)] <- 0
  
  return(x)
}

qic.u <- function(x){
  base <- x$baseline & x$include
  
  if (anyNA(x$cl)) {
    x$cl   <- sum(x$y.sum[base], na.rm = TRUE) / sum(x$n[base], na.rm = TRUE)
  }
  
  # Calculate standard deviation, Montgomery 7.19
  stdev <- sqrt(x$cl / x$n)
  
  # Calculate control limits
  x$ucl          <- x$cl + 3 * stdev
  x$lcl          <- x$cl - 3 * stdev
  x$lcl[x$lcl < 0 & is.finite(x$lcl)] <- 0
  
  return(x)
}

qic.up <- function(x){
  base <- x$baseline & x$include
  
  if (anyNA(x$cl)) {
    x$cl   <- sum(x$y.sum[base], na.rm = TRUE) / sum(x$n[base], na.rm = TRUE)
  }
  
  # Calculate standard deviation, Montgomery 7.19
  stdev <- sqrt(x$cl / x$n)
  
  # Calculate standard deviation for Laney's u-prime chart, incorporating
  # between-subgroup variation.
  z_i     <- (x$y[base] - x$cl[base]) / stdev[base]
  
  # TESTING ##############################################
  # sigma_z <- mean(abs(diff(z_i)), na.rm = TRUE) / 1.128
  if(is.factor(x$x) || is.character(x$x))
    sigma_z <- stats::sd(z_i)
  else
    sigma_z <- mean(abs(diff(z_i)), na.rm = TRUE) / 1.128
  # TESTING ##############################################  
  
  stdev   <- stdev * sigma_z
  
  # Calculate limits
  x$ucl          <- x$cl + 3 * stdev
  x$lcl          <- x$cl - 3 * stdev
  x$lcl[x$lcl < 0 & is.finite(x$lcl)] <- 0
  
  return(x)
}

qic.g <- function(x){
  base <- x$baseline & x$include
  
  # Calculate centre line
  calccl <- anyNA(x$cl)
  if (anyNA(x$cl)) {
    x$cl <- mean(x$y[base], na.rm = TRUE)
  }
  
  
  # Calculate standard deviation, Montgomery, p. 319
  stdev <- sqrt(x$cl * (x$cl + 1))
  
  # Calculate control limits
  x$ucl          <- x$cl + 3 * stdev
  x$lcl          <- x$cl - 3 * stdev
  x$lcl[x$lcl < 0] <- 0
  
  # # Set centre line to theoretical median, Provost (2011) p. 228
  # x$cl <- 0.693 * x$cl
  
  # Set centre line to median
  if(calccl) {
    x$cl <- stats::median(x$y[base], na.rm = TRUE)
  } else {
    x$cl <- 0.693 * x$cl
  }
  
  return(x)
}

c4 <- function(n) {
  n[n <= 1] <- NA
  sqrt(2 / (n - 1)) * exp(lgamma(n / 2) - lgamma((n - 1) / 2))
}

c5 <- function(n) {
  n[n <= 1] <- NA
  sqrt(1 - c4(n) ^ 2)
}

a3 <- function(n) {
  n[n <= 1] <- NA
  3 / (c4(n) * sqrt(n))
}

b3 <- function(n) {
  n[n <= 1] <- NA
  pmax(0, 1 - 3 * c5(n) / c4(n))
}

b4 <- function(n) {
  n[n <= 1] <- NA
  1 + 3 * c5(n) / c4(n)
}

# Format line labels function
lab.format <- function(x, decimals = 1, percent = FALSE) {
  if (percent) x <- x * 100
  x <- sprintf(paste0("%.", decimals, "f"), x)
  if (percent) x <- paste0(x, '%')
  
  return(x)
}

# Make parts function
makeparts <- function(x, n) {
  x <- unique(c(0, x))
  x <- x[x >= 0 & x < n]
  x <- x[order(x)]
  x <- rep(c(seq_along(x)), diff(c(x, n)))
  
  return(x)
}

# Fix notes function
fixnotes <- function(x) {
  x <- gsub("\\|{2, }", "\\|", x)
  x <- gsub("^\\||\\|$", "", x)
  x <- gsub("\\|", " | ", x)
  x <- gsub("^$", NA, x)
  
  return(x)
}

# Function for data aggregation and analysis
qic.agg <- function(d, got.n, part, agg.fun, freeze, exclude, 
                    chart.fun, multiply, dots.only, chart, method, y.neg) {
  d <- d[!is.na(d$x), ]
  d <- split(d, d[,c('x', 'facet1', 'facet2')])
  d <- lapply(d, function(x) {
    data.frame(x        = x$x[1],
               facet1   = x$facet1[1],
               facet2   = x$facet2[1],
               y.sum    = sum(x$y, na.rm = TRUE),
               y.length = sum(!is.na(x$y)),
               y.mean   = mean(x$y, na.rm = TRUE),
               y.sd     = stats::sd(x$y, na.rm = TRUE),
               n        = sum(x$n, na.rm = got.n),
               y        = ifelse(got.n,
                                 sum(x$y, na.rm = TRUE) / 
                                   sum(x$n, na.rm = got.n),
                                 do.call(agg.fun, list(x$y, na.rm = TRUE))),
               cl       = x$cl[1],
               target   = x$target[1],
               notes    = paste(unique(x$notes), collapse = '|'))
  })
  
  d <- do.call(rbind, d)
  
  d <- split(d, d[c('facet1', 'facet2')]) 
  d <- lapply(d, function(x) {
    x$part <- makeparts(part, nrow(x))
    x$xx <- seq_along(x$part)
    x
  })
  
  d <- do.call(rbind, d)
  
  d$baseline <- d$xx <= freeze
  d$include  <- !d$xx %in% exclude
  d$notes    <- fixnotes(d$notes)
  
  d <- split(d, d[c('facet1', 'facet2', 'part')])
  d <- lapply(d, chart.fun)
  d <- lapply(d, runs.analysis, method)
  d <- lapply(d, function(x) {
    x$y          <- x$y * multiply
    x$cl         <- x$cl * multiply
    x$lcl        <- x$lcl * multiply
    x$ucl        <- x$ucl * multiply
    x$cl.lab     <- ifelse(x$xx == max(x$xx), x$cl, NA)
    x$lcl.lab    <- ifelse(x$xx == max(x$xx), x$lcl, NA)
    x$ucl.lab    <- ifelse(x$xx == max(x$xx), x$ucl, NA)
    x$target.lab <- ifelse(x$xx == max(x$xx), x$target, NA)
    x
  })
  
  d <- do.call(rbind, c(d, make.row.names = F))
  
  # Remove control lines from missing subgroups
  d$ucl[!is.finite(d$ucl)]         <- NA
  d$lcl[!is.finite(d$lcl)]         <- NA
  d$lcl.lab[!is.finite(d$lcl.lab)] <- NA
  d$ucl.lab[!is.finite(d$ucl.lab)] <- NA
  
  # Add sigma signals
  d$sigma.signal                        <- d$y > d$ucl | d$y < d$lcl
  d$sigma.signal[is.na(d$sigma.signal)] <- FALSE
  
  # Ignore runs analysis if subgroups are categorical or if chart type is MR
  if (dots.only || chart == 'mr')
    d$runs.signal <- FALSE
  
  # Prevent negative y axis if y.neg argument is FALSE
  if (!y.neg & min(d$y, na.rm = TRUE) >= 0) {
    d$lcl[d$lcl < 0]         <- 0
    d$lcl.lab[d$lcl.lab < 0] <- 0
  }
  
  return(d)
}

.onAttach <- function(libname, pkgname) {
  options(qic.linecol   = '#5DA5DA',
          qic.signalcol = '#F15854',
          qic.targetcol = '#059748',
          qic.clshade   = TRUE)
}

.onDetach <- function(libpath) {
  options(qic.linecol   = NULL,
          qic.signalcol = NULL,
          qic.targetcol = NULL,
          qic.clshade   = NULL)
}