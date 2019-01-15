#' @import dplyr
runs.analysis <- function(x) {
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
      # x$cl <- sum(x$y[base] * x$y.length[base]) / sum(x$y.length[base])
    } else { # Constant subgroup size: Montgomery 6.29
      x$cl <- mean(x$y.sd, na.rm = TRUE)
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
  sigma_z <- mean(abs(diff(z_i)), na.rm = TRUE) / 1.128
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
  sigma_z <- mean(abs(diff(z_i)), na.rm = TRUE) / 1.128
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
  x$cl <- stats::median(x$y, na.rm = TRUE)
  
  return(x)
}

c4 <- function(n) {
  n[n <= 1] <- NA
  # sqrt(2 / (n - 1)) * gamma(n / 2) / gamma((n - 1) / 2)
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

# a3 <- function(n) {
#   n[n == 0]    <- NA
#   tbl          <- c(NA,
#                     2.659, 1.954, 1.628, 1.427, 1.287, 1.182,
#                     1.099, 1.032, 0.975, 0.927, 0.886, 0.850,
#                     0.817, 0.789, 0.763, 0.739, 0.718, 0.698,
#                     0.680, 0.663, 0.647, 0.633, 0.619, 0.606)
#   # x            <- 3 / (4 * (n - 1)) * (4 * n - 3) / sqrt(n)
#   x            <- 3 / c4(n) / sqrt(n)
#   w            <- which(n <= 25)
#   x[w]         <- tbl[n[w]]
#   x[is.nan(x)] <- NA
#   return(x)
# }
# 
# b3 <- function(n) {
#   n[n == 0]    <- NA
#   tbl          <- c(NA,
#                     0.000, 0.000, 0.000, 0.000, 0.030, 0.118,
#                     0.185, 0.239, 0.284, 0.321, 0.354, 0.382,
#                     0.406, 0.428, 0.448, 0.466, 0.482, 0.497,
#                     0.510, 0.523, 0.534, 0.545, 0.555, 0.565)
#   x            <- 1 - (3 / c4(n) / sqrt(2 * (n - 1)))
#   w            <- which(n <= 25)
#   x[w]         <- tbl[n[w]]
#   x[is.nan(x)] <- NA
#   return(x)
# }
# 
# b4 <- function(n) {
#   n[n == 0]    <- NA
#   tbl          <- c(NA,
#                     3.267, 2.568, 2.266, 2.089, 1.970, 1.882,
#                     1.815, 1.761, 1.716, 1.679, 1.646, 1.618,
#                     1.594, 1.572, 1.552, 1.534, 1.518, 1.503,
#                     1.490, 1.477, 1.466, 1.455, 1.445, 1.435)
#   x            <- 1 + (3 / c4(n) / sqrt(2 * (n - 1)))
#   w            <- which(n <= 25)
#   x[w]         <- tbl[n[w]]
#   x[is.nan(x)] <- NA
#   return(x)
# }
# 
# c4 <- function(n) {
#   n[n == 0]   <- NA
#   tbl         <- c(NA,
#                    0.7979, 0.8862, 0.9213, 0.9400, 0.9515, 0.9594,
#                    0.9650, 0.9693, 0.9727, 0.9754, 0.9776, 0.9794,
#                    0.9810, 0.9823, 0.9835, 0.9845, 0.9854, 0.9862,
#                    0.9869, 0.9876, 0.9882, 0.9887, 0.9892, 0.9896)
#   
#   x            <- 4 * (n - 1) / (4 * n - 3)
#   w            <- which(n <= 25)
#   x[w]         <- tbl[n[w]]
#   x[is.nan(x)] <- NA
#   return(x)
# }

# Format line labels function
lab.format <- function(x, decimals = 1, percent = FALSE) {
  if (percent) x <- x * 100
  x <- sprintf(paste0("%.", decimals, "f"), x)
  if (percent) x <- paste0(x, '%')
  
  x
}

# Make parts function
makeparts <- function(x, n) {
  x <- unique(c(0, x))
  x <- x[x >= 0 & x < n]
  x <- x[order(x)]
  x <- rep(c(seq_along(x)), diff(c(x, n)))
}

# Fix notes function
fixnotes <- function(x) {
  x <- gsub("\\|{2, }", "\\|", x)
  x <- gsub("^\\||\\|$", "", x)
  x <- gsub("\\|", " | ", x)
  x <- gsub("^$", NA, x)
}

# Function for data aggregation and analysis
qic.agg <- function(d, got.n, part, agg.fun, freeze, exclude, 
                    chart.fun, multiply, dots.only, chart, y.neg) {
  x      <- quo(x)
  y      <- quo(y)
  n      <- quo(n)
  cl     <- quo(cl)
  target <- quo(target)
  notes  <- quo(notes)
  facet1 <- quo(facet1)
  facet2 <- quo(facet2)
  
  d <- d %>% 
    filter(!is.na(!!x)) %>% 
    group_by(!!x, !!facet1, !!facet2) %>% 
    summarise(y.sum    = sum(!!y, na.rm = TRUE),
              y.length = sum(!is.na(!!y)),
              y.mean   = mean(!!y, na.rm = TRUE),
              y.sd     = stats::sd(!!y, na.rm = TRUE),
              n        = sum(!!n, na.rm = got.n),
              y        = ifelse(got.n,
                                y.sum / n,
                                do.call(agg.fun, list(y, na.rm = TRUE))),
              cl       = first(!!cl),
              target   = first(!!target),
              notes    = paste(!!notes, collapse = '|')
    ) %>% 
    group_by(facet1, facet2) %>%
    mutate(part = makeparts(part, n()),
           xx   = seq_along(part)) %>% 
    ungroup() %>% 
    mutate(baseline = xx <= freeze,
           include  = !xx %in% exclude,
           notes    = fixnotes(notes))
  
  d <- split(d, d[c('facet1', 'facet2', 'part')]) %>% 
    lapply(chart.fun) %>% 
    lapply(runs.analysis) %>% 
    lapply(function(x) {
      within(x, {
        y          <- y * multiply
        cl         <- cl * multiply
        lcl        <- lcl * multiply
        ucl        <- ucl * multiply
        cl.lab     <- ifelse(xx == max(xx), cl, NA)
        lcl.lab    <- ifelse(xx == max(xx), lcl, NA)
        ucl.lab    <- ifelse(xx == max(xx), ucl, NA)
        target.lab <- ifelse(xx == max(xx), target, NA)
      })
    })
  
  d <- do.call(rbind, d) %>% 
    arrange(!!facet1, !!facet2, !!x)
  
  # Remove control lines from missing subgroups
  d$ucl[!is.finite(d$ucl)] <- NA
  d$lcl[!is.finite(d$lcl)] <- NA
  
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
  options(qic.linecol = NULL,
          qic.signalcol = NULL,
          qic.targetcol = NULL,
          qic.clshade   = NULL)
}