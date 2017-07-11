#' Quality improvement charts.
#' 
#' The \code{qic} function creates run charts and Shewhart control charts for 
#' process control and improvement. Included control charts are: I, MR, Xbar, S,
#' T, C, U, Uprime, P, Pprime, and G charts.
#' 
#' Long explanation.
#' 
#' @param x Vector of subgroup values to plot along the x axis.
#' @param y Vector of measures or counts to plot on the y axis (numerator).
#' @param n Vector of subgroup sizes (denominator).
#' @param notes Character vector of notes to be added to individual data points.
#' @param data Data frame containing variables used in the plot.
#' @param facets One or two sided formula with factors used for facetting plots.
#' @param chart Character value indicating the chart type. Possible values are: 
#'   'run' (default), 'i', 'mr', 'xbar', 't', 's', 'c', 'u', 'uprime', 'p', 
#'   'pprime', and 'g'.
#' @param agg.fun Aggregate function for summarising the y variable if there are
#'   more than one observation per subgroup. Only relevant for run charts and I 
#'   charts. Possible values are: 'mean' (default), 'median', 'sum', and 'sd'.
#' @param multiply Number indicating a number to multiply y axis by, e.g. 100 
#'   for percents rather than proportions. See also \code{y.percent} argument.
#' @param freeze Integer indicating the last data point to include in 
#'   calculation of baseline paramenters for centre and control lines. Ignored 
#'   if break.points argument is given.
#' @param break.points Integer vector indicating break points before 
#'   recalculation of centre and control lines.
#' @param exclude Integer vector indicating data points to exclude from 
#'   calculations of centre and control lines.
#' @param target Numeric value indicating a target value to be plotted as a 
#'   horizontal line (same for each facet).
#' @param cl Numeric value indicating the centre line if known in advance.
#' @param nrow,ncol Number indicating the preferred number of rows and columns 
#'   in facets.
#' @param scales Character string, one of 'fixed' (default), 'free_y', 'free_x',
#'   or 'free' indicating whether y and x axis scales should be the same for all
#'   panels or free.
#' @param main Character string specifying the title of the plot.
#' @param xlab Character string specifying the x axis label.
#' @param ylab Character string specifying the y axis label.
#' @param subtitle Character string specifying the subtitle.
#' @param caption Character string specifying the caption.
#' @param part.labels Character vector of length two specifying before and after
#'   labels for chart parts created with freeze arguments.
#' @param show.linelabels Logical indicating whether to show labels for centre 
#'   and control lines on chart. Defaults to TRUE when \code{facets} is NULL.
#' @param digits Integer indicating the preferred number of digits in centre and
#'   control line labels.
#' @param x.format Date format of x axis labels. See \code{?strftime()} for 
#'   possible date formats.
#' @param x.angle Number indicating the angle of x axis labels.
#' @param y.expand Numeric value to include in y axis. Useful e.g. for starting
#'   the y axis at zero.
#' @param y.neg If TRUE (default), the y axis is allowed to be negative (only 
#'   relevant for I and Xbar charts).
#' @param y.percent Logical. If TRUE, formats y axis labels as percent. y axis 
#'   at zero.
#' @param show.grid If TRUE shows grid.
#' @param flip If TRUE rotates the plot 90 degrees.
#' @param print.summary If TRUE, prints summary.
#' @param ... Additional arguments to plot function.
#'   
#' @return A \code{qic} object. Inherits from 'ggplot'.
#'   
#' @examples
#' # Lock random number generator to make reproducible results.
#' set.seed(2)
#' 
#' # Run chart from 24 random normal values
#' qic(rnorm(24))
#' 
#' # Run chart with non-random variation
#' qic(c(rnorm(12), rnorm(12, 2)))
#' 
#' # C control chart from 24 random poisson variables
#' qic(rpois(24, 16), chart = 'c')
#' 
#' # C control chart with special cause variation
#' qic(c(rpois(23, 16), 36), chart = 'c')
#' 
#' # U chart from build-in data set of hospital infection rates faceted
#' #   by hospital and type of infection.
#' qic(month, n, days,
#'   data = hospital_infections,
#'   facets = hospital ~ infection,
#'   chart = 'u',
#'   multiply = 10000,
#'   main = 'Hospital infection rates',
#'   ylab = 'Number of infections per 10.000 risk days',
#'   xlab = 'Month')
#' 
#' @importFrom stats median
#' @export

qic <- function(x,
                y               = NULL,
                n               = NULL,
                notes           = NULL,
                data            = NULL,
                facets          = NULL,
                chart           = c('run', 'i', 'mr', 'xbar', 's', 't',
                                    'p', 'pprime', 'c', 'u', 'uprime', 'g'),
                agg.fun         = c('mean', 'median', 'sum', 'sd'),
                multiply        = 1,
                freeze          = NULL,
                break.points    = NULL,
                exclude         = NULL,
                target          = NA * 1,
                cl              = NULL,
                nrow            = NULL,
                ncol            = NULL,
                scales          = 'fixed',
                main            = NULL,
                ylab            = 'Value',
                xlab            = 'Subgroup',
                subtitle        = NULL,
                caption         = NULL,
                part.labels     = NULL,
                show.linelabels = is.null(facets),
                digits          = 3,
                x.format        = NULL,
                x.angle         = NULL,
                y.expand        = NULL,
                y.neg           = TRUE,
                y.percent       = FALSE,
                show.grid       = FALSE,
                flip            = FALSE,
                print.summary   = FALSE,
                ...) {
  
  # Preserve show.linelabels value
  show.linelabels <- show.linelabels
  y.name <- deparse(substitute(y))
  
  # Get chart type
  chart.fun <- get(paste0('qic.', match.arg(chart)))
  
  # Check data and get variables
  if (missing(x)) {
    stop('Missing mandatory argument \"x\"')
  }
  
  x      <- eval(substitute(x), data, parent.frame())
  y      <- eval(substitute(y), data, parent.frame())
  n      <- eval(substitute(n), data, parent.frame())
  notes  <- eval(substitute(notes), data, parent.frame())
  facets <- all.vars(facets)
  
  if(is.null(data)) {
    facets <- mget(facets, parent.frame())
  } else {
    facets <- mget(facets, as.environment(data))
  }

  facets <- as.data.frame(facets)
  
  if(ncol(facets) %in% 1:2) {
    names(facets) <- paste0('facet', seq_len(ncol(facets)))
    facets[setdiff(c('facet1', 'facet2'), names(facets))] <- 1
  } else {
    facets <- data.frame(facet1 = 1, facet2 = 1)
  }
  
  if (is.null(y)) {
    y <- x
    x <- seq_along(y)
  }
  
  if (is.null(n)) {
    n <- NA
    got.n <- FALSE
  } else {
    got.n <- TRUE
  }
  
  if (is.null(freeze) || !is.null(break.points)) {
    freeze <- Inf
  }
  
  if (is.null(exclude)) {
    exclude <- Inf
  }
  
  if (is.null(notes)) {
    notes <- ''
  } else {
    notes[is.na(notes)] <- ''
  }
  
  # Only connect data points and perform runs analysis if x is numeric
  dots.only <- is.factor(x) || mode(x) != 'numeric'
  
  # Convert dates and datetimes to POSIXct
  if (inherits(x, c('Date', 'POSIXt'))) {
    x <- as.POSIXct(x)
  }
  
  # Fix missing values and get aggregate function
  if (got.n) {
    cases     <- stats::complete.cases(y, n)
    y[!cases] <- NA
    n[!cases] <- NA
    agg.fun   <- 'sum'
  } else {
    agg.fun   <- match.arg(agg.fun)
  }
  
  # Get title
  if(is.null(main)) {
    main <- paste(toupper(match.arg(chart)), 'Chart')
  }
  
  # Prepare data frame
  d <- data.frame(x, y, n, notes, facets)
  
  # Aggregate data by subgroup
  d <- split(d, d[c('x', 'facet1', 'facet2')])
  d <- lapply(d,
              function(x) {
                data.frame(
                  x        = x$x[1],
                  facet1   = x$facet1[1],
                  facet2   = x$facet2[1],
                  y.sum    = sum(x$y, na.rm = TRUE),
                  y.length = sum(!is.na(x$y)),
                  y.mean   = mean(x$y, na.rm = TRUE),
                  y.sd     = stats::sd(x$y, na.rm = TRUE),
                  y        = ifelse(got.n,
                                    sum(x$y, na.rm = TRUE) /
                                      sum(x$n, na.rm = TRUE),
                                    do.call(agg.fun, list(x$y, na.rm = TRUE))),
                  n         = sum(x$n, na.rm = got.n),
                  notes     = paste(x$notes, collapse = '|')
                )
              })
  d <- do.call(rbind, d)
  
  # Add part and include variables
  d <- split(d, d[c('facet1', 'facet2')])
  d <- lapply(d,
              function(x) {
                break.points <- unique(c(0, break.points))
                break.points <- break.points[break.points >= 0 &
                                               break.points < nrow(x)]
                break.points <- break.points[order(break.points)]
                x$part       <- rep(c(seq_along(break.points)),
                                    diff(c(break.points, nrow(x))))
                x$xx         <- seq_along(x$part)
                x$include    <- !x$xx %in% exclude
                return(x)
              })
  d <- do.call(rbind, d)

  # Fix notes variables
  d$notes    <- gsub("\\|{2, }", "\\|", d$notes)
  d$notes    <- gsub("^\\||\\|$", "", d$notes)
  d$notes    <- gsub("\\|", " | ", d$notes)
  d$notes    <- gsub("^$", NA, d$notes)
  
  # Add baseline variable
  d$baseline <- d$xx <= freeze
  
  # Add centre and control lines
  if (!is.null(cl)) {
    d$cl <- cl
  }

  d <- split(d, d[c('facet1', 'facet2', 'part')])
  d <- lapply(d, chart.fun)
  d <- lapply(d, runs.analysis)
  d <- lapply(d,
              function(x) {
                x$y       <- x$y * multiply
                x$cl      <- x$cl * multiply
                x$lcl     <- x$lcl * multiply
                x$ucl     <- x$ucl * multiply
                x$cl.lab  <- ifelse(x$xx == max(x$xx), x$cl, NA)
                x$lcl.lab <- ifelse(x$xx == max(x$xx), x$lcl, NA)
                x$ucl.lab <- ifelse(x$xx == max(x$xx), x$ucl, NA)
                return(x)
              })
  d <- do.call(rbind, d)
  rownames(d) <- NULL
  
  # Remove control lines from missing subgroups
  d$ucl[is.na(d$y)] <- NA
  d$lcl[is.na(d$y)] <- NA
  
  # Add sigma signals
  d$sigma.signal                        <- d$y > d$ucl | d$y < d$lcl
  d$sigma.signal[is.na(d$sigma.signal)] <- FALSE
  
  # Add target line
  d$target <- target
  
  # Ignore runs analysis if subgroups are categorical
  if (dots.only) {
    d$runs.signal <- FALSE
  }
  
  # Prevent negative y axis if negy argument is FALSE
  if(!y.neg & min(d$y, na.rm = TRUE) >= 0)
    d$lcl[d$lcl < 0] <- 0
  
  # Return
  p <- plot.qic(d, main = main, xlab = xlab, ylab = ylab,
                subtitle = subtitle, caption = caption, part.labels,
                nrow = nrow, ncol = ncol, scales = scales,
                show.linelabels,
                show.grid,
                digits,
                flip,
                dots.only,
                x.format,
                x.angle,
                y.expand,
                y.percent,
                ...)
  class(p) <- c('qic', class(p))
  
  if (print.summary) {
    print(summary(p))
  }
  
  return(p)
}
