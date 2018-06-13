#' Statistical process control charts.
#' 
#' The \code{qic()} function creates run charts and Shewhart control charts for 
#' process control and improvement. Included control charts are: I, MR, Xbar, S,
#' T, C, U, U', P, P', and G charts.
#' 
#' Non-random variation in the form of minor to moderate persistens shifts in 
#' data over time is identified by the Anhoej rules for unusually long runs and 
#' unusually few crossing. Special cause variation in the form of larger, 
#' possibly transient, shifts in data is identified by Shewhart's 3-sigma rule.
#' 
#' @param x Vector of subgroup values to plot along the x axis.
#' @param y Vector of measures or counts to plot on the y axis (numerator).
#' @param n Vector of subgroup sizes (denominator).
#' @param data Data frame containing variables used in the plot.
#' @param notes Character vector of notes to be added to individual data points.
#' @param facets One or two sided formula with factors used for facetting plots.
#' @param chart Character value indicating the chart type. Possible values are: 
#'   'run' (default), 'i', 'mr', 'xbar', 't', 's', 'c', 'u', 'up', 'p', 'pp', 
#'   and 'g'.
#' @param agg.fun Aggregate function for summarising the y variable if there are
#'   more than one observation per subgroup. Only relevant for run charts and I 
#'   charts. Possible values are: 'mean' (default), 'median', 'sum', and 'sd'.
#' @param multiply Number indicating a number to multiply y axis by, e.g. 100 
#'   for percents rather than proportions. See also \code{y.percent} argument.
#' @param freeze Integer indicating the last data point to include in 
#'   calculation of baseline paramenters for centre and control lines. Ignored 
#'   if part argument is given.
#' @param part Integer vector indicating data points before 
#'   recalculation of centre and control lines.
#' @param exclude Integer vector indicating data points to exclude from 
#'   calculations of centre and control lines.
#' @param target Numeric, either a single value indicating a target value to be plotted as a 
#'   horizontal line or a vector for variable target line.
#' @param cl Numeric, either a single value indicating the centre line if known in 
#'   advance or a vector for variable centre line.
#' @param nrow,ncol Number indicating the preferred number of rows and columns 
#'   in facets.
#' @param scales Character string, one of 'fixed' (default), 'free_y', 'free_x',
#'   or 'free' indicating whether y and x axis scales should be the same for all
#'   panels or free.
#' @param title Character string specifying the title of the plot.
#' @param xlab Character string specifying the x axis label.
#' @param ylab Character string specifying the y axis label.
#' @param subtitle Character string specifying the subtitle.
#' @param caption Character string specifying the caption.
#' @param part.labels Character vector specifying  labels for 
#'   chart parts created with the freeze or part argument.
#' @param show.labels Logical indicating whether to show labels for centre and
#'   control lines on chart. Defaults to TRUE when facets argument is NULL.
#' @param decimals Integer indicating the preferred number of decimals in centre
#'   and control line labels.
#' @param point.size Number specifying the size of data points.
#' @param x.period Character string specifying the interval cut points of 
#'   datetime x values used for aggregating y values by week, month, etc.
#'   See the breaks argument of \code{?cut.POSIXt()} for possible values.
#' @param x.format Date format of x axis labels. See \code{?strftime()} for 
#'   possible date formats.
#' @param x.angle Number indicating the angle of x axis labels.
#' @param x.pad Number indicating expansion of x axis to make room for axis labels.
#' @param y.expand Numeric value to include in y axis. Useful e.g. for starting 
#'   the y axis at zero.
#' @param y.neg If TRUE (default), the y axis is allowed to be negative (only 
#'   relevant for I and Xbar charts).
#' @param y.percent If TRUE, formats y axis labels as percentages.
#' @param show.grid If TRUE, shows grid.
#' @param flip If TRUE, rotates the plot 90 degrees.
#' @param strip.horizontal If TRUE, makes y strip horizontal.
#' @param print.summary If TRUE, prints summary.
# @param ... Additional arguments to plot function.
#'   
#' @return A \code{qic} object. Inherits from 'ggplot'.
#'   
#' @seealso \code{vignette('qic')}
#'   
#' @examples
#' # Lock random number generator to make reproducible results.
#' set.seed(2)
#' 
#' # Generate vector of 24 random normal numbers
#' y <- rnorm(24)
#' 
#' # Run chart
#' qic(y)
#' 
#' # I control chart
#' qic(y, chart = 'i')
#' 
#' # U control chart from build-in data set of hospital infection rates faceted
#' #   by hospital and type of infection.
#' qic(month, n, 
#'     n        = days,
#'     data     = hospital_infections,
#'     facets   = infection ~ hospital,
#'     chart    = 'u',
#'     multiply = 10000,
#'     title     = 'Hospital infection rates',
#'     ylab     = 'Number of infections per 10.000 risk days',
#'     xlab     = 'Month')
#' 
#' @importFrom stats median
#' @export

qic <- function(x,
                y                = NULL,
                n                = NULL,
                data             = NULL,
                facets           = NULL,
                notes            = NULL,
                chart            = c('run', 'i', 'mr', 'xbar', 's', 't',
                                     'p', 'pp', 'c', 'u', 'up', 'g'),
                agg.fun          = c('mean', 'median', 'sum', 'sd'),
                multiply         = 1,
                freeze           = NULL,
                part             = NULL,
                exclude          = NULL,
                target           = NA * 1,
                cl               = NA * 1, #NULL,
                nrow             = NULL,
                ncol             = NULL,
                scales           = 'fixed',
                title            = '',
                ylab             = 'Value',
                xlab             = 'Subgroup',
                subtitle         = NULL,
                caption          = NULL,
                part.labels      = NULL,
                show.labels      = is.null(facets),
                decimals         = 1,
                point.size       = 1,
                x.period         = NULL,
                x.format         = NULL,
                x.angle          = NULL,
                x.pad            = 1,
                y.expand         = NULL,
                y.neg            = TRUE,
                y.percent        = FALSE,
                show.grid        = FALSE,
                flip             = FALSE,
                strip.horizontal = FALSE,
                print.summary    = FALSE) {
  
  # Check data
  if (missing(x))
    stop('Missing mandatory argument \"x\"')
  
  # Preserve show.labels value
  show.labels <- show.labels
  
  # Build title
  y.name <- deparse(substitute(y))
  n.name <- deparse(substitute(n))
  
  if (y.name == 'NULL') 
    y.name <- deparse(substitute(x))
  
  if (n.name != 'NULL')  
    y.name <- paste(y.name, '/', n.name)
  
  if (multiply != 1)  
    y.name <- paste(y.name, 'x', multiply)
  
  if (!is.null(title) && title == '') 
    title <- paste(toupper(match.arg(chart)), 'Chart', 'of', y.name)
 
  # Get chart type
  chart     <- match.arg(chart)
  chart.fun <- get(paste0('qic.', chart))
  
  # Get aggregate function
  agg.fun <- match.arg(agg.fun)
  
  # Get variables
  x      <- eval(substitute(x), data, parent.frame())
  y      <- eval(substitute(y), data, parent.frame())
  n      <- eval(substitute(n), data, parent.frame())
  notes  <- eval(substitute(notes), data, parent.frame())
  cl     <- eval(substitute(cl), data, parent.frame())
  target <- eval(substitute(target), data, parent.frame())
  facets <- all.vars(facets)
  
  if (is.null(data)) {
    facets <- mget(facets, parent.frame())
  } else {
    facets <- mget(facets, as.environment(data))
  }
  
  facets <- as.data.frame(facets)
  
  if (ncol(facets) %in% 1:2) {
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
  
  if (is.null(freeze) || !is.null(part)) 
    freeze <- Inf
  
  if (is.null(exclude)) 
    exclude <- Inf
  
  if (is.null(notes)) {
    notes <- ''
  } else {
    notes[is.na(notes)] <- ''
  }
  
  # Only connect data points and perform runs analysis if x is numeric
  dots.only <- is.factor(x) || mode(x) != 'numeric'
  
  # Convert dates and datetimes to POSIXct
  if (inherits(x, c('Date', 'POSIXt'))) {
    x <- as.POSIXct(as.character(x), tz = 'UTC')
    if (!missing(x.period)) {
      x <- as.POSIXct(cut(x, breaks = x.period))
    }
  }
  
  # Fix missing values
  if (got.n) {
    cases     <- stats::complete.cases(y, n)
    y[!cases] <- NA
    n[!cases] <- NA
  }
  
  # Prepare data frame
  d <- data.frame(x, y, n, notes, facets, cl, target)
  d <- droplevels(d)
  
  # Aggregate data and perform analyses
  d <- qic.agg(d, got.n, part, agg.fun, freeze, exclude, 
               chart.fun, multiply, dots.only, chart, y.neg)
  
  # Format y for p charts
  if (missing(y.percent) & chart %in% c('p', 'pp')) {
    y.percent <- TRUE
  }
  
  if (y.percent & missing(ylab)) {
    ylab <- NULL
  }

  # Build plot
  p <- plot.qic(d, 
                title            = title, 
                xlab             = xlab, 
                ylab             = ylab,
                subtitle         = subtitle, 
                caption          = caption, 
                part.labels      = part.labels, 
                nrow             = nrow, 
                ncol             = ncol, 
                scales           = scales,
                show.labels      = show.labels,
                show.grid        = show.grid,
                # show.sigma.lines = show.sigma.lines,
                decimals         = decimals,
                flip             = flip,
                dots.only        = dots.only,
                point.size       = point.size,
                x.format         = x.format,
                x.angle          = x.angle,
                x.pad            = x.pad,
                y.expand         = y.expand,
                y.percent        = y.percent,
                strip.horizontal = strip.horizontal)
  
  class(p) <- c('qic', class(p))
  
  # Tell how data was aggregated  
  if(!got.n & match.arg(chart) %in% c('run', 'i') & max(d$y.length > 1))
    message(paste0('Subgroup size > 1. Data have been aggregated using ', 
                  agg.fun, '().'))
  
  # Print summary
  if (print.summary)
    print(summary(p))
  
  # Return plot object
  return(p)
}
