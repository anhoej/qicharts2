#' Prints summary of a qic object
#'
#' @param object A qic object.
#' @param ... For compatibility with generic summary function.
#'
#' @return A data frame of summary values of each facet and part of a qic plot.
#'   \itemize{
#'     \item facet1 Vertical facets.
#'     \item facet2 Horizontal facets
#'     \item part Number of chart part when argument break.points is given.
#'     \item aLCL Average of lower control limit.
#'     \item CL Centre line.
#'     \item aUCL Average of upper control limit.
#'     \item longest.run Length of the longest run of data points on the same side of the centre line.
#'     \item longest.run.max Upper limit of expected length of longest run.
#'     \item n.crossings Number of times the data line crosses the centre line.
#'     \item n.crossings.min Lower limit of expected number of crossings.
#'     \item runs.signal 1 if either longest run or number of crossings are outside expected limits.
#'     \item sigma.signal Number of data points outside control limits.
#'   }
#'
#' @examples
#' p <- qic(rnorm(24), chart = 'i')
#' p
#' summary(p)
#'
#' @export
#'
summary.qic <- function(object, ...) {
  x <- object$data
  x <- as.data.frame(x)
  x <- x[c('facet1', 'facet2', 'part', 'lcl', 'lcl.95', 'cl', 'ucl.95', 'ucl', 'n.obs', 'n.useful',
           'longest.run', 'longest.run.max', 'n.crossings', 'n.crossings.min',
           'runs.signal', 'sigma.signal')]

  x <- split(x, x[c('facet1', 'facet2', 'part')])
  x <- lapply(x, function(x) {
    data.frame(
      facet1 = x$facet1[1],
      facet2 = x$facet2[1],
      part = x$part[1],
      n.obs = x$n.obs[1],
      n.useful = x$n.useful[1],
      longest.run = x$longest.run[1],
      longest.run.max = x$longest.run.max[1],
      n.crossings = x$n.crossings[1],
      n.crossings.min = x$n.crossings.min[1],
      runs.signal = max(x$runs.signal),
      aLCL = mean(x$lcl, na.rm = TRUE),
      aLCL.95 = mean(x$lcl.95, na.rm = TRUE),
      CL = mean(x$cl, na.rm = TRUE),
      aUCL.95 = mean(x$ucl.95, na.rm = TRUE),
      aUCL = mean(x$ucl, na.rm = TRUE),
      sigma.signal = sum(x$sigma.signal, na.rm = TRUE)
    )
  })
  x <- do.call(rbind, x)
  rownames(x) <- NULL
  x <- x[order(x$facet1, x$facet2, x$part), ]

  return(x)
}
