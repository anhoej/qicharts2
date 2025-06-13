#' \code{qicharts2} package
#'
#' Statistical Process Control chart for R
#' 
#' #' See the README on
#' \url{https://github.com/anhoej/qicharts2/}
#' 
#' @name qicharts2
#' @aliases qicharts2-package
NULL

## Prevent R CMD check notes about missing bindings for global variables
if(getRversion() >= "2.15.1")  
  utils::globalVariables(c(
    'xx',
    'y.sum',
    'cl',
    'cl.lab',
    'dotcol',
    'lcl',
    'lcl.95',
    'lcl.lab',
    'linecol',
    'notes',
    'part',
    'runs.signal',
    'target',
    'target.lab',
    'ucl',
    'ucl.95',
    'ucl.lab',
    'y',
    'z',
    'cusum1',
    'cusum2',
    'limit1',
    'limit2',
    'p.cum',
    'signal1',
    'signal2',
    'x')
  )

