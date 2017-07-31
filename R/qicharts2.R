#' \code{qicharts2} package
#'
#' Statistical Process Control chart for R
#' 
#' #' See the README on
#' \url{https://github.com/anhoej/qicharts2/}
#' 
#' @docType package
#' @name qicharts2
NULL

## Prevent R CMD check notes about missing bindings for global variables
if(getRversion() >= "2.15.1")  
  utils::globalVariables(c(
    # '.',
    # 'baseline',
    # 'dotcol',
    # 'linecol',
    # 'facet1',
    # 'facet2',
    # 'include',
    # 'runs.signal',
    # 'sigma.signal',
    # 'lcl',
    # 'ucl',
    # 'notes',
    'xx',
    'y.sum')
  )

