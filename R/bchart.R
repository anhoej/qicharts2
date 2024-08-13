#' Bernoulli CUSUM chart for binary data (EXPERIMENTAL)
#'
#' The Bernoulli CUSUM chart is useful for monitoring rare events data, e.g.
#' surgical site infections and other types of complications. Based on Neuburger
#' et al. (2017) \doi{10.1136/bmjqs-2016-005526}.
#'
#' Note that the diagnostic properties of the Bernoulli CUSUM chart is highly
#' dependent on the choice of parameters, target, or and limit, and that these
#' parameters should be decided by people with a solid understanding of the
#' process at hand. The default parameters, or = 2 and limit = 3.5, should,
#' however, work for most processes where the baseline (target) level is about
#' 1%-10%, and one wants to detect changes in the order of a doubling or a
#' halving of the event rate relative to the target.
#'
#' @param x Logical, vector of successes and failures.
#' @param target Baseline risk (0-1) or number (>1) of last observation to end
#'   baseline period.
#' @param or Positive odds ratio of minimal detectable change relative to
#'   baseline risk.
#' @param limit Control limit.
#' @param title Chart title.
#' @param ylab Y axis label.
#' @param xlab X axis label.
#'
#' @return An object of class ggplot.
#'
#' @examples
#' # Generate 1000 random successes and failures with success rate = 0.02
#' set.seed(1)
#' y <- rbinom(1000, 1, 0.02)
#'
#' # Plot bchart assuming success rate = 0.01, OR = 2, control limits = +/- 3.5.
#' bchart(y, target = 0.01)
#'
#' # Plot bchart of CABG mortality using the first 200 surgeries to estimate target.
#' bchart(cabg$death, target = 200)
#'
#' # Plot bchart of CABG readmissions setting the control limits = +/-5.
#' bchart(cabg$readmission, target = 200, limit = 5)
#'
#' @import ggplot2
#' @export

bchart <- function(
    x,
    target,
    or    = 2,
    limit = 3.5,
    title = '',
    ylab  = 'CUSUM', 
    xlab  = 'Case #') {
  if(target > 1) {
    freeze <- target
    p0 <- mean(x[1:target])
  } else {
    freeze <- NULL
    p0 <- target
  }
  
  if (!is.null(title) && title == '') 
    title <- paste('CUSUM chart of', deparse(substitute(x)))
  
  p1      <- getp(p0, or)
  p2      <- getp(p0, 1 / or)
  s1      <- x * log(p1 / p0) + (1 - x) * log((1 - p1) / (1 - p0))
  s2      <- x * log(p2 / p0) + (1 - x) * log((1 - p2) / (1 - p0))
  limit2  <- -limit
  cusum2  <- cusum1  <- rep(0, length(x))
  signal2 <- signal1 <- rep(F, length(x))
  z2      <- z1      <- 0
  
  for (i in seq_along(x)) {
    z1i        <- z1 + s1[i]
    signal1[i] <- z1i >= limit
    cusum1[i]  <- z1i * (z1i > 0)
    cusum1[i]  <- cusum1[i] * (z1i <= limit)
    z1         <- cusum1[i]
    
    z2i        <- z2 + -s2[i]
    signal2[i] <- z2i <= limit2
    cusum2[i]  <- z2i * (z2i < 0)
    cusum2[i]  <- cusum2[i] * (z2i >= limit2)
    z2         <- cusum2[i]
  }
  
  signal1[signal1]  <- limit
  signal1[!signal1] <- NA
  signal2[signal2]  <- limit2
  signal2[!signal2] <- NA
  
  d <- data.frame(x = seq_along(x),
                  y = x, 
                  s1, 
                  cusum1, 
                  limit1 = limit, 
                  signal1, 
                  s2, 
                  cusum2, 
                  limit2, 
                  signal2,
                  p0,
                  p1,
                  p2,
                  or)
  class(d) <- c('bchart', class(d))
  p <- plot.bchart(d, freeze, title = title, ylab = ylab, xlab = xlab)
  return(p)
}

# Function to calculate probability from odds ratio ----
getp <- function(p0, or) {  
  odds0 <- p0 / (1 - p0)
  odds1 <- or * odds0
  p1    <- odds1 / (1 + odds1)
  return(p1)
}

# Function to plot bchart object ----
plot.bchart <- function(data, title, ylab, xlab, freeze) {
  target <- signif(data$p0[1], 2)
  or     <- signif(data$or[1], 2)
  limit  <- data$limit1[1]
  p0     <- signif(data$p0[1], 2)
  p1     <- signif(data$p1[1], 2)
  p2     <- signif(data$p2[1], 2)
  
  col1 <- '#8C8C8C'
  col2 <- '#5DA5DA'
  col3 <- '#F15854'
  
  p <- ggplot(data, aes_(~x, ~cusum1)) +
    geom_line(aes_(y = ~limit1), colour = col1) +
    geom_line(aes_(y = ~limit2), colour = col1) +
    geom_line(aes_(y = 0), colour = col1) +
    geom_point(aes_(y = ~signal1), na.rm = T, colour = col3) +
    geom_point(aes_(y = ~signal2),  na.rm = T, colour = col3, size = 2) +
    geom_line(colour = col2, size = 1.1) +
    geom_line(aes_(y = ~cusum2), colour = col2, size = 1.1) +
    geom_text(aes_(y = ~signal1, label = ~x, vjust = -0.7),
              na.rm = T, 
              colour = 'grey40',
              size = 3.1) +
    geom_text(aes_(y = ~signal2, label = ~x, vjust = 1.7),
              na.rm = T, 
              colour = 'grey40',
              size = 3.1) +
    annotate('text', max(data$x), limit, label = limit,
             check_overlap = T,
             size = 3.1,
             colour = 'grey40',
             hjust = -0.2) +
    annotate('text', max(data$x), -limit, label = -limit,
             check_overlap = T,
             size = 3.1,
             colour = 'grey40',
             hjust = -0.2) +
    # geom_text(aes_(~max(x), limit, label = limit),
    #           check_overlap = T,
    #           size = 3.1,
    #           colour = 'grey40',
    #           hjust = -0.2) +
    # geom_text(aes_(~max(x), -limit, label = -limit),
    #           check_overlap = T,
    #           size = 3.1,
    #           colour = 'grey40',
    #           hjust = -0.2) +
    scale_y_continuous(expand = expansion(0.1)) +
    theme_bw() +
    theme(panel.border     = element_rect(colour = 'grey93'),
          strip.background = element_rect(colour = 'grey93', fill = 'grey93'),
          axis.ticks       = element_line(colour = 'grey80'),
          panel.grid       = element_blank(),
          legend.position  = 'none') +
    labs(title = title,
         y = ylab,
         x = xlab,
         caption = paste0('Target = ', target,
                          ', OR+ = ', or,
                          ', P+ = ', p1,
                          ', OR- = ', signif(1 / or, 2),
                          ', P- = ', p2),
         ', Limit = ', limit)
  
  if(!is.null(freeze))
    p <- p + geom_vline(aes_(xintercept = ~freeze), linetype = 3, colour = col1)
  
  return(p)
}
