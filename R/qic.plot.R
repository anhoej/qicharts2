#' @import ggplot2
plot.qic <- function(x, main, ylab, xlab, subtitle, caption, part.labels,
                     nrow, ncol, scales, show.linelabels, show.grid, digits,
                     flip, dots.only, x.format, x.angle, y.expand, y.percent,
                     ...) {
  # Set colours
  col1      <- '#8C8C8C' # rgb(140, 140, 140, maxColorValue = 255) # grey
  col2      <- '#5DA5DA' # rgb(093, 165, 218, maxColorValue = 255) # blue
  col3      <- '#F15854' # rgb(241, 088, 084, maxColorValue = 255) # red
  col4      <- '#059748' # rgb(005, 151, 072, maxColorValue = 255) # green
  col5      <- '#C8C8C8' # rgb(200, 200, 200, maxColorValue = 255) # grey
  cols      <- c('col1' = col1,
                 'col2' = col2,
                 'col3' = col3,
                 'col4' = col4,
                 'col5' = col5)
  
  x$dotcol  <- ifelse(x$sigma.signal, 'col3', 'col2')
  x$dotcol  <- ifelse(x$include, x$dotcol, 'col5')
  x$linecol <- ifelse(x$runs.signal, 'col3', 'col1')
  
  # Set label parameters
  lab.size <- 3.5
  lab.pad  <- unit(0.05, 'lines')
  lab.just <- ifelse(flip, 'center', 1)
  
  # Get number of facet dimensions
  n.facets <- sum((length(unique(x$facet1))) > 1,
                  (length(unique(x$facet2)) > 1))
  
  # Get freeze point
  freeze <- max(x$xx[x$baseline])
  if (freeze < (max(x$xx) - 1)) {
    freeze <- (as.numeric(x$x[freeze]) + as.numeric(x$x[freeze + 1])) / 2
  } else {
    freeze <- Inf
  }
  
  # Prepare plot
  p <- ggplot(x, aes_( ~ x, ~ y, group = ~ part)) +
    theme_bw() +
    theme(panel.border     = element_rect(colour = 'grey93'),
          strip.background = element_rect(colour = 'grey93', fill = 'grey93'),
          axis.ticks       = element_line(colour = 'grey85'),
          panel.grid       = element_blank(),
          legend.position  = 'none')
  
  # Add control limits and centre and target lines
  p <- p +
    geom_ribbon(aes_(ymin = ~ lcl, ymax = ~ ucl),
                fill = 'grey85',
                alpha = 0.25) +
    geom_line(aes_(y = ~ target),
              colour = col4,
              linetype = 5,
              size = 0.5) +
    geom_line(aes_(y = ~ cl, linetype = ~ runs.signal, colour = ~ linecol)) +
    scale_linetype_manual(values = c('FALSE' = 'solid', 'TRUE' = 'dashed'))
  
  # Add notes
  x.notes <- x[!is.na(x$notes), ]
  if (nrow(x.notes)) {
    p <- p +
      geom_segment(aes_(xend = ~ x, yend = Inf),
                   data = x.notes,
                   linetype = 3,
                   colour = col1) +
      geom_label(aes_(y = Inf, label = ~ notes),
                 data = x.notes,
                 # na.rm = TRUE,
                 label.size = 0.1,
                 size = lab.size,
                 alpha = 0.9,
                 vjust = ifelse(flip, 'center', 'inward'),
                 hjust = ifelse(flip, 'inward', 'center'))
  }
  
  # Add data points and line
  if (dots.only) {
    p <- p + geom_point(aes_(colour = ~ dotcol), size = 3, na.rm = TRUE)
  } else {
    p <- p +
      geom_line(colour = col2, size = 1.1, na.rm = TRUE) +
      geom_point(aes_(colour = ~ dotcol), na.rm = TRUE)
  }
  p <- p + scale_colour_manual(values = cols)
  
  # Add labels for centre and control lines
  if (show.linelabels) {
    p <- p +
      geom_label(aes_(x = ~ x[length(x)], y = ~ target[length(x)],
                      label = ~ prettyNum(target, digits = digits)),
                 na.rm = TRUE,
                 label.size = 0,
                 label.padding = lab.pad,
                 size = lab.size,
                 hjust = lab.just) +
      geom_label(aes_(y = ~ cl.lab,
                      # label = ~ prettyNum(cl.lab, digits = digits)),
                      label = if (y.percent) {
                        ~ scales::percent(round(cl.lab, digits = digits)) 
                      } else {
                        ~ prettyNum(cl.lab, digits = digits)
                      }),
                 label.size = 0,
                 na.rm = TRUE,
                 label.padding = lab.pad,
                 size = lab.size,
                 hjust = lab.just) +
      geom_label(aes_(y = ~ lcl.lab,
                      # label = ~ prettyNum(lcl.lab, digits = digits)),
                      label = if (y.percent) {
                        ~ scales::percent(round(lcl.lab, digits = digits)) 
                      } else {
                        ~ prettyNum(lcl.lab, digits = digits)
                      }),
                 na.rm = TRUE,
                 label.size = 0,
                 label.padding = lab.pad,
                 size = lab.size,
                 hjust = lab.just) +
      geom_label(aes_(y = ~ ucl.lab,
                      # label = ~ prettyNum(ucl.lab, digits = digits)),
                      label = if (y.percent) {
                        ~ scales::percent(round(ucl.lab, digits = digits)) 
                      } else {
                        ~ prettyNum(ucl.lab, digits = digits)
                      }),
                 na.rm = TRUE,
                 label.size = 0,
                 label.padding = lab.pad,
                 size = lab.size,
                 hjust = lab.just)
  }
  
  # Add freeze line and part labels
  if (is.finite(freeze)) {
    p <- p + geom_vline(aes_(xintercept = freeze), colour = col1, linetype = 5)
    
    if (!is.null(part.labels)) {
      plabs <- split(x, x['baseline'])
      plabs <- lapply(plabs, function(x) {median(x$x)})
      plabs <- as.data.frame(do.call(rbind, plabs))
      colnames(plabs) <- 'x'
      plabs$z <- rev(part.labels)
      plabs$y <- Inf
      
      if (inherits(x$x, 'POSIXct')) {
        plabs$x <- as.POSIXct(plabs$x, origin = '1970-01-01')
      }
      
      p <- p +
        geom_label(aes_(y = ~ y, label = ~ z, group = 1),
                   data = plabs,
                   label.size = 0.1,
                   size = lab.size,
                   alpha = 0.9,
                   vjust = ifelse(flip, 'center', 'inward'),
                   hjust = ifelse(flip, 'inward', 'center'))
    }
  }
  
  # Facets
  if (n.facets == 1) {
    p <- p + facet_wrap( ~ facet1,
                         nrow = nrow,
                         ncol = ncol,
                         scales = scales)
  } else if (n.facets == 2) {
    p <- p + facet_grid(facet1 ~ facet2,
                        scales = scales)
  } else {
    p <- p + facet_null()
  }
  
  # Add title and axis labels
  p <- p +
    labs(title = main,
         x = xlab,
         y = ylab,
         caption = caption,
         subtitle = subtitle)
  
  # Format x axis labels
  if (!is.null(x.format) && inherits(x$x, 'POSIXct')) {
    p <- p + scale_x_datetime(date_labels = x.format)
  }
  
  # Rotate x axis labels
  if (!is.null(x.angle)) {
    p <- p +
      theme(axis.text.x = element_text(angle = x.angle, vjust = 1, hjust = 1))
  }
  
  # Expand y limits
  p <- p + expand_limits(y = y.expand)
  
  # Format percent axis
  if (y.percent) {
    p <- p + scale_y_continuous(labels = scales::percent)
  }
  
  # Show grid
  if (show.grid) {
    p <- p + theme(panel.grid = element_line())
  }
  
  # Flip
  if (flip) {
    p <- p + coord_flip()
  }
  
  return(p)
}
