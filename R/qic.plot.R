#' @import ggplot2
plot.qic <- function(x, title, ylab, xlab, subtitle, caption, part.labels,
                     nrow, ncol, scales, show.labels, show.grid, 
                     # show.sigma.lines, 
                     decimals, flip, dots.only, x.format,
                     x.angle, x.pad, y.expand, y.percent, strip.horizontal,
                     ...) {
  # Set colours
  col1      <- '#8C8C8C' # rgb(140, 140, 140, maxColorValue = 255) # grey
  col2      <- '#5DA5DA' # rgb(093, 165, 218, maxColorValue = 255) # blue
  col3      <- '#F15854' # rgb(241, 088, 084, maxColorValue = 255) # red
  col4      <- '#059748' # rgb(005, 151, 072, maxColorValue = 255) # green
  col5      <- '#C8C8C8' # rgb(200, 200, 200, maxColorValue = 255) # light grey
  cols      <- c('col1' = col1,
                 'col2' = col2,
                 'col3' = col3,
                 'col4' = col4,
                 'col5' = col5)
  x$dotcol  <- ifelse(x$sigma.signal, 'col3', 'col2')
  x$dotcol  <- ifelse(x$include, x$dotcol, 'col5')
  x$dotcol  <- ifelse(x$y == x$cl, 'col5', x$dotcol)
  x$linecol <- ifelse(x$runs.signal, 'col3', 'col1')

  # Set label parameters
  lab.size <- 3
  lab.just <- ifelse(flip, 'center', -0.2)
  
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
          axis.ticks       = element_line(colour = 'grey80'),
          panel.grid       = element_blank(),
          legend.position  = 'none')
  
  # Add control limits and centre and target lines
  p <- p +
    geom_ribbon(aes_(ymin = ~ lcl, ymax = ~ ucl),
                fill = 'grey80',
                alpha = 0.4)
  
  # if(show.sigma.lines) {
  #   p <- p +
  #   geom_line(aes_(y = ~ cl + (ucl - cl) / 3), colour = 'white') +
  #   geom_line(aes_(y = ~ cl + (ucl - cl) / 3 * 2), colour = 'white') +
  #   geom_line(aes_(y = ~ cl - (ucl - cl) / 3), colour = 'white') +
  #   geom_line(aes_(y = ~ cl - (ucl - cl) / 3 * 2), colour = 'white') +
  #   geom_line(aes_(y = ~ target),
  #             colour = col4,
  #             linetype = 5,
  #             size = 0.5)
  # }
  
  p <- p +
    geom_line(aes_(y = ~ cl, linetype = ~ runs.signal, colour = ~ linecol),
              na.rm = TRUE) +
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
  
  # Add line labels
  if (show.labels) {
    p <- p +
      geom_text(aes_(y = ~ target.lab,
                      label = ~ lab.format(target.lab, decimals, y.percent)),
                 na.rm = TRUE,
                 size = lab.size,
                 hjust = lab.just) +
      geom_text(aes_(y = ~ lcl.lab,
                      label = ~ lab.format(lcl.lab, decimals, y.percent)),
                 na.rm = TRUE,
                 size = lab.size,
                 hjust = lab.just) +
      geom_text(aes_(y = ~ ucl.lab,
                      label = ~ lab.format(ucl.lab, decimals, y.percent)),
                 na.rm = TRUE,
                 size = lab.size,
                 hjust = lab.just) +
      geom_text(aes_(y = ~ cl.lab,
                      label = ~ lab.format(cl.lab, decimals, y.percent)),
                 na.rm = TRUE,
                 size = lab.size,
                 hjust = lab.just)
  }
  
  # Add freeze line and part labels
  if (is.finite(freeze)) {
    p <- p + 
      geom_vline(aes_(xintercept = freeze), colour = col1, linetype = 5)
  }
  
  if (!is.null(part.labels)) {
    if (is.finite(freeze)) {
      plabs <- split(x, x['baseline'])
      part.labels <- rev(part.labels)
    } else {
      plabs <- split(x, x['part'])
    }
    
    plabs <- lapply(plabs, function(x) {median(as.numeric(x$x))})
    plabs <- as.data.frame(do.call(rbind, plabs))
    colnames(plabs) <- 'x'
    
    if (length(part.labels) == nrow(plabs)) {
      plabs$z <- part.labels
      plabs$y <- Inf
      
      if (inherits(x$x, 'POSIXct')) {
        plabs$x <- as.POSIXct(plabs$x, origin = '1970-01-01')
      }

      p <- p +
        geom_label(aes_(y = ~ y, label = ~ z, group = 1), 
                   data = plabs,
                   na.rm = T,
                   label.size = 0.1,
                   size = lab.size,
                   alpha = 0.9,
                   vjust = ifelse(flip, 'center', 'inward'),
                   hjust = ifelse(flip, 'inward', 'center'))
    } else {
      warning('Length of part.labels argument must match the number of parts to label.')
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
    labs(title = title,
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

  # Add space for line labels
  subgroups <- unique(x$x)
  
  if (is.factor(subgroups))
    subgroups <- as.numeric(subgroups)

  p <- p +
    expand_limits(x = max((subgroups)) +
                    diff(range((subgroups))) / 30 * x.pad * show.labels)
  
  # Show grid
  if (show.grid) {
    p <- p + theme(panel.grid = element_line())
  }
  
  # Flip chart
  if (flip) {
    p <- p + coord_flip()
  }
  
  # Horizontal y strip
  if (strip.horizontal)
    p <- p + theme(strip.text.y = element_text(angle = 0,
                                               hjust = 0),
                   strip.background = element_blank())

  return(p)
}
