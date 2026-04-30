library(plotly)

histly <- function(data = NULL, y = NULL, hist_orient = 'v', 
                   hist_opacity = 1, hist_type = 'count', auto_binx = TRUE,
                   xbins_start = NULL, xbins_end = NULL, xbins_size = NULL,
                   hist_col = 'blue', hist_l_col = 'black', hist_l_w = 1,
                   hist_bargap = 0, auto_size = TRUE, plot_width = NULL, 
                   plot_height = NULL, axis_range = FALSE, x_min, x_max, 
                   y_min, y_max, title = NA, x_title = NA, 
                   y_title = NA, x_showgrid = TRUE, y_showgrid = TRUE,
                   ax_title_font_family = 'Arial, sans-serif',
                      ax_title_font_size = 18, ax_title_font_color = 'black',
                      ax_tick_font_family = 'Arial, sans-serif',
                      ax_tick_font_size = 18, ax_tick_font_color = 'black',
                      x_autotick = TRUE, x_ticks = 'outside', x_tick0 = NULL,
                      x_dtick = NULL, x_ticklen = 5, x_tickwidth = 1,
                      x_tickcolor = '#444', x_showticklab = TRUE, 
                      x_tickangle = 'auto', x_zeroline = FALSE, 
                      x_showline = TRUE, x_gridcolor = "rgb(204, 204, 204)",
                      x_gridwidth = 1, x_zerolinecol = "#444", 
                      x_zerolinewidth = 1, x_linecol = '#444',
                      x_linewidth = 1, 
                      y_autotick = TRUE, y_ticks = 'outside', 
                      y_tick0 = NULL, y_dtick = NULL, y_ticklen = 5, 
                      y_tickwidth = 1, y_tickcolor = '#444', 
                      y_showticklab = TRUE, y_tickangle = 'auto', 
                      y_zeroline = FALSE, y_showline = TRUE, 
                      y_gridcolor = "rgb(204, 204, 204)",
                      y_gridwidth = 1, y_zerolinecol = "#444", 
                      y_zerolinewidth = 1, y_linecol = '#444',
                      y_linewidth = 1, left_margin = 80, right_margin = 80,
                      top_margin = 100, bottom_margin = 80, padding = 0,
                      add_annotate = FALSE,
                      x_annotate, y_annotate, text_annotate,
                      annotate_xanchor = 'auto', show_arrow, arrow_head = 1,
                      ax_anntate = 20, ay_annotate = -40, 
                      annotate_family = 'sans-serif',
                      annotate_size = 14, annotate_col = 'red') {
  
  y1 <- data %>%
    select_(y) %>%
    unlist()
  

  f1 <- list(
    family = ax_title_font_family,
    size = ax_title_font_size,
    color = ax_title_font_color
  )
  
  f2 <- list(
    family = ax_tick_font_family,
    size = ax_tick_font_size,
    color = ax_tick_font_color
  )
  
  xaxis <- list(
    title = x_title,
    showgrid = x_showgrid,
    autotick = x_autotick,
    ticks = x_ticks,
    tick0 = x_tick0,
    dtick = x_dtick,
    ticklen = x_ticklen, 
    tickwidth = x_tickwidth,
    tickcolor = x_tickcolor,
    titlefont = f1,
    showticklabels = x_showticklab,
    tickangle = x_tickangle, 
    tickfont = f2,
    zeroline = x_zeroline,
    showline = x_showline,
    gridcolor = x_gridcolor,
    gridwidth = x_gridwidth,
    zerolinecolor = x_zerolinecol,
    zerolinewidth = x_zerolinewidth,
    linecolor = x_linecol,
    linewidth = x_linewidth
  )
  
  yaxis <- list(
    title = y_title,
    showgrid = y_showgrid,
    autotick = y_autotick,
    ticks = y_ticks,
    tick0 = y_tick0,
    dtick = y_dtick,
    ticklen = y_ticklen, 
    tickwidth = y_tickwidth,
    tickcolor = y_tickcolor,
    titlefont = f1,
    showticklabels = y_showticklab,
    tickangle = y_tickangle, 
    tickfont = f2,
    zeroline = y_zeroline,
    showline = y_showline,
    mirror = 'ticks',
    gridcolor = y_gridcolor,
    gridwidth = y_gridwidth,
    zerolinecolor = y_zerolinecol,
    zerolinewidth = y_zerolinewidth,
    linecolor = y_linecol,
    linewidth = y_linewidth
  )
  
  # margins
  m <- list(
    l = left_margin,
    r = right_margin, 
    t = top_margin,
    b = bottom_margin, 
    pad = padding
  )  

  if(add_annotate) {
    a <- list(
      x = x_annotate,
      y = y_annotate,
      text = text_annotate,
      xref = 'x',
      yref = 'y',
      xanchor = annotate_xanchor,
      showarrow = show_arrow,
      arrowhead = arrow_head,
      ax = ax_annotate,
      ay = ay_annotate,
      font = list(
        family = annotate_family,
        size = annotate_size,
        color = annotate_col
      )
    )
  }


  if(hist_orient == 'v') {
  
    p <- plot_ly(data, x = y1, 
                 type = "histogram", 
                 opacity = hist_opacity,
                 histnorm = hist_type,
                 autobinx = auto_binx,
                 xbins = list(
                   start = xbins_start,
                   end = xbins_end,
                   size = xbins_size
                 ),
                 marker = list(
                   color = hist_col,
                   line = list(
                     color = hist_l_col,
                     width = hist_l_w
                   )
                 )
    )
  
  } else {
    
    p <- plot_ly(data, y = y1, 
                 type = "histogram", 
                 opacity = hist_opacity,
                 histnorm = hist_type,
                 autobinx = auto_binx,
                 xbins = list(
                   start = xbins_start,
                   end = xbins_end,
                   size = xbins_size
                 ),
                 marker = list(
                   color = hist_col,
                   line = list(
                     color = hist_l_col,
                     width = hist_l_w
                   )
                 )
    )
    
  }
  
  p <- p %>%
    layout(
      title = title,
      xaxis = xaxis,
      yaxis = yaxis,
      autosize = auto_size,
      margin = m,
      bargap = hist_bargap
    )

  if(add_annotate) {
    p <- p %>%
      layout(annotations = a)
  }
  
  if(axis_range) {
    p <- p %>%
      layout(
        xaxis = list(
          range = list(x_min, x_max)
        ),
        yaxis = list(
          range = list(y_min, y_max)
        )
      )
  }
  
  p
  
}

