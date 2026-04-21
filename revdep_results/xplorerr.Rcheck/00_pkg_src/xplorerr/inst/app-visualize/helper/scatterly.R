library(plotly)

scatterly <- function(data, x, y, name = NA, text = NA,
                      color = '#1F77B4', opacity = 1, 
                      auto_size = TRUE, 
                      plot_width = NULL, plot_height = NULL,
                      axis_range = FALSE, x_min, x_max, y_min, y_max,
                      symbol = 'circle', size = 5, 
                      title = NA, show_legend = TRUE,
                      x_title = NA, y_title = NA,
                      x_showgrid = TRUE, y_showgrid = TRUE,
                      fit_line = FALSE, line_col = 'red', 
                      line_type = 'dashed', line_width = 1,
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
                      leg_x = 100, leg_y = 0.5, leg_orientation = 'v', 
                      leg_font_family = 'sans-serif',
                      leg_font_size = 12, leg_font_color = '#000', 
                      leg_bg_color = '#E2E2E2',
                      leg_border_col = "#FFFFFF", leg_border_width = 2,
                      add_annotate = FALSE,
                      x_annotate, y_annotate, text_annotate,
                      annotate_xanchor = 'auto', show_arrow, arrow_head = 1,
                      ax_anntate = 20, ay_annotate = -40, 
                      annotate_family = 'sans-serif',
                      annotate_size = 14, annotate_col = 'red') {
  
  x <- data %>% select_(x) %>% unlist() 
  y <- data %>% select_(y) %>% unlist()
  
  # style axes title and tickfont
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
  
  # legend
  l <- list(
    x = leg_x,
    y = leg_y,
    orientation = leg_orientation,
    font = list(
      family = leg_font_family,
      size = leg_font_size,
      color = leg_font_color),
    bgcolor = leg_bg_color,
    bordercolor = leg_border_col,
    borderwidth = leg_border_width)
  
  # annotations
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
  
  p <- plot_ly(data = data,
               type = "scatter", 
               mode = "markers",
               x = x,
               y = y, 
               name = name,
               text = text,
               marker = list(
                 color = color,
                 opacity = opacity,
                 symbol = symbol,
                 size = size
               ),
               width = plot_width,
               height = plot_height) %>%
    
    layout(
      title = title,
      xaxis = xaxis,
      yaxis = yaxis,
      autosize = auto_size,
      margin = m,
      legend = l,
      showlegend = show_legend
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
  
  if(fit_line) {
    
    p <- p %>%
      add_trace(x = x,
                y = ~fitted(lm(y ~ x)),
                mode = 'markers+lines',
                line = list(
                  color = line_col,
                  dash = line_type,
                  width = line_width
                )
      )
    
    p  
  }
  
  p 

}

