library(dplyr)
library(plotly)

linely <- function(data, x, y, mode = 'lines', lcol = 'blue', lwidth = 1, ltype = 'plain',
                   title = NULL, p_bgcol = NULL, plot_bgcol = NULL,
                   title_family = 'Arial', title_size = 12, title_color = 'black',
                   axis_modify = FALSE, x_min, x_max, y_min, y_max,
                   x_title = NULL, x_showline = FALSE, x_showgrid = TRUE,
                   x_gridcol = NULL, x_showticklabels = TRUE,
                   x_lcol = NULL, x_lwidth = NULL, x_zline = FALSE, 
                   x_autotick = TRUE, x_ticks = TRUE, x_tickcol = 'black',
                   x_ticklen = NULL, x_tickw = NULL, x_ticfont = 'Arial',
                   x_tickfsize = 10, x_tickfcol = 'black', y_title = NULL, 
                   y_showline = FALSE, y_showgrid = TRUE,
                   y_gridcol = NULL, y_showticklabels = TRUE,
                   y_lcol = NULL, y_lwidth = NULL, y_zline = FALSE, 
                   y_autotick = TRUE, y_ticks = TRUE, y_tickcol = 'black',
                   y_ticklen = NULL, y_tickw = NULL, y_ticfont = 'Arial',
                   y_tickfsize = 10, y_tickfcol = 'black', 
                   ax_family = 'Arial', ax_size = 12, ax_color = 'black',
                   add_txt = FALSE, t_x, t_y, t_text, t_showarrow = FALSE, 
                   t_font = 'Arial', t_size = 10, t_col = 'blue') {
  
  yax <- data %>% select(y) %>% pull(1)
  xax <- data %>% select(x) %>% pull(1)
  
  p <- plot_ly(data = data,
               type = "scatter", 
               mode = mode,
               x = xax,
               y = yax,
               line = list(
                 color = lcol,
                 width = lwidth,
                 dash = ltype
               )) 

  title_font <- list(
    family = title_family,
    size = title_size,
    color = title_color
  )
  
  axis_font <- list(
    family = ax_family,
    size = ax_size,
    color = ax_color
  )
  
  xaxis <- list(title = x_title,
                titlefont = axis_font,
                showline = x_showline,
                showgrid = x_showgrid,
                gridcolor = x_gridcol,
                showticklabels = x_showticklabels,
                linecolor = x_lcol,
                linewidth = x_lwidth,
                zeroline = x_zline,
                autotick = x_autotick,
                ticks = x_ticks,
                tickcolor = x_tickcol,
                tickwidth = x_tickw,
                ticklen = x_ticklen,
                tickfont = list(family = x_ticfont,
                                size = x_tickfsize,
                                color = x_tickfcol))
  
  yaxis <- list(title = y_title,
                titlefont = axis_font,
                showline = y_showline,
                showgrid = y_showgrid,
                gridcolor = y_gridcol,
                showticklabels = y_showticklabels,
                linecolor = y_lcol,
                linewidth = y_lwidth,
                zeroline = y_zline,
                autotick = y_autotick,
                ticks = y_ticks,
                tickcolor = y_tickcol,
                tickwidth = y_tickw,
                ticklen = y_ticklen,
                tickfont = list(family = y_ticfont,
                                size = y_tickfsize,
                                color = y_tickfcol))
  
  p <- p %>%
    layout(title = title,
         font = title_font, 
         paper_bgcolor = p_bgcol,
         plot_bgcolor = plot_bgcol,
         xaxis = xaxis,
         yaxis = yaxis) 
  
  if(add_txt) {
    
    annote <- list(
      x = t_x,
      y = t_y,
      text = t_text,
      font = list(family = t_font,
                  size = t_size,
                  color = t_col),
      showarrow = t_showarrow
    )
    
    p <- p %>%
      layout(annotations = annote)
  }
  
  if(axis_modify) {
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

