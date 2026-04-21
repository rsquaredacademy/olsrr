hscatter <- function(data, x, y, xax_title = '', xax_title_align = 'center',
                    xax_tick_int = 5, xax_title_col = 'black', 
                    xax_title_ftype = 'italic', xax_title_fsize = '18px',
                    yax_title = '', yax_title_col = 'black', 
                    yax_title_ftype = 'italic', yax_title_fsize = '18px',
                    point_size = 4, scatter_series_name = ' ',
                    point_col = 'blue', point_shape = 'circle',
                    fit_line = FALSE, line_col = 'red',
                    line_width = 0.1, point_on_line = FALSE, title = '',
                    title_align = 'center', title_col = 'black', 
                    title_ftype = 'italic', title_size = '12px',
                    sub = '', sub_align = 'center', sub_col = 'black', 
                    sub_ftype = 'italic', sub_size = '12px') {

  da <- data %>%
    select_(x, y) %>%
    arrange_(x) 
  
  colnames(da) <- c('x', 'y')
  
  j <- seq(from = min(da$x),
           to = max(da$x),
           length.out = length(da$x))
  
  fit <- lm(y ~ x, data = da)
  new <- tibble::tibble(x = j)
  fits <- tibble::tibble(value = predict(fit, newdata = new))
  
  h <- highchart() %>%
    
    hc_xAxis(categories = da$x,
             tickInterval = xax_tick_int,
             title = list(text = xax_title,
                          align = xax_title_align,
                          style = list(color = xax_title_col,
                                       fontWeight = xax_title_ftype,
                                       fontSize = xax_title_fsize))) %>%
    
    hc_yAxis(title = list(text = yax_title,
                          style = list(color = yax_title_col,
                                       fontWeight = yax_title_ftype,
                                       fontSize = yax_title_fsize)),
             opposite = FALSE) %>%
    
    hc_add_series(type = "scatter",
                  data = da$y,
                  name = scatter_series_name,
                  marker = list(radius = point_size)) %>%
    
    hc_plotOptions(line = list(color = line_col,
                               marker = list(lineWidth = line_width,
                                             enabled = point_on_line)),
                   scatter = list(marker = list(symbol = point_shape),
                                  color = point_col)) %>%
    
    hc_title(text = title,
             align = title_align,
             style = list(color = title_col,
                          fontWeight = title_ftype,
                          fontSize = title_size)) %>%
    
    hc_subtitle(text = sub,
                align = sub_align,
                style = list(color = sub_col,
                             fontWeight = sub_ftype,
                             fontSize = sub_size))
  
  if (fit_line) {
    
    h <- h %>%
      hc_add_series(type = "line",
                    data = fits$value,
                    name = 'Regression Line',
                    pointIntervalUnit = 0) 
    
  }
  
  h
  
}