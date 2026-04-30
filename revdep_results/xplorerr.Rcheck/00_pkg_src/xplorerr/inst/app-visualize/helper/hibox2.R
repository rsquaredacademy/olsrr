library(highcharter)

highbox <- function(data, y, x, col = 'blue',
                    xax_title = '', xax_title_align = 'center',
                    xax_title_col = 'black', title = '',
                    xax_title_ftype = 'italic', xax_title_fsize = '18px',
                    yax_title = '', yax_title_col = 'black', 
                    yax_title_ftype = 'italic', yax_title_fsize = '18px',
                    title_align = 'center', title_col = 'black', 
                    title_ftype = 'italic', title_size = '12px',
                    sub = '', sub_align = 'center', sub_col = 'black', 
                    sub_ftype = 'italic', sub_size = '12px') {
  
  da <- data %>%
    select_(y, x)
  
  colnames(da) <- c('y', 'x')
  
  h <- hcboxplot(x = da$y, var = da$x, color = col)
  
  h <- h %>%
    
    hc_xAxis(categories = levels(as.factor(da$x)),
             title = list(text = xax_title,
                          align = xax_title_align,
                          style = list(color = xax_title_col,
                                       fontWeight = xax_title_ftype,
                                       fontSize = xax_title_fsize))) %>%
    
    hc_yAxis(
             title = list(text = yax_title,
                          style = list(color = yax_title_col,
                                       fontWeight = yax_title_ftype,
                                       fontSize = yax_title_fsize)),
             opposite = FALSE) %>%
    
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
  
  h
  
}

