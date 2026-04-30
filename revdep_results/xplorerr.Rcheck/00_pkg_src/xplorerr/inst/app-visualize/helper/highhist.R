highist <- function(data, column, xlab = ' ', color = 'blue') {
  
  da <- data %>%
    select_(column) %>%
    pull(1) 
  
  
  da %>%
    hchart(name = xlab) %>%
    hc_colors(colors = color) %>%
    hc_yAxis(title = list(text = 'Frequency'))
  
}

