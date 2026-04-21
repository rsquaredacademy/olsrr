library(highcharter)

highline <- function(data, x, columns, add_labels = FALSE) {
  
  x <- data %>%
    select_(x) %>%
    pull(1)
  
  column <- data %>%
    select(columns)
  
  n <- column %>% ncol() %>% seq_len()
  
  nam <- column %>% names()
  
  h <- highchart() %>%
    hc_xAxis(categories = x)
  
  for (i in n) {
    h <- h %>%
      hc_add_series(name = nam[i], data = column[[i]])
  }
  
  if (add_labels) {
    h %>%
      hc_plotOptions(line = list(dataLabels = list(enabled = TRUE)))
  }
  
  h
  
}


