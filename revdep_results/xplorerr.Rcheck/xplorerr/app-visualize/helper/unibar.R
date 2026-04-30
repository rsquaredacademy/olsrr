library(highcharter)
library(dplyr)
library(magrittr)

highbar <- function(data, column, title = '', name = '', horizontal = FALSE) {
  
  da <- data %>%
    select_(column) %>%
    pull(1) %>%
    as.factor()
  
  lev <- da %>%
    levels()
  
  tab <- da %>%
    table() %>%
    as.vector()
  
  if (horizontal) {
    bartype <- 'bar'
  } else {
    bartype <- 'column'
  }
  
  highchart() %>%
    hc_chart(type = bartype) %>%
    hc_title(text = title) %>%
    hc_xAxis(categories = lev) %>%
    hc_add_series(data = tab,
                  name = name)
  
}

