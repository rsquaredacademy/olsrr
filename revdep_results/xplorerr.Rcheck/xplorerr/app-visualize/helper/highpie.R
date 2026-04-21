highpie <- function(data, column) {
  
  da <- data %>%
    select_(column) %>%
    pull(1) %>%
    as.factor() 
  
  freq <- da %>%
    table() %>%
    as.vector()
  
  labels <- da %>%
    levels()
  
  highchart() %>%
    hc_chart(type = 'pie') %>%
    hc_add_series_labels_values(labels, freq, 
                                name = "Pie", colorByPoint = TRUE, 
                                type = "pie")
  
}

