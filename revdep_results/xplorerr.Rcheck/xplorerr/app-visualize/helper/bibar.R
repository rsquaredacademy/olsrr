bibar <- function(data, x, y, horizontal = FALSE, stacked = FALSE) {

	da <- data %>%
	  select_(x, y) %>%
	  table()

	xcat <- da %>%
	  rownames() %>%
	  as.numeric()
	  
	types <- da %>%
    ncol() %>%
    seq_len()

  if (horizontal) {

    h <- highchart() %>%
		  hc_chart(type = 'bar') %>%
		  hc_xAxis(categories = xcat) 

		if (stacked) {

      h <- h %>%
        hc_plotOptions(bar = list(stacking = 'normal'))

		}

  } else {

		h <- highchart() %>%
		  hc_chart(type = 'column') %>%
		  hc_xAxis(categories = xcat) 

		if (stacked) {

      h <- h %>%
        hc_plotOptions(column = list(stacking = 'normal'))  	

		}

  }


	for (i in types) {
	  h <- h %>%
	    hc_add_series(data = da[i, ] %>% as.vector()) 
	}

	h

}