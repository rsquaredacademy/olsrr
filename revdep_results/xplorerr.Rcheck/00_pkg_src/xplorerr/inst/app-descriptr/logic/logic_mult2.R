output$mult2 <- renderPrint({
	num_data <- final_split$train[, sapply(final_split$train, is.factor)]
  # validate(need(!is.null(dim(num_data)), 'Data must have at least 3 factor variables.'))
  if (is.null(dim(num_data))) {
    out <- NULL
  } else if (ncol(num_data) < 3) {
    out <- NULL
  } else {
    ds_auto_cross_table(final_split$train)  
  }
})

