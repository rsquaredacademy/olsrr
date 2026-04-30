observeEvent(input$finalok, {
    num_data <- final_split$train[, sapply(final_split$train, is.factor)]
    if (is.null(dim(num_data))) {
      k <- final_split$train %>% map(is.factor) %>% unlist()
      j <- names(which(k == TRUE))
      f_data <- tibble::as_data_frame(num_data)
      colnames(f_data) <- j
      updateSelectInput(session,
                        inputId = "var_table",
                        choices = names(f_data))
    } else {
      updateSelectInput(session,
                      inputId = "var_table",
                      choices = names(num_data))

    }
})

observeEvent(input$submit_part_train_per, {
    num_data <- final_split$train[, sapply(final_split$train, is.factor)]
    if (is.null(dim(num_data))) {
      k <- final_split$train %>% map(is.factor) %>% unlist()
      j <- names(which(k == TRUE))
      f_data <- tibble::as_data_frame(num_data)
      colnames(f_data) <- j
      updateSelectInput(session,
                        inputId = "var_table",
                        choices = names(f_data))
    } else {
      updateSelectInput(session,
                      inputId = "var_table",
                      choices = names(num_data))

    }
})

# selected data
d_freq_qual <- eventReactive(input$submit_fqual, {
	# validate(need(input$var_table != '', 'Please select a variable.'))
    data <- final_split$train[, c(input$var_table)]
    fdata <- as.data.frame(data)
    names(fdata) <- input$var_table
    fdata
})

fqual_out <- eventReactive(input$submit_fqual, {
  ki <- ds_freq_table(final_split$train, !! sym(as.character(input$var_table)))
  ki
})

f1_title <- eventReactive(input$submit_fqual, {
  h3('Frequency Table')
})

output$freq1_title <- renderUI({
  f1_title()
})

# output
output$freq_qual <- renderPrint({
  fqual_out()
})

output$qual_vert <- renderPlot({
    plot(fqual_out())
})

