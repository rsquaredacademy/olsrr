observeEvent(input$finalok, {
    num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
    if (is.null(dim(num_data))) {
        k <- final_split$train %>% map(is.numeric) %>% unlist()
        j <- names(which(k == TRUE))
        numdata <- tibble::as_data_frame(num_data)
        colnames(numdata) <- j
        updateSelectInput(session, inputId = "var_summary",
            choices = names(numdata), selected = names(numdata)[1])
        updateSliderInput(session = session,
                        inputId = 'filter_summary',
                        min = min(numdata),
                        max = max(numdata),
                        step = 1,
                        value = c(min(numdata), max(numdata))
        )
      } else if (ncol(num_data) < 1) {
        updateSelectInput(session, inputId = "var_summary",
            choices = '', selected = '')
        updateSliderInput(session = session,
                        inputId = 'filter_summary',
                        value = '')
      } else {
          updateSelectInput(session, 'var_summary',
            choices = names(num_data), selected = names(num_data)[1])
          updateSliderInput(session = session,
                        inputId = 'filter_summary',
                        min = min(num_data),
                        max = max(num_data),
                        step = 1,
                        value = c(min(num_data), max(num_data))
          )
      }

})


observeEvent(input$submit_part_train_per, {
    num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
    if (is.null(dim(num_data))) {
        k <- final_split$train %>% map(is.numeric) %>% unlist()
        j <- names(which(k == TRUE))
        numdata <- tibble::as_data_frame(num_data)
        colnames(numdata) <- j
        updateSelectInput(session, inputId = "var_summary",
            choices = names(numdata), selected = names(numdata)[1])
      } else if (ncol(num_data) < 1) {
        updateSelectInput(session, inputId = "var_summary",
            choices = '', selected = '')
      } else {
          updateSelectInput(session, 'var_summary',
            choices = names(num_data), selected = names(num_data)[1])
      }

})


# selected data
d_summary <- eventReactive(input$submit_summary, {
  # validate(need(input$var_summary != '', 'Please select a variable.'))
  req(input$var_summary)
  ds_summary_stats(final_split$train, !! sym(input$var_summary))
})


# output
output$summary <- renderPrint({
    # summary_stats(fil_data())
    d_summary()
})

