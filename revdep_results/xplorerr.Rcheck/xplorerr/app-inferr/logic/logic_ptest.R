source("xpl-helpers.R")
source("xpl-output.R")
source("xpl-format.R")

observe({
    updateSelectInput(session,
                      inputId = "var_ptest1",
                      choices = names(data()),
                      selected = '')
    updateSelectInput(session,
      inputId = "var_ptest2",
      choices = names(data()),
      selected = '')

})

observeEvent(input$finalok, {
    num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
    if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'var_ptest1',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'var_ptest2',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'var_ptest1',
              choices = '', selected = '')
             updateSelectInput(session, 'var_ptest2',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'var_ptest1', choices = names(num_data))
             updateSelectInput(session, 'var_ptest2', choices = names(num_data))
        }
    updateSelectInput(session,
                      inputId = "var_ptest1",
                      choices = names(num_data))
    updateSelectInput(session,
                      inputId = "var_ptest2",
                      choices = names(num_data))

})

observeEvent(input$submit_part_train_per, {
    num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
    if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'var_ptest1',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'var_ptest2',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'var_ptest1',
              choices = '', selected = '')
             updateSelectInput(session, 'var_ptest2',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'var_ptest1', choices = names(num_data))
             updateSelectInput(session, 'var_ptest2', choices = names(num_data))
        }
    updateSelectInput(session,
                      inputId = "var_ptest1",
                      choices = names(num_data))
    updateSelectInput(session,
                      inputId = "var_ptest2",
                      choices = names(num_data))

})

d_ptest <- eventReactive(input$submit_ptest, {
  data <- final_split$train
  xpl_ts_paired_ttest(data, input$var_ptest1, input$var_ptest2, 
                      input$ptest_conf, input$ptest_type)
})

output$ptest_out <- renderPrint({
  d_ptest()
})
