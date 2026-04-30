source("xpl-helpers.R")
source("xpl-output.R")
source("xpl-format.R")

observe({
    updateSelectInput(session,
      inputId = "var_tsvartest1",
      choices = names(data()),
      selected = '')
    updateSelectInput(session,
      inputId = "var_tsvartest2",
      choices = names(data()),
      selected = '')
    updateSelectInput(session,
      inputId = "var_tsvartestg1",
      choices = names(data()),
      selected = '')
    updateSelectInput(session,
      inputId = "var_tsvartestg2",
      choices = names(data()),
      selected = '')
})

observeEvent(input$finalok, {
    num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'var_tsvartest1',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'var_tsvartest2',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'var_tsvartestg1',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'var_tsvartest1',
              choices = '', selected = '')
             updateSelectInput(session, 'var_tsvartest2',
              choices = '', selected = '')
             updateSelectInput(session, 'var_tsvartestg1',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'var_tsvartest1', choices = names(num_data))
             updateSelectInput(session, 'var_tsvartest2', choices = names(num_data))
             updateSelectInput(session, 'var_tsvartestg1', choices = names(num_data))
        }
    if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "var_tsvartestg2",
            choices = names(fdata))
        } else {
          updateSelectInput(session, 'var_tsvartestg2', choices = names(f_data))
        }
})

observeEvent(input$submit_part_train_per, {
    num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'var_tsvartest1',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'var_tsvartest2',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'var_tsvartestg1',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'var_tsvartest1',
              choices = '', selected = '')
             updateSelectInput(session, 'var_tsvartest2',
              choices = '', selected = '')
             updateSelectInput(session, 'var_tsvartestg1',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'var_tsvartest1', choices = names(num_data))
             updateSelectInput(session, 'var_tsvartest2', choices = names(num_data))
             updateSelectInput(session, 'var_tsvartestg1', choices = names(num_data))
        }
    if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "var_tsvartestg2",
            choices = names(fdata))
        } else {
          updateSelectInput(session, 'var_tsvartestg2', choices = names(f_data))
        }
})


d_tsvartest <- eventReactive(input$submit_tsvartest, {
  req(input$var_tsvartest1)
  req(input$var_tsvartest2)
  data <- final_split$train[, c(input$var_tsvartest1, input$var_tsvartest2)]
  xpl_ts_var_test(data, input$var_tsvartest1, input$var_tsvartest2,
                  alternative = input$tsvartest_type)
})

d_tsvartestg <- eventReactive(input$submit_tsvartestg, {
  req(input$var_tsvartestg1)
  req(input$var_tsvartestg2)
  data <- final_split$train[, c(input$var_tsvartestg1, input$var_tsvartestg2)]
  xpl_ts_var_test(data, input$var_tsvartestg1, input$var_tsvartestg2,
                         alternative = input$tsvartestg_type)
})

output$tsvartest_out <- renderPrint({
  d_tsvartest()
})

output$tsvartestg_out <- renderPrint({
  d_tsvartestg()
})
