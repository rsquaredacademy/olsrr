source("xpl-helpers.R")
source("xpl-output.R")
source("xpl-format.R")

observe({
  updateSelectInput(session,inputId = "var_levtest",
    choices = names(data()), selected = '')
  updateSelectInput(session,inputId = "var_levtestg1",
    choices = names(data()), selected = '')
  updateSelectInput(session,inputId = "var_levtestg2",
    choices = names(data()), selected = '')
})

observeEvent(input$finalok, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
    if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'var_levtest',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'var_levtestg1',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'var_levtest',
              choices = '', selected = '')
             updateSelectInput(session, 'var_levtestg1',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'var_levtest', choices = names(num_data))
             updateSelectInput(session, 'var_levtestg1', choices = names(num_data))
        }
    if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "var_levtestg2",
            choices = names(fdata))
        } else {
          updateSelectInput(session, 'var_levtestg2', choices = names(f_data))
        }
})


observeEvent(input$submit_part_train_per, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
    if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'var_levtest',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'var_levtestg1',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'var_levtest',
              choices = '', selected = '')
             updateSelectInput(session, 'var_levtestg1',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'var_levtest', choices = names(num_data))
             updateSelectInput(session, 'var_levtestg1', choices = names(num_data))
        }
    if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "var_levtestg2",
            choices = names(fdata))
        } else {
          updateSelectInput(session, 'var_levtestg2', choices = names(f_data))
        }
})


d_levtest <- eventReactive(input$submit_levtest, {
  req(input$var_levtest)
  data <- final_split$train[, c(input$var_levtest)]
  xpl_levene_test(data, variables = input$var_levtest)
})

d_levtestg <- eventReactive(input$submit_levtestg, {
  req(input$var_levtestg1)
  req(input$var_levtestg2)
  data <- final_split$train[, c(input$var_levtestg1, input$var_levtestg2)]
  xpl_levene_test(data, input$var_levtestg1, group_var = input$var_levtestg2)
})


output$levtest_out <- renderPrint({
  d_levtest()
})

output$levtestg_out <- renderPrint({
  d_levtestg()
})

