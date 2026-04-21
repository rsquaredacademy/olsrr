source("xpl-helpers.R")
source("xpl-output.R")
source("xpl-format.R")

observe({
    updateSelectInput(session, 'var_runs', choices = names(data()))
})

observeEvent(input$finalok, {
    num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
    if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'var_runs',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'var_runs',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'var_runs', choices = names(num_data))
        }
    updateSelectInput(session, 'var_runs', choices = names(num_data))
})


observeEvent(input$submit_part_train_per, {
    num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
    if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'var_runs',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'var_runs',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'var_runs', choices = names(num_data))
        }
    updateSelectInput(session, 'var_runs', choices = names(num_data))
})


d_runs <- eventReactive(input$submit_runs, {
  req(input$var_runs)
  data <- final_split$train
  xpl_runs_test(data, input$var_runs,
                as.logical(input$runs_drop),
                as.logical(input$runs_split),
                as.logical(input$runs_mean),
                input$runs_thold)
})

output$runs_out <- renderPrint({
  d_runs()
})

