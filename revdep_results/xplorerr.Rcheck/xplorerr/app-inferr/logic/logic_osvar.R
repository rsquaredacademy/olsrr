source("xpl-helpers.R")
source("xpl-output.R")
source("xpl-format.R")

observe({
    updateSelectInput(session,
                      inputId = "var_osvartest",
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
            updateSelectInput(session, 'var_osvartest',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'var_osvartest',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'var_osvartest', choices = names(num_data))
        }
})

observeEvent(input$submit_part_train_per, {
    num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
    if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'var_osvartest',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'var_osvartest',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'var_osvartest', choices = names(num_data))
        }
})


d_osvartest <- eventReactive(input$submit_osvartest, {
  data <- final_split$train
  xpl_os_var_test(data, input$var_osvartest, input$sd_osvartest, 
                  input$osvartest_conf, input$osvartest_type)
})

output$osvartest_out <- renderPrint({
  d_osvartest()
})
