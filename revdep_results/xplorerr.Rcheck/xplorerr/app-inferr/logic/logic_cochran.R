source("xpl-helpers.R")
source("xpl-output.R")
source("xpl-format.R")

observe({
    updateSelectInput(session, 'var_cochran', choices = names(data()))
})

observeEvent(input$finalok, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    if (is.null(dim(f_data))) {
            k <- final_split$train %>% map(is.factor) %>% unlist()
            j <- names(which(k == TRUE))
            fdata <- tibble::as_data_frame(f_data)
            colnames(fdata) <- j
            updateSelectInput(session, 'var_cochran',
              choices = names(fdata), selected = names(fdata))
        } else if (ncol(f_data) < 1) {
             updateSelectInput(session, 'var_cochran',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'var_cochran', choices = names(f_data))
        }

})

observeEvent(input$submit_part_train_per, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    if (is.null(dim(f_data))) {
            k <- final_split$train %>% map(is.factor) %>% unlist()
            j <- names(which(k == TRUE))
            fdata <- tibble::as_data_frame(f_data)
            colnames(fdata) <- j
            updateSelectInput(session, 'var_cochran',
              choices = names(fdata), selected = names(fdata))
        } else if (ncol(f_data) < 1) {
             updateSelectInput(session, 'var_cochran',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'var_cochran', choices = names(f_data))
        }

})

d_cochran <- eventReactive(input$submit_cochran, {
  req(input$var_cochran)
  data <- final_split$train[, c(input$var_cochran)]
  xpl_cochran_qtest(data, input$var_cochran)
})

output$cochran_out <- renderPrint({
  d_cochran()
})
