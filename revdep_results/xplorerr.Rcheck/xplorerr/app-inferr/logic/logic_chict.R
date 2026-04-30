source("xpl-helpers.R")
source("xpl-output.R")
source("xpl-format.R")

observe({
    updateSelectInput(session, 'var_chict1', choices = names(data()))
    updateSelectInput(session, 'var_chict2', choices = names(data()))
})

observeEvent(input$finalok, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, 'var_chict1', choices = names(fdata))
    		updateSelectInput(session, 'var_chict2', choices = names(fdata))
        } else {
          updateSelectInput(session, 'var_chict1', choices = names(f_data))
    			updateSelectInput(session, 'var_chict2', choices = names(f_data))
        }
})


observeEvent(input$submit_part_train_per, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, 'var_chict1', choices = names(fdata))
            updateSelectInput(session, 'var_chict2', choices = names(fdata))
        } else {
          updateSelectInput(session, 'var_chict1', choices = names(f_data))
                updateSelectInput(session, 'var_chict2', choices = names(f_data))
        }
})


d_chict <- eventReactive(input$submit_chict, {
    data <- final_split$train[, c(input$var_chict1, input$var_chict2)]
})

output$chict_out <- renderPrint({
  xpl_chisq_assoc_test(d_chict(), input$var_chict1, input$var_chict2)
})
