source('helper/ggpie.R')

observeEvent(input$finalok, {

        f_data <- final_split$train[, sapply(final_split$train, is.factor)]
        # validate(need(!dim(f_data)[2] == 0, 'No factor variables in the data.'))
        if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, 'gpie_select_x',
              choices = names(fdata), selected = names(fdata))
        } else if (dim(f_data)[2] == 0) {
          updateSelectInput(session, 'gpie_select_x', choices = '', selected = '')
        } else {
          updateSelectInput(session, 'gpie_select_x', choices = names(f_data))
        }
})

observeEvent(input$submit_part_train_per, {
        f_data <- final_split$train[, sapply(final_split$train, is.factor)]
        # validate(need(!dim(f_data)[2] == 0, 'No factor variables in the data.'))
        if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, 'gpie_select_x',
              choices = names(fdata), selected = names(fdata))
        } else if (dim(f_data)[2] == 0) {
          updateSelectInput(session, 'gpie_select_x', choices = '', selected = '')
        } else {
          updateSelectInput(session, 'gpie_select_x', choices = names(f_data)) 
        }
})


output$gpie_plot_1 <- renderPlot({
  ggpie(data = final_split$train, x = input$gpie_select_x,     
    title = input$gpie_title, xlab = input$gpie_xlabel, 
    ylab = input$gpie_ylabel)
})