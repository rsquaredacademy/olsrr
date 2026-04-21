source('helper/boxly1.R')

observeEvent(input$finalok, {
        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'boxly1_select_x',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'boxly1_select_x',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'boxly1_select_x', choices = names(num_data))
        }

})

observeEvent(input$submit_part_train_per, {
        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'boxly1_select_x',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'boxly1_select_x',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'boxly1_select_x', choices = names(num_data))
        }

})


output$boxly1_plot_1 <- plotly::renderPlotly({
  boxly1(data = final_split$train, y = input$boxly1_select_x, 
    title = input$boxly1_title, name = input$boxly1_xlabel,
    x_title = NULL, y_title = input$boxly1_ylabel)
})

