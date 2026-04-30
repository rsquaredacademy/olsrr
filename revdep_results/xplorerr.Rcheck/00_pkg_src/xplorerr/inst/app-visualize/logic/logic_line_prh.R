source('helper/linely.R')
source('helper/highline.R')

observeEvent(input$finalok, {
        num_data <- cbind.data.frame(final_split$train[, sapply(final_split$train, is.numeric)],
                      final_split$train[, sapply(final_split$train, is.Date)])
    
    k <- final_split$train %>%
      map(is.numeric) %>%
      unlist()

    t <- final_split$train %>%
      map(is.Date) %>%
      unlist()

    j1 <- names(which(k == TRUE))
    j2 <- names(which(t == TRUE))

    if (length(j1) == 0) {
      j <- j2
    } else if (length(j2) == 0) {
      j <- j1
    } else {
      j <- c(j1, j2)
    }
    colnames(num_data) <- j
        if (is.null(dim(num_data))) {
            numdata <- tibble::as_data_frame(num_data)
            updateSelectInput(session, 'linely_select_x',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'linely_select_y',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'hiline_select_x',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'hiline_select_y',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'linely_select_x', choices = '', selected = '')
             updateSelectInput(session, 'linely_select_y', choices = '', selected = '')
             updateSelectInput(session, 'hiline_select_x', choices = '', selected = '')
             updateSelectInput(session, 'hiline_select_y', choices = '', selected = '')
        } else {
             updateSelectInput(session, 'linely_select_x', choices = names(num_data), selected = names(num_data))
             updateSelectInput(session, 'linely_select_y', choices = names(num_data), selected = names(num_data))
             updateSelectInput(session, 'hiline_select_x', choices = names(num_data), selected = names(num_data))
             updateSelectInput(session, 'hiline_select_y', choices = names(num_data), selected = names(num_data))
        }

})

observeEvent(input$submit_part_train_per, {
        
  num_data <- cbind.data.frame(final_split$train[, sapply(final_split$train, is.numeric)],
                      final_split$train[, sapply(final_split$train, is.Date)])
    
    k <- final_split$train %>%
      map(is.numeric) %>%
      unlist()

    t <- final_split$train %>%
      map(is.Date) %>%
      unlist()

    j1 <- names(which(k == TRUE))
    j2 <- names(which(t == TRUE))

    if (length(j1) == 0) {
      j <- j2
    } else if (length(j2) == 0) {
      j <- j1
    } else {
      j <- c(j1, j2)
    }
    colnames(num_data) <- j
        if (is.null(dim(num_data))) {
            numdata <- tibble::as_data_frame(num_data)
            updateSelectInput(session, 'linely_select_x',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'linely_select_y',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'hiline_select_x',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'hiline_select_y',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'linely_select_x', choices = '', selected = '')
             updateSelectInput(session, 'linely_select_y', choices = '', selected = '')
             updateSelectInput(session, 'hiline_select_x', choices = '', selected = '')
             updateSelectInput(session, 'hiline_select_y', choices = '', selected = '')
        } else {
             updateSelectInput(session, 'linely_select_x', choices = names(num_data), selected = names(num_data))
             updateSelectInput(session, 'linely_select_y', choices = names(num_data), selected = names(num_data))
             updateSelectInput(session, 'hiline_select_x', choices = names(num_data), selected = names(num_data))
             updateSelectInput(session, 'hiline_select_y', choices = names(num_data), selected = names(num_data))
        }

})


output$linely_plot_1 <- plotly::renderPlotly({
  linely(data = final_split$train, x = input$linely_select_x,
    y = input$linely_select_y, title = input$linely_title, 
    x_title = input$linely_xlabel, y_title = input$linely_ylabel, 
    lcol = input$linely_color, lwidth = input$linely_width, 
    ltype = input$linely_type)
})

output$hiline_plot_1 <- highcharter::renderHighchart({
  highline(data = final_split$train, x = input$hiline_select_x, 
    columns = input$hiline_select_y, add_labels = input$hiline_labels)
})
