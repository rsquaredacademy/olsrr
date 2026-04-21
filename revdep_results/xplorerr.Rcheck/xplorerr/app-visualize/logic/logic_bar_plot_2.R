source('helper/barly2.R')
source('helper/bibar.R')

observeEvent(input$finalok, {

        f_data <- final_split$train[, sapply(final_split$train, is.factor)]
        # validate(need(!dim(f_data)[2] == 0, 'No factor variables in the data.'))
        if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, 'barly2_select_x',
              choices = names(fdata), selected = names(fdata))
        updateSelectInput(session, 'barly2_select_y',
              choices = names(fdata), selected = names(fdata))
        updateSelectInput(session, 'hibar2_select_x',
              choices = names(fdata), selected = names(fdata))
        updateSelectInput(session, 'hibar2_select_y',
              choices = names(fdata), selected = names(fdata))
        } else if (dim(f_data)[2] == 0) {
          updateSelectInput(session, 'barly2_select_x', choices = '', selected = '')
          updateSelectInput(session, 'barly2_select_y', choices = '', selected = '')
          updateSelectInput(session, 'hibar2_select_x', choices = '', selected = '')
          updateSelectInput(session, 'hibar2_select_y', choices = '', selected = '')
        } else {
          updateSelectInput(session, 'barly2_select_x', choices = names(f_data))
          updateSelectInput(session, 'barly2_select_y', choices = names(f_data))
          updateSelectInput(session, 'hibar2_select_x', choices = names(f_data))
          updateSelectInput(session, 'hibar2_select_y', choices = names(f_data))
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
        updateSelectInput(session, 'barly2_select_x',
              choices = names(fdata), selected = names(fdata))
        updateSelectInput(session, 'barly2_select_y',
              choices = names(fdata), selected = names(fdata))
        updateSelectInput(session, 'hibar2_select_x',
              choices = names(fdata), selected = names(fdata))
        updateSelectInput(session, 'hibar2_select_y',
              choices = names(fdata), selected = names(fdata))
        } else if (dim(f_data)[2] == 0) {
          updateSelectInput(session, 'barly2_select_x', choices = '', selected = '')
          updateSelectInput(session, 'barly2_select_y', choices = '', selected = '')
          updateSelectInput(session, 'hibar2_select_x', choices = '', selected = '')
          updateSelectInput(session, 'hibar2_select_y', choices = '', selected = '')
        } else {
          updateSelectInput(session, 'barly2_select_x', choices = names(f_data))
          updateSelectInput(session, 'barly2_select_y', choices = names(f_data))
          updateSelectInput(session, 'hibar2_select_x', choices = names(f_data))
          updateSelectInput(session, 'hibar2_select_y', choices = names(f_data))
        }
})


output$barly2_plot_1 <- plotly::renderPlotly({
  barly2(data = final_split$train, x = input$barly2_select_x, 
    y = input$barly2_select_y, title = input$barly2_title, 
    x_title = input$barly2_xlabel, y_title = input$barly2_ylabel)
})

output$hibar2_plot_1 <- highcharter::renderHighchart({
  bibar(data = final_split$train, x = input$hibar2_select_x, 
    y = input$hibar2_select_y, horizontal = input$hibar2_horiz,
    stacked = input$hibar2_stack)
})
