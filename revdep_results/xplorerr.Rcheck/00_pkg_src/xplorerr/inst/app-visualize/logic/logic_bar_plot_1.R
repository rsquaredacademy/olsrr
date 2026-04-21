source('helper/barly1.R')
source('helper/unibar.R')

observeEvent(input$finalok, {

        f_data <- final_split$train[, sapply(final_split$train, is.factor)]
        # validate(need(!dim(f_data)[2] == 0, 'No factor variables in the data.'))
        if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, 'barly1_select_x',
              choices = names(fdata), selected = names(fdata))
        updateSelectInput(session, 'hibar1_select_x',
              choices = names(fdata), selected = names(fdata))
        } else if (dim(f_data)[2] == 0) {
          updateSelectInput(session, 'barly1_select_x', choices = '', selected = '')
          updateSelectInput(session, 'hibar1_select_x', choices = '', selected = '')
        } else {
          updateSelectInput(session, 'barly1_select_x', choices = names(f_data))
          updateSelectInput(session, 'hibar1_select_x', choices = names(f_data))
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
        updateSelectInput(session, 'barly1_select_x',
              choices = names(fdata), selected = names(fdata))
        updateSelectInput(session, 'hibar1_select_x',
              choices = names(fdata), selected = names(fdata))
        } else if (dim(f_data)[2] == 0) {
          updateSelectInput(session, 'barly1_select_x', choices = '', selected = '')
          updateSelectInput(session, 'hibar1_select_x', choices = '', selected = '')
        } else {
          updateSelectInput(session, 'barly1_select_x', choices = names(f_data))
          updateSelectInput(session, 'hibar1_select_x', choices = names(f_data))
        }
})


output$barly1_plot_1 <- plotly::renderPlotly({
  barly1(data = final_split$train, x_data = input$barly1_select_x, 
    title = input$barly1_title, x_title = input$barly1_xlabel, 
    y_title = input$barly1_ylabel, bar_col = input$barly1_color,
    b_text = input$barly1_btext)
})

output$hibar1_plot_1 <- highcharter::renderHighchart({
  highbar(data = final_split$train, column = input$hibar1_select_x, 
    title = input$hibar1_title, name = input$hibar1_xlabel)
})
