source('helper/scatterly.R')
source('helper/hscatter.R')

observeEvent(input$finalok, {

        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'scatly_select_x',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'scatly_select_y',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'hiscat_select_x',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'hiscat_select_y',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'scatly_select_x',
              choices = '', selected = '')
             updateSelectInput(session, 'scatly_select_y',
              choices = '', selected = '')
             updateSelectInput(session, 'hiscat_select_x',
              choices = '', selected = '')
             updateSelectInput(session, 'hiscat_select_y',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'scatly_select_x', choices = names(num_data))
             updateSelectInput(session, 'scatly_select_y', choices = names(num_data))
             updateSelectInput(session, 'hiscat_select_x', choices = names(num_data))
             updateSelectInput(session, 'hiscat_select_y', choices = names(num_data))
        }

})

observeEvent(input$submit_part_train_per, {

        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'scatly_select_x',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'scatly_select_y',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'hiscat_select_x',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'hiscat_select_y',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'scatly_select_x',
              choices = '', selected = '')
             updateSelectInput(session, 'scatly_select_y',
              choices = '', selected = '')
             updateSelectInput(session, 'hiscat_select_x',
              choices = '', selected = '')
             updateSelectInput(session, 'hiscat_select_y',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'scatly_select_x', choices = names(num_data))
             updateSelectInput(session, 'scatly_select_y', choices = names(num_data))
             updateSelectInput(session, 'hiscat_select_x', choices = names(num_data))
             updateSelectInput(session, 'hiscat_select_y', choices = names(num_data))
        }
})


output$scatly_plot_1 <- plotly::renderPlotly({
  scatterly(data = final_split$train, y = input$scatly_select_y, 
    x = input$scatly_select_x, title = input$scatly_title, show_legend = FALSE,
    x_title = input$scatly_xlabel, y_title = input$scatly_ylabel,
    text = input$scatly_text, color = input$scatly_color, opacity = input$scatly_opacity, 
    symbol = input$scatly_symbol, size = input$scatly_size, 
    fit_line = input$scatly_fit, line_col = input$scatly_lcol,
    line_type = input$scatly_ltype, line_width = input$scatly_lsize)
})

output$hiscat_plot_1 <- highcharter::renderHighchart({
  hscatter(data = final_split$train, x = input$hiscat_select_x, 
    y = input$hiscat_select_y, xax_title = input$hiscat_xlabel, 
    yax_title = input$hiscat_ylabel, point_size = input$hiscat_size, 
    scatter_series_name = ' ', point_col = input$hiscat_color, 
    point_shape = input$hiscat_symbol, fit_line = input$hiscat_fit, 
    line_col = input$hiscat_lcol, line_width = input$hiscat_lsize, 
    point_on_line = FALSE, title = input$hiscat_title, sub = input$hiscat_subtitle)
})
