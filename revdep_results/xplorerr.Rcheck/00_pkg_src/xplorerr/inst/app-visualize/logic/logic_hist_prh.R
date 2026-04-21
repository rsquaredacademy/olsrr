source('helper/histly.R')
source('helper/highhist.R')

observeEvent(input$finalok, {
        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'histly_select_x',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'hihist_select_x',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'histly_select_x', choices = '', selected = '')
             updateSelectInput(session, 'hihist_select_x', choices = '', selected = '')
        } else {
             updateSelectInput(session, 'histly_select_x', choices = names(num_data))
             updateSelectInput(session, 'hihist_select_x', choices = names(num_data))
        }

})

observeEvent(input$submit_part_train_per, {

        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'histly_select_x',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'hihist_select_x',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'histly_select_x', choices = '', selected = '')
             updateSelectInput(session, 'hihist_select_x', choices = '', selected = '')
        } else {
             updateSelectInput(session, 'histly_select_x', choices = names(num_data))
             updateSelectInput(session, 'hihist_select_x', choices = names(num_data))
        }

})


histlydata <- reactive({
  req(input$histly_select_x)
  final_split$train[, input$histly_select_x]
})

observe({
  updateNumericInput(session, "histly_binstart", value = min(histlydata()))
  updateNumericInput(session, "histly_binend", value = max(histlydata()))
})

output$histly_plot_1 <- plotly::renderPlotly({
  if (input$histly_auto == TRUE) {  
      histly(data = final_split$train, y = input$histly_select_x, 
        title = input$histly_title, x_title = input$histly_xlabel, hist_col = input$histly_color,
        y_title = input$histly_ylabel, hist_orient = input$histly_horiz, 
        hist_opacity = input$histly_opacity, hist_type = input$histly_type, 
        auto_binx = input$histly_auto)
  } else {
    histly(data = final_split$train, y = input$histly_select_x, 
        title = input$histly_title, x_title = input$histly_xlabel, hist_col = input$histly_color,
        y_title = input$histly_ylabel, hist_orient = input$histly_horiz, 
        hist_opacity = input$histly_opacity, hist_type = input$histly_type, 
        auto_binx = input$histly_auto, xbins_size = input$histly_binsize,
        xbins_start = input$histly_binstart, xbins_end = input$histly_binend)
  }
})

output$hihist_plot_1 <- highcharter::renderHighchart({
  highist(data = final_split$train, column = input$hihist_select_x,
    xlab = input$hihist_xlabel, color = input$hihist_color)
})
