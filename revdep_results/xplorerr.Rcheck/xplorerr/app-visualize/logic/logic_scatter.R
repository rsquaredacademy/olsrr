source("helper/scatter-plot.R")

    # observe({
    #     updateSelectInput(session, 'scatter_select_x', choices = names(data()))
    #     updateSelectInput(session, 'scatter_select_y', choices = names(data()))
    # })

    observeEvent(input$finalok, {
        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'scatter_select_x',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'scatter_select_y',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'scatter_select_x',
              choices = '', selected = '')
             updateSelectInput(session, 'scatter_select_y',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'scatter_select_x', choices = names(num_data))
             updateSelectInput(session, 'scatter_select_y', choices = names(num_data))
        }
    })

    observeEvent(input$submit_part_train_per, {
        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'scatter_select_x',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'scatter_select_y',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'scatter_select_x',
              choices = '', selected = '')
             updateSelectInput(session, 'scatter_select_y',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'scatter_select_x', choices = names(num_data))
             updateSelectInput(session, 'scatter_select_y', choices = names(num_data))
        }
    })

    # selected data
    selected_x <- reactive({
      req(input$scatter_select_x)
      out <- final_split$train[, input$scatter_select_x]
    })

    selected_y <- reactive({
      req(input$scatter_select_y)
      out <- final_split$train[, input$scatter_select_y]
    })

    output$ui_xrange_min <- renderUI({
    df <- final_split$train
    if (is.null(df)) return(NULL)
    numericInput('x_range_min', 'X Axis Min', value = min(as.numeric(selected_x())))
    })

    output$ui_xrange_max <- renderUI({
    df <- final_split$train
    if (is.null(df)) return(NULL)
    numericInput('x_range_max', 'X Axis Max', value = max(as.numeric(selected_x())))
    })

    output$ui_yrange_min <- renderUI({
    df <- final_split$train
    if (is.null(df)) return(NULL)
    numericInput('y_range_min', 'Y Axis Min', value = min(as.numeric(selected_y())))
    })

    output$ui_yrange_max <- renderUI({
    df <- final_split$train
    if (is.null(df)) return(NULL)
    numericInput('y_range_max', 'Y Axis Max', value = max(as.numeric(selected_y())))
    })

    output$scatter_plot_1 <- renderPlot({
      scatter_plot(selected_x(), selected_y(), input$scatter_title, input$scatter_subtitle,
      input$scatter_xlabel, input$scatter_ylabel, input$scatter_colors, input$scatter_fill,
      input$scatter_shape, size = input$scatter_size1)
    })

    output$scatter_plot_2 <- renderPlot({
      scatter_plot(selected_x(), selected_y(), input$scatter_title, input$scatter_subtitle,
      input$scatter_xlabel, input$scatter_ylabel, input$scatter_colors, input$scatter_fill,
      input$scatter_shape, input$x_range_min, input$x_range_max,
      input$y_range_min, input$y_range_max, input$scatter_size1)
    })

    output$scatter_plot_3 <- renderPlot({
      scatter_plot(selected_x(), selected_y(), input$scatter_title,
      input$scatter_subtitle, input$scatter_xlabel, input$scatter_ylabel,
      input$scatter_colors, input$scatter_fill, input$scatter_shape, 
      input$x_range_min, input$x_range_max, input$y_range_min, input$y_range_max,
      input$scatter_size1, fitline = input$fitline_y, col_abline = input$col_fitline,
      lty_abline = input$lty_fitline, lwd_abline = input$lwd_fitline)
    })

    output$scatter_plot_4 <- renderPlot({

        scatter_plot(
            selected_x(), selected_y(), input$scatter_title, input$scatter_subtitle,
            input$scatter_xlabel, input$scatter_ylabel, input$scatter_colors, input$scatter_fill,
            input$scatter_shape, input$x_range_min, input$x_range_max,
            input$y_range_min, input$y_range_max, input$scatter_size1,
            input$scatter_coltitle, input$scatter_colsub, input$scatter_colaxis,
            input$scatter_collabel, input$scatter_fontmain, input$scatter_fontsub,
            input$scatter_fontaxis, input$scatter_fontlab, input$scatter_cexmain,
            input$scatter_cexsub, input$scatter_cexaxis, input$scatter_cexlab,
            fitline = input$fitline_y, col_abline = input$col_fitline,
            lty_abline = input$lty_fitline, lwd_abline = input$lwd_fitline
        )
    })

    output$scatter_plot_5 <- renderPlot({

        scatter_plot(
            selected_x(), selected_y(), input$scatter_title, input$scatter_subtitle,
            input$scatter_xlabel, input$scatter_ylabel, input$scatter_colors, input$scatter_fill,
            input$scatter_shape, input$x_range_min, input$x_range_max,
            input$y_range_min, input$y_range_max, input$scatter_size1,
            input$scatter_coltitle, input$scatter_colsub, input$scatter_colaxis,
            input$scatter_collabel, input$scatter_fontmain, input$scatter_fontsub,
            input$scatter_fontaxis, input$scatter_fontlab, input$scatter_cexmain,
            input$scatter_cexsub, input$scatter_cexaxis, input$scatter_cexlab,
            input$scatter_plottext, input$scatter_text_x_loc, input$scatter_text_y_loc,
            input$scatter_textcolor, input$scatter_textfont, input$scatter_textsize,
            input$scatter_mtextplot, input$scatter_mtext_side, input$scatter_mtext_line,
            input$scatter_mtextadj, input$scatter_mtextcolor, input$scatter_mtextfont,
            input$scatter_mtextsize, input$fitline_y, input$col_fitline, input$lty_fitline,
            input$lwd_fitline
        )
    })

    output$scatter_plot_final <- renderPlot({

        scatter_plot(
            selected_x(), selected_y(), input$scatter_title, input$scatter_subtitle,
            input$scatter_xlabel, input$scatter_ylabel, input$scatter_colors, input$scatter_fill,
            input$scatter_shape, input$x_range_min, input$x_range_max,
            input$y_range_min, input$y_range_max, input$scatter_size1,
            input$scatter_coltitle, input$scatter_colsub, input$scatter_colaxis,
            input$scatter_collabel, input$scatter_fontmain, input$scatter_fontsub,
            input$scatter_fontaxis, input$scatter_fontlab, input$scatter_cexmain,
            input$scatter_cexsub, input$scatter_cexaxis, input$scatter_cexlab,
            input$scatter_plottext, input$scatter_text_x_loc, input$scatter_text_y_loc,
            input$scatter_textcolor, input$scatter_textfont, input$scatter_textsize,
            input$scatter_mtextplot, input$scatter_mtext_side, input$scatter_mtext_line,
            input$scatter_mtextadj, input$scatter_mtextcolor, input$scatter_mtextfont,
            input$scatter_mtextsize, input$fitline_y, input$col_fitline, input$lty_fitline,
            input$lwd_fitline
        )
    })
