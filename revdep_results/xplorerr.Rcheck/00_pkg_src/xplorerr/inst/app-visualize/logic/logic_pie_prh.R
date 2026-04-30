source('helper/piely.R')
source('helper/highpie.R')

observeEvent(input$finalok, {

        f_data <- final_split$train[, sapply(final_split$train, is.factor)]
        # validate(need(!dim(f_data)[2] == 0, 'No factor variables in the data.'))
        if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, 'piely_select_x',
              choices = names(fdata), selected = names(fdata))
        updateSelectInput(session, 'hipie_select_x',
              choices = names(fdata), selected = names(fdata))
        } else if (dim(f_data)[2] == 0) {
          updateSelectInput(session, 'piely_select_x', choices = '', selected = '')
          updateSelectInput(session, 'hipie_select_x', choices = '', selected = '')
        } else {
          updateSelectInput(session, 'piely_select_x', choices = names(f_data))
          updateSelectInput(session, 'hipie_select_x', choices = names(f_data))
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
        updateSelectInput(session, 'piely_select_x',
              choices = names(fdata), selected = names(fdata))
        updateSelectInput(session, 'hipie_select_x',
              choices = names(fdata), selected = names(fdata))
        } else if (dim(f_data)[2] == 0) {
          updateSelectInput(session, 'piely_select_x', choices = '', selected = '')
          updateSelectInput(session, 'hipie_select_x', choices = '', selected = '')
        } else {
          updateSelectInput(session, 'piely_select_x', choices = names(f_data))
          updateSelectInput(session, 'hipie_select_x', choices = names(f_data))
        }
})


output$piely_plot_1 <- plotly::renderPlotly({
  piely(data = final_split$train, x = input$piely_select_x, 
    title = input$piely_title, 
    x_title = input$piely_xlabel, y_title = input$piely_ylabel,
    text_pos = input$piely_text_pos, text_info = input$piely_text_info,
    text_direction = input$piely_text_dir, text_rotation = input$piely_text_rotation,  
    pie_pull = input$piely_pull, pie_hole = input$piely_hole, 
    col_opacity = input$piely_opacity, pie_l_col = input$piely_color)
})

output$hipie_plot_1 <- highcharter::renderHighchart({
  highpie(data = final_split$train, column = input$hipie_select_x)
})
