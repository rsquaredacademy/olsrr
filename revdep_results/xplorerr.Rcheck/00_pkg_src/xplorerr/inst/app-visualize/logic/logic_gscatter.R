source('helper/ggscatter.R')

observeEvent(input$finalok, {
        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'gscatter_select_x',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'gscatter_select_y',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'gscatter_select_x',
              choices = '', selected = '')
             updateSelectInput(session, 'gscatter_select_y',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'gscatter_select_x', choices = names(num_data))
             updateSelectInput(session, 'gscatter_select_y', choices = names(num_data))
        }

        f_data <- final_split$train[, sapply(final_split$train, is.factor)]
        # validate(need(!dim(f_data)[2] == 0, 'No factor variables in the data.'))
        if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "gaes_color",
            choices = names(fdata))
        updateSelectInput(session, inputId = "gaes_shape",
            choices = names(fdata))
        updateSelectInput(session, inputId = "gaes_size",
            choices = names(fdata))
        } else if (dim(f_data)[2] == 0) {
          updateSelectInput(session, 'gaes_color', choices = '', selected = '')
          updateSelectInput(session, 'gaes_shape', choices = '', selected = '')
          updateSelectInput(session, 'gaes_size', choices = '', selected = '')
        } else {
          updateSelectInput(session, 'gaes_color', choices = names(f_data))  
          updateSelectInput(session, 'gaes_shape', choices = names(f_data))  
          updateSelectInput(session, 'gaes_size', choices = names(f_data))  
        }
})

observeEvent(input$submit_part_train_per, {
        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'gscatter_select_x',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'gscatter_select_y',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'gscatter_select_x',
              choices = '', selected = '')
             updateSelectInput(session, 'gscatter_select_y',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'gscatter_select_x', choices = names(num_data))
             updateSelectInput(session, 'gscatter_select_y', choices = names(num_data))
        }

        f_data <- final_split$train[, sapply(final_split$train, is.factor)]
        # validate(need(!dim(f_data)[2] == 0, 'No factor variables in the data.'))
        if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "gaes_color",
            choices = names(fdata))
        updateSelectInput(session, inputId = "gaes_shape",
            choices = names(fdata))
        updateSelectInput(session, inputId = "gaes_size",
            choices = names(fdata))
        } else if (dim(f_data)[2] == 0) {
          updateSelectInput(session, 'gaes_color', choices = '', selected = '')
          updateSelectInput(session, 'gaes_shape', choices = '', selected = '')
          updateSelectInput(session, 'gaes_size', choices = '', selected = '')
        } else {
          updateSelectInput(session, 'gaes_color', choices = names(f_data))  
          updateSelectInput(session, 'gaes_shape', choices = names(f_data))  
          updateSelectInput(session, 'gaes_size', choices = names(f_data))  
        }
})

# selected data
gselectedscat <- reactive({
  req(input$gscatter_select_x)
	out <- final_split$train[, c(input$gscatter_select_x, input$gscatter_select_y,
    input$gaes_color, input$gaes_shape, input$gaes_size)]
})

output$ui_gxrange_min <- renderUI({
  df <- final_split$train
  if (is.null(df)) return(NULL)
  numericInput('gx_range_min', 'X Axis Min', value = min(as.numeric(gselectedscat()[[1]])))
})

output$ui_gxrange_max <- renderUI({
  df <- final_split$train
  if (is.null(df)) return(NULL)
  numericInput('gx_range_max', 'X Axis Max', value = max(as.numeric(gselectedscat()[[1]])))
})

output$ui_gyrange_min <- renderUI({
  df <- final_split$train
  if (is.null(df)) return(NULL)
  numericInput('gy_range_min', 'Y Axis Min', value = min(as.numeric(gselectedscat()[[2]])))
})

output$ui_gyrange_max <- renderUI({
  df <- final_split$train
  if (is.null(df)) return(NULL)
  numericInput('gy_range_max', 'Y Axis Max', value = max(as.numeric(gselectedscat()[[2]])))
})

# gselected_y <- reactive({
#   req(input$scatter_select_y)
#   out <- final_split$train[, input$scatter_select_y]
# })

output$gscatter_plot_1 <- renderPlot({
  gscatter(data = gselectedscat(), x = input$gscatter_select_x, 
  	y = input$gscatter_select_y, title = input$gscatter_title, 
  	sub = input$gscatter_subtitle,
    xlab = input$gscatter_xlabel, ylab = input$gscatter_ylabel)
})

output$gscatter_plot_2 <- renderPlot({

	if (input$geas == 'Use Variables') {
		gscatter(data = gselectedscat(), x = input$gscatter_select_x, 
  	y = input$gscatter_select_y, aes_var = TRUE, 
  	title = input$gscatter_title, sub = input$gscatter_subtitle,
    xlab = input$gscatter_xlabel, ylab = input$gscatter_ylabel,
    color = input$gaes_color, shape = input$gaes_shape, 
    size = input$gaes_size)
	} else {
		gscatter(data = gselectedscat(), x = input$gscatter_select_x, 
  	y = input$gscatter_select_y, aes_var = FALSE, 
  	title = input$gscatter_title, sub = input$gscatter_subtitle,
    xlab = input$gscatter_xlabel, ylab = input$gscatter_ylabel,
    color = input$gscat_color, shape = input$gscat_shape, 
    size = input$gscat_size, fill = input$gscat_fill)
	}

  
})

output$gscatter_plot_3 <- renderPlot({

  if (input$geas == 'Use Variables') {
    gscatter(data = gselectedscat(), x = input$gscatter_select_x, 
    y = input$gscatter_select_y, aes_var = TRUE, 
    title = input$gscatter_title, sub = input$gscatter_subtitle,
    xlab = input$gscatter_xlabel, ylab = input$gscatter_ylabel,
    color = input$gaes_color, shape = input$gaes_shape, 
    size = input$gaes_size, xaxlimit = TRUE, yaxlimit = TRUE, 
    x1 = input$gx_range_min, x2 = input$gx_range_max, 
    y1 = input$gy_range_min, y2 = input$gy_range_max)
  } else {
    gscatter(data = gselectedscat(), x = input$gscatter_select_x, 
    y = input$gscatter_select_y, aes_var = FALSE, 
    title = input$gscatter_title, sub = input$gscatter_subtitle,
    xlab = input$gscatter_xlabel, ylab = input$gscatter_ylabel,
    color = input$gscat_color, shape = input$gscat_shape, 
    size = input$gscat_size, fill = input$gscat_fill,
    xaxlimit = TRUE, yaxlimit = TRUE, 
    x1 = input$gx_range_min, x2 = input$gx_range_max, 
    y1 = input$gy_range_min, y2 = input$gy_range_max)
  }

  
})

output$gscatter_plot_4 <- renderPlot({

  if (input$geas == 'Use Variables') {
    gscatter(data = gselectedscat(), x = input$gscatter_select_x, 
    y = input$gscatter_select_y, aes_var = TRUE, 
    title = input$gscatter_title, sub = input$gscatter_subtitle,
    xlab = input$gscatter_xlabel, ylab = input$gscatter_ylabel,
    color = input$gaes_color, shape = input$gaes_shape, 
    size = input$gaes_size, xaxlimit = TRUE, yaxlimit = TRUE, 
    x1 = input$gx_range_min, x2 = input$gx_range_max, 
    y1 = input$gy_range_min, y2 = input$gy_range_max,
    reg_line = input$gscat_line, reg_method = as.character(input$greg_type), 
    reg_se = input$greg_se)
  } else {
    gscatter(data = gselectedscat(), x = input$gscatter_select_x, 
    y = input$gscatter_select_y, aes_var = FALSE, 
    title = input$gscatter_title, sub = input$gscatter_subtitle,
    xlab = input$gscatter_xlabel, ylab = input$gscatter_ylabel,
    color = input$gscat_color, shape = input$gscat_shape, 
    size = input$gscat_size, fill = input$gscat_fill,
    xaxlimit = TRUE, yaxlimit = TRUE, 
    x1 = input$gx_range_min, x2 = input$gx_range_max, 
    y1 = input$gy_range_min, y2 = input$gy_range_max,
    reg_line = input$gscat_line, reg_method = as.character(input$greg_type), 
    reg_se = input$greg_se)
  }

  
})

output$gscatter_plot_5 <- renderPlot({

  if (input$geas == 'Use Variables') {
    gscatter(data = gselectedscat(), x = input$gscatter_select_x, 
    y = input$gscatter_select_y, aes_var = TRUE, 
    title = input$gscatter_title, sub = input$gscatter_subtitle,
    xlab = input$gscatter_xlabel, ylab = input$gscatter_ylabel,
    color = input$gaes_color, shape = input$gaes_shape, 
    size = input$gaes_size, xaxlimit = TRUE, yaxlimit = TRUE, 
    x1 = input$gx_range_min, x2 = input$gx_range_max, 
    y1 = input$gy_range_min, y2 = input$gy_range_max,
    reg_line = input$gscat_line, reg_method = as.character(input$greg_type), 
    reg_se = input$greg_se, add_text = input$gscat_text, 
    xloc = input$gscatter_text_x_loc, yloc = input$gscatter_text_y_loc, 
    label = input$gscatter_plottext, tex_color = input$gscatter_textcolor, 
    tex_size = input$gscatter_textsize)
  } else {
    gscatter(data = gselectedscat(), x = input$gscatter_select_x, 
    y = input$gscatter_select_y, aes_var = FALSE, 
    title = input$gscatter_title, sub = input$gscatter_subtitle,
    xlab = input$gscatter_xlabel, ylab = input$gscatter_ylabel,
    color = input$gscat_color, shape = input$gscat_shape, 
    size = input$gscat_size, fill = input$gscat_fill,
    xaxlimit = TRUE, yaxlimit = TRUE, 
    x1 = input$gx_range_min, x2 = input$gx_range_max, 
    y1 = input$gy_range_min, y2 = input$gy_range_max,
    reg_line = input$gscat_line, reg_method = as.character(input$greg_type), 
    reg_se = input$greg_se, add_text = input$gscat_text, 
    xloc = input$gscatter_text_x_loc, yloc = input$gscatter_text_y_loc, 
    label = input$gscatter_plottext, tex_color = input$gscatter_textcolor, 
    tex_size = input$gscatter_textsize)
  }

  
})

output$gscatter_plot_6 <- renderPlot({

  if (input$geas == 'Use Variables') {
    gscatter(data = gselectedscat(), x = input$gscatter_select_x, 
    y = input$gscatter_select_y, aes_var = TRUE, 
    title = input$gscatter_title, sub = input$gscatter_subtitle,
    xlab = input$gscatter_xlabel, ylab = input$gscatter_ylabel,
    color = input$gaes_color, shape = input$gaes_shape, 
    size = input$gaes_size, xaxlimit = TRUE, yaxlimit = TRUE, 
    x1 = input$gx_range_min, x2 = input$gx_range_max, 
    y1 = input$gy_range_min, y2 = input$gy_range_max,
    reg_line = input$gscat_line, reg_method = as.character(input$greg_type), 
    reg_se = input$greg_se, add_text = input$gscat_text, 
    xloc = input$gscatter_text_x_loc, yloc = input$gscatter_text_y_loc, 
    label = input$gscatter_plottext, tex_color = input$gscatter_textcolor, 
    tex_size = input$gscatter_textsize, title_col = input$gscat_title_col, 
    title_fam = input$gscat_title_fam, title_face = input$gscat_title_font, 
    title_size = input$gscat_title_size, title_hjust = input$gscat_title_hjust, title_vjust = input$gscat_title_vjust,
    sub_col = input$gscat_sub_col, sub_fam = input$gscat_sub_fam, sub_face = input$gscat_subtitle_font, 
    sub_size = input$gscat_sub_size, sub_hjust = input$gscat_sub_hjust, sub_vjust = input$gscat_sub_vjust,
    xax_col = input$gscat_xlab_col, xax_fam = input$gscat_xlab_fam, 
    xax_face = input$gscat_xlab_font, xax_size = input$gscat_xlab_size, 
    xax_hjust = input$gscat_xlab_hjust, xax_vjust = input$gscat_xlab_vjust,
    yax_col = input$gscat_ylab_col, yax_fam = input$gscat_ylab_fam, 
    yax_face = input$gscat_ylab_font, yax_size = input$gscat_ylab_size, 
    yax_hjust = input$gscat_ylab_hjust, yax_vjust = input$gscat_ylab_vjust)
  } else {
    gscatter(data = gselectedscat(), x = input$gscatter_select_x, 
    y = input$gscatter_select_y, aes_var = FALSE, 
    title = input$gscatter_title, sub = input$gscatter_subtitle,
    xlab = input$gscatter_xlabel, ylab = input$gscatter_ylabel,
    color = input$gscat_color, shape = input$gscat_shape, 
    size = input$gscat_size, fill = input$gscat_fill,
    xaxlimit = TRUE, yaxlimit = TRUE, 
    x1 = input$gx_range_min, x2 = input$gx_range_max, 
    y1 = input$gy_range_min, y2 = input$gy_range_max,
    reg_line = input$gscat_line, reg_method = as.character(input$greg_type), 
    reg_se = input$greg_se, add_text = input$gscat_text, 
    xloc = input$gscatter_text_x_loc, yloc = input$gscatter_text_y_loc, 
    label = input$gscatter_plottext, tex_color = input$gscatter_textcolor, 
    tex_size = input$gscatter_textsize, title_col = input$gscat_title_col, 
    title_fam = input$gscat_title_fam, title_face = input$gscat_title_font, 
    title_size = input$gscat_title_size, title_hjust = input$gscat_title_hjust, title_vjust = input$gscat_title_vjust,
    sub_col = input$gscat_sub_col, sub_fam = input$gscat_sub_fam, sub_face = input$gscat_subtitle_font, 
    sub_size = input$gscat_sub_size, sub_hjust = input$gscat_sub_hjust, sub_vjust = input$gscat_sub_vjust,
    xax_col = input$gscat_xlab_col, xax_fam = input$gscat_xlab_fam, 
    xax_face = input$gscat_xlab_font, xax_size = input$gscat_xlab_size, 
    xax_hjust = input$gscat_xlab_hjust, xax_vjust = input$gscat_xlab_vjust,
    yax_col = input$gscat_ylab_col, yax_fam = input$gscat_ylab_fam, 
    yax_face = input$gscat_ylab_font, yax_size = input$gscat_ylab_size, 
    yax_hjust = input$gscat_ylab_hjust, yax_vjust = input$gscat_ylab_vjust)
  }

  
})

output$gscatter_plot_7 <- renderPlot({

  if (input$geas == 'Use Variables') {
    gscatter(data = gselectedscat(), x = input$gscatter_select_x, 
    y = input$gscatter_select_y, aes_var = TRUE, 
    title = input$gscatter_title, sub = input$gscatter_subtitle,
    xlab = input$gscatter_xlabel, ylab = input$gscatter_ylabel,
    color = input$gaes_color, shape = input$gaes_shape, 
    size = input$gaes_size, xaxlimit = TRUE, yaxlimit = TRUE, 
    x1 = input$gx_range_min, x2 = input$gx_range_max, 
    y1 = input$gy_range_min, y2 = input$gy_range_max,
    reg_line = input$gscat_line, reg_method = as.character(input$greg_type), 
    reg_se = input$greg_se, add_text = input$gscat_text, 
    xloc = input$gscatter_text_x_loc, yloc = input$gscatter_text_y_loc, 
    label = input$gscatter_plottext, tex_color = input$gscatter_textcolor, 
    tex_size = input$gscatter_textsize, title_col = input$gscat_title_col, 
    title_fam = input$gscat_title_fam, title_face = input$gscat_title_font, 
    title_size = input$gscat_title_size, title_hjust = input$gscat_title_hjust, title_vjust = input$gscat_title_vjust,
    sub_col = input$gscat_sub_col, sub_fam = input$gscat_sub_fam, sub_face = input$gscat_subtitle_font, 
    sub_size = input$gscat_sub_size, sub_hjust = input$gscat_sub_hjust, sub_vjust = input$gscat_sub_vjust,
    xax_col = input$gscat_xlab_col, xax_fam = input$gscat_xlab_fam, 
    xax_face = input$gscat_xlab_font, xax_size = input$gscat_xlab_size, 
    xax_hjust = input$gscat_xlab_hjust, xax_vjust = input$gscat_xlab_vjust,
    yax_col = input$gscat_ylab_col, yax_fam = input$gscat_ylab_fam, 
    yax_face = input$gscat_ylab_font, yax_size = input$gscat_ylab_size, 
    yax_hjust = input$gscat_ylab_hjust, yax_vjust = input$gscat_ylab_vjust, theme = input$gscatter_theme)
  } else {
    gscatter(data = gselectedscat(), x = input$gscatter_select_x, 
    y = input$gscatter_select_y, aes_var = FALSE, 
    title = input$gscatter_title, sub = input$gscatter_subtitle,
    xlab = input$gscatter_xlabel, ylab = input$gscatter_ylabel,
    color = input$gscat_color, shape = input$gscat_shape, 
    size = input$gscat_size, fill = input$gscat_fill,
    xaxlimit = TRUE, yaxlimit = TRUE, 
    x1 = input$gx_range_min, x2 = input$gx_range_max, 
    y1 = input$gy_range_min, y2 = input$gy_range_max,
    reg_line = input$gscat_line, reg_method = as.character(input$greg_type), 
    reg_se = input$greg_se, add_text = input$gscat_text, 
    xloc = input$gscatter_text_x_loc, yloc = input$gscatter_text_y_loc, 
    label = input$gscatter_plottext, tex_color = input$gscatter_textcolor, 
    tex_size = input$gscatter_textsize, title_col = input$gscat_title_col, 
    title_fam = input$gscat_title_fam, title_face = input$gscat_title_font, 
    title_size = input$gscat_title_size, title_hjust = input$gscat_title_hjust, title_vjust = input$gscat_title_vjust,
    sub_col = input$gscat_sub_col, sub_fam = input$gscat_sub_fam, sub_face = input$gscat_subtitle_font, 
    sub_size = input$gscat_sub_size, sub_hjust = input$gscat_sub_hjust, sub_vjust = input$gscat_sub_vjust,
    xax_col = input$gscat_xlab_col, xax_fam = input$gscat_xlab_fam, 
    xax_face = input$gscat_xlab_font, xax_size = input$gscat_xlab_size, 
    xax_hjust = input$gscat_xlab_hjust, xax_vjust = input$gscat_xlab_vjust,
    yax_col = input$gscat_ylab_col, yax_fam = input$gscat_ylab_fam, 
    yax_face = input$gscat_ylab_font, yax_size = input$gscat_ylab_size, 
    yax_hjust = input$gscat_ylab_hjust, yax_vjust = input$gscat_ylab_vjust, theme = input$gscatter_theme)
  }

  
})

output$gscatter_plot_8 <- renderPlot({

  if (input$geas == 'Use Variables') {
    gscatter(data = gselectedscat(), x = input$gscatter_select_x, 
    y = input$gscatter_select_y, aes_var = TRUE, 
    title = input$gscatter_title, sub = input$gscatter_subtitle,
    xlab = input$gscatter_xlabel, ylab = input$gscatter_ylabel,
    color = input$gaes_color, shape = input$gaes_shape, 
    size = input$gaes_size, xaxlimit = TRUE, yaxlimit = TRUE, 
    x1 = input$gx_range_min, x2 = input$gx_range_max, 
    y1 = input$gy_range_min, y2 = input$gy_range_max,
    reg_line = input$gscat_line, reg_method = as.character(input$greg_type), 
    reg_se = input$greg_se, add_text = input$gscat_text, 
    xloc = input$gscatter_text_x_loc, yloc = input$gscatter_text_y_loc, 
    label = input$gscatter_plottext, tex_color = input$gscatter_textcolor, 
    tex_size = input$gscatter_textsize, title_col = input$gscat_title_col, 
    title_fam = input$gscat_title_fam, title_face = input$gscat_title_font, 
    title_size = input$gscat_title_size, title_hjust = input$gscat_title_hjust, title_vjust = input$gscat_title_vjust,
    sub_col = input$gscat_sub_col, sub_fam = input$gscat_sub_fam, sub_face = input$gscat_subtitle_font, 
    sub_size = input$gscat_sub_size, sub_hjust = input$gscat_sub_hjust, sub_vjust = input$gscat_sub_vjust,
    xax_col = input$gscat_xlab_col, xax_fam = input$gscat_xlab_fam, 
    xax_face = input$gscat_xlab_font, xax_size = input$gscat_xlab_size, 
    xax_hjust = input$gscat_xlab_hjust, xax_vjust = input$gscat_xlab_vjust,
    yax_col = input$gscat_ylab_col, yax_fam = input$gscat_ylab_fam, 
    yax_face = input$gscat_ylab_font, yax_size = input$gscat_ylab_size, 
    yax_hjust = input$gscat_ylab_hjust, yax_vjust = input$gscat_ylab_vjust, theme = input$gscatter_theme)
  } else {
    gscatter(data = gselectedscat(), x = input$gscatter_select_x, 
    y = input$gscatter_select_y, aes_var = FALSE, 
    title = input$gscatter_title, sub = input$gscatter_subtitle,
    xlab = input$gscatter_xlabel, ylab = input$gscatter_ylabel,
    color = input$gscat_color, shape = input$gscat_shape, 
    size = input$gscat_size, fill = input$gscat_fill,
    xaxlimit = TRUE, yaxlimit = TRUE, 
    x1 = input$gx_range_min, x2 = input$gx_range_max, 
    y1 = input$gy_range_min, y2 = input$gy_range_max,
    reg_line = input$gscat_line, reg_method = as.character(input$greg_type), 
    reg_se = input$greg_se, add_text = input$gscat_text, 
    xloc = input$gscatter_text_x_loc, yloc = input$gscatter_text_y_loc, 
    label = input$gscatter_plottext, tex_color = input$gscatter_textcolor, 
    tex_size = input$gscatter_textsize, title_col = input$gscat_title_col, 
    title_fam = input$gscat_title_fam, title_face = input$gscat_title_font, 
    title_size = input$gscat_title_size, title_hjust = input$gscat_title_hjust, title_vjust = input$gscat_title_vjust,
    sub_col = input$gscat_sub_col, sub_fam = input$gscat_sub_fam, sub_face = input$gscat_subtitle_font, 
    sub_size = input$gscat_sub_size, sub_hjust = input$gscat_sub_hjust, sub_vjust = input$gscat_sub_vjust,
    xax_col = input$gscat_xlab_col, xax_fam = input$gscat_xlab_fam, 
    xax_face = input$gscat_xlab_font, xax_size = input$gscat_xlab_size, 
    xax_hjust = input$gscat_xlab_hjust, xax_vjust = input$gscat_xlab_vjust,
    yax_col = input$gscat_ylab_col, yax_fam = input$gscat_ylab_fam, 
    yax_face = input$gscat_ylab_font, yax_size = input$gscat_ylab_size, 
    yax_hjust = input$gscat_ylab_hjust, yax_vjust = input$gscat_ylab_vjust, theme = input$gscatter_theme)
  }

  
})