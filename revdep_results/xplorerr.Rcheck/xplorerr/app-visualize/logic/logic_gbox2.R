source('helper/ggbox2.R')

observeEvent(input$finalok, {

        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        f_data <- final_split$train[, sapply(final_split$train, is.factor)]
        if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "gbox2_select_x",
            choices = names(fdata))
        } else if (dim(f_data)[2] == 0) {
          updateSelectInput(session, 'gbox2_select_x', choices = '', selected = '')
        } else {
          updateSelectInput(session, 'gbox2_select_x', choices = names(f_data))
        }
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'gbox2_select_y',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
            updateSelectInput(session, 'gbox2_select_y',
              choices = '', selected = '')
        } else {
            updateSelectInput(session, 'gbox2_select_y', choices = names(num_data))
        }
    })

    observeEvent(input$submit_part_train_per, {

        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        f_data <- final_split$train[, sapply(final_split$train, is.factor)]
        if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "gbox2_select_x",
            choices = names(fdata))
        } else if (dim(f_data)[2] == 0) {
          updateSelectInput(session, 'gbox2_select_x', choices = '', selected = '')
        } else {
          updateSelectInput(session, 'gbox2_select_x', choices = names(f_data))
        }
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'gbox2_select_y',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
            updateSelectInput(session, 'gbox2_select_y',
              choices = '', selected = '')
        } else {
            updateSelectInput(session, 'gbox2_select_y', choices = names(num_data))
        }
    })

gselectedbox2 <- reactive({
  req(input$gbox2_select_x)
  out <- final_split$train %>%
    select(input$gbox2_select_x, input$gbox2_select_y)
  out
})

ybox2max <- reactive({
  out <- gselectedbox2() %>%
  select(2) %>%
  max 

  out
})

output$ui_gbox2yrange_min <- renderUI({
  df <- final_split$train
  if (is.null(df)) return(NULL)
  numericInput('gbox2y_range_min', 'Y Axis Min', value = 0, min = 0)
})

output$ui_gbox2yrange_max <- renderUI({
  df <- final_split$train
  if (is.null(df)) return(NULL)
  numericInput('gbox2y_range_max', 'Y Axis Max', value = ybox2max())
})


output$gbox2_plot_1 <- renderPlot({
  ggbox2(data = final_split$train, x = input$gbox2_select_x,
    y = input$gbox2_select_y,  
    horizontal = input$gbox2_horiz, notch = input$gbox2_notch,
    title = input$gbox2_title, sub = input$gbox2_subtitle,
    xlab = input$gbox2_xlabel, ylab = input$gbox2_ylabel,
    fill = input$gbox2_fill, col = input$gbox2_col)
})

output$gbox2_plot_2 <- renderPlot({
  ggbox2(data = final_split$train, x = input$gbox2_select_x,
    y = input$gbox2_select_y,  
    horizontal = input$gbox2_horiz, notch = input$gbox2_notch,
    title = input$gbox2_title, sub = input$gbox2_subtitle,
    xlab = input$gbox2_xlabel, ylab = input$gbox2_ylabel,
    fill = input$gbox2_fill, col = input$gbox2_col,
    o_col = input$gbox2_ocol, o_fill = input$gbox2_ofill, 
    o_shape = input$gbox2_oshape, o_alpha = input$gbox2_oalpha, 
    o_size = input$gbox2_osize)
})

output$gbox2_plot_3<- renderPlot({
  ggbox2(data = final_split$train, x = input$gbox2_select_x,
    y = input$gbox2_select_y,  
    horizontal = input$gbox2_horiz, notch = input$gbox2_notch,
    title = input$gbox2_title, sub = input$gbox2_subtitle,
    xlab = input$gbox2_xlabel, ylab = input$gbox2_ylabel,
    fill = input$gbox2_fill, col = input$gbox2_col,
    o_col = input$gbox2_ocol, o_fill = input$gbox2_ofill, 
    o_shape = input$gbox2_oshape, o_alpha = input$gbox2_oalpha, 
    o_size = input$gbox2_osize, add_jitter = input$gbox2_jitter,  
    j_width = input$gbox2_jwidth, j_height = input$gbox2_jheight, 
    j_fill = input$gbox2_jfill, j_col = input$gbox2_jcol, 
    j_shape = input$gbox2_jshape, j_size = input$gbox2_jsize, 
    j_alpha = input$gbox2_jalpha)
})

output$gbox2_plot_4<- renderPlot({
  ggbox2(data = final_split$train, x = input$gbox2_select_x,
    y = input$gbox2_select_y,  
    horizontal = input$gbox2_horiz, notch = input$gbox2_notch,
    title = input$gbox2_title, sub = input$gbox2_subtitle,
    xlab = input$gbox2_xlabel, ylab = input$gbox2_ylabel,
    fill = input$gbox2_fill, col = input$gbox2_col,
    o_col = input$gbox2_ocol, o_fill = input$gbox2_ofill, 
    o_shape = input$gbox2_oshape, o_alpha = input$gbox2_oalpha, 
    o_size = input$gbox2_osize, add_jitter = input$gbox2_jitter,  
    j_width = input$gbox2_jwidth, j_height = input$gbox2_jheight, 
    j_fill = input$gbox2_jfill, j_col = input$gbox2_jcol, 
    j_shape = input$gbox2_jshape, j_size = input$gbox2_jsize, 
    j_alpha = input$gbox2_jalpha, yaxlimit = TRUE, 
    y1 = input$gbox2y_range_min, y2 = input$gbox2y_range_max, 
    remove_xax = input$gbox2_remx, remove_yax = input$gbox2_remy)
})

output$gbox2_plot_5<- renderPlot({
  ggbox2(data = final_split$train, x = input$gbox2_select_x,
    y = input$gbox2_select_y,  
    horizontal = input$gbox2_horiz, notch = input$gbox2_notch,
    title = input$gbox2_title, sub = input$gbox2_subtitle,
    xlab = input$gbox2_xlabel, ylab = input$gbox2_ylabel,
    fill = input$gbox2_fill, col = input$gbox2_col,
    o_col = input$gbox2_ocol, o_fill = input$gbox2_ofill, 
    o_shape = input$gbox2_oshape, o_alpha = input$gbox2_oalpha, 
    o_size = input$gbox2_osize, add_jitter = input$gbox2_jitter,  
    j_width = input$gbox2_jwidth, j_height = input$gbox2_jheight, 
    j_fill = input$gbox2_jfill, j_col = input$gbox2_jcol, 
    j_shape = input$gbox2_jshape, j_size = input$gbox2_jsize, 
    j_alpha = input$gbox2_jalpha, yaxlimit = TRUE, 
    y1 = input$gbox2y_range_min, y2 = input$gbox2y_range_max, 
    remove_xax = input$gbox2_remx, remove_yax = input$gbox2_remy,
    add_text = input$gbox2_text, xloc = input$gbox2_text_x_loc, 
    yloc = input$gbox2_text_y_loc, label = input$gbox2_plottext, 
    tex_color = input$gbox2_textcolor, tex_size = input$gbox2_textsize)
})

output$gbox2_plot_6<- renderPlot({
  ggbox2(data = final_split$train, x = input$gbox2_select_x,
    y = input$gbox2_select_y,  
    horizontal = input$gbox2_horiz, notch = input$gbox2_notch,
    title = input$gbox2_title, sub = input$gbox2_subtitle,
    xlab = input$gbox2_xlabel, ylab = input$gbox2_ylabel,
    fill = input$gbox2_fill, col = input$gbox2_col,
    o_col = input$gbox2_ocol, o_fill = input$gbox2_ofill, 
    o_shape = input$gbox2_oshape, o_alpha = input$gbox2_oalpha, 
    o_size = input$gbox2_osize, add_jitter = input$gbox2_jitter,  
    j_width = input$gbox2_jwidth, j_height = input$gbox2_jheight, 
    j_fill = input$gbox2_jfill, j_col = input$gbox2_jcol, 
    j_shape = input$gbox2_jshape, j_size = input$gbox2_jsize, 
    j_alpha = input$gbox2_jalpha, yaxlimit = TRUE, 
    y1 = input$gbox2y_range_min, y2 = input$gbox2y_range_max, 
    remove_xax = input$gbox2_remx, remove_yax = input$gbox2_remy,
    add_text = input$gbox2_text, xloc = input$gbox2_text_x_loc, 
    yloc = input$gbox2_text_y_loc, label = input$gbox2_plottext, 
    tex_color = input$gbox2_textcolor, tex_size = input$gbox2_textsize,
    title_col = input$gbox2_title_col, 
    title_fam = input$gbox2_title_fam, title_face = input$gbox2_title_font, 
    title_size = input$gbox2_title_size, title_hjust = input$gbox2_title_hjust, 
    title_vjust = input$gbox2_title_vjust, sub_col = input$gbox2_sub_col, 
    sub_fam = input$gbox2_sub_fam, sub_face = input$gbox2_subtitle_font, 
    sub_size = input$gbox2_sub_size, sub_hjust = input$gbox2_sub_hjust, 
    sub_vjust = input$gbox2_sub_vjust, xax_col = input$gbox2_xlab_col, 
    xax_fam = input$gbox2_xlab_fam, xax_face = input$gbox2_xlab_font, 
    xax_size = input$gbox2_xlab_size, xax_hjust = input$gbox2_xlab_hjust, 
    xax_vjust = input$gbox2_xlab_vjust, yax_col = input$gbox2_ylab_col, 
    yax_fam = input$gbox2_ylab_fam, yax_face = input$gbox2_ylab_font, 
    yax_size = input$gbox2_ylab_size, yax_hjust = input$gbox2_ylab_hjust, 
    yax_vjust = input$gbox2_ylab_vjust)
})

output$gbox2_plot_7<- renderPlot({
  ggbox2(data = final_split$train, x = input$gbox2_select_x,
    y = input$gbox2_select_y,  
    horizontal = input$gbox2_horiz, notch = input$gbox2_notch,
    title = input$gbox2_title, sub = input$gbox2_subtitle,
    xlab = input$gbox2_xlabel, ylab = input$gbox2_ylabel,
    fill = input$gbox2_fill, col = input$gbox2_col,
    o_col = input$gbox2_ocol, o_fill = input$gbox2_ofill, 
    o_shape = input$gbox2_oshape, o_alpha = input$gbox2_oalpha, 
    o_size = input$gbox2_osize, add_jitter = input$gbox2_jitter,  
    j_width = input$gbox2_jwidth, j_height = input$gbox2_jheight, 
    j_fill = input$gbox2_jfill, j_col = input$gbox2_jcol, 
    j_shape = input$gbox2_jshape, j_size = input$gbox2_jsize, 
    j_alpha = input$gbox2_jalpha, yaxlimit = TRUE, 
    y1 = input$gbox2y_range_min, y2 = input$gbox2y_range_max, 
    remove_xax = input$gbox2_remx, remove_yax = input$gbox2_remy,
    add_text = input$gbox2_text, xloc = input$gbox2_text_x_loc, 
    yloc = input$gbox2_text_y_loc, label = input$gbox2_plottext, 
    tex_color = input$gbox2_textcolor, tex_size = input$gbox2_textsize,
    title_col = input$gbox2_title_col, 
    title_fam = input$gbox2_title_fam, title_face = input$gbox2_title_font, 
    title_size = input$gbox2_title_size, title_hjust = input$gbox2_title_hjust, 
    title_vjust = input$gbox2_title_vjust, sub_col = input$gbox2_sub_col, 
    sub_fam = input$gbox2_sub_fam, sub_face = input$gbox2_subtitle_font, 
    sub_size = input$gbox2_sub_size, sub_hjust = input$gbox2_sub_hjust, 
    sub_vjust = input$gbox2_sub_vjust, xax_col = input$gbox2_xlab_col, 
    xax_fam = input$gbox2_xlab_fam, xax_face = input$gbox2_xlab_font, 
    xax_size = input$gbox2_xlab_size, xax_hjust = input$gbox2_xlab_hjust, 
    xax_vjust = input$gbox2_xlab_vjust, yax_col = input$gbox2_ylab_col, 
    yax_fam = input$gbox2_ylab_fam, yax_face = input$gbox2_ylab_font, 
    yax_size = input$gbox2_ylab_size, yax_hjust = input$gbox2_ylab_hjust, 
    yax_vjust = input$gbox2_ylab_vjust, theme = input$gbox2_theme)
})

output$gbox2_plot_8<- renderPlot({
  ggbox2(data = final_split$train, x = input$gbox2_select_x,
    y = input$gbox2_select_y,  
    horizontal = input$gbox2_horiz, notch = input$gbox2_notch,
    title = input$gbox2_title, sub = input$gbox2_subtitle,
    xlab = input$gbox2_xlabel, ylab = input$gbox2_ylabel,
    fill = input$gbox2_fill, col = input$gbox2_col,
    o_col = input$gbox2_ocol, o_fill = input$gbox2_ofill, 
    o_shape = input$gbox2_oshape, o_alpha = input$gbox2_oalpha, 
    o_size = input$gbox2_osize, add_jitter = input$gbox2_jitter,  
    j_width = input$gbox2_jwidth, j_height = input$gbox2_jheight, 
    j_fill = input$gbox2_jfill, j_col = input$gbox2_jcol, 
    j_shape = input$gbox2_jshape, j_size = input$gbox2_jsize, 
    j_alpha = input$gbox2_jalpha, yaxlimit = TRUE, 
    y1 = input$gbox2y_range_min, y2 = input$gbox2y_range_max, 
    remove_xax = input$gbox2_remx, remove_yax = input$gbox2_remy,
    add_text = input$gbox2_text, xloc = input$gbox2_text_x_loc, 
    yloc = input$gbox2_text_y_loc, label = input$gbox2_plottext, 
    tex_color = input$gbox2_textcolor, tex_size = input$gbox2_textsize,
    title_col = input$gbox2_title_col, 
    title_fam = input$gbox2_title_fam, title_face = input$gbox2_title_font, 
    title_size = input$gbox2_title_size, title_hjust = input$gbox2_title_hjust, 
    title_vjust = input$gbox2_title_vjust, sub_col = input$gbox2_sub_col, 
    sub_fam = input$gbox2_sub_fam, sub_face = input$gbox2_subtitle_font, 
    sub_size = input$gbox2_sub_size, sub_hjust = input$gbox2_sub_hjust, 
    sub_vjust = input$gbox2_sub_vjust, xax_col = input$gbox2_xlab_col, 
    xax_fam = input$gbox2_xlab_fam, xax_face = input$gbox2_xlab_font, 
    xax_size = input$gbox2_xlab_size, xax_hjust = input$gbox2_xlab_hjust, 
    xax_vjust = input$gbox2_xlab_vjust, yax_col = input$gbox2_ylab_col, 
    yax_fam = input$gbox2_ylab_fam, yax_face = input$gbox2_ylab_font, 
    yax_size = input$gbox2_ylab_size, yax_hjust = input$gbox2_ylab_hjust, 
    yax_vjust = input$gbox2_ylab_vjust, theme = input$gbox2_theme)
})



