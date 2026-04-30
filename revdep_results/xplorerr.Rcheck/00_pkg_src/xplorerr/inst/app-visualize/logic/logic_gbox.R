source('helper/ggbox1.R')

observeEvent(input$finalok, {
        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'gbox_select_x',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'gbox_select_x',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'gbox_select_x', choices = names(num_data))
        }

})

observeEvent(input$submit_part_train_per, {
        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'gbox_select_x',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'gbox_select_x',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'gbox_select_x', choices = names(num_data))
        }
})


gselectedbox <- reactive({
  req(input$gbox_select_x)
  out <- final_split$train %>%
    select(input$gbox_select_x)
  out
})

yboxmax <- reactive({
  out <- gselectedbox() %>%
  select(1) %>%
  max 

  out
})

output$ui_gboxyrange_min <- renderUI({
  df <- final_split$train
  if (is.null(df)) return(NULL)
  numericInput('gboxy_range_min', 'Y Axis Min', value = 0, min = 0)
})

output$ui_gboxyrange_max <- renderUI({
  df <- final_split$train
  if (is.null(df)) return(NULL)
  numericInput('gboxy_range_max', 'Y Axis Max', value = yboxmax())
})


output$gbox_plot_1 <- renderPlot({
  ggbox1(data = final_split$train, y = input$gbox_select_x, 
    horizontal = input$gbox_horiz, notch = input$gbox_notch,
    title = input$gbox_title, sub = input$gbox_subtitle,
    xlab = input$gbox_xlabel, ylab = input$gbox_ylabel,
    fill = input$gbox_fill, col = input$gbox_col)
})

output$gbox_plot_2 <- renderPlot({
  ggbox1(data = final_split$train, y = input$gbox_select_x, 
    horizontal = input$gbox_horiz, notch = input$gbox_notch,
    title = input$gbox_title, sub = input$gbox_subtitle,
    xlab = input$gbox_xlabel, ylab = input$gbox_ylabel,
    fill = input$gbox_fill, col = input$gbox_col,
    o_col = input$gbox_ocol, o_fill = input$gbox_ofill, 
    o_shape = input$gbox_oshape, o_alpha = input$gbox_oalpha, 
    o_size = input$gbox_osize
  )
})

output$gbox_plot_3 <- renderPlot({
  ggbox1(data = final_split$train, y = input$gbox_select_x, 
    horizontal = input$gbox_horiz, notch = input$gbox_notch,
    title = input$gbox_title, sub = input$gbox_subtitle,
    xlab = input$gbox_xlabel, ylab = input$gbox_ylabel,
    fill = input$gbox_fill, col = input$gbox_col,
    o_col = input$gbox_ocol, o_fill = input$gbox_ofill, 
    o_shape = input$gbox_oshape, o_alpha = input$gbox_oalpha, 
    o_size = input$gbox_osize, add_jitter = input$gbox_jitter,  
    j_width = input$gbox_jwidth, j_height = input$gbox_jheight, 
    j_fill = input$gbox_jfill, j_col = input$gbox_jcol, 
    j_shape = input$gbox_jshape, j_size = input$gbox_jsize, 
    j_alpha = input$gbox_jalpha
  )
})

output$gbox_plot_4 <- renderPlot({
  ggbox1(data = final_split$train, y = input$gbox_select_x, 
    horizontal = input$gbox_horiz, notch = input$gbox_notch,
    title = input$gbox_title, sub = input$gbox_subtitle,
    xlab = input$gbox_xlabel, ylab = input$gbox_ylabel,
    fill = input$gbox_fill, col = input$gbox_col,
    o_col = input$gbox_ocol, o_fill = input$gbox_ofill, 
    o_shape = input$gbox_oshape, o_alpha = input$gbox_oalpha, 
    o_size = input$gbox_osize, add_jitter = input$gbox_jitter,  
    j_width = input$gbox_jwidth, j_height = input$gbox_jheight, 
    j_fill = input$gbox_jfill, j_col = input$gbox_jcol, 
    j_shape = input$gbox_jshape, j_size = input$gbox_jsize, 
    j_alpha = input$gbox_jalpha, yaxlimit = TRUE, 
    y1 = input$gboxy_range_min, y2 = input$gboxy_range_max, 
    remove_xax = input$gbox_remx, remove_yax = input$gbox_remy
  )
})

output$gbox_plot_5 <- renderPlot({
  ggbox1(data = final_split$train, y = input$gbox_select_x, 
    horizontal = input$gbox_horiz, notch = input$gbox_notch,
    title = input$gbox_title, sub = input$gbox_subtitle,
    xlab = input$gbox_xlabel, ylab = input$gbox_ylabel,
    fill = input$gbox_fill, col = input$gbox_col,
    o_col = input$gbox_ocol, o_fill = input$gbox_ofill, 
    o_shape = input$gbox_oshape, o_alpha = input$gbox_oalpha, 
    o_size = input$gbox_osize, add_jitter = input$gbox_jitter,  
    j_width = input$gbox_jwidth, j_height = input$gbox_jheight, 
    j_fill = input$gbox_jfill, j_col = input$gbox_jcol, 
    j_shape = input$gbox_jshape, j_size = input$gbox_jsize, 
    j_alpha = input$gbox_jalpha, yaxlimit = TRUE, 
    y1 = input$gboxy_range_min, y2 = input$gboxy_range_max, 
    remove_xax = input$gbox_remx, remove_yax = input$gbox_remy,
    add_text = input$gbox_text, xloc = input$gbox_text_x_loc, 
    yloc = input$gbox_text_y_loc, label = input$gbox_plottext, 
    tex_color = input$gbox_textcolor, tex_size = input$gbox_textsize
  )
})

output$gbox_plot_6 <- renderPlot({
  ggbox1(data = final_split$train, y = input$gbox_select_x, 
    horizontal = input$gbox_horiz, notch = input$gbox_notch,
    title = input$gbox_title, sub = input$gbox_subtitle,
    xlab = input$gbox_xlabel, ylab = input$gbox_ylabel,
    fill = input$gbox_fill, col = input$gbox_col,
    o_col = input$gbox_ocol, o_fill = input$gbox_ofill, 
    o_shape = input$gbox_oshape, o_alpha = input$gbox_oalpha, 
    o_size = input$gbox_osize, add_jitter = input$gbox_jitter,  
    j_width = input$gbox_jwidth, j_height = input$gbox_jheight, 
    j_fill = input$gbox_jfill, j_col = input$gbox_jcol, 
    j_shape = input$gbox_jshape, j_size = input$gbox_jsize, 
    j_alpha = input$gbox_jalpha, yaxlimit = TRUE, 
    y1 = input$gboxy_range_min, y2 = input$gboxy_range_max, 
    remove_xax = input$gbox_remx, remove_yax = input$gbox_remy,
    add_text = input$gbox_text, xloc = input$gbox_text_x_loc, 
    yloc = input$gbox_text_y_loc, label = input$gbox_plottext, 
    tex_color = input$gbox_textcolor, tex_size = input$gbox_textsize,
    title_col = input$gbox_title_col, 
    title_fam = input$gbox_title_fam, title_face = input$gbox_title_font, 
    title_size = input$gbox_title_size, title_hjust = input$gbox_title_hjust, 
    title_vjust = input$gbox_title_vjust, sub_col = input$gbox_sub_col, 
    sub_fam = input$gbox_sub_fam, sub_face = input$gbox_subtitle_font, 
    sub_size = input$gbox_sub_size, sub_hjust = input$gbox_sub_hjust, 
    sub_vjust = input$gbox_sub_vjust, xax_col = input$gbox_xlab_col, 
    xax_fam = input$gbox_xlab_fam, xax_face = input$gbox_xlab_font, 
    xax_size = input$gbox_xlab_size, xax_hjust = input$gbox_xlab_hjust, 
    xax_vjust = input$gbox_xlab_vjust, yax_col = input$gbox_ylab_col, 
    yax_fam = input$gbox_ylab_fam, yax_face = input$gbox_ylab_font, 
    yax_size = input$gbox_ylab_size, yax_hjust = input$gbox_ylab_hjust, 
    yax_vjust = input$gbox_ylab_vjust
  )
})

output$gbox_plot_7 <- renderPlot({
  ggbox1(data = final_split$train, y = input$gbox_select_x, 
    horizontal = input$gbox_horiz, notch = input$gbox_notch,
    title = input$gbox_title, sub = input$gbox_subtitle,
    xlab = input$gbox_xlabel, ylab = input$gbox_ylabel,
    fill = input$gbox_fill, col = input$gbox_col,
    o_col = input$gbox_ocol, o_fill = input$gbox_ofill, 
    o_shape = input$gbox_oshape, o_alpha = input$gbox_oalpha, 
    o_size = input$gbox_osize, add_jitter = input$gbox_jitter,  
    j_width = input$gbox_jwidth, j_height = input$gbox_jheight, 
    j_fill = input$gbox_jfill, j_col = input$gbox_jcol, 
    j_shape = input$gbox_jshape, j_size = input$gbox_jsize, 
    j_alpha = input$gbox_jalpha, yaxlimit = TRUE, 
    y1 = input$gboxy_range_min, y2 = input$gboxy_range_max, 
    remove_xax = input$gbox_remx, remove_yax = input$gbox_remy,
    add_text = input$gbox_text, xloc = input$gbox_text_x_loc, 
    yloc = input$gbox_text_y_loc, label = input$gbox_plottext, 
    tex_color = input$gbox_textcolor, tex_size = input$gbox_textsize,
    title_col = input$gbox_title_col, 
    title_fam = input$gbox_title_fam, title_face = input$gbox_title_font, 
    title_size = input$gbox_title_size, title_hjust = input$gbox_title_hjust, 
    title_vjust = input$gbox_title_vjust, sub_col = input$gbox_sub_col, 
    sub_fam = input$gbox_sub_fam, sub_face = input$gbox_subtitle_font, 
    sub_size = input$gbox_sub_size, sub_hjust = input$gbox_sub_hjust, 
    sub_vjust = input$gbox_sub_vjust, xax_col = input$gbox_xlab_col, 
    xax_fam = input$gbox_xlab_fam, xax_face = input$gbox_xlab_font, 
    xax_size = input$gbox_xlab_size, xax_hjust = input$gbox_xlab_hjust, 
    xax_vjust = input$gbox_xlab_vjust, yax_col = input$gbox_ylab_col, 
    yax_fam = input$gbox_ylab_fam, yax_face = input$gbox_ylab_font, 
    yax_size = input$gbox_ylab_size, yax_hjust = input$gbox_ylab_hjust, 
    yax_vjust = input$gbox_ylab_vjust, theme = input$gbox_theme
  )
})

output$gbox_plot_8 <- renderPlot({
  ggbox1(data = final_split$train, y = input$gbox_select_x, 
    horizontal = input$gbox_horiz, notch = input$gbox_notch,
    title = input$gbox_title, sub = input$gbox_subtitle,
    xlab = input$gbox_xlabel, ylab = input$gbox_ylabel,
    fill = input$gbox_fill, col = input$gbox_col,
    o_col = input$gbox_ocol, o_fill = input$gbox_ofill, 
    o_shape = input$gbox_oshape, o_alpha = input$gbox_oalpha, 
    o_size = input$gbox_osize, add_jitter = input$gbox_jitter,  
    j_width = input$gbox_jwidth, j_height = input$gbox_jheight, 
    j_fill = input$gbox_jfill, j_col = input$gbox_jcol, 
    j_shape = input$gbox_jshape, j_size = input$gbox_jsize, 
    j_alpha = input$gbox_jalpha, yaxlimit = TRUE, 
    y1 = input$gboxy_range_min, y2 = input$gboxy_range_max, 
    remove_xax = input$gbox_remx, remove_yax = input$gbox_remy,
    add_text = input$gbox_text, xloc = input$gbox_text_x_loc, 
    yloc = input$gbox_text_y_loc, label = input$gbox_plottext, 
    tex_color = input$gbox_textcolor, tex_size = input$gbox_textsize,
    title_col = input$gbox_title_col, 
    title_fam = input$gbox_title_fam, title_face = input$gbox_title_font, 
    title_size = input$gbox_title_size, title_hjust = input$gbox_title_hjust, 
    title_vjust = input$gbox_title_vjust, sub_col = input$gbox_sub_col, 
    sub_fam = input$gbox_sub_fam, sub_face = input$gbox_subtitle_font, 
    sub_size = input$gbox_sub_size, sub_hjust = input$gbox_sub_hjust, 
    sub_vjust = input$gbox_sub_vjust, xax_col = input$gbox_xlab_col, 
    xax_fam = input$gbox_xlab_fam, xax_face = input$gbox_xlab_font, 
    xax_size = input$gbox_xlab_size, xax_hjust = input$gbox_xlab_hjust, 
    xax_vjust = input$gbox_xlab_vjust, yax_col = input$gbox_ylab_col, 
    yax_fam = input$gbox_ylab_fam, yax_face = input$gbox_ylab_font, 
    yax_size = input$gbox_ylab_size, yax_hjust = input$gbox_ylab_hjust, 
    yax_vjust = input$gbox_ylab_vjust, theme = input$gbox_theme
  )
})