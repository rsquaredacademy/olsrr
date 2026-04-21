source('helper/ggbibar.R')

observeEvent(input$finalok, {

        f_data <- final_split$train[, sapply(final_split$train, is.factor)]
        # validate(need(!dim(f_data)[2] == 0, 'No factor variables in the data.'))
        if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, 'gbar2_select_x',
              choices = names(fdata), selected = names(fdata))
        updateSelectInput(session, 'gbar2_select_y',
              choices = names(fdata), selected = names(fdata))
        } else if (dim(f_data)[2] == 0) {
          updateSelectInput(session, 'gbar2_select_x', choices = '', selected = '')
          updateSelectInput(session, 'gbar2_select_y', choices = '', selected = '')
        } else {
          updateSelectInput(session, 'gbar2_select_x', choices = names(f_data))
          updateSelectInput(session, 'gbar2_select_y', choices = names(f_data))
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
        updateSelectInput(session, 'gbar2_select_x',
              choices = names(fdata), selected = names(fdata))
        updateSelectInput(session, 'gbar2_select_y',
              choices = names(fdata), selected = names(fdata))
        } else if (dim(f_data)[2] == 0) {
          updateSelectInput(session, 'gbar2_select_x', choices = '', selected = '')
          updateSelectInput(session, 'gbar2_select_y', choices = '', selected = '')
        } else {
          updateSelectInput(session, 'gbar2_select_x', choices = names(f_data))
          updateSelectInput(session, 'gbar2_select_y', choices = names(f_data))
        }
})


gselectedbar2 <- reactive({
  req(input$gbar_select_x)
  out <- final_split$train %>%
    select(input$gbar2_select_x, input$gbar2_select_y)
  out
})

ybar2max <- reactive({
  out <- gselectedbar2() %>%
  select(1, 2) %>%
  table() %>%
  max

  out
})

output$ui_gbar2yrange_min <- renderUI({
  df <- final_split$train
  if (is.null(df)) return(NULL)
  numericInput('gbar2y_range_min', 'Y Axis Min', value = 0, min = 0)
})

output$ui_gbar2yrange_max <- renderUI({
  df <- final_split$train
  if (is.null(df)) return(NULL)
  numericInput('gbar2y_range_max', 'Y Axis Max', value = ybar2max())
})


output$gbar2_plot_1 <- renderPlot({
  ggbibar(data = final_split$train, x = input$gbar2_select_x, 
    y = input$gbar2_select_y,
    stacked = input$gbar2_stack, horizontal = input$gbar2_horiz, 
    title = input$gbar2_title, sub = input$gbar2_subtitle,
    xlab = input$gbar2_xlabel, ylab = input$gbar2_ylabel)
})

output$gbar2_plot_2 <- renderPlot({
  ggbibar(data = final_split$train, x = input$gbar2_select_x, 
    y = input$gbar2_select_y,
    stacked = input$gbar2_stack, horizontal = input$gbar2_horiz, 
    title = input$gbar2_title, sub = input$gbar2_subtitle,
    xlab = input$gbar2_xlabel, ylab = input$gbar2_ylabel,
    yaxlimit = TRUE, y1 = input$gbar2y_range_min, y2 = input$gbar2y_range_max,
    remove_xax = input$gbar2_remx, remove_yax = input$gbar2_remy)
})

output$gbar2_plot_3 <- renderPlot({
  ggbibar(data = final_split$train, x = input$gbar2_select_x, 
    y = input$gbar2_select_y,
    stacked = input$gbar2_stack, horizontal = input$gbar2_horiz, 
    title = input$gbar2_title, sub = input$gbar2_subtitle,
    xlab = input$gbar2_xlabel, ylab = input$gbar2_ylabel,
    yaxlimit = TRUE, y1 = input$gbar2y_range_min, 
    y2 = input$gbar2y_range_max, remove_xax = input$gbar2_remx, 
    remove_yax = input$gbar2_remy, add_text = input$gbar2_text, 
    xloc = input$gbar2_text_x_loc, yloc = input$gbar2_text_y_loc, 
    label = input$gbar2_plottext, tex_color = input$gbar2_textcolor, 
    tex_size = input$gbar2_textsize)
})

output$gbar2_plot_4 <- renderPlot({
  ggbibar(data = final_split$train, x = input$gbar2_select_x, 
    y = input$gbar2_select_y,
    stacked = input$gbar2_stack, horizontal = input$gbar2_horiz, 
    title = input$gbar2_title, sub = input$gbar2_subtitle,
    xlab = input$gbar2_xlabel, ylab = input$gbar2_ylabel,
    yaxlimit = TRUE, y1 = input$gbar2y_range_min, 
    y2 = input$gbar2y_range_max, remove_xax = input$gbar2_remx, 
    remove_yax = input$gbar2_remy, add_text = input$gbar2_text, 
    xloc = input$gbar2_text_x_loc, yloc = input$gbar2_text_y_loc, 
    label = input$gbar2_plottext, tex_color = input$gbar2_textcolor, 
    tex_size = input$gbar2_textsize, title_col = input$gbar2_title_col, 
    title_fam = input$gbar2_title_fam, title_face = input$gbar2_title_font, 
    title_size = input$gbar2_title_size, title_hjust = input$gbar2_title_hjust, 
    title_vjust = input$gbar2_title_vjust, sub_col = input$gbar2_sub_col, 
    sub_fam = input$gbar2_sub_fam, sub_face = input$gbar2_subtitle_font, 
    sub_size = input$gbar2_sub_size, sub_hjust = input$gbar2_sub_hjust, 
    sub_vjust = input$gbar2_sub_vjust, xax_col = input$gbar2_xlab_col, 
    xax_fam = input$gbar2_xlab_fam, xax_face = input$gbar2_xlab_font, 
    xax_size = input$gbar2_xlab_size, xax_hjust = input$gbar2_xlab_hjust, 
    xax_vjust = input$gbar2_xlab_vjust, yax_col = input$gbar2_ylab_col, 
    yax_fam = input$gbar2_ylab_fam, yax_face = input$gbar2_ylab_font, 
    yax_size = input$gbar2_ylab_size, yax_hjust = input$gbar2_ylab_hjust, 
    yax_vjust = input$gbar2_ylab_vjust)
})

output$gbar2_plot_5 <- renderPlot({
  ggbibar(data = final_split$train, x = input$gbar2_select_x, 
    y = input$gbar2_select_y,
    stacked = input$gbar2_stack, horizontal = input$gbar2_horiz, 
    title = input$gbar2_title, sub = input$gbar2_subtitle,
    xlab = input$gbar2_xlabel, ylab = input$gbar2_ylabel,
    yaxlimit = TRUE, y1 = input$gbar2y_range_min, 
    y2 = input$gbar2y_range_max, remove_xax = input$gbar2_remx, 
    remove_yax = input$gbar2_remy, add_text = input$gbar2_text, 
    xloc = input$gbar2_text_x_loc, yloc = input$gbar2_text_y_loc, 
    label = input$gbar2_plottext, tex_color = input$gbar2_textcolor, 
    tex_size = input$gbar2_textsize, title_col = input$gbar2_title_col, 
    title_fam = input$gbar2_title_fam, title_face = input$gbar2_title_font, 
    title_size = input$gbar2_title_size, title_hjust = input$gbar2_title_hjust, 
    title_vjust = input$gbar2_title_vjust, sub_col = input$gbar2_sub_col, 
    sub_fam = input$gbar2_sub_fam, sub_face = input$gbar2_subtitle_font, 
    sub_size = input$gbar2_sub_size, sub_hjust = input$gbar2_sub_hjust, 
    sub_vjust = input$gbar2_sub_vjust, xax_col = input$gbar2_xlab_col, 
    xax_fam = input$gbar2_xlab_fam, xax_face = input$gbar2_xlab_font, 
    xax_size = input$gbar2_xlab_size, xax_hjust = input$gbar2_xlab_hjust, 
    xax_vjust = input$gbar2_xlab_vjust, yax_col = input$gbar2_ylab_col, 
    yax_fam = input$gbar2_ylab_fam, yax_face = input$gbar2_ylab_font, 
    yax_size = input$gbar2_ylab_size, yax_hjust = input$gbar2_ylab_hjust, 
    yax_vjust = input$gbar2_ylab_vjust, theme = input$gbar2_theme)
})

output$gbar2_plot_6 <- renderPlot({
  ggbibar(data = final_split$train, x = input$gbar2_select_x, 
    y = input$gbar2_select_y,
    stacked = input$gbar2_stack, horizontal = input$gbar2_horiz, 
    title = input$gbar2_title, sub = input$gbar2_subtitle,
    xlab = input$gbar2_xlabel, ylab = input$gbar2_ylabel,
    yaxlimit = TRUE, y1 = input$gbar2y_range_min, 
    y2 = input$gbar2y_range_max, remove_xax = input$gbar2_remx, 
    remove_yax = input$gbar2_remy, add_text = input$gbar2_text, 
    xloc = input$gbar2_text_x_loc, yloc = input$gbar2_text_y_loc, 
    label = input$gbar2_plottext, tex_color = input$gbar2_textcolor, 
    tex_size = input$gbar2_textsize, title_col = input$gbar2_title_col, 
    title_fam = input$gbar2_title_fam, title_face = input$gbar2_title_font, 
    title_size = input$gbar2_title_size, title_hjust = input$gbar2_title_hjust, 
    title_vjust = input$gbar2_title_vjust, sub_col = input$gbar2_sub_col, 
    sub_fam = input$gbar2_sub_fam, sub_face = input$gbar2_subtitle_font, 
    sub_size = input$gbar2_sub_size, sub_hjust = input$gbar2_sub_hjust, 
    sub_vjust = input$gbar2_sub_vjust, xax_col = input$gbar2_xlab_col, 
    xax_fam = input$gbar2_xlab_fam, xax_face = input$gbar2_xlab_font, 
    xax_size = input$gbar2_xlab_size, xax_hjust = input$gbar2_xlab_hjust, 
    xax_vjust = input$gbar2_xlab_vjust, yax_col = input$gbar2_ylab_col, 
    yax_fam = input$gbar2_ylab_fam, yax_face = input$gbar2_ylab_font, 
    yax_size = input$gbar2_ylab_size, yax_hjust = input$gbar2_ylab_hjust, 
    yax_vjust = input$gbar2_ylab_vjust, theme = input$gbar2_theme)
})
