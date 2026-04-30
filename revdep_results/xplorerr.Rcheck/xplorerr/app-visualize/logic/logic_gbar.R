source('helper/ggunibar.R')

observeEvent(input$finalok, {

        f_data <- final_split$train[, sapply(final_split$train, is.factor)]
        # validate(need(!dim(f_data)[2] == 0, 'No factor variables in the data.'))
        if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, 'gbar_select_x',
              choices = names(fdata), selected = names(fdata))
        } else if (dim(f_data)[2] == 0) {
          updateSelectInput(session, 'gbar_select_x', choices = '', selected = '')
        } else {
          updateSelectInput(session, 'gbar_select_x', choices = names(f_data))
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
        updateSelectInput(session, 'gbar_select_x',
              choices = names(fdata), selected = names(fdata))
        } else if (dim(f_data)[2] == 0) {
          updateSelectInput(session, 'gbar_select_x', choices = '', selected = '')
        } else {
          updateSelectInput(session, 'gbar_select_x', choices = names(f_data)) 
        }
})


gselectedbar <- reactive({
  req(input$gbar_select_x)
  out <- final_split$train %>%
    select(input$gbar_select_x)
  out
})

ybarmax <- reactive({
  out <- gselectedbar() %>%
  select(1) %>%
  table() %>%
  max

  out
})

output$ui_gbaryrange_min <- renderUI({
  df <- final_split$train
  if (is.null(df)) return(NULL)
  numericInput('gbary_range_min', 'Y Axis Min', value = 0, min = 0)
})

output$ui_gbaryrange_max <- renderUI({
  df <- final_split$train
  if (is.null(df)) return(NULL)
  numericInput('gbary_range_max', 'Y Axis Max', value = ybarmax())
})


output$gbar_plot_1 <- renderPlot({
  ggbar1(data = final_split$train, column = input$gbar_select_x, 
    horizontal = input$gbar_horiz, 
    title = input$gbar_title, sub = input$gbar_subtitle,
    xlab = input$gbar_xlabel, ylab = input$gbar_ylabel,
    bar_col = input$gbar_barcol, bor_col = input$gbar_borcol)
})

output$gbar_plot_2 <- renderPlot({
  ggbar1(data = final_split$train, column = input$gbar_select_x, 
    horizontal = input$gbar_horiz,
    bar_col = input$gbar_barcol, bor_col = input$gbar_borcol,
    title = input$gbar_title, sub = input$gbar_subtitle,
    xlab = input$gbar_xlabel, ylab = input$gbar_ylabel,
    yaxlimit = TRUE, y1 = input$gbary_range_min, y2 = input$gbary_range_max,
    remove_xax = input$gbar_remx, remove_yax = input$gbar_remy)
})

output$gbar_plot_3 <- renderPlot({
  ggbar1(data = final_split$train, column = input$gbar_select_x, 
    horizontal = input$gbar_horiz, title = input$gbar_title, 
    sub = input$gbar_subtitle, xlab = input$gbar_xlabel, 
    bar_col = input$gbar_barcol, bor_col = input$gbar_borcol,
    ylab = input$gbar_ylabel, yaxlimit = TRUE, y1 = input$gbary_range_min, 
    y2 = input$gbary_range_max, remove_xax = input$gbar_remx, 
    remove_yax = input$gbar_remy, add_text = input$gbar_text, 
    xloc = input$gbar_text_x_loc, yloc = input$gbar_text_y_loc, 
    label = input$gbar_plottext, tex_color = input$gbar_textcolor, 
    tex_size = input$gbar_textsize)
})

output$gbar_plot_4 <- renderPlot({
  ggbar1(data = final_split$train, column = input$gbar_select_x, 
    horizontal = input$gbar_horiz, title = input$gbar_title, 
    sub = input$gbar_subtitle, xlab = input$gbar_xlabel, 
    bar_col = input$gbar_barcol, bor_col = input$gbar_borcol,
    ylab = input$gbar_ylabel, yaxlimit = TRUE, y1 = input$gbary_range_min, 
    y2 = input$gbary_range_max, remove_xax = input$gbar_remx, 
    remove_yax = input$gbar_remy, add_text = input$gbar_text, 
    xloc = input$gbar_text_x_loc, yloc = input$gbar_text_y_loc, 
    label = input$gbar_plottext, tex_color = input$gbar_textcolor, 
    tex_size = input$gbar_textsize, title_col = input$gbar_title_col, 
    title_fam = input$gbar_title_fam, title_face = input$gbar_title_font, 
    title_size = input$gbar_title_size, title_hjust = input$gbar_title_hjust, 
    title_vjust = input$gbar_title_vjust, sub_col = input$gbar_sub_col, 
    sub_fam = input$gbar_sub_fam, sub_face = input$gbar_subtitle_font, 
    sub_size = input$gbar_sub_size, sub_hjust = input$gbar_sub_hjust, 
    sub_vjust = input$gbar_sub_vjust, xax_col = input$gbar_xlab_col, 
    xax_fam = input$gbar_xlab_fam, xax_face = input$gbar_xlab_font, 
    xax_size = input$gbar_xlab_size, xax_hjust = input$gbar_xlab_hjust, 
    xax_vjust = input$gbar_xlab_vjust, yax_col = input$gbar_ylab_col, 
    yax_fam = input$gbar_ylab_fam, yax_face = input$gbar_ylab_font, 
    yax_size = input$gbar_ylab_size, yax_hjust = input$gbar_ylab_hjust, 
    yax_vjust = input$gbar_ylab_vjust)
})

output$gbar_plot_5 <- renderPlot({
  ggbar1(data = final_split$train, column = input$gbar_select_x, 
    horizontal = input$gbar_horiz, title = input$gbar_title, 
    sub = input$gbar_subtitle, xlab = input$gbar_xlabel, 
    bar_col = input$gbar_barcol, bor_col = input$gbar_borcol,
    ylab = input$gbar_ylabel, yaxlimit = TRUE, y1 = input$gbary_range_min, 
    y2 = input$gbary_range_max, remove_xax = input$gbar_remx, 
    remove_yax = input$gbar_remy, add_text = input$gbar_text, 
    xloc = input$gbar_text_x_loc, yloc = input$gbar_text_y_loc, 
    label = input$gbar_plottext, tex_color = input$gbar_textcolor, 
    tex_size = input$gbar_textsize, title_col = input$gbar_title_col, 
    title_fam = input$gbar_title_fam, title_face = input$gbar_title_font, 
    title_size = input$gbar_title_size, title_hjust = input$gbar_title_hjust, 
    title_vjust = input$gbar_title_vjust, sub_col = input$gbar_sub_col, 
    sub_fam = input$gbar_sub_fam, sub_face = input$gbar_subtitle_font, 
    sub_size = input$gbar_sub_size, sub_hjust = input$gbar_sub_hjust, 
    sub_vjust = input$gbar_sub_vjust, xax_col = input$gbar_xlab_col, 
    xax_fam = input$gbar_xlab_fam, xax_face = input$gbar_xlab_font, 
    xax_size = input$gbar_xlab_size, xax_hjust = input$gbar_xlab_hjust, 
    xax_vjust = input$gbar_xlab_vjust, yax_col = input$gbar_ylab_col, 
    yax_fam = input$gbar_ylab_fam, yax_face = input$gbar_ylab_font, 
    yax_size = input$gbar_ylab_size, yax_hjust = input$gbar_ylab_hjust, 
    yax_vjust = input$gbar_ylab_vjust, theme = input$gbar_theme)
})

output$gbar_plot_6 <- renderPlot({
  ggbar1(data = final_split$train, column = input$gbar_select_x, 
    horizontal = input$gbar_horiz, title = input$gbar_title, 
    sub = input$gbar_subtitle, xlab = input$gbar_xlabel, 
    bar_col = input$gbar_barcol, bor_col = input$gbar_borcol,
    ylab = input$gbar_ylabel, yaxlimit = TRUE, y1 = input$gbary_range_min, 
    y2 = input$gbary_range_max, remove_xax = input$gbar_remx, 
    remove_yax = input$gbar_remy, add_text = input$gbar_text, 
    xloc = input$gbar_text_x_loc, yloc = input$gbar_text_y_loc, 
    label = input$gbar_plottext, tex_color = input$gbar_textcolor, 
    tex_size = input$gbar_textsize, title_col = input$gbar_title_col, 
    title_fam = input$gbar_title_fam, title_face = input$gbar_title_font, 
    title_size = input$gbar_title_size, title_hjust = input$gbar_title_hjust, 
    title_vjust = input$gbar_title_vjust, sub_col = input$gbar_sub_col, 
    sub_fam = input$gbar_sub_fam, sub_face = input$gbar_subtitle_font, 
    sub_size = input$gbar_sub_size, sub_hjust = input$gbar_sub_hjust, 
    sub_vjust = input$gbar_sub_vjust, xax_col = input$gbar_xlab_col, 
    xax_fam = input$gbar_xlab_fam, xax_face = input$gbar_xlab_font, 
    xax_size = input$gbar_xlab_size, xax_hjust = input$gbar_xlab_hjust, 
    xax_vjust = input$gbar_xlab_vjust, yax_col = input$gbar_ylab_col, 
    yax_fam = input$gbar_ylab_fam, yax_face = input$gbar_ylab_font, 
    yax_size = input$gbar_ylab_size, yax_hjust = input$gbar_ylab_hjust, 
    yax_vjust = input$gbar_ylab_vjust, theme = input$gbar_theme)
})