source('helper/ggline.R')

# out <- eventReactive(input$button_split_no, {
#   num_data <- cbind.data.frame(final_split$train[, sapply(final_split$train, is.numeric)],
#                     final_split$train[, sapply(final_split$train, is.Date)])
  
#   k <- final_split$train %>%
#     map(is.numeric) %>%
#     unlist()

#   t <- final_split$train %>%
#     map(is.Date) %>%
#     unlist()

#   j1 <- names(which(k == TRUE))
#   j2 <- names(which(t == TRUE))

#   if (length(j1) == 0) {
#     j <- j2
#   } else if (length(j2) == 0) {
#     j <- j1
#   } else {
#     j <- c(j1, j2)
#   }
#   colnames(num_data) <- j
#   num_data
# })

# output$gline_data <- renderPrint({
#   out()
# })

observeEvent(input$finalok, {

  num_data <- cbind.data.frame(final_split$train[, sapply(final_split$train, is.numeric)],
                      final_split$train[, sapply(final_split$train, is.Date)])
    
    k <- final_split$train %>%
      map(is.numeric) %>%
      unlist()

    t <- final_split$train %>%
      map(is.Date) %>%
      unlist()

    j1 <- names(which(k == TRUE))
    j2 <- names(which(t == TRUE))

    if (length(j1) == 0) {
      j <- j2
    } else if (length(j2) == 0) {
      j <- j1
    } else {
      j <- c(j1, j2)
    }
    colnames(num_data) <- j
            
            
        if (is.null(dim(num_data))) {
            numdata <- tibble::as_data_frame(num_data)
            updateSelectInput(session, 'gline_select_x',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'gline_y',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'gline_select_x',
              choices = '', selected = '')
             updateSelectInput(session, 'gline_y',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'gline_select_x', choices = names(num_data))
             updateSelectInput(session, 'gline_y', choices = names(num_data))
        }


})

observeEvent(input$submit_part_train_per, {
  num_data <- cbind.data.frame(final_split$train[, sapply(final_split$train, is.numeric)],
                      final_split$train[, sapply(final_split$train, is.Date)])
    
    k <- final_split$train %>%
      map(is.numeric) %>%
      unlist()

    t <- final_split$train %>%
      map(is.Date) %>%
      unlist()

    j1 <- names(which(k == TRUE))
    j2 <- names(which(t == TRUE))

    if (length(j1) == 0) {
      j <- j2
    } else if (length(j2) == 0) {
      j <- j1
    } else {
      j <- c(j1, j2)
    }
    colnames(num_data) <- j

    if (is.null(dim(num_data))) {
            numdata <- tibble::as_data_frame(num_data)
            updateSelectInput(session, 'gline_select_x',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'gline_y',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'gline_select_x',
              choices = '', selected = '')
             updateSelectInput(session, 'gline_y',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'gline_select_x', choices = names(num_data))
             updateSelectInput(session, 'gline_y', choices = names(num_data))
        }


})


gselectedline <- reactive({
  req(input$gline_select_x)
  out <- final_split$train %>%
    select(input$gline_select_x, input$gline_y)
  out
})

ylinemax <- reactive({
  out <- gselectedline() %>%
    select(-1) %>%
    unlist() %>%
    max 
  out
})

output$ui_glineyrange_min <- renderUI({
  df <- final_split$train
  if (is.null(df)) return(NULL)
  numericInput('gliney_range_min', 'Y Axis Min', value = 0, min = 0)
})

output$ui_glineyrange_max <- renderUI({
  df <- final_split$train
  if (is.null(df)) return(NULL)
  numericInput('gliney_range_max', 'Y Axis Max', value = ylinemax())
})


output$gline_plot_1 <- renderPlot({
  ggline(data = final_split$train, x = input$gline_select_x, 
    columns = input$gline_y,
    title = input$gbox_title, sub = input$gbox_subtitle,
    xlab = input$gbox_xlabel, ylab = input$gbox_ylabel)
})


# gline2_out <- eventReactive(input$gline2_submit, {
#   p <- ggline(data = final_split$train, x = input$gline_select_x, 
#     columns = input$gline_y,
#     title = input$gbox_title, sub = input$gbox_subtitle,
#     xlab = input$gbox_xlabel, ylab = input$gbox_ylabel,
#     cols = input$gline_col, alphas = as.numeric(input$gline_alpha),
#     ltypes = as.numeric(input$gline_ltype), sizes = as.numeric(input$gline_size))
#   print(p)
# })

# output$gline_plot_2 <- renderPlot({
#   ggline(data = final_split$train, x = input$gline_select_x, 
#     columns = input$gline_y,
#     title = input$gbox_title, sub = input$gbox_subtitle,
#     xlab = input$gbox_xlabel, ylab = input$gbox_ylabel,
#     cols = input$gline_col, alphas = as.numeric(input$gline_alpha),
#     ltypes = as.numeric(input$gline_ltype), sizes = as.numeric(input$gline_size))
# })


output$gline_plot_3 <- renderPlot({
  ggline(data = final_split$train, x = input$gline_select_x, 
    columns = input$gline_y,
    title = input$gbox_title, sub = input$gbox_subtitle,
    xlab = input$gbox_xlabel, ylab = input$gbox_ylabel,
    yaxlimit = TRUE, y1 = input$gliney_range_min, y2 = input$gliney_range_max,
    remove_xax = input$gline_remx, remove_yax = input$gline_remy)
})


output$gline_plot_4 <- renderPlot({
  ggline(data = final_split$train, x = input$gline_select_x, 
    columns = input$gline_y,
    title = input$gbox_title, sub = input$gbox_subtitle,
    xlab = input$gbox_xlabel, ylab = input$gbox_ylabel,
    yaxlimit = TRUE, y1 = input$gliney_range_min, y2 = input$gliney_range_max,
    remove_xax = input$gline_remx, remove_yax = input$gline_remy,
    add_text = input$gline_text, xloc = input$gline_text_x_loc, 
    yloc = input$gline_text_y_loc, label = input$gline_plottext, 
    tex_color = input$gline_textcolor, tex_size = input$gline_textsize)
})

output$gline_plot_5 <- renderPlot({
  ggline(data = final_split$train, x = input$gline_select_x, 
    columns = input$gline_y,
    title = input$gbox_title, sub = input$gbox_subtitle,
    xlab = input$gbox_xlabel, ylab = input$gbox_ylabel,
    yaxlimit = TRUE, y1 = input$gliney_range_min, y2 = input$gliney_range_max,
    remove_xax = input$gline_remx, remove_yax = input$gline_remy,
    add_text = input$gline_text, xloc = input$gline_text_x_loc, 
    yloc = input$gline_text_y_loc, label = input$gline_plottext, 
    tex_color = input$gline_textcolor, tex_size = input$gline_textsize,
    title_col = input$gline_title_col, 
    title_fam = input$gline_title_fam, title_face = input$gline_title_font, 
    title_size = input$gline_title_size, title_hjust = input$gline_title_hjust, 
    title_vjust = input$gline_title_vjust, sub_col = input$gline_sub_col, 
    sub_fam = input$gline_sub_fam, sub_face = input$gline_subtitle_font, 
    sub_size = input$gline_sub_size, sub_hjust = input$gline_sub_hjust, 
    sub_vjust = input$gline_sub_vjust, xax_col = input$gline_xlab_col, 
    xax_fam = input$gline_xlab_fam, xax_face = input$gline_xlab_font, 
    xax_size = input$gline_xlab_size, xax_hjust = input$gline_xlab_hjust, 
    xax_vjust = input$gline_xlab_vjust, yax_col = input$gline_ylab_col, 
    yax_fam = input$gline_ylab_fam, yax_face = input$gline_ylab_font, 
    yax_size = input$gline_ylab_size, yax_hjust = input$gline_ylab_hjust, 
    yax_vjust = input$gline_ylab_vjust)
})


output$gline_plot_6 <- renderPlot({
  ggline(data = final_split$train, x = input$gline_select_x, 
    columns = input$gline_y,
    title = input$gbox_title, sub = input$gbox_subtitle,
    xlab = input$gbox_xlabel, ylab = input$gbox_ylabel,
    yaxlimit = TRUE, y1 = input$gliney_range_min, y2 = input$gliney_range_max,
    remove_xax = input$gline_remx, remove_yax = input$gline_remy,
    add_text = input$gline_text, xloc = input$gline_text_x_loc, 
    yloc = input$gline_text_y_loc, label = input$gline_plottext, 
    tex_color = input$gline_textcolor, tex_size = input$gline_textsize,
    title_col = input$gline_title_col, 
    title_fam = input$gline_title_fam, title_face = input$gline_title_font, 
    title_size = input$gline_title_size, title_hjust = input$gline_title_hjust, 
    title_vjust = input$gline_title_vjust, sub_col = input$gline_sub_col, 
    sub_fam = input$gline_sub_fam, sub_face = input$gline_subtitle_font, 
    sub_size = input$gline_sub_size, sub_hjust = input$gline_sub_hjust, 
    sub_vjust = input$gline_sub_vjust, xax_col = input$gline_xlab_col, 
    xax_fam = input$gline_xlab_fam, xax_face = input$gline_xlab_font, 
    xax_size = input$gline_xlab_size, xax_hjust = input$gline_xlab_hjust, 
    xax_vjust = input$gline_xlab_vjust, yax_col = input$gline_ylab_col, 
    yax_fam = input$gline_ylab_fam, yax_face = input$gline_ylab_font, 
    yax_size = input$gline_ylab_size, yax_hjust = input$gline_ylab_hjust, 
    yax_vjust = input$gline_ylab_vjust, theme = input$gline_theme)
})

output$gline_plot_7 <- renderPlot({
  ggline(data = final_split$train, x = input$gline_select_x, 
    columns = input$gline_y,
    title = input$gbox_title, sub = input$gbox_subtitle,
    xlab = input$gbox_xlabel, ylab = input$gbox_ylabel,
    yaxlimit = TRUE, y1 = input$gliney_range_min, y2 = input$gliney_range_max,
    remove_xax = input$gline_remx, remove_yax = input$gline_remy,
    add_text = input$gline_text, xloc = input$gline_text_x_loc, 
    yloc = input$gline_text_y_loc, label = input$gline_plottext, 
    tex_color = input$gline_textcolor, tex_size = input$gline_textsize,
    title_col = input$gline_title_col, 
    title_fam = input$gline_title_fam, title_face = input$gline_title_font, 
    title_size = input$gline_title_size, title_hjust = input$gline_title_hjust, 
    title_vjust = input$gline_title_vjust, sub_col = input$gline_sub_col, 
    sub_fam = input$gline_sub_fam, sub_face = input$gline_subtitle_font, 
    sub_size = input$gline_sub_size, sub_hjust = input$gline_sub_hjust, 
    sub_vjust = input$gline_sub_vjust, xax_col = input$gline_xlab_col, 
    xax_fam = input$gline_xlab_fam, xax_face = input$gline_xlab_font, 
    xax_size = input$gline_xlab_size, xax_hjust = input$gline_xlab_hjust, 
    xax_vjust = input$gline_xlab_vjust, yax_col = input$gline_ylab_col, 
    yax_fam = input$gline_ylab_fam, yax_face = input$gline_ylab_font, 
    yax_size = input$gline_ylab_size, yax_hjust = input$gline_ylab_hjust, 
    yax_vjust = input$gline_ylab_vjust, theme = input$gline_theme)
})
















