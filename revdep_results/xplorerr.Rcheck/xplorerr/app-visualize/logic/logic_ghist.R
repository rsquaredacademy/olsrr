source('helper/gghist.R')

observeEvent(input$finalok, {
        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'ghist_select_x',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'ghist_select_x',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'ghist_select_x', choices = names(num_data))
        }

})

observeEvent(input$submit_part_train_per, {
        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'ghist_select_x',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'ghist_select_x',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'ghist_select_x', choices = names(num_data))
        }
})



output$ghist_plot_1 <- renderPlot({
  gghist(data = final_split$train, x = input$ghist_select_x, 
    bins = input$ghist_bins,
    title = input$ghist_title, sub = input$ghist_subtitle,
    xlab = input$ghist_xlabel, ylab = input$ghist_ylabel,
    fill = input$ghist_fill, col = input$ghist_col)
})

output$ghist_plot_2 <- renderPlot({
  gghist(data = final_split$train, x = input$ghist_select_x, 
    bins = input$ghist_bins,
    title = input$ghist_title, sub = input$ghist_subtitle,
    xlab = input$ghist_xlabel, ylab = input$ghist_ylabel,
    fill = input$ghist_fill, col = input$ghist_col,
    remove_xax = input$ghist_remx, remove_yax = input$ghist_remy)
})

output$ghist_plot_3 <- renderPlot({
  gghist(data = final_split$train, x = input$ghist_select_x, 
    bins = input$ghist_bins,
    title = input$ghist_title, sub = input$ghist_subtitle,
    xlab = input$ghist_xlabel, ylab = input$ghist_ylabel,
    fill = input$ghist_fill, col = input$ghist_col,
    remove_xax = input$ghist_remx, remove_yax = input$ghist_remy,
    add_text = input$ghist_text, xloc = input$ghist_text_x_loc, 
    yloc = input$ghist_text_y_loc, label = input$ghist_plottext, 
    tex_color = input$ghist_textcolor, tex_size = input$ghist_textsize)
})

output$ghist_plot_4 <- renderPlot({
  gghist(data = final_split$train, x = input$ghist_select_x, 
    bins = input$ghist_bins,
    title = input$ghist_title, sub = input$ghist_subtitle,
    xlab = input$ghist_xlabel, ylab = input$ghist_ylabel,
    fill = input$ghist_fill, col = input$ghist_col,
    remove_xax = input$ghist_remx, remove_yax = input$ghist_remy,
    add_text = input$ghist_text, xloc = input$ghist_text_x_loc, 
    yloc = input$ghist_text_y_loc, label = input$ghist_plottext, 
    tex_color = input$ghist_textcolor, tex_size = input$ghist_textsize,
    title_col = input$ghist_title_col, 
    title_fam = input$ghist_title_fam, title_face = input$ghist_title_font, 
    title_size = input$ghist_title_size, title_hjust = input$ghist_title_hjust, 
    title_vjust = input$ghist_title_vjust, sub_col = input$ghist_sub_col, 
    sub_fam = input$ghist_sub_fam, sub_face = input$ghist_subtitle_font, 
    sub_size = input$ghist_sub_size, sub_hjust = input$ghist_sub_hjust, 
    sub_vjust = input$ghist_sub_vjust, xax_col = input$ghist_xlab_col, 
    xax_fam = input$ghist_xlab_fam, xax_face = input$ghist_xlab_font, 
    xax_size = input$ghist_xlab_size, xax_hjust = input$ghist_xlab_hjust, 
    xax_vjust = input$ghist_xlab_vjust, yax_col = input$ghist_ylab_col, 
    yax_fam = input$ghist_ylab_fam, yax_face = input$ghist_ylab_font, 
    yax_size = input$ghist_ylab_size, yax_hjust = input$ghist_ylab_hjust, 
    yax_vjust = input$ghist_ylab_vjust)
})

output$ghist_plot_5 <- renderPlot({
  gghist(data = final_split$train, x = input$ghist_select_x, 
    bins = input$ghist_bins,
    title = input$ghist_title, sub = input$ghist_subtitle,
    xlab = input$ghist_xlabel, ylab = input$ghist_ylabel,
    fill = input$ghist_fill, col = input$ghist_col,
    remove_xax = input$ghist_remx, remove_yax = input$ghist_remy,
    add_text = input$ghist_text, xloc = input$ghist_text_x_loc, 
    yloc = input$ghist_text_y_loc, label = input$ghist_plottext, 
    tex_color = input$ghist_textcolor, tex_size = input$ghist_textsize,
    title_col = input$ghist_title_col, 
    title_fam = input$ghist_title_fam, title_face = input$ghist_title_font, 
    title_size = input$ghist_title_size, title_hjust = input$ghist_title_hjust, 
    title_vjust = input$ghist_title_vjust, sub_col = input$ghist_sub_col, 
    sub_fam = input$ghist_sub_fam, sub_face = input$ghist_subtitle_font, 
    sub_size = input$ghist_sub_size, sub_hjust = input$ghist_sub_hjust, 
    sub_vjust = input$ghist_sub_vjust, xax_col = input$ghist_xlab_col, 
    xax_fam = input$ghist_xlab_fam, xax_face = input$ghist_xlab_font, 
    xax_size = input$ghist_xlab_size, xax_hjust = input$ghist_xlab_hjust, 
    xax_vjust = input$ghist_xlab_vjust, yax_col = input$ghist_ylab_col, 
    yax_fam = input$ghist_ylab_fam, yax_face = input$ghist_ylab_font, 
    yax_size = input$ghist_ylab_size, yax_hjust = input$ghist_ylab_hjust, 
    yax_vjust = input$ghist_ylab_vjust, theme = input$ghist_theme)
})

output$ghist_plot_6 <- renderPlot({
  gghist(data = final_split$train, x = input$ghist_select_x, 
    bins = input$ghist_bins,
    title = input$ghist_title, sub = input$ghist_subtitle,
    xlab = input$ghist_xlabel, ylab = input$ghist_ylabel,
    fill = input$ghist_fill, col = input$ghist_col,
    remove_xax = input$ghist_remx, remove_yax = input$ghist_remy,
    add_text = input$ghist_text, xloc = input$ghist_text_x_loc, 
    yloc = input$ghist_text_y_loc, label = input$ghist_plottext, 
    tex_color = input$ghist_textcolor, tex_size = input$ghist_textsize,
    title_col = input$ghist_title_col, 
    title_fam = input$ghist_title_fam, title_face = input$ghist_title_font, 
    title_size = input$ghist_title_size, title_hjust = input$ghist_title_hjust, 
    title_vjust = input$ghist_title_vjust, sub_col = input$ghist_sub_col, 
    sub_fam = input$ghist_sub_fam, sub_face = input$ghist_subtitle_font, 
    sub_size = input$ghist_sub_size, sub_hjust = input$ghist_sub_hjust, 
    sub_vjust = input$ghist_sub_vjust, xax_col = input$ghist_xlab_col, 
    xax_fam = input$ghist_xlab_fam, xax_face = input$ghist_xlab_font, 
    xax_size = input$ghist_xlab_size, xax_hjust = input$ghist_xlab_hjust, 
    xax_vjust = input$ghist_xlab_vjust, yax_col = input$ghist_ylab_col, 
    yax_fam = input$ghist_ylab_fam, yax_face = input$ghist_ylab_font, 
    yax_size = input$ghist_ylab_size, yax_hjust = input$ghist_ylab_hjust, 
    yax_vjust = input$ghist_ylab_vjust, theme = input$ghist_theme)
})
