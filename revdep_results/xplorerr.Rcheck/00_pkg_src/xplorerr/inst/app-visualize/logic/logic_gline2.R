source('helper/ggline2.R')


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
            colnames(numdata) <- j
            updateSelectInput(session, 'gline2_select_x',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'gline2_y',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'gline2_select_x',
              choices = '', selected = '')
             updateSelectInput(session, 'gline2_y',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'gline2_select_x', choices = names(num_data))
             updateSelectInput(session, 'gline2_y', choices = names(num_data))
        }

        f_data <- final_split$train[, sapply(final_split$train, is.factor)]
        # validate(need(!dim(f_data)[2] == 0, 'No factor variables in the data.'))
        if (is.null(dim(f_data))) {
        h <- final_split$train %>% map(is.factor) %>% unlist()
        q <- names(which(h == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- q
        updateSelectInput(session, inputId = "gline2_group",
            choices = names(fdata))
        updateSelectInput(session, inputId = "gline2_col",
            choices = names(fdata))
        updateSelectInput(session, inputId = "gline2_ltype",
            choices = names(fdata))
        updateSelectInput(session, inputId = "gline2_size",
            choices = names(fdata))
        } else if (dim(f_data)[2] == 0) {
          updateSelectInput(session, 'gline2_group', choices = '', selected = '')
          updateSelectInput(session, 'gline2_col', choices = '', selected = '')
          updateSelectInput(session, 'gline2_ltype', choices = '', selected = '')
          updateSelectInput(session, 'gline2_size', choices = '', selected = '')
        } else {
          updateSelectInput(session, 'gline2_group', choices = names(f_data))  
          updateSelectInput(session, 'gline2_col', choices = names(f_data))  
          updateSelectInput(session, 'gline2_ltype', choices = names(f_data))  
          updateSelectInput(session, 'gline2_size', choices = names(f_data))  
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
            colnames(numdata) <- j
            updateSelectInput(session, 'gline2_select_x',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'gline2_y',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'gline2_select_x',
              choices = '', selected = '')
             updateSelectInput(session, 'gline2_y',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'gline2_select_x', choices = names(num_data))
             updateSelectInput(session, 'gline2_y', choices = names(num_data))
        }

        f_data <- final_split$train[, sapply(final_split$train, is.factor)]
        # validate(need(!dim(f_data)[2] == 0, 'No factor variables in the data.'))
        if (is.null(dim(f_data))) {
        h <- final_split$train %>% map(is.factor) %>% unlist()
        q <- names(which(h == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- q
        updateSelectInput(session, inputId = "gline2_group",
            choices = names(fdata), selected = names(fdata))
        updateSelectInput(session, inputId = "gline2_col",
            choices = names(fdata), selected = names(fdata))
        updateSelectInput(session, inputId = "gline2_ltype",
            choices = names(fdata), selected = names(fdata))
        updateSelectInput(session, inputId = "gline2_size",
            choices = names(fdata), selected = names(fdata))
        } else if (dim(f_data)[2] == 0) {
          updateSelectInput(session, 'gline2_group', choices = '', selected = '')
          updateSelectInput(session, 'gline2_col', choices = '', selected = '')
          updateSelectInput(session, 'gline2_ltype', choices = '', selected = '')
          updateSelectInput(session, 'gline2_size', choices = '', selected = '')
        } else {
          updateSelectInput(session, 'gline2_group', choices = names(f_data), selected = names(f_data))  
          updateSelectInput(session, 'gline2_col', choices = names(f_data), selected = names(f_data))  
          updateSelectInput(session, 'gline2_ltype', choices = names(f_data), selected = names(f_data))  
          updateSelectInput(session, 'gline2_size', choices = names(f_data), selected = names(f_data))  
        }
})


gline2_col_map <- eventReactive(input$gline2_col_yes, {
  selectInput('gline2_col', '',
      choices = "", selected = "")
})

observeEvent(input$gline2_col_yes, {
  f_data <- final_split$train[, sapply(final_split$train, is.factor)]
  if (is.null(dim(f_data))) {
    h <- final_split$train %>% map(is.factor) %>% unlist()
    q <- names(which(h == TRUE))
    fdata <- tibble::as_data_frame(f_data)
    colnames(fdata) <- q
    updateSelectInput(session, inputId = "gline2_col",
      choices = names(fdata), selected = '')        
  } else if (dim(f_data)[2] == 0) {
    updateSelectInput(session, 'gline2_col', choices = '', selected = '')
  } else {      
    updateSelectInput(session, 'gline2_col', choices = names(f_data), selected = '')  
  }
})

output$gline2_col_ui <- renderUI({
  gline2_col_map()
})

gline2_ltype_map <- eventReactive(input$gline2_ltype_yes, {
  selectInput('gline2_ltype', '',
      choices = "", selected = "")
})

observeEvent(input$gline2_ltype_yes, {
  f_data <- final_split$train[, sapply(final_split$train, is.factor)]
  if (is.null(dim(f_data))) {
    h <- final_split$train %>% map(is.factor) %>% unlist()
    q <- names(which(h == TRUE))
    fdata <- tibble::as_data_frame(f_data)
    colnames(fdata) <- q
    updateSelectInput(session, inputId = "gline2_ltype",
      choices = names(fdata), selected = '')        
  } else if (dim(f_data)[2] == 0) {
    updateSelectInput(session, 'gline2_ltype', choices = '', selected = '')
  } else {      
    updateSelectInput(session, 'gline2_ltype', choices = names(f_data), selected = '')  
  }
})

output$gline2_ltype_ui <- renderUI({
  gline2_ltype_map()
})

gline2_size_map <- eventReactive(input$gline2_size_yes, {
  selectInput('gline2_size', '',
      choices = "", selected = "")
})

observeEvent(input$gline2_size_yes, {
  f_data <- final_split$train[, sapply(final_split$train, is.factor)]
  if (is.null(dim(f_data))) {
    h <- final_split$train %>% map(is.factor) %>% unlist()
    q <- names(which(h == TRUE))
    fdata <- tibble::as_data_frame(f_data)
    colnames(fdata) <- q
    updateSelectInput(session, inputId = "gline2_size",
      choices = names(fdata), selected = '')        
  } else if (dim(f_data)[2] == 0) {
    updateSelectInput(session, 'gline2_size', choices = '', selected = '')
  } else {      
    updateSelectInput(session, 'gline2_size', choices = names(f_data), selected = '')  
  }
})

output$gline2_size_ui <- renderUI({
  gline2_size_map()
})

gselectedline2 <- reactive({
  req(input$gline2_select_x)
  out <- final_split$train %>%
    select(input$gline2_y)
  out
})

yline2min <- reactive({
  out <- gselectedline2() %>%
    select(1) %>%
    unlist() %>%
    min

  result <- out - ceiling(out * 0.1)
  result
})

ylinemax2 <- reactive({
  out <- gselectedline2() %>%
    select(1) %>%
    unlist() %>%
    max
     
  out
})

output$ui_gline2yrange_min <- renderUI({
  df <- final_split$train
  if (is.null(df)) return(NULL)
  numericInput('gline2y_range_min', 'Y Axis Min', value = yline2min())
})

output$ui_gline2yrange_max <- renderUI({
  df <- final_split$train
  if (is.null(df)) return(NULL)
  numericInput('gline2y_range_max', 'Y Axis Max', value = ylinemax2())
})



output$gline2_plot_1 <- renderPlot({
  ggline2(data = final_split$train, x = input$gline2_select_x, 
    columns = input$gline2_y, groups = input$gline2_group,
    title = input$gline2_title, sub = input$gline2_subtitle,
    xlab = input$gline2_xlabel, ylab = input$gline2_ylabel)
})

output$gline2_plot_2 <- renderPlot({
  ggline2(data = final_split$train, x = input$gline2_select_x, 
    columns = input$gline2_y, groups = input$gline2_group,
    cols = input$gline2_col, ltypes = input$gline2_ltype, 
    sizes = input$gline2_size,
    title = input$gline2_title, sub = input$gline2_subtitle,
    xlab = input$gline2_xlabel, ylab = input$gline2_ylabel)
})

output$gline2_plot_3 <- renderPlot({
  ggline2(data = final_split$train, x = input$gline2_select_x, 
    columns = input$gline2_y, groups = input$gline2_group,
    cols = input$gline2_col, ltypes = input$gline2_ltype, 
    sizes = input$gline2_size,
    title = input$gline2_title, sub = input$gline2_subtitle,
    xlab = input$gline2_xlabel, ylab = input$gline2_ylabel,
    yaxlimit = TRUE, y1 = input$gline2y_range_min, y2 = input$gline2y_range_max,
    remove_xax = input$gline2_remx, remove_yax = input$gline2_remy)
})

output$gline2_plot_4 <- renderPlot({
  ggline2(data = final_split$train, x = input$gline2_select_x, 
    columns = input$gline2_y, groups = input$gline2_group,
    cols = input$gline2_col, ltypes = input$gline2_ltype, 
    sizes = input$gline2_size,
    title = input$gline2_title, sub = input$gline2_subtitle,
    xlab = input$gline2_xlabel, ylab = input$gline2_ylabel,
    yaxlimit = TRUE, y1 = input$gline2y_range_min, y2 = input$gline2y_range_max,
    remove_xax = input$gline2_remx, remove_yax = input$gline2_remy,
    add_text = input$gline2_text, xloc = input$gline2_text_x_loc, 
    yloc = input$gline2_text_y_loc, label = input$gline2_plottext, 
    tex_color = input$gline2_textcolor, tex_size = input$gline2_textsize)
})

output$gline2_plot_5 <- renderPlot({
  ggline2(data = final_split$train, x = input$gline2_select_x, 
    columns = input$gline2_y, groups = input$gline2_group,
    cols = input$gline2_col, ltypes = input$gline2_ltype, 
    sizes = input$gline2_size,
    title = input$gline2_title, sub = input$gline2_subtitle,
    xlab = input$gline2_xlabel, ylab = input$gline2_ylabel,
    yaxlimit = TRUE, y1 = input$gline2y_range_min, y2 = input$gline2y_range_max,
    remove_xax = input$gline2_remx, remove_yax = input$gline2_remy,
    add_text = input$gline2_text, xloc = input$gline2_text_x_loc, 
    yloc = input$gline2_text_y_loc, label = input$gline2_plottext, 
    tex_color = input$gline2_textcolor, tex_size = input$gline2_textsize,
    title_col = input$gline22_title_col, 
    title_fam = input$gline2_title_fam, title_face = input$gline2_title_font, 
    title_size = input$gline2_title_size, title_hjust = input$gline2_title_hjust, 
    title_vjust = input$gline2_title_vjust, sub_col = input$gline2_sub_col, 
    sub_fam = input$gline2_sub_fam, sub_face = input$gline2_subtitle_font, 
    sub_size = input$gline2_sub_size, sub_hjust = input$gline2_sub_hjust, 
    sub_vjust = input$gline2_sub_vjust, xax_col = input$gline2_xlab_col, 
    xax_fam = input$gline2_xlab_fam, xax_face = input$gline2_xlab_font, 
    xax_size = input$gline2_xlab_size, xax_hjust = input$gline2_xlab_hjust, 
    xax_vjust = input$gline2_xlab_vjust, yax_col = input$gline2_ylab_col, 
    yax_fam = input$gline2_ylab_fam, yax_face = input$gline2_ylab_font, 
    yax_size = input$gline2_ylab_size, yax_hjust = input$gline2_ylab_hjust, 
    yax_vjust = input$gline2_ylab_vjust)
})

output$gline2_plot_6 <- renderPlot({
  ggline2(data = final_split$train, x = input$gline2_select_x, 
    columns = input$gline2_y, groups = input$gline2_group,
    cols = input$gline2_col, ltypes = input$gline2_ltype, 
    sizes = input$gline2_size,
    title = input$gline2_title, sub = input$gline2_subtitle,
    xlab = input$gline2_xlabel, ylab = input$gline2_ylabel,
    yaxlimit = TRUE, y1 = input$gline2y_range_min, y2 = input$gline2y_range_max,
    remove_xax = input$gline2_remx, remove_yax = input$gline2_remy,
    add_text = input$gline2_text, xloc = input$gline2_text_x_loc, 
    yloc = input$gline2_text_y_loc, label = input$gline2_plottext, 
    tex_color = input$gline2_textcolor, tex_size = input$gline2_textsize,
    title_col = input$gline22_title_col, 
    title_fam = input$gline2_title_fam, title_face = input$gline2_title_font, 
    title_size = input$gline2_title_size, title_hjust = input$gline2_title_hjust, 
    title_vjust = input$gline2_title_vjust, sub_col = input$gline2_sub_col, 
    sub_fam = input$gline2_sub_fam, sub_face = input$gline2_subtitle_font, 
    sub_size = input$gline2_sub_size, sub_hjust = input$gline2_sub_hjust, 
    sub_vjust = input$gline2_sub_vjust, xax_col = input$gline2_xlab_col, 
    xax_fam = input$gline2_xlab_fam, xax_face = input$gline2_xlab_font, 
    xax_size = input$gline2_xlab_size, xax_hjust = input$gline2_xlab_hjust, 
    xax_vjust = input$gline2_xlab_vjust, yax_col = input$gline2_ylab_col, 
    yax_fam = input$gline2_ylab_fam, yax_face = input$gline2_ylab_font, 
    yax_size = input$gline2_ylab_size, yax_hjust = input$gline2_ylab_hjust, 
    yax_vjust = input$gline2_ylab_vjust, theme = input$gline2_theme)
})

output$gline2_plot_7 <- renderPlot({
  ggline2(data = final_split$train, x = input$gline2_select_x, 
    columns = input$gline2_y, groups = input$gline2_group,
    cols = input$gline2_col, ltypes = input$gline2_ltype, 
    sizes = input$gline2_size,
    title = input$gline2_title, sub = input$gline2_subtitle,
    xlab = input$gline2_xlabel, ylab = input$gline2_ylabel,
    yaxlimit = TRUE, y1 = input$gline2y_range_min, y2 = input$gline2y_range_max,
    remove_xax = input$gline2_remx, remove_yax = input$gline2_remy,
    add_text = input$gline2_text, xloc = input$gline2_text_x_loc, 
    yloc = input$gline2_text_y_loc, label = input$gline2_plottext, 
    tex_color = input$gline2_textcolor, tex_size = input$gline2_textsize,
    title_col = input$gline22_title_col, 
    title_fam = input$gline2_title_fam, title_face = input$gline2_title_font, 
    title_size = input$gline2_title_size, title_hjust = input$gline2_title_hjust, 
    title_vjust = input$gline2_title_vjust, sub_col = input$gline2_sub_col, 
    sub_fam = input$gline2_sub_fam, sub_face = input$gline2_subtitle_font, 
    sub_size = input$gline2_sub_size, sub_hjust = input$gline2_sub_hjust, 
    sub_vjust = input$gline2_sub_vjust, xax_col = input$gline2_xlab_col, 
    xax_fam = input$gline2_xlab_fam, xax_face = input$gline2_xlab_font, 
    xax_size = input$gline2_xlab_size, xax_hjust = input$gline2_xlab_hjust, 
    xax_vjust = input$gline2_xlab_vjust, yax_col = input$gline2_ylab_col, 
    yax_fam = input$gline2_ylab_fam, yax_face = input$gline2_ylab_font, 
    yax_size = input$gline2_ylab_size, yax_hjust = input$gline2_ylab_hjust, 
    yax_vjust = input$gline2_ylab_vjust, theme = input$gline2_theme)
})


