source("helper/line-plot.R")

    # update variable selection for scatter plots
    # observe({
    #     updateSelectInput(session, 'line_select_x', choices = names(data()))
    #     updateSelectInput(session, 'line_select_y', choices = names(data()))
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
            updateSelectInput(session, 'line_select_x',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'line_select_y',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'line_select_x',
              choices = '', selected = '')
             updateSelectInput(session, 'line_select_y',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'line_select_x', choices = names(num_data))
             updateSelectInput(session, 'line_select_y', choices = names(num_data))
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
            updateSelectInput(session, 'line_select_x',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'line_select_y',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'line_select_x',
              choices = '', selected = '')
             updateSelectInput(session, 'line_select_y',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'line_select_x', choices = names(num_data))
             updateSelectInput(session, 'line_select_y', choices = names(num_data))
        }
    })


    # selected data
    selected_xl <- reactive({
      req(input$line_select_x)
      final_split$train[, input$line_select_x]
    })

    selected_yl <- reactive({
      req(input$line_select_y)
      final_split$train[, input$line_select_y]
    })

    # dynamic UI for line colors
    output$ui_nlines <- renderUI({
        ncol <- as.integer(input$n_lines)

        if (ncol < 1) {
          NULL 
        } else {
          lapply(1:ncol, function(i) {
              selectInput(paste("n_addline_", i),
                        label = paste0("Line ", i),
                        choices = names(final_split$train))
          })
        }
    })

    addvars <- reactive({
        ncol <- as.integer(input$n_lines)
        if (ncol < 1) {
          colors <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
                      input[[paste("n_addline_", i)]]
                  }))
          colors <- unlist(collect)
        }
        colors
    })

    # dynamic UI for bar colors
    output$ui_ncolors <- renderUI({
        ncol <- as.integer(input$n_lines)

        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
              textInput(paste("n_addcol_", i),
                        label = paste0("Line ", i, " Color"),
                        value = "black")
          })
        }
    })

    colours <- reactive({
        ncol <- as.integer(input$n_lines)
        if (ncol < 1) {
          colors <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
                          input[[paste("n_addcol_", i)]]
                      }))
          colors <- unlist(collect)
        }
        colors
    })


      selected_z <- reactive({
          # final_split$train[, addvars()]
        ncol <- as.integer(input$n_lines)
        if (ncol > 0) {
          if (ncol == 1) {
            final_split$train %>% select_(addvars())    
          } else {
            final_split$train[, addvars()]
          }
        }
      })

    nz <- reactive({
      ncol(selected_z())
    })

    output$ui_nltys <- renderUI({
        ncol <- as.integer(input$n_lines)
        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
              numericInput(paste("n_addlty_", i),
                        label = paste0("Line ", i, " Type"),
                        value = 1, min = 1, max = 5, step = 1)
          })
        }
    })

    ltys <- reactive({
        ncol <- as.integer(input$n_lines)
        if (ncol < 1) {
          colors <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
                          input[[paste("n_addlty_", i)]]
                      }))
          colors <- unlist(collect)
        }
        colors
    })

    output$ui_nlwds <- renderUI({
        ncol <- as.integer(input$n_lines)
        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
              numericInput(paste("n_addlwd_", i),
                        label = paste0("Line ", i, " Width"),
                        value = 1, min = 0.1, step = 0.1)
          })
        }
    })

    lwds <- reactive({
        ncol <- as.integer(input$n_lines)
        if (ncol < 1) {
          colors <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
                          input[[paste("n_addlwd_", i)]]
                      }))
          colors <- unlist(collect)
        }
        colors
    })

    output$ui_pcolors <- renderUI({
        ncol <- as.integer(input$n_lines)
        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
              textInput(paste("n_addpcol_", i),
                        label = paste0("Point ", i, " Color"),
                        value = 'black')
          })
        }
    })

    pcolors <- reactive({
        ncol <- as.integer(input$n_lines)
        if (ncol < 1) {
          colors <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
                          input[[paste("n_addpcol_", i)]]
                      }))
          colors <- unlist(collect)
        }
        colors
    })

    output$ui_pbgcolor <- renderUI({
        ncol <- as.integer(input$n_lines)
        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
              textInput(paste("n_addpbgcol_", i),
                        label = paste0("Point ", i, " Bg Col"),
                        value = 'black')
          })
        }
    })

    pbgcolors <- reactive({
        ncol <- as.integer(input$n_lines)
        if (ncol < 1) {
          colors <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
                          input[[paste("n_addpbgcol_", i)]]
                      }))
          colors <- unlist(collect)
        }
        colors
    })

    output$ui_psize <- renderUI({
        ncol <- as.integer(input$n_lines)
        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
              numericInput(paste("n_addpsize_", i),
                        label = paste0("Point ", i, " Size"),
                        value = 1, min = 0.1, step = 0.1)
          })
        }
    })

    psizes <- reactive({
        ncol <- as.integer(input$n_lines)
        if (ncol < 1) {
          colors <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
                          input[[paste("n_addpsize_", i)]]
                      }))
          colors <- unlist(collect)
        }
        colors
    })

    output$ui_pshape <- renderUI({
        ncol <- as.integer(input$n_lines)
        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
              numericInput(paste("n_addpshape_", i),
                        label = paste0("Point ", i, "Shape"),
                        min = 0, max = 25, value = 1)
          })
        }
    })

    pshapes <- reactive({
        ncol <- as.integer(input$n_lines)
        if (ncol < 1) {
          colors <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
                          input[[paste("n_addpshape_", i)]]
                      }))
          colors <- unlist(collect)
        }
        colors
    })

    # dynamic UI for legend names
    output$ui_line_legnames <- renderUI({
        ncol <- as.integer(input$line_legnames)
        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
              textInput(paste("n_namesline_", i),
                        label = paste0("Legend Name ", i))
          })
        }
    })


    # dynamic UI for legend border
    output$ui_line_legpoint <- renderUI({
        ncol <- as.integer(input$line_leg_point)
        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
              numericInput(paste("n_pointline_", i),
                        label = paste0("Legend Point ", i), value = 15)
          })
        }
    })

    output$ui_line_legline <- renderUI({
        ncol <- as.integer(input$line_leg_line)
        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
              numericInput(paste("n_lineline_", i),
                        label = paste0("Legend Line ", i), value = 1)
          })
        }
    })

    # vector of legend names
    name_line <- reactive({
        ncol <- as.integer(input$line_legnames)
        if (ncol < 1) {
          names <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
              input[[paste("n_namesline_", i)]]
          }))
          names <- unlist(collect)
        }
        names
    })

    # vector of point types
    point_line <- reactive({
        ncol <- as.integer(input$line_leg_point)
        if (ncol < 1) {
          names <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
              input[[paste("n_pointline_", i)]]
          }))
          names <- unlist(collect)
        }
        names
    })

    line_line <- reactive({
        ncol <- as.integer(input$line_leg_line)
        if (ncol < 1) {
          names <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
              input[[paste("n_lineline_", i)]]
          }))
          names <- unlist(collect)
        }
        names
    })

    output$ui_yrange_minl <- renderUI({
    df <- final_split$train
    if (is.null(df)) return(NULL)
    numericInput('y_range_minl', 'Y Axis Min', value = min(as.numeric(selected_yl())))
    })

    output$ui_yrange_maxl <- renderUI({
    df <- final_split$train
    if (is.null(df)) return(NULL)
    numericInput('y_range_maxl', 'Y Axis Max', value = max(as.numeric(selected_yl())))
    })

    limits <- reactive({
        c(input$y_range_minl, input$y_range_maxl)
    })



    # plot 1
    output$line_plot_1 <- renderPlot({
      line_graph(selected_xl(), selected_yl(), title = input$line_title,
      subtitle = input$line_subtitle, xlabel = input$line_xlabel,
      ylabel = input$line_ylabel)
    })

    # plot 2
    output$line_plot_2 <- renderPlot({
      line_graph(selected_xl(), selected_yl(), input$line_type, input$line_width,
        input$line_color,title = input$line_title, subtitle = input$line_subtitle,
        xlabel = input$line_xlabel, ylabel = input$line_ylabel, input$add_points,
        input$point_shape, input$point_size, input$point_col, input$point_bg)
    })

    # plot 3
    output$line_plot_3 <- renderPlot({
      # addvars()

      line_graph(selected_xl(), selected_yl(), input$line_type, input$line_width,
        input$line_color,title = input$line_title, sub = input$line_subtitle, xlab = input$line_xlabel,
        ylab = input$line_ylabel, input$add_points, input$point_shape, input$point_size, input$point_col,
        input$point_bg, ylim_l = limits()[1], ylim_u = limits()[2])
    })

    output$line_plot_4 <- renderPlot({
      # addvars()

      line_graph(selected_xl(), selected_yl(), input$line_type, input$line_width,
        input$line_color,title = input$line_title, sub = input$line_subtitle, xlab = input$line_xlabel,
        ylab = input$line_ylabel, input$add_points, input$point_shape, input$point_size, input$point_col,
        input$point_bg, ylim_l = limits()[1], ylim_u = limits()[2], extra_lines = nz(),
        extra_vars = selected_z(), extra_cols = colours(), ltys = ltys(), lwds = lwds())

    })

    output$line_plot_5 <- renderPlot({
      # addvars()

      line_graph(selected_xl(), selected_yl(), input$line_type, input$line_width,
        input$line_color,title = input$line_title, sub = input$line_subtitle,
        xlab = input$line_xlabel, ylab = input$line_ylabel, input$add_points,
        input$point_shape, input$point_size, input$point_col, input$point_bg,
        ylim_l = limits()[1], ylim_u = limits()[2], extra_lines = nz(),
        extra_vars = selected_z(), extra_cols = colours(), ltys = ltys(),
        lwds = lwds(), extra_p = input$extra_p, pcolors = pcolors(), pbgcolors = pbgcolors(),
        pshapes = pshapes(), psizes = psizes())

    })

    output$line_plot_6 <- renderPlot({
      # addvars()

      line_graph(selected_xl(), selected_yl(), input$line_type, input$line_width,
        input$line_color,title = input$line_title, sub = input$line_subtitle,
        xlab = input$line_xlabel, ylab = input$line_ylabel, input$add_points,
        input$point_shape, input$point_size, input$point_col, input$point_bg,
        ylim_l = limits()[1], ylim_u = limits()[2], extra_lines = nz(),
        extra_vars = selected_z(), extra_cols = colours(), ltys = ltys(),
        lwds = lwds(), extra_p = input$extra_p, pcolors = pcolors(), pbgcolors = pbgcolors(),
        pshapes = pshapes(), psizes = psizes(), colmain = input$line_coltitle,
        colsub = input$line_colsub, colaxis = input$line_colaxis,
      	collab = input$line_collabel, fontmain = input$line_fontmain,
        fontsub = input$line_fontsub, fontaxis = input$line_fontaxis,
        fontlab = input$line_fontlab, cexmain = input$line_cexmain,
        cexsub = input$line_cexsub, cexaxis = input$cexaxis,
        cexlab = input$cexlab)

    })

    output$line_plot_7 <- renderPlot({
      # addvars()

      line_graph(selected_xl(), selected_yl(), input$line_type, input$line_width,
        input$line_color,title = input$line_title, sub = input$line_subtitle,
        xlab = input$line_xlabel, ylab = input$line_ylabel, input$add_points,
        input$point_shape, input$point_size, input$point_col, input$point_bg,
        ylim_l = limits()[1], ylim_u = limits()[2], extra_lines = nz(),
        extra_vars = selected_z(), extra_cols = colours(), ltys = ltys(),
        lwds = lwds(), extra_p = input$extra_p, pcolors = pcolors(), pbgcolors = pbgcolors(),
        pshapes = pshapes(), psizes = psizes(), colmain = input$line_coltitle,
        colsub = input$line_colsub, colaxis = input$line_colaxis,
      	collab = input$line_collabel, fontmain = input$line_fontmain,
        fontsub = input$line_fontsub, fontaxis = input$line_fontaxis,
        fontlab = input$line_fontlab, cexmain = input$line_cexmain,
        cexsub = input$line_cexsub, cexaxis = input$cexaxis,
        cexlab = input$cexlab, text_p = input$line_plottext,
      	text_x_loc = input$line_text_x_loc, text_y_loc = input$line_text_y_loc,
        text_col = input$line_textcolor, text_font = input$line_textfont,
        text_size = input$line_textsize, m_text = input$line_mtextplot,
        m_side = input$line_mtext_side, m_line = input$line_mtext_line,
        m_adj = input$line_mtextadj, m_col = input$line_mtextcolor,
        m_font = input$line_mtextfont, m_cex = input$line_mtextsize)

    })

    output$line_plot_8 <- renderPlot({
      # addvars()

      line_graph(selected_xl(), selected_yl(), input$line_type, input$line_width,
        input$line_color,title = input$line_title, sub = input$line_subtitle,
        xlab = input$line_xlabel, ylab = input$line_ylabel, input$add_points,
        input$point_shape, input$point_size, input$point_col, input$point_bg,
        ylim_l = limits()[1], ylim_u = limits()[2], extra_lines = nz(),
        extra_vars = selected_z(), extra_cols = colours(), ltys = ltys(),
        lwds = lwds(), extra_p = input$extra_p, pcolors = pcolors(), pbgcolors = pbgcolors(),
        pshapes = pshapes(), psizes = psizes(), colmain = input$line_coltitle,
        colsub = input$line_colsub, colaxis = input$line_colaxis,
      	collab = input$line_collabel, fontmain = input$line_fontmain,
        fontsub = input$line_fontsub, fontaxis = input$line_fontaxis,
        fontlab = input$line_fontlab, cexmain = input$line_cexmain,
        cexsub = input$line_cexsub, cexaxis = input$cexaxis,
        cexlab = input$cexlab, text_p = input$line_plottext,
      	text_x_loc = input$line_text_x_loc, text_y_loc = input$line_text_y_loc,
        text_col = input$line_textcolor, text_font = input$line_textfont,
        text_size = input$line_textsize, m_text = input$line_mtextplot,
        m_side = input$line_mtext_side, m_line = input$line_mtext_line,
        m_adj = input$line_mtextadj, m_col = input$line_mtextcolor,
        m_font = input$line_mtextfont, m_cex = input$line_mtextsize,
        leg = as.logical(input$line_leg_yn), leg_x = input$line_leg_x,
        leg_y = input$line_leg_y, legend = name_line(),
        leg_line = line_line(),
        leg_point = point_line(), leg_colour = c(input$line_color, colours()),
        leg_boxtype = input$line_leg_boxtype,
        leg_boxcol = input$line_leg_boxcol,
        leg_boxlty = input$line_leg_boxlty,
        leg_boxlwd = input$line_leg_boxlwd,
        leg_boxborcol = input$line_leg_boxborcol,
        leg_boxxjust = input$line_leg_boxxjust,
        leg_boxyjust = input$line_leg_boxyjust,
        leg_textcol = input$line_leg_textcol,
        leg_textfont = input$line_leg_textfont,
        leg_textcolumns = input$line_leg_textcolumns,
        leg_texthoriz = input$line_leg_texthoriz,
        leg_title = input$line_leg_title,
        leg_titlecol = input$line_leg_titlecol,
        leg_textadj = input$line_leg_textadj)

    })

    output$line_final <- renderPlot({
      # addvars()

      line_graph(selected_xl(), selected_yl(), input$line_type, input$line_width,
        input$line_color,title = input$line_title, sub = input$line_subtitle,
        xlab = input$line_xlabel, ylab = input$line_ylabel, input$add_points,
        input$point_shape, input$point_size, input$point_col, input$point_bg,
        ylim_l = limits()[1], ylim_u = limits()[2], extra_lines = nz(),
        extra_vars = selected_z(), extra_cols = colours(), ltys = ltys(),
        lwds = lwds(), extra_p = input$extra_p, pcolors = pcolors(), pbgcolors = pbgcolors(),
        pshapes = pshapes(), psizes = psizes(), colmain = input$line_coltitle,
        colsub = input$line_colsub, colaxis = input$line_colaxis,
        collab = input$line_collabel, fontmain = input$line_fontmain,
        fontsub = input$line_fontsub, fontaxis = input$line_fontaxis,
        fontlab = input$line_fontlab, cexmain = input$line_cexmain,
        cexsub = input$line_cexsub, cexaxis = input$cexaxis,
        cexlab = input$cexlab, text_p = input$line_plottext,
        text_x_loc = input$line_text_x_loc, text_y_loc = input$line_text_y_loc,
        text_col = input$line_textcolor, text_font = input$line_textfont,
        text_size = input$line_textsize, m_text = input$line_mtextplot,
        m_side = input$line_mtext_side, m_line = input$line_mtext_line,
        m_adj = input$line_mtextadj, m_col = input$line_mtextcolor,
        m_font = input$line_mtextfont, m_cex = input$line_mtextsize,
        leg = as.logical(input$line_leg_yn), leg_x = input$line_leg_x,
        leg_y = input$line_leg_y, legend = name_line(),
        leg_line = line_line(),
        leg_point = point_line(), leg_colour = c(input$line_color, colours()),
        leg_boxtype = input$line_leg_boxtype,
        leg_boxcol = input$line_leg_boxcol,
        leg_boxlty = input$line_leg_boxlty,
        leg_boxlwd = input$line_leg_boxlwd,
        leg_boxborcol = input$line_leg_boxborcol,
        leg_boxxjust = input$line_leg_boxxjust,
        leg_boxyjust = input$line_leg_boxyjust,
        leg_textcol = input$line_leg_textcol,
        leg_textfont = input$line_leg_textfont,
        leg_textcolumns = input$line_leg_textcolumns,
        leg_texthoriz = input$line_leg_texthoriz,
        leg_title = input$line_leg_title,
        leg_titlecol = input$line_leg_titlecol,
        leg_textadj = input$line_leg_textadj)

    })



    # # plot 4
    # output$line_4 <- renderPlot({

    #   line_graph(selected_x(), selected_y(), input$line_style, input$line_type, input$line_width,
    #     input$line_color,title = input$line_title, sub = input$line_subtitle, xlab = input$line_xlabel,
    #     yalb = input$line_ylabel, input$add_points, input$point_shape, input$point_size, input$point_col,
    #     input$point_bg, ylim_l = input$y_range_min, ylim_u = input$y_range_max, extra_lines = input$naddlines,
    #     extra_vars = addvars(), extra_cols = colours(), leg = input$leg_yn, leg_x = input$leg_x, leg_y = input$leg_y,
    #     legend = name(), leg_line = lines(), leg_point = point(), leg_colour = colours(), leg_boxtype = input$leg_boxtype,
    #     leg_boxcol = input$leg_boxcol, leg_boxlty = input$leg_boxlty, leg_boxlwd = input$leg_boxlwd,
    #     leg_boxborcol = input$leg_boxborcol, leg_boxxjust = input$leg_boxxjust, leg_boxyjust = input$leg_boxyjust,
    #     leg_textcol = input$leg_textcol, leg_textfont = input$leg_textfont, leg_textcolumns = input$leg_textcolumns,
    #     leg_texthoriz = input$leg_texthoriz, leg_title = input$leg_title,
    #     leg_titlecol = input$leg_titlecol, leg_textadj = input$leg_textadj
    #   )

    # })


    # # plot 5
    # output$line_5 <- renderPlot({

    #   line_graph(selected_x(), selected_y(), input$line_style, input$line_type, input$line_width,
    #     input$line_color,title = input$line_title, sub = input$line_subtitle, xlab = input$line_xlabel,
    #     yalb = input$line_ylabel, input$add_points, input$point_shape, input$point_size, input$point_col,
    #     input$point_bg, input$l_coltitle, input$l_colsub, input$l_colaxis,
    #         input$l_collabel, input$l_fontmain, input$l_fontsub,
    #         input$l_fontaxis, input$l_fontlab, input$l_cexmain,
    #         input$l_cexsub, input$l_cexaxis, input$l_cexlab,ylim_l = input$y_range_min, ylim_u = input$y_range_max, extra_lines = input$naddlines,
    #     extra_vars = addvars(), extra_cols = colours(), leg = input$leg_yn, leg_x = input$leg_x, leg_y = input$leg_y,
    #     legend = name(), leg_line = lines(), leg_point = point(), leg_colour = colours(), leg_boxtype = input$leg_boxtype,
    #     leg_boxcol = input$leg_boxcol, leg_boxlty = input$leg_boxlty, leg_boxlwd = input$leg_boxlwd,
    #     leg_boxborcol = input$leg_boxborcol, leg_boxxjust = input$leg_boxxjust, leg_boxyjust = input$leg_boxyjust,
    #     leg_textcol = input$leg_textcol, leg_textfont = input$leg_textfont, leg_textcolumns = input$leg_textcolumns,
    #     leg_texthoriz = input$leg_texthoriz, leg_title = input$leg_title,
    #     leg_titlecol = input$leg_titlecol, leg_textadj = input$leg_textadj, input$l_plottext, input$l_text_x_loc, input$l_text_y_loc,
    #         input$l_textcolor, input$l_textfont, input$l_textsize,
    #         input$l_mtextplot, input$l_mtext_side, input$l_mtext_line,
    #         input$l_mtextadj, input$l_mtextcolor, input$l_mtextfont,
    #         input$l_mtextsize
    #   )

    # })


    # # final plot
    # output$line_final <- renderPlot({

    #   line_graph(selected_x(), selected_y(), input$line_style, input$line_type, input$line_width,
    #     input$line_color,title = input$line_title, sub = input$line_subtitle, xlab = input$line_xlabel,
    #     yalb = input$line_ylabel, input$add_points, input$point_shape, input$point_size, input$point_col,
    #     input$point_bg, input$l_coltitle, input$l_colsub, input$l_colaxis,
    #         input$l_collabel, input$l_fontmain, input$l_fontsub,
    #         input$l_fontaxis, input$l_fontlab, input$l_cexmain,
    #         input$l_cexsub, input$l_cexaxis, input$l_cexlab,ylim_l = input$y_range_min, ylim_u = input$y_range_max, extra_lines = input$naddlines,
    #     extra_vars = addvars(), extra_cols = colours(), leg = input$leg_yn, leg_x = input$leg_x, leg_y = input$leg_y,
    #     legend = name(), leg_line = lines(), leg_point = point(), leg_colour = colours(), leg_boxtype = input$leg_boxtype,
    #     leg_boxcol = input$leg_boxcol, leg_boxlty = input$leg_boxlty, leg_boxlwd = input$leg_boxlwd,
    #     leg_boxborcol = input$leg_boxborcol, leg_boxxjust = input$leg_boxxjust, leg_boxyjust = input$leg_boxyjust,
    #     leg_textcol = input$leg_textcol, leg_textfont = input$leg_textfont, leg_textcolumns = input$leg_textcolumns,
    #     leg_texthoriz = input$leg_texthoriz, leg_title = input$leg_title,
    #     leg_titlecol = input$leg_titlecol, leg_textadj = input$leg_textadj, input$l_plottext, input$l_text_x_loc, input$l_text_y_loc,
    #         input$l_textcolor, input$l_textfont, input$l_textsize,
    #         input$l_mtextplot, input$l_mtext_side, input$l_mtext_line,
    #         input$l_mtextadj, input$l_mtextcolor, input$l_mtextfont,
    #         input$l_mtextsize
    #   )

    # })

    # # plot download
    # output$line_downloadGraph <- downloadHandler(

    #     filename <- function() {

    #         paste(input$line_fileName, ".png")
    #     },

    #     content <- function(file) {

    #         png(file)

    #         plot <- line_graph(selected_x(), selected_y(), input$line_style,
    #                            input$line_type, input$line_width, input$line_color,
    #                            input$line_title, input$line_subtitle,
    #                            input$line_xlabel, input$line_ylabel, input$line_coltitle, input$line_colsub, input$line_colaxis,
    #                            input$line_collabel, input$line_fontmain, input$line_fontsub,
    #                            input$line_fontaxis, input$line_fontlab, input$line_cexmain,
    #                            input$line_cexsub, input$line_cexaxis, input$line_cexlab,
    #                            input$line_plottext, input$line_text_x_loc, input$line_text_y_loc,
    #                            input$line_textcolor, input$line_textfont, input$line_textsize,
    #                            input$line_mtextplot, input$line_mtext_side, input$line_mtext_line,
    #                            input$line_mtextadj, input$line_mtextcolor, input$line_mtextfont,
    #                            input$line_mtextsize)

    #         print(plot)

    #         dev.off()

    #     }
    # )
