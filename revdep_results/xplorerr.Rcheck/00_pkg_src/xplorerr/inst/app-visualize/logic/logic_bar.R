source("helper/ubar_plot.R")

    observeEvent(input$finalok, {

        f_data <- final_split$train[, sapply(final_split$train, is.factor)]
        # validate(need(!dim(f_data)[2] == 0, 'No factor variables in the data.'))
        if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "ubar_select",
            choices = names(fdata))
        } else if (dim(f_data)[2] == 0) {
          updateSelectInput(session, 'ubar_select', choices = '', selected = '')
        } else {
          updateSelectInput(session, 'ubar_select', choices = names(f_data))  
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
        updateSelectInput(session, inputId = "ubar_select",
            choices = names(fdata))
        } else if (dim(f_data)[2] == 0) {
          updateSelectInput(session, 'ubar_select', choices = '', selected = '')
        } else {
          updateSelectInput(session, 'ubar_select', choices = names(f_data))  
        }
        

    })

    # selected data
    selectedVar <- reactive({
      req(input$ubar_select)
      bar_data <- final_split$train[, input$ubar_select]    
    })

    # dynamic UI for bar colors
    output$ui_ncolbar <- renderUI({
        ncol <- as.integer(input$ncolbar)
        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
              textInput(paste("n_barcol_", i),
                        label = paste0("Bar ", i, " Color"),
                        value = 'blue')
          })
        }
    })

    colours_bar <- reactive({
        ncol <- as.integer(input$ncolbar)

        if (ncol < 1) {
          colors <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
                          input[[paste("n_barcol_", i)]]
                      }))

          colors <- unlist(collect)
        }
        colors

    })

    # dynamic UI for bar border colors
    output$ui_nborbar <- renderUI({
        ncol <- as.integer(input$nborbar)
        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
              textInput(paste("n_bor_", i),
                        label = paste0("Border Color ", i),
                        value = 'black')
          })
        }
    })

    borders_bar <- reactive({
        ncol <- as.integer(input$nborbar)

        if (ncol < 1) {
          colors <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
                          input[[paste("n_bor_", i)]]
                      }))

          colors <- unlist(collect)
        }
        colors
    })


    # dynamic UI for bar labels
    output$ui_nbarlabel <- renderUI({
        ncol <- as.integer(input$nbarlabel)
        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
              textInput(paste("n_barlabel_", i),
                        label = paste0("Bar ", i, " Label"))
          })
        }
    })

    labels_bar <- reactive({
        ncol <- as.integer(input$nbarlabel)
        if (ncol < 1) {
          colors <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
                          input[[paste("n_barlabel_", i)]]
                      }))
          colors <- unlist(collect)
        }
        colors
    })

    # dynamic UI for bar width
    output$ui_nbarwidth <- renderUI({
        ncol <- as.integer(input$nbarwidth)
        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
              numericInput(paste("n_barwidth_", i),
                        label = paste0("Bar ", i, " Width"),
                        value = 1, min = 1)
          })
        }
    })

    widths_bar <- reactive({
        ncol <- as.integer(input$nbarwidth)
        if (ncol < 1) {
          colors <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
                          input[[paste("n_barwidth_", i)]]
                      }))
          colors <- unlist(collect)
        }
        colors
    })

    # dynamic UI for legend names
    output$ui_legnames <- renderUI({
        ncol <- as.integer(input$leg_names)

        lapply(1:ncol, function(i) {
            textInput(paste("n_names_", i),
                      label = paste0("Legend Name ", i))
        })
    })


    # dynamic UI for legend border
    output$ui_legpoint <- renderUI({
        ncol <- as.integer(input$leg_point)

        lapply(1:ncol, function(i) {
            numericInput(paste("n_point_", i),
                      label = paste0("Legend Point ", i), value = 1)
        })
    })

    # vector of legend names
    name_bar <- reactive({
        ncol <- as.integer(input$leg_names)

        collect <- list(lapply(1:ncol, function(i) {
            input[[paste("n_names_", i)]]
        }))

        names <- unlist(collect)

    })



    # vector of point types
    point_bar <- reactive({
        ncol <- as.integer(input$leg_point)

        collect <- list(lapply(1:ncol, function(i) {
            input[[paste("n_point_", i)]]
        }))

        names <- unlist(collect)

    })




    # bar plot

    output$ubar_plot_1 <- renderPlot({

        bar_plotu(
            x = selectedVar(), horizontal = input$ubar_horiz, title = input$ubar_title, xlab = input$ubar_xlabel,
            space = input$ubar_barspace, ylab = input$ubar_ylabel
        )
    })

    output$ubar_plot_2 <- renderPlot({

        bar_plotu(
            x = selectedVar(), horizontal = input$ubar_horiz, color = colours_bar(),
            border = borders_bar(), title = input$ubar_title, xlab = input$ubar_xlabel, labels = labels_bar(),
            space = input$ubar_barspace, width = widths_bar(), ylab = input$ubar_ylabel
        )
    })

    output$ubar_plot_3 <- renderPlot({

            bar_plotu(
            selectedVar(), input$ubar_horiz, colours_bar(),
            borders_bar(), input$ubar_title, input$ubar_xlabel, labels_bar(),
            input$ubar_barspace, widths_bar(), input$ubar_axes,
            input$ubar_axislty, input$ubar_offset, input$ubar_ylabel
        )
    })

    output$ubar_plot_4 <- renderPlot({

        bar_plotu(
            selectedVar(), input$ubar_horiz, colours_bar(),
            borders_bar(), input$ubar_title, input$ubar_xlabel, labels_bar(),
            input$ubar_barspace, widths_bar(), input$ubar_axes,
            input$ubar_axislty, input$ubar_offset, input$ubar_ylabel,
            leg = input$leg_yn, leg_x = input$leg_x, leg_y = input$leg_y, legend = name_bar(),
            leg_point = point_bar(), leg_colour = colours_bar(), leg_boxtype = input$leg_boxtype,
            leg_boxcol = input$leg_boxcol, leg_boxlty = input$leg_boxlty, leg_boxlwd = input$leg_boxlwd,
            leg_boxborcol = input$leg_boxborcol, leg_boxxjust = input$leg_boxxjust, leg_boxyjust = input$leg_boxyjust,
            leg_textcol = input$leg_textcol, leg_textfont = input$leg_textfont, leg_textcolumns = input$leg_textcolumns,
            leg_texthoriz = input$leg_texthoriz, leg_title = input$leg_title,
            leg_titlecol = input$leg_titlecol, leg_textadj = input$leg_textadj
        )
    })

    output$ubar_plot_5 <- renderPlot({

        bar_plotu(
            selectedVar(), input$ubar_horiz, colours_bar(),
            borders_bar(), input$ubar_title, input$ubar_xlabel, labels_bar(),
            input$ubar_barspace, widths_bar(), input$ubar_axes,
            input$ubar_axislty, input$ubar_offset, input$ubar_ylabel,
            input$ubar_coltitle, input$ubar_colsub, input$ubar_colaxis,
            input$ubar_collabel, input$ubar_fontmain, input$ubar_fontsub,
            input$ubar_fontaxis, input$ubar_fontlab, input$ubar_cexmain,
            input$ubar_cexsub, input$ubar_cexaxis, input$ubar_cexlab, input$leg_yn,
            input$leg_x, input$leg_y, name_bar(), point_bar(), colours_bar(),
            input$leg_boxtype, input$leg_boxcol,
            input$leg_boxlty, input$leg_boxlwd, input$leg_boxborcol, input$leg_boxxjust,
            input$leg_boxyjust, input$leg_textcol, input$leg_textfont, input$leg_textcolumns,
            input$leg_texthoriz, input$leg_title, input$leg_titlecol, input$leg_textadj,
            input$ubar_plottext, input$ubar_text_x_loc, input$ubar_text_y_loc,
            input$ubar_textcolor, input$ubar_textfont, input$ubar_textsize,
            input$ubar_mtextplot, input$ubar_mtext_side, input$ubar_mtext_line,
            input$ubar_mtextadj, input$ubar_mtextcolor, input$ubar_mtextfont,
            input$ubar_mtextsize
        )
    })


    output$ubar_plot_final <- renderPlot({

        bar_plotu(
            selectedVar(), input$ubar_horiz, colours_bar(),
            borders_bar(), input$ubar_title, input$ubar_xlabel, labels_bar(),
            input$ubar_barspace, widths_bar(), input$ubar_axes,
            input$ubar_axislty, input$ubar_offset, input$ubar_ylabel,
            input$ubar_coltitle, input$ubar_colsub, input$ubar_colaxis,
            input$ubar_collabel, input$ubar_fontmain, input$ubar_fontsub,
            input$ubar_fontaxis, input$ubar_fontlab, input$ubar_cexmain,
            input$ubar_cexsub, input$ubar_cexaxis, input$ubar_cexlab, input$leg_yn,
            input$leg_x, input$leg_y, name_bar(), point_bar(), colours_bar(),
            input$leg_boxtype, input$leg_boxcol,
            input$leg_boxlty, input$leg_boxlwd, input$leg_boxborcol, input$leg_boxxjust,
            input$leg_boxyjust, input$leg_textcol, input$leg_textfont, input$leg_textcolumns,
            input$leg_texthoriz, input$leg_title, input$leg_titlecol, input$leg_textadj,
            input$ubar_plottext, input$ubar_text_x_loc, input$ubar_text_y_loc,
            input$ubar_textcolor, input$ubar_textfont, input$ubar_textsize,
            input$ubar_mtextplot, input$ubar_mtext_side, input$ubar_mtext_line,
            input$ubar_mtextadj, input$ubar_mtextcolor, input$ubar_mtextfont,
            input$ubar_mtextsize
        )
    })
