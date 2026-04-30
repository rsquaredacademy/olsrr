library(shiny)
source("helper/histogram.R")
source("helper/freq-cont.R")


    # update variable selection for bar plots
    # observe({
    #     updateSelectInput(session, 'hist_select', choices = names(data()))
    # })

    observeEvent(input$finalok, {

        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
             updateSelectInput(session, 'hist_select',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'hist_select',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'hist_select', choices = names(num_data))
        }
    })

    observeEvent(input$submit_part_train_per, {

        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
             updateSelectInput(session, 'hist_select',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'hist_select',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'hist_select', choices = names(num_data))
        }
    })

    # selected data
    hist_data <- reactive({
        req(input$hist_select)
        box_data <- final_split$train[, input$hist_select]
        box_data <- as.data.frame(box_data)
        names(box_data) <- as.character(input$hist_select)
        box_data
    })

    # dynamic UI for histogram colors
    output$ui_ncolhist <- renderUI({
        ncol <- as.integer(input$ncolhist)
        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
              textInput(paste("n_col_hist", i),
                        label = paste0("Color ", i),
                        value = 'blue')
          })
        }
    })

    colours_hist <- reactive({
        ncol <- as.integer(input$ncolhist)
        if (ncol < 1) {
          colors <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
                          input[[paste("n_col_hist", i)]]
                      }))
          colors <- unlist(collect)
        }
        colors
    })

    # dynamic UI for histogram border colors
    output$ui_nborhist <- renderUI({
        ncol <- as.integer(input$nborhist)
        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
              textInput(paste("n_bor_hist", i),
                        label = paste0("Border Color ", i),
                        value = 'black')
          })
        }
    })

    borders_hist <- reactive({
        ncol <- as.integer(input$nborhist)
        if (ncol < 1) {
          colors <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
                          input[[paste("n_bor_hist", i)]]
                      }))
          colors <- unlist(collect)
        }
        colors
    })

    # # dynamic UI for shading density
    # output$ui_nhistdensity <- renderUI({
    #     ncol <- as.integer(input$nhistdensity)

    #     lapply(1:ncol, function(i) {
    #         numericInput(paste("n_histdensity_", i),
    #                   label = paste0("Density ", i),
    #                   value = 1, min = 1)
    #     })
    # })

    # density_hist <- reactive({
    #     ncol <- as.integer(input$nhistdensity)

    #     collect <- list(lapply(1:ncol, function(i) {
    #                     input[[paste("n_histdensity_", i)]]
    #                 }))

    #     colors <- unlist(collect)

    # })

    # # dynamic UI for shading angle
    # output$ui_nhistangle <- renderUI({
    #     ncol <- as.integer(input$nhistangle)

    #     lapply(1:ncol, function(i) {
    #         numericInput(paste("n_histangle_", i),
    #                   label = paste0("Density Angle ", i),
    #                   value = 1, min = 1)
    #     })
    # })

    # angle_hist <- reactive({
    #     ncol <- as.integer(input$nhistangle)

    #     collect <- list(lapply(1:ncol, function(i) {
    #                     input[[paste("n_histangle_", i)]]
    #                 }))

    #     colors <- unlist(collect)

    # })

    # dynamic UI for histogram binning intervals
    output$ui_nbin_intervals <- renderUI({
        ncol <- as.integer(input$bin_intervals) + 1

        lapply(1:ncol, function(i) {
            numericInput(paste("n_bin_int", i),
                      label = paste0("Int ", i), value = 1)
        })
    })

    binints <- reactive({
        ncol <- as.integer(input$bin_intervals) + 1

        collect <- list(lapply(1:ncol, function(i) {
            input[[paste("n_bin_int", i)]]
        }))

        colors <- unlist(collect)

    })

    # dynamic UI for legend names
    output$ui_hist_legnames <- renderUI({
        ncol <- as.integer(input$hist_leg_names)

        lapply(1:ncol, function(i) {
            textInput(paste("n_nameshist_", i),
                      label = paste0("Legend Name ", i))
        })
    })


    # dynamic UI for legend border
    output$ui_hist_legpoint <- renderUI({
        ncol <- as.integer(input$hist_leg_point)

        lapply(1:ncol, function(i) {
            numericInput(paste("n_pointhist_", i),
                      label = paste0("Legend Point ", i), value = 1)
        })
    })

    # vector of legend names
    name_hist <- reactive({
        ncol <- as.integer(input$hist_leg_names)

        collect <- list(lapply(1:ncol, function(i) {
            input[[paste("n_nameshist_", i)]]
        }))

        names <- unlist(collect)

    })



    # vector of point types
    point_hist <- reactive({
        ncol <- as.integer(input$hist_leg_point)

        collect <- list(lapply(1:ncol, function(i) {
            input[[paste("n_pointhist_", i)]]
        }))

        names <- unlist(collect)

    })

    # frequency table
    h <- reactive({
        freq_cont(hist_data(), as.character(input$hist_select), input$nbins)
    })

    binner <- reactive({

        if (input$bin_opt == 'Bins') {
            binners <- h()$bins
        } else if (input$bin_opt == 'Algorithms') {
            binners <- input$alg_options
        } else {
            binners <- binints()
        }

        return(binners)

    })

    ymax <- reactive({
        if(input$hist_frequency) {
            max(hist(h()$data, probability = T)$density) + 0.02
        } else {
            max(h()$cumulative) + 2
        }
    })

    # histogram
    output$hist_1 <- renderPlot({

        hist_plot(h()$data, bins = h()$bins, title = input$hist_title,
            xlab = input$hist_xlabel, ylab = input$hist_ylabel, ylimit = c(0, ymax()),
            probability = input$hist_frequency, right = input$hist_interval,
            axes = input$hist_hideaxes, labels = as.logical(input$hist_showlabels))

    })

    output$hist_2 <- renderPlot({

        hist_plot(h()$dat, binner(), input$hist_title,
            input$hist_xlabel, input$hist_ylabel, ylimit = c(0, ymax()),
            input$hist_frequency, input$hist_interval,
            input$hist_hideaxes, as.logical(input$hist_showlabels))

    })

    output$hist_3 <- renderPlot({

        hist_plot(h()$dat, binner(), input$hist_title,
            input$hist_xlabel, input$hist_ylabel, ylimit = c(0, ymax()),
            input$hist_frequency, input$hist_interval,
            input$hist_hideaxes, as.logical(input$hist_showlabels),
            colours_hist(), borders_hist())

    })

    output$hist_4 <- renderPlot({

        hist_plot(h()$dat, binner(), input$hist_title,
            input$hist_xlabel, input$hist_ylabel, ylimit = c(0, ymax()),
            input$hist_frequency, input$hist_interval,
            input$hist_hideaxes, as.logical(input$hist_showlabels),
            colours_hist(), borders_hist(),
            leg = input$hist_leg_yn, leg_x = input$hist_leg_x, leg_y = input$hist_leg_y,
            legend = name_hist(), leg_point = point_hist(), leg_colour = colours_hist(), leg_boxtype = input$hist_leg_boxtype,
            leg_boxcol = input$hist_leg_boxcol, leg_boxlty = input$hist_leg_boxlty, leg_boxlwd = input$hist_leg_boxlwd,
            leg_boxborcol = input$hist_leg_boxborcol, leg_boxxjust = input$hist_leg_boxxjust, leg_boxyjust = input$hist_leg_boxyjust,
            leg_textcol = input$hist_leg_textcol, leg_textfont = input$hist_leg_textfont, leg_textcolumns = input$hist_leg_textcolumns,
            leg_texthoriz = input$hist_leg_texthoriz, leg_title = input$hist_leg_title,
            leg_titlecol = input$hist_leg_titlecol, leg_textadj = input$hist_leg_textadj)

    })


    output$hist_5 <- renderPlot({

        hist_plot(h()$dat, binner(), input$hist_title,
            input$hist_xlabel, input$hist_ylabel, ylimit = c(0, ymax()),
            input$hist_frequency, input$hist_interval,
            input$hist_hideaxes, as.logical(input$hist_showlabels),
            colours_hist(), borders_hist(),
            leg = input$hist_leg_yn, leg_x = input$hist_leg_x, leg_y = input$hist_leg_y,
            legend = name_hist(), leg_point = point_hist(), leg_colour = colours_hist(), leg_boxtype = input$hist_leg_boxtype,
            leg_boxcol = input$hist_leg_boxcol, leg_boxlty = input$hist_leg_boxlty, leg_boxlwd = input$hist_leg_boxlwd,
            leg_boxborcol = input$hist_leg_boxborcol, leg_boxxjust = input$hist_leg_boxxjust, leg_boxyjust = input$hist_leg_boxyjust,
            leg_textcol = input$hist_leg_textcol, leg_textfont = input$hist_leg_textfont, leg_textcolumns = input$hist_leg_textcolumns,
            leg_texthoriz = input$hist_leg_texthoriz, leg_title = input$hist_leg_title,
            leg_titlecol = input$hist_leg_titlecol, leg_textadj = input$hist_leg_textadj,
            text_p = input$bbox_plottext, text_x_loc = input$bbox_text_x_loc,
            text_y_loc = input$bbox_text_y_loc, text_col = input$bbox_textcolor, text_font = input$bbox_textfont,
            text_size = input$bbox_textsize, m_text = input$bbox_mtextplot, m_side = input$bbox_mtext_side,
            m_line = input$bbox_mtext_line, m_adj = input$bbox_mtextadj, m_col = input$bbox_mtextcolor,
            m_font = input$bbox_mtextfont, m_cex = input$bbox_mtextsize)

    })

    output$hist_6 <- renderPlot({

        hist_plot(h()$dat, binner(), input$hist_title,
            input$hist_xlabel, input$hist_ylabel, ylimit = c(0, ymax()),
            input$hist_frequency, input$hist_interval,
            input$hist_hideaxes, as.logical(input$hist_showlabels),
            colours_hist(), borders_hist(),
            input$hist_coltitle, input$hist_colsub, input$hist_colaxis,
            input$hist_collabel, fontmain = input$hist_fontmain, fontsub = input$hist_fontsub,
            fontaxis = input$hist_fontaxis, fontlab = input$hist_fontlab, input$hist_cexmain,
            input$hist_cexsub, input$hist_cexaxis, input$hist_cexlab,
            leg = input$hist_leg_yn, leg_x = input$hist_leg_x, leg_y = input$hist_leg_y,
            legend = name_hist(), leg_point = point_hist(), leg_colour = colours_hist(), leg_boxtype = input$hist_leg_boxtype,
            leg_boxcol = input$hist_leg_boxcol, leg_boxlty = input$hist_leg_boxlty, leg_boxlwd = input$hist_leg_boxlwd,
            leg_boxborcol = input$hist_leg_boxborcol, leg_boxxjust = input$hist_leg_boxxjust, leg_boxyjust = input$hist_leg_boxyjust,
            leg_textcol = input$hist_leg_textcol, leg_textfont = input$hist_leg_textfont, leg_textcolumns = input$hist_leg_textcolumns,
            leg_texthoriz = input$hist_leg_texthoriz, leg_title = input$hist_leg_title,
            leg_titlecol = input$hist_leg_titlecol, leg_textadj = input$hist_leg_textadj,
            text_p = input$bbox_plottext, text_x_loc = input$bbox_text_x_loc,
            text_y_loc = input$bbox_text_y_loc, text_col = input$bbox_textcolor, text_font = input$bbox_textfont,
            text_size = input$bbox_textsize, m_text = input$bbox_mtextplot, m_side = input$bbox_mtext_side,
            m_line = input$bbox_mtext_line, m_adj = input$bbox_mtextadj, m_col = input$bbox_mtextcolor,
            m_font = input$bbox_mtextfont, m_cex = input$bbox_mtextsize)

    })



    output$hist_final <- renderPlot({

        hist_plot(h()$dat, binner(), input$hist_title,
            input$hist_xlabel, input$hist_ylabel, ylimit = c(0, ymax()),
            input$hist_frequency, input$hist_interval,
            input$hist_hideaxes, as.logical(input$hist_showlabels),
            colours_hist(), borders_hist(),
            input$hist_coltitle, input$hist_colsub, input$hist_colaxis,
            input$hist_collabel, fontmain = input$hist_fontmain, fontsub = input$hist_fontsub,
            fontaxis = input$hist_fontaxis, fontlab = input$hist_fontlab, input$hist_cexmain,
            input$hist_cexsub, input$hist_cexaxis, input$hist_cexlab,
            leg = input$hist_leg_yn, leg_x = input$lhist_leg_x, leg_y = input$hist_leg_y,
            legend = name_hist(), leg_point = point_hist(), leg_colour = colours_hist(), leg_boxtype = input$hist_leg_boxtype,
            leg_boxcol = input$hist_leg_boxcol, leg_boxlty = input$hist_leg_boxlty, leg_boxlwd = input$hist_leg_boxlwd,
            leg_boxborcol = input$hist_leg_boxborcol, leg_boxxjust = input$hist_leg_boxxjust, leg_boxyjust = input$hist_leg_boxyjust,
            leg_textcol = input$hist_leg_textcol, leg_textfont = input$hist_leg_textfont, leg_textcolumns = input$hist_leg_textcolumns,
            leg_texthoriz = input$hist_leg_texthoriz, leg_title = input$hist_leg_title,
            leg_titlecol = input$hist_leg_titlecol, leg_textadj = input$hist_leg_textadj,
            text_p = input$bbox_plottext, text_x_loc = input$bbox_text_x_loc,
            text_y_loc = input$bbox_text_y_loc, text_col = input$bbox_textcolor, text_font = input$bbox_textfont,
            text_size = input$bbox_textsize, m_text = input$bbox_mtextplot, m_side = input$bbox_mtext_side,
            m_line = input$bbox_mtext_line, m_adj = input$bbox_mtextadj, m_col = input$bbox_mtextcolor,
            m_font = input$bbox_mtextfont, m_cex = input$bbox_mtextsize)
    })
