source("helper/pie-plot.R")

    # update variable selection for bar plots
    observe({
        updateSelectInput(session, 'pie_select', choices = names(data()))
        updateSelectInput(session, 'pie_label', choices = names(data()))
    })

    observeEvent(input$finalok, {

        f_data <- final_split$train[, sapply(final_split$train, is.factor)]
        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "pie_select",
            choices = names(fdata))
        updateSelectInput(session, inputId = "pie_label",
            choices = names(fdata))
        } else {
          updateSelectInput(session, 'pie_select', choices = names(f_data))
          updateSelectInput(session, 'pie_label', choices = names(f_data))
        }
    })


    observeEvent(input$submit_part_train_per, {

        f_data <- final_split$train[, sapply(final_split$train, is.factor)]
        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "pie_select",
            choices = names(fdata))
        updateSelectInput(session, inputId = "pie_label",
            choices = names(fdata))
        } else {
          updateSelectInput(session, 'pie_select', choices = names(f_data))
          updateSelectInput(session, 'pie_label', choices = names(f_data))
        }
    })

    # selected data
    pie_data <- reactive({
      req(input$pie_select)
      out <- table(final_split$train[, input$pie_select])
    })

    pie_labels <- reactive({
        out <- final_split$train[, input$pie_label]
        labs <- levels(out)
    })

    # dynamic UI for bar colors
    output$ui_ncolpie <- renderUI({
        ncol <- as.integer(input$ncolpie)
        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
              textInput(paste("n_piecol_", i),
                        label = paste0("Color ", i),
                        value = 'blue')
          })
        }
    })

    colours_pie <- reactive({
        ncol <- as.integer(input$ncolpie)
        if (ncol < 1) {
          colors <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
                          input[[paste("n_piecol_", i)]]
                      }))
          colors <- unlist(collect)
        }
        colors
    })


    output$pie_1 <- renderPlot({
        pie_plot(pie_data(), clock = input$pie_clock, rad = input$pie_radius, initangle = input$pie_angle,
            lab = pie_labels(), edg = input$pie_edges
        )
    })

    output$pie_2 <- renderPlot({
      if (is.null(colours_pie())) {
        pie_plot(pie_data(), clock = input$pie_clock, rad = input$pie_radius, initangle = input$pie_angle,
            lab = pie_labels(), edg = input$pie_edges, bord = input$pie_border, ltype = input$pie_lty,
            den = input$pie_density, ang = input$pie_dangle, colors = colours_pie()
        )
      } else {
        pie_plot(pie_data(), clock = input$pie_clock, rad = input$pie_radius, initangle = input$pie_angle,
            lab = pie_labels(), edg = input$pie_edges, bord = input$pie_border, ltype = input$pie_lty,
            den = NULL, ang = input$pie_dangle, colors = colours_pie()
        )
      }

    })

    output$pie_3 <- renderPlot({
      if (is.null(colours_pie())) {
        pie_plot(pie_data(), clock = input$pie_clock, rad = input$pie_radius, initangle = input$pie_angle,
            lab = pie_labels(), edg = input$pie_edges,
            bord = input$pie_border, ltype = input$pie_lty, den = input$pie_density, ang = input$pie_dangle,
            colors = colours_pie(), title = input$pie_title, colmain = input$pie_titlecol,
            fontmain = input$pie_font, cexmain = input$pie_cex
        )
      } else {
        pie_plot(pie_data(), clock = input$pie_clock, rad = input$pie_radius,
          initangle = input$pie_angle, lab = pie_labels(), edg = input$pie_edges,
            bord = input$pie_border, ltype = input$pie_lty, den = NULL,
            ang = input$pie_dangle, colors = colours_pie(), title = input$pie_title,
            colmain = input$pie_titlecol, fontmain = input$pie_font, cexmain = input$pie_cex
        )
      }
    })

    output$pie_final <- renderPlot({
      if (is.null(colours_pie())) {
        pie_plot(pie_data(), clock = input$pie_clock, rad = input$pie_radius, initangle = input$pie_angle,
            lab = pie_labels(), edg = input$pie_edges,
            bord = input$pie_border, ltype = input$pie_lty, den = input$pie_density, ang = input$pie_dangle,
            colors = colours_pie(), title = input$pie_title, colmain = input$pie_titlecol,
            fontmain = input$pie_font, cexmain = input$pie_cex
        )
      } else {
        pie_plot(pie_data(), clock = input$pie_clock, rad = input$pie_radius,
          initangle = input$pie_angle, lab = pie_labels(), edg = input$pie_edges,
          bord = input$pie_border, ltype = input$pie_lty, den = NULL,
          ang = input$pie_dangle, colors = colours_pie(), title = input$pie_title,
          colmain = input$pie_titlecol, fontmain = input$pie_font, cexmain = input$pie_cex
        )
      }
    })

    # plot download
    output$ubar_downloadGraph <- downloadHandler(

        filename <- function() {

            paste(input$ubar_fileName, ".png")
        },

        content <- function(file) {

            png(file)

            plot <-  bar_plotb(
                counts(), input$ubar_horiz,
                input$ubar_beside, input$ubar_barspace, input$ubar_title,
                input$ubar_subtitle, input$ubar_xlabel, input$ubar_ylabel,
                input$ubar_coltitle, input$ubar_colsub, input$ubar_colaxis,
                input$ubar_collabel, input$ubar_fontmain, input$ubar_fontsub,
                input$ubar_fontaxis, input$ubar_fontlab, input$ubar_cexmain,
                input$ubar_cexsub, input$ubar_cexaxis, input$ubar_cexlab,
                input$ubar_plottext, input$ubar_text_x_loc, input$ubar_text_y_loc,
                input$ubar_textcolor, input$ubar_textfont, input$ubar_textsize,
                input$ubar_mtextplot, input$ubar_mtext_side, input$ubar_mtext_line,
                input$ubar_mtextadj, input$ubar_mtextcolor, input$ubar_mtextfont,
                input$ubar_mtextsize
            )

            print(plot)

            dev.off()

        }
    )
