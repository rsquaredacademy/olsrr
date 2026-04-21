source("helper/pie3d-plot.R")

    # update variable selection for bar plots
    observe({
        updateSelectInput(session, 'pie3_select', choices = names(data()))
        updateSelectInput(session, 'pie3_label', choices = names(data()))
    })

    observeEvent(input$finalok, {

        f_data <- final_split$train[, sapply(final_split$train, is.factor)]
        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "pie3_select",
            choices = names(fdata))
        updateSelectInput(session, inputId = "pie3_label",
            choices = names(fdata))
        } else {
          updateSelectInput(session, 'pie3_select', choices = names(f_data))
          updateSelectInput(session, 'pie3_label', choices = names(f_data))
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
        updateSelectInput(session, inputId = "pie3_select",
            choices = names(fdata))
        updateSelectInput(session, inputId = "pie3_label",
            choices = names(fdata))
        } else {
          updateSelectInput(session, 'pie3_select', choices = names(f_data))
          updateSelectInput(session, 'pie3_label', choices = names(f_data))
        }

    })

    # selected data
    pie3_data <- reactive({
      req(input$pie3_select)
      out <- table(final_split$train[, input$pie3_select])
    })

    pie3_labels <- reactive({
        out <- final_split$train[, input$pie3_label]
        labs <- levels(out)
    })

    # dynamic UI for bar colors
    output$ui_ncolpie3 <- renderUI({
        ncol <- as.integer(input$ncolpie3)
        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
              textInput(paste("n_pie3col_", i),
                        label = paste0("Color ", i),
                        value = 'blue')
          })
        }
    })

    colours_pie3 <- reactive({
        ncol <- as.integer(input$ncolpie3)
        if (ncol < 1) {
          colors <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
                          input[[paste("n_pie3col_", i)]]
                      }))
          colors <- unlist(collect)
        }
        colors
    })

    # dynamic UI for label positions
    output$ui_nlabpospie3 <- renderUI({
        ncol <- as.integer(input$nlabpospie3)
        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
              textInput(paste("n_pie3labpos_", i),
                        label = paste0("n_labpos_pie3", i))
          })
        }
    })

    labpos_pie3 <- reactive({
        ncol <- as.integer(input$nlabpospie3)
        if (ncol < 1) {
          colors <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
                          input[[paste("n_pie3labpos_", i)]]
                      }))
          colors <- unlist(collect)
        }
        colors
    })


    output$pie3_1 <- renderPlot({
        pie3_plot(pie3_data(), rad = input$pie3_radius,
            lab = pie3_labels(), high = input$pie3_height
        )
    })

    output$pie3_2 <- renderPlot({
        pie3_plot(pie3_data(), rad = input$pie3_radius, lab = pie3_labels(), high = input$pie3_height,
            bord = input$pie3_border, begin = input$pie3_start, explo = input$pie3_explode,
            shd = input$pie3_shade, edg = input$pie3_edges
        )
    })

    output$pie3_3 <- renderPlot({
        pie3_plot(pie3_data(), rad = input$pie3_radius, lab = pie3_labels(), high = input$pie3_height,
            bord = input$pie3_border, begin = input$pie3_start, explo = input$pie3_explode,
            shd = input$pie3_shade, edg = input$pie3_edges, colors = colours_pie3()
        )
    })

    output$pie3_4 <- renderPlot({
        pie3_plot(pie3_data(), rad = input$pie3_radius, lab = pie3_labels(), high = input$pie3_height,
            bord = input$pie3_border, begin = input$pie3_start, explo = input$pie3_explode,
            shd = input$pie3_shade, edg = input$pie3_edges, colors = colours_pie3(),
            labcol = input$pie3_labcol, labcex = input$pie3_labcex, labrad = input$pie3_labrad
        )
    })

    output$pie3_5 <- renderPlot({
        pie3_plot(pie3_data(), rad = input$pie3_radius, lab = pie3_labels(), high = input$pie3_height,
            bord = input$pie3_border, begin = input$pie3_start, explo = input$pie3_explode,
            shd = input$pie3_shade, edg = input$pie3_edges, colors = colours_pie3(),
            labcol = input$pie3_labcol, labcex = input$pie3_labcex, labrad = input$pie3_labrad,
            title = input$pie3_title, colmain = input$pie3_titlecol, fontmain = input$pie3_font,
            cexmain = input$pie3_cex
        )
    })

    output$pie3_final <- renderPlot({
        pie3_plot(pie3_data(), rad = input$pie3_radius, lab = pie3_labels(), high = input$pie3_height,
            bord = input$pie3_border, begin = input$pie3_start, explo = input$pie3_explode,
            shd = input$pie3_shade, edg = input$pie3_edges, colors = colours_pie3(),
            labcol = input$pie3_labcol, labcex = input$pie3_labcex, labrad = input$pie3_labrad,
            title = input$pie3_title, colmain = input$pie3_titlecol, fontmain = input$pie3_font,
            cexmain = input$pie3_cex
        )
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
