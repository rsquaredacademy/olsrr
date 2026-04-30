library(shiny)
source("helper/ubox-plot.R")


    # update variable selection for bar plots
    observe({
        updateSelectInput(session, 'ubox_select', choices = names(data()))
    })

    observeEvent(input$finalok, {

        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'ubox_select',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
            updateSelectInput(session, 'ubox_select',
              choices = '', selected = '')
        } else {
            updateSelectInput(session, 'ubox_select', choices = names(num_data))
        }
    })

    observeEvent(input$submit_part_train_per, {

        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'ubox_select',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
            updateSelectInput(session, 'ubox_select',
              choices = '', selected = '')
        } else {
            updateSelectInput(session, 'ubox_select', choices = names(num_data))
        }
    })

    # selected data
    ubox_data <- reactive({
      req(input$ubox_select)
      box_data <- final_split$train[, input$ubox_select]
    })

    # bar plot*
    output$ubox_plot_1 <- renderPlot({

        box_plotu(ubox_data(), input$ubox_title, input$ubox_xlabel, input$ubox_ylabel,
            input$ubox_colour, input$ubox_borcolour)

    })

    output$ubox_plot_2 <- renderPlot({

        box_plotu(ubox_data(), input$ubox_title, input$ubox_xlabel, input$ubox_ylabel,
            input$ubox_colour, input$ubox_borcolour, as.logical(input$ubox_horiz),
            as.logical(input$ubox_notch), input$ubox_range,
            as.logical(input$ubox_outline), as.logical(input$ubox_varwidth))

    })

    output$ubox_plot_3 <- renderPlot({

        box_plotu(ubox_data(), input$ubox_title, input$ubox_xlabel, input$ubox_ylabel,
            input$ubox_colour, input$ubox_borcolour, as.logical(input$ubox_horiz),
            as.logical(input$ubox_notch), input$ubox_range,
            as.logical(input$ubox_outline), as.logical(input$ubox_varwidth),
            text_p = input$ubox_plottext, text_x_loc = input$ubox_text_x_loc, text_y_loc =
            input$ubox_text_y_loc, text_col = input$ubox_textcolor, text_font = input$ubox_textfont,
            text_size = input$ubox_textsize, m_text = input$ubox_mtextplot, m_side = input$ubox_mtext_side,
            m_line = input$ubox_mtext_line, m_adj = input$ubox_mtextadj, m_col = input$ubox_mtextcolor,
            m_font = input$ubox_mtextfont, m_cex = input$ubox_mtextsize)

    })


    # bar plot
    output$ubox_plot_4 <- renderPlot({

        box_plotu(
            ubox_data(), input$ubox_title, input$ubox_xlabel, input$ubox_ylabel,
            input$ubox_colour, input$ubox_borcolour, as.logical(input$ubox_horiz),
            as.logical(input$ubox_notch), input$ubox_range,
            as.logical(input$ubox_outline), as.logical(input$ubox_varwidth),
            input$ubox_coltitle, input$ubox_colsub, input$ubox_colaxis,
            input$ubox_collabel, input$ubox_fontmain, input$ubox_fontsub,
            input$ubox_fontaxis, input$ubox_fontlab, input$ubox_cexmain,
            input$ubox_cexsub, input$ubox_cexaxis, input$ubox_cexlab,
            text_p = input$ubox_plottext, text_x_loc = input$ubox_text_x_loc, text_y_loc =
            input$ubox_text_y_loc, text_col = input$ubox_textcolor, text_font = input$ubox_textfont,
            text_size = input$ubox_textsize, m_text = input$ubox_mtextplot, m_side = input$ubox_mtext_side,
            m_line = input$ubox_mtext_line, m_adj = input$ubox_mtextadj, m_col = input$ubox_mtextcolor,
            m_font = input$ubox_mtextfont, m_cex = input$ubox_mtextsize
        )
    })

    # bar plot
    output$ubox_plot_final <- renderPlot({

        box_plotu(
            ubox_data(), input$ubox_title, input$ubox_xlabel, input$ubox_ylabel,
            input$ubox_colour, input$ubox_borcolour, as.logical(input$ubox_horiz),
            as.logical(input$ubox_notch), input$ubox_range,
            as.logical(input$ubox_outline), as.logical(input$ubox_varwidth),
            input$ubox_coltitle, input$ubox_colsub, input$ubox_colaxis,
            input$ubox_collabel, input$ubox_fontmain, input$ubox_fontsub,
            input$ubox_fontaxis, input$ubox_fontlab, input$ubox_cexmain,
            input$ubox_cexsub, input$ubox_cexaxis, input$ubox_cexlab,
            text_p = input$ubox_plottext, text_x_loc = input$ubox_text_x_loc, text_y_loc =
            input$ubox_text_y_loc, text_col = input$ubox_textcolor, text_font = input$ubox_textfont,
            text_size = input$ubox_textsize, m_text = input$ubox_mtextplot, m_side = input$ubox_mtext_side,
            m_line = input$ubox_mtext_line, m_adj = input$ubox_mtextadj, m_col = input$ubox_mtextcolor,
            m_font = input$ubox_mtextfont, m_cex = input$ubox_mtextsize
        )
    })

    # plot download
    # output$box_downloadGraph <- downloadHandler(

    #     filename <- function() {

    #         paste(input$box_fileName, ".png")
    #     },

    #     content <- function(file) {

    #         png(file)

    #         plot <- box_plot(
    #             box_plotu(
    #                 selectedVar(),
    #                 input$box_col, input$box_bordercol, input$box_title,
    #                 input$box_subtitle, input$box_xlabel, input$box_ylabel,
    #                 input$box_coltitle, input$box_colsub, input$box_colaxis,
    #                 input$box_collabel, input$box_fontmain, input$box_fontsub,
    #                 input$box_fontaxis, input$box_fontlab, input$box_cexmain,
    #                 input$box_cexsub, input$box_cexaxis, input$box_cexlab,
    #                 input$box_plottext, input$box_text_x_loc, input$box_text_y_loc,
    #                 input$box_textcolor, input$box_textfont, input$box_textsize,
    #                 input$box_mtextplot, input$box_mtext_side, input$box_mtext_line,
    #                 input$box_mtextadj, input$box_mtextcolor, input$box_mtextfont,
    #                 input$box_mtextsize
    #             )
    #         )

    #         print(plot)

    #         dev.off()

    #     }
    # )
