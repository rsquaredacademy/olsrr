library(shiny)
source("helper/bbox-plot.R")


    # update variable selection for bar plots
    observe({
        updateSelectInput(session, 'bbox_select_x', choices = names(filt_data$p))
        updateSelectInput(session, 'bbox_select_y', choices = names(filt_data$p))
    })

    observeEvent(input$finalok, {

        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        f_data <- final_split$train[, sapply(final_split$train, is.factor)]
        if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "bbox_select_x",
            choices = names(fdata))
        } else {
          updateSelectInput(session, 'bbox_select_x', choices = names(f_data))
        }
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'bbox_select_y',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
            updateSelectInput(session, 'bbox_select_y',
              choices = '', selected = '')
        } else {
            updateSelectInput(session, 'bbox_select_y', choices = names(num_data))
        }
    })

    observeEvent(input$submit_part_train_per, {

        num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
        f_data <- final_split$train[, sapply(final_split$train, is.factor)]
        if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "bbox_select_x",
            choices = names(fdata))
        } else {
          updateSelectInput(session, 'bbox_select_x', choices = names(f_data))
        }
        if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'bbox_select_y',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
            updateSelectInput(session, 'bbox_select_y',
              choices = '', selected = '')
        } else {
            updateSelectInput(session, 'bbox_select_y', choices = names(num_data))
        }
    })

    f_split <- reactiveValues(num_data = NULL)

    num_data1 <- eventReactive(input$button_split_no, {
      numdata <- final_split$train[, sapply(final_split$train, is.factor)]
      if (is.factor(numdata)) {
        out <- 1
      } else {
        out <- ncol(numdata)  
      }
      out
    })

    num_data2 <- eventReactive(input$submit_part_train_per, {
      numdata <- final_split$train[, sapply(final_split$train, is.factor)]
      if (is.factor(numdata)) {
        out <- 1
      } else {
        out <- ncol(numdata)  
      }
      out
    })  

    observeEvent(input$button_split_no, {
      f_split$num_data <- num_data1()
    })

    observeEvent(input$submit_part_train_per, {
      f_split$num_data <- num_data2()
    })

    # selected data
    bbox_x <- eventReactive(input$box2_create, {
        # req(input$bbox_select_x)
        if (f_split$num_data > 0) {
          box_data <- final_split$train[, input$bbox_select_x]          
        } else {
          box_data <- NULL
        }
        box_data
    })

    bbox_y <- eventReactive(input$box2_create, {
      # req(input$bbox_select_y)
      box_data <- final_split$train[, input$bbox_select_y]
    })

    n_labels <- eventReactive(input$box2_create, {
        if (!is.null(bbox_x())) {
          k <- nlevels(bbox_x())    
        }
        k
    })

    observeEvent(input$box2_create, {
        # req(input$bbox_select_x)
        if (!is.null(bbox_x())) {
          updateNumericInput(session, 'nbox2label', value = n_labels())    
        }
        
    })

    # dynamic UI for histogram colors
    output$ui_ncolbox2 <- renderUI({
        ncol <- as.integer(input$ncolbox2)

        lapply(1:ncol, function(i) {
            textInput(paste("n_box2col_", i), label = paste0("Box Color ", i),
            value = 'blue')
        })
    })

    colours_box2 <- reactive({
        ncol <- as.integer(input$ncolbox2)

        collect <- list(lapply(1:ncol, function(i) {
                        input[[paste("n_box2col_", i)]]
                    }))

        colors <- unlist(collect)

    })

    # dynamic UI for histogram border colors
    output$ui_nborbox2 <- renderUI({
        ncol <- as.integer(input$nborbox2)

        lapply(1:ncol, function(i) {
            textInput(paste("n_box2bor_", i), label = paste0("Border Color ", i),
            value = 'black')
        })
    })

    borders_box2 <- reactive({
        ncol <- as.integer(input$nborbox2)

        collect <- list(lapply(1:ncol, function(i) {
                        input[[paste("n_box2bor_", i)]]
                    }))

        colors <- unlist(collect)

    })

    output$ui_nbox2label <- renderUI({
        ncol <- as.integer(input$nbox2label)
        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
              textInput(paste("n_box2label_", i),
                        label = paste0("Label ", i))
          })
        }
    })

    labels_box2 <- reactive({
        ncol <- as.integer(input$nbox2label)
        if (ncol < 1) {
          colors <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
                          input[[paste("n_box2label_", i)]]
                      }))
          colors <- unlist(collect)
        }
        colors
    })

    output$ui_box2_legnames <- renderUI({
        ncol <- as.integer(input$box2_legnames)
        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
              textInput(paste("n_legnamesbox2_", i),
                        label = paste0("Legend Name ", i))
          })
        }
    })

    output$ui_box2_legpoint <- renderUI({
        ncol <- as.integer(input$box2_leg_point)
        if (ncol < 1) {
          NULL
        } else {
          lapply(1:ncol, function(i) {
            numericInput(paste("n_pointbox2_", i),
                      label = paste0("Legend Point ", i), value = 15)
          })
        }
    })

    name_box2 <- reactive({
        ncol <- as.integer(input$box2_legnames)
        if (ncol < 1) {
          colors <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
                          input[[paste("n_legnamesbox2_", i)]]
                      }))
          colors <- unlist(collect)
        }
        colors
    })

    point_box2 <- reactive({
        ncol <- as.integer(input$box2_leg_point)
        if (ncol < 1) {
          colors <- NULL
        } else {
          collect <- list(lapply(1:ncol, function(i) {
                          input[[paste("n_pointbox2_", i)]]
                      }))
          colors <- unlist(collect)
        }
        colors
    })


    # bar plot
    output$bbox_plot_1 <- renderPlot({

        if (!is.null(bbox_x())) {
          box_plotb(bbox_x(), bbox_y(), title = input$bbox_title, subs = input$bbox_subtitle,
            xlabel = input$bbox_xlabel, ylabel = input$bbox_ylabel)    
        }
    
    })

    output$bbox_plot_2 <- renderPlot({

        box_plotb(bbox_x(), bbox_y(), title = input$bbox_title, subs = input$bbox_subtitle,
            xlabel = input$bbox_xlabel, ylabel = input$bbox_ylabel, horiz = as.logical(input$bbox_horiz),
            notches = as.logical(input$bbox_notch), ranges = input$bbox_range, outlines = as.logical(input$bbox_outline),
            varwidths = as.logical(input$bbox_varwidth))

    })

    output$bbox_plot_3 <- renderPlot({

        box_plotb(bbox_x(), bbox_y(), title = input$bbox_title, subs = input$bbox_subtitle,
            xlabel = input$bbox_xlabel, ylabel = input$bbox_ylabel, horiz = as.logical(input$bbox_horiz),
            notches = as.logical(input$bbox_notch), ranges = input$bbox_range, outlines = as.logical(input$bbox_outline),
            varwidths = as.logical(input$bbox_varwidth), color = colours_box2(), borders = borders_box2(),
            labels = labels_box2())

    })


    # bar plot
    output$bbox_plot_5 <- renderPlot({

        box_plotb(
            bbox_x(), bbox_y(), title = input$bbox_title, subs = input$bbox_subtitle,
            xlabel = input$bbox_xlabel, ylabel = input$bbox_ylabel, horiz = as.logical(input$bbox_horiz),
            notches = as.logical(input$bbox_notch), ranges = input$bbox_range, outlines = as.logical(input$bbox_outline),
            varwidths = as.logical(input$bbox_varwidth), color = colours_box2(), borders = borders_box2(),
            text_p = input$bbox_plottext, text_x_loc = input$bbox_text_x_loc,
            text_y_loc = input$bbox_text_y_loc, text_col = input$bbox_textcolor, text_font = input$bbox_textfont,
            text_size = input$bbox_textsize, m_text = input$bbox_mtextplot, m_side = input$bbox_mtext_side,
            m_line = input$bbox_mtext_line, m_adj = input$bbox_mtextadj, m_col = input$bbox_mtextcolor,
            m_font = input$bbox_mtextfont, m_cex = input$bbox_mtextsize
        )
    })

    # bar plot
    output$bbox_plot_6 <- renderPlot({

        box_plotb(
            bbox_x(), bbox_y(), title = input$bbox_title, subs = input$bbox_subtitle,
            xlabel = input$bbox_xlabel, ylabel = input$bbox_ylabel, horiz = as.logical(input$bbox_horiz),
            notches = as.logical(input$bbox_notch), ranges = input$bbox_range, outlines = as.logical(input$bbox_outline),
            varwidths = as.logical(input$bbox_varwidth), color = colours_box2(), borders = borders_box2(),
            labels = labels_box2(), input$bbox_coltitle, input$bbox_colsub, input$bbox_colaxis,
            input$bbox_collabel, fontmain = input$bbox_fontmain, fontsub = input$bbox_fontsub,
            fontaxis = input$bbox_fontaxis, fontlab = input$bbox_fontlab, input$bbox_cexmain,
            input$bbox_cexsub, input$bbox_cexaxis, input$bbox_cexlab,
            text_p = input$bbox_plottext, text_x_loc = input$bbox_text_x_loc,
            text_y_loc = input$bbox_text_y_loc, text_col = input$bbox_textcolor, text_font = input$bbox_textfont,
            text_size = input$bbox_textsize, m_text = input$bbox_mtextplot, m_side = input$bbox_mtext_side,
            m_line = input$bbox_mtext_line, m_adj = input$bbox_mtextadj, m_col = input$bbox_mtextcolor,
            m_font = input$bbox_mtextfont, m_cex = input$bbox_mtextsize
        )
    })

    # bar plot
    output$bbox_plot_final <- renderPlot({

        box_plotb(
            bbox_x(), bbox_y(), title = input$bbox_title, subs = input$bbox_subtitle,
            xlabel = input$bbox_xlabel, ylabel = input$bbox_ylabel, horiz = as.logical(input$bbox_horiz),
            notches = as.logical(input$bbox_notch), ranges = input$bbox_range, outlines = as.logical(input$bbox_outline),
            varwidths = as.logical(input$bbox_varwidth), color = colours_box2(), borders = borders_box2(),
            labels = labels_box2(), input$bbox_coltitle, input$bbox_colsub, input$bbox_colaxis,
            input$bbox_collabel, fontmain = input$bbox_fontmain, fontsub = input$bbox_fontsub,
            fontaxis = input$bbox_fontaxis, fontlab = input$bbox_fontlab, input$bbox_cexmain,
            input$bbox_cexsub, input$bbox_cexaxis, input$bbox_cexlab,
            text_p = input$bbox_plottext, text_x_loc = input$bbox_text_x_loc,
            text_y_loc = input$bbox_text_y_loc, text_col = input$bbox_textcolor, text_font = input$bbox_textfont,
            text_size = input$bbox_textsize, m_text = input$bbox_mtextplot, m_side = input$bbox_mtext_side,
            m_line = input$bbox_mtext_line, m_adj = input$bbox_mtextadj, m_col = input$bbox_mtextcolor,
            m_font = input$bbox_mtextfont, m_cex = input$bbox_mtextsize
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
