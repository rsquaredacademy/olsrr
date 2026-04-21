source("xpl-helpers.R")
source("xpl-output.R")
source("xpl-format.R")

observe({
    updateSelectInput(session, 'var_chigof', choices = names(data()))
})

observeEvent(input$finalok, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "var_chigof",
            choices = names(fdata))
        } else {
          updateSelectInput(session, 'var_chigof', choices = names(f_data))
        }
})

observeEvent(input$submit_part_train_per, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "var_chigof",
            choices = names(fdata))
        } else {
          updateSelectInput(session, 'var_chigof', choices = names(f_data))
        }
})


d_chigof <- eventReactive(input$update_chigof, {
	# validate(need((input$var_chigof != ''), 'Please select variable.'))
    data <- final_split$train[, c(input$var_chigof)]
})

chigof_lev <- reactive({
    nlevels(d_chigof())
})

output$chigof_prop <- renderUI({
    ncol <- chigof_lev()
    if (ncol < 1) {
      NULL
    } else {
      lapply(1:ncol, function(i) {
          numericInput(paste0("prop_", i), label = paste0("Proportion ", i),
            value = 0.25, min = 0, max = 1, step = 0.1)
      })
    }
})

props <- reactive({
    ncol <- chigof_lev()
    if (ncol < 1) {
      proportions <- NULL
    } else {
      collect <- list(lapply(1:ncol, function(i) {
                      input[[paste0("prop_", i)]]
                  }))
      proportions <- unlist(collect)
    }
    proportions
})

df_chigof <- eventReactive(input$submit_chigof, {
  data <- final_split$train
  xpl_chisq_gof_test(data, input$var_chigof, props(), as.logical(input$chigof_cc))
  
})


output$chigof_out <- renderPrint({
  df_chigof()
})
