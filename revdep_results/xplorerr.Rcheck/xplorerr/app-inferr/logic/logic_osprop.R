source("xpl-helpers.R")
source("xpl-output.R")
source("xpl-format.R")

observe({
    updateSelectInput(session,
                      inputId = "var_osproptest",
                      choices = names(data()),
                      selected = '')
})

observeEvent(input$finalok, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "var_osproptest",
            choices = names(fdata))
        } else {
          updateSelectInput(session, 'var_osproptest', choices = names(f_data))
        }
})

observeEvent(input$submit_part_train_per, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "var_osproptest",
            choices = names(fdata))
        } else {
          updateSelectInput(session, 'var_osproptest', choices = names(f_data))
        }
})


d_osproptest <- eventReactive(input$submit_osproptest, {
  req(input$var_osproptest)
  data <- final_split$train
  xpl_os_prop_test(data, input$var_osproptest, input$osproptest_prob,
                   input$osproptest_type)
})

output$osproptest_out <- renderPrint({
  d_osproptest()
})

ospropcalc <- eventReactive(input$submit_ospropcalc, {
  xpl_os_prop_test(data = input$n_ospropcalc, phat = as.numeric(input$p_ospropcalc), prob = input$prob_ospropcalc,
      alternative = input$ospropcalc_type)
})

output$ospropcalc_out <- renderPrint({
  ospropcalc()
})



