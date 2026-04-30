source("xpl-helpers.R")
source("xpl-output.R")
source("xpl-format.R")

observe({
    updateSelectInput(session,
                      inputId = "var_binomtest",
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
        updateSelectInput(session, inputId = "var_binomtest",
            choices = names(fdata))
        } else {
          updateSelectInput(session, 'var_binomtest', choices = names(f_data))
        }
})

observeEvent(input$submit_part_train_per, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "var_binomtest",
            choices = names(fdata))
        } else {
          updateSelectInput(session, 'var_binomtest', choices = names(f_data))
        }
})

d_binomtest <- eventReactive(input$submit_binomtest, {
  req(input$var_binomtest)
  data <- final_split$train[, c(input$var_binomtest)]
  xpl_binom_test(final_split$train, input$var_binomtest, input$binomtest_prob)
})

d_binom_calc <- eventReactive(input$submit_binomcalc, {
	xpl_binom_calc(input$n_binomcalc, input$s_binomcalc, input$p_binomcalc)
})

output$binomtest_out <- renderPrint({
  d_binomtest()
})

output$binomcalc_out <- renderPrint({
  d_binom_calc()
})
