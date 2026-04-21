source("xpl-helpers.R")
source("xpl-output.R")
source("xpl-format.R")

observe({
  updateSelectInput(session,inputId = "var_tsproptest1",
    choices = names(data()), selected = '')
  updateSelectInput(session,inputId = "var_tsproptestg1",
    choices = names(data()), selected = '')
  updateSelectInput(session,inputId = "var_tsproptest2",
    choices = names(data()), selected = '')
  updateSelectInput(session,inputId = "var_tsproptestg2",
    choices = names(data()), selected = '')
})

observeEvent(input$finalok, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "var_tsproptest1", choices = names(fdata))
        updateSelectInput(session, inputId = "var_tsproptest2", choices = names(fdata))
        updateSelectInput(session, inputId = "var_tsproptestg1", choices = names(fdata))
        updateSelectInput(session, inputId = "var_tsproptestg2", choices = names(fdata))
    } else {
          updateSelectInput(session, inputId = "var_tsproptest1", choices = names(f_data))
          updateSelectInput(session, inputId = "var_tsproptest2", choices = names(f_data))
          updateSelectInput(session, inputId = "var_tsproptestg1", choices = names(f_data))
          updateSelectInput(session, inputId = "var_tsproptestg2", choices = names(f_data))
        }
})

observeEvent(input$submit_part_train_per, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "var_tsproptest1", choices = names(fdata))
        updateSelectInput(session, inputId = "var_tsproptest2", choices = names(fdata))
        updateSelectInput(session, inputId = "var_tsproptestg1", choices = names(fdata))
        updateSelectInput(session, inputId = "var_tsproptestg2", choices = names(fdata))
    } else {
          updateSelectInput(session, inputId = "var_tsproptest1", choices = names(f_data))
          updateSelectInput(session, inputId = "var_tsproptest2", choices = names(f_data))
          updateSelectInput(session, inputId = "var_tsproptestg1", choices = names(f_data))
          updateSelectInput(session, inputId = "var_tsproptestg2", choices = names(f_data))
        }
})

d_tsproptest <- eventReactive(input$submit_tsproptest, {
  req(input$var_tsproptest1)
  req(input$var_tsproptest2)
  data <- final_split$train[, c(input$var_tsproptest1, input$var_tsproptest2)]
  xpl_ts_prop_test(data, input$var_tsproptest1, input$var_tsproptest2,
                   input$tsproptest_type)
})

d_tsproptestg <- eventReactive(input$submit_tsproptestg, {
  req(input$var_tsproptestg1)
  req(input$var_tsproptestg2)
  data <- final_split$train[, c(input$var_tsproptestg1, input$var_tsproptestg2)]
  xpl_ts_prop_grp(data, input$var_tsproptestg1, 
                  input$var_tsproptestg2,
                  input$tsproptestg_type)
})

output$tsproptest_out <- renderPrint({
    d_tsproptest()
})

output$tsproptestg_out <- renderPrint({
  d_tsproptestg()
})

tspropcalc <- eventReactive(input$submit_tspropcalc, {
  xpl_ts_prop_calc(input$n1_tspropcalc, input$n2_tspropcalc, input$prop_tspropcalc1,
      input$prop_tspropcalc2, input$tspropcalc_type)
})

output$tspropcalc_out <- renderPrint({
  tspropcalc()
})
