# influence diagnostics
infl_result <- eventReactive(input$submit_infl, {
  if (input$infl_use_prev) {
    k <- model()
  } else {
    data <- final_split$train
    k <- glm(input$infl_fmla, data = data, family = binomial(link = "logit"))
  }
  return(k)
})


output$infl_out <- renderPlot({
	blorr::blr_plot_diag_influence(infl_result())
})

# leverage diagnostics
lev_result <- eventReactive(input$submit_lev, {
  if (input$lev_use_prev) {
    k <- model()
  } else {
    data <- final_split$train
    k <- glm(input$lev_fmla, data = data, family = binomial(link = "logit"))
  }
  return(k)
})


output$lev_out <- renderPlot({
	blorr::blr_plot_diag_leverage(lev_result())
})

# fit diagnostics
fit_result <- eventReactive(input$submit_fit, {
  if (input$fit_use_prev) {
    k <- model()
  } else {
    data <- final_split$train
    k <- glm(input$fit_fmla, data = data, family = binomial(link = "logit"))
  }
  return(k)
})


output$fit_out <- renderPlot({
	blorr::blr_plot_diag_fit(fit_result())
})


# fit diagnostics
dfbetas_result <- eventReactive(input$submit_dfbetas, {
  if (input$dfbetas_use_prev) {
    k <- model()
  } else {
    data <- final_split$train
    k <- glm(input$dfbetas_fmla, data = data, family = binomial(link = "logit"))
  }
  return(k)
})


output$dfbetas_out <- renderPlot({
	blorr::blr_plot_dfbetas_panel(dfbetas_result())
})