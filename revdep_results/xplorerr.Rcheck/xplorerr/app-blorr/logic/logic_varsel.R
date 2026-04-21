model_be <- eventReactive(input$submit_varsel_be, {
  data <- final_split$train
  m <- glm(input$varsel_be_fmla, data = data, family = binomial(link = "logit"))
  k <- blr_step_aic_backward(m, details = as.logical(input$trace_varsel_be))
  k
})

output$regress_varsel_be <- renderPrint({
  model_be()
})


model_fe <- eventReactive(input$submit_varsel_fe, {
  data <- final_split$train
  m <- glm(input$varsel_fe_fmla, data = data, family = binomial(link = "logit"))
  k <- blr_step_aic_forward(m, details = as.logical(input$trace_varsel_fe))
  k
})

output$regress_varsel_fe <- renderPrint({
  model_fe()
})


model_se <- eventReactive(input$submit_varsel_se, {
  data <- final_split$train
  m <- glm(input$varsel_se_fmla, data = data, family = binomial(link = "logit"))
  k <- blr_step_aic_both(m, details = as.logical(input$trace_varsel_se))
  k
})

output$regress_varsel_se <- renderPrint({
  model_se()
})
