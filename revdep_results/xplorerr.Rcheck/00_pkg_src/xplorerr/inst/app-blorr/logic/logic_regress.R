model <- eventReactive(input$submit_regress, {
  data <- final_split$train
  k <- glm(input$regress_fmla, data = data, family = binomial(link = "logit"))
  k
})

d_regress <- eventReactive(input$submit_regress, {
  data <- final_split$train
  k <- blr_regress(input$regress_fmla, data = data)
  k
})

r1_title <- eventReactive(input$submit_regress, {
  column(12, align = 'center', h4('Regression Result'))
})

output$reg1_title <- renderUI({
  r1_title()
})

output$regress_out <- renderPrint({
    d_regress()
})


# model fit statistics
result <- eventReactive(input$submit_mfs, {
  if (input$mfs_use_prev) {
    k <- model()
  } else {
    data <- final_split$train
    k <- glm(input$mfs_fmla, data = data, family = binomial(link = "logit"))
  }
  return(blorr::blr_model_fit_stats(k))
})

output$mfs <- renderPrint({
  result()
})


# multiple model fit statistics
mmfs_result <- eventReactive(input$submit_mmfs, {
  data <- final_split$train
  m1 <- glm(input$mmfs_fmla_1, data = data, family = binomial(link = "logit"))
  m2 <- glm(input$mmfs_fmla_2, data = data, family = binomial(link = "logit"))
  blorr::blr_multi_model_fit_stats(m1, m2)
})

output$mmfs <- renderPrint({
  mmfs_result()
})