d_regress <- eventReactive(input$submit_regress, {
	# validate(need((input$regress_fmla != ''), 'Please specify model'))
  data <- final_split$train
  k <- ols_regress(input$regress_fmla, data = data)
  k
})

model <- reactive({
	d_regress()
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


# main regression
all_use_n <- eventReactive(input$submit_regress, {
  data <- final_split$train
  k <- lm(input$regress_fmla, data = data)
  k
})