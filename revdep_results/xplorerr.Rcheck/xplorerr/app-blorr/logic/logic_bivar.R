observe({

	updateSelectInput(session, inputId = "resp_bivar",
    choices = names(final_split$train))
	updateSelectInput(session, inputId = "var_bivar",
    choices = names(final_split$train))

	updateSelectInput(session, inputId = "resp_woe",
    choices = names(final_split$train))
	updateSelectInput(session, inputId = "var_woe",
    choices = names(final_split$train))

	updateSelectInput(session, inputId = "resp_woe2",
    choices = names(final_split$train))
	updateSelectInput(session, inputId = "var_woe2",
    choices = names(final_split$train))

	updateSelectInput(session, inputId = "resp_segdist",
    choices = names(final_split$train))
	updateSelectInput(session, inputId = "var_segdist",
    choices = names(final_split$train))

	updateSelectInput(session, inputId = "resp_2wayseg",
    choices = names(final_split$train))
	updateSelectInput(session, inputId = "var1_2wayseg",
    choices = names(final_split$train))
	updateSelectInput(session, inputId = "var2_2wayseg",
    choices = names(final_split$train))

})

bivar <- eventReactive(input$submit_bivar, {
	blorr::blr_bivariate_analysis(data = final_split$train,
		response = input$resp_bivar, input$var_bivar)
})

woe_iv <- eventReactive(input$submit_woe, {
	blorr::blr_woe_iv(data = final_split$train,
		predictor = input$var_woe, response = input$resp_woe)
})

woe_iv_2 <- eventReactive(input$submit_woe2, {
	blorr::blr_woe_iv_stats(data = final_split$train,
		response = input$resp_woe2, input$var_woe2)
})

seg_dist <- eventReactive(input$submit_segdist, {
	blorr::blr_segment_dist(data = final_split$train,
		response = !! sym(as.character(input$resp_segdist)), 
		predictor = !! sym(as.character(input$var_segdist)))
})

twowayseg <- eventReactive(input$submit_2wayseg, {
	blorr::blr_segment_twoway(data = final_split$train,
		response = input$resp_2wayseg, variable_1 = input$var1_2wayseg,
		variable_2 = input$var2_2wayseg)
})

output$bivar_out <- renderPrint({
	bivar()
})

output$woe_out <- renderPrint({
	woe_iv()
})

output$woe2_out <- renderPrint({
	woe_iv_2()
})

output$segdist_out <- renderPrint({
	seg_dist()
})

output$segdist_plot <- renderPlot({
	plot(seg_dist())
})

output$twowayseg_out <- renderPrint({
	twowayseg()
})
