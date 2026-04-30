observeEvent(input$finalok, {

		updateSelectInput(
			session,
			inputId = "rfm_customer_id_c",
			choices = names(final_sel$a),
			selected = names(final_sel$a)
		)

		updateSelectInput(
			session,
			inputId = "rfm_n_transactions_c",
			choices = names(final_sel$a),
			selected = names(final_sel$a)
		)

		updateSelectInput(
			session,
			inputId = "rfm_recency_days_c",
			choices = names(final_sel$a),
			selected = names(final_sel$a)
		)

		updateSelectInput(
			session,
			inputId = "rfm_total_revenue_c",
			choices = names(final_sel$a),
			selected = names(final_sel$a)
		)

}) 

observeEvent(input$finalok, {

	updateSelectInput(
		session,
		inputId = "rfm_customer_id_t",
		choices = names(final_sel$a),
		selected = names(final_sel$a)
	)

	updateSelectInput(
		session,
		inputId = "rfm_order_date_t",
		choices = names(final_sel$a),
		selected = names(final_sel$a)
	)

	updateSelectInput(
		session,
		inputId = "rfm_revenue_t",
		choices = names(final_sel$a),
		selected = names(final_sel$a)
	)

}) 


comp_rfm_transaction_score <- eventReactive(input$submit_rfm_transaction_score, {

		rfm_table_order(data = final_sel$a, customer_id = !! sym(as.character(input$rfm_customer_id_t)),
			order_date = !! sym(as.character(input$rfm_order_date_t)), 
			revenue = !! sym(as.character(input$rfm_revenue_t)), 		
			analysis_date = input$rfm_analysis_date_t, recency_bins = input$rfm_recency_bins_t,
			frequency_bins = input$rfm_frequency_bins_t, monetary_bins = input$rfm_monetary_bins_t)

}) 

comp_rfm_customer_score <- eventReactive(input$submit_rfm_customer_score, {

		rfm_table_customer(data = final_sel$a, customer_id = !! sym(as.character(input$rfm_customer_id_c)),
			n_transactions = !! sym(as.character(input$rfm_n_transactions_c)),
			recency_days = !! sym(as.character(input$rfm_recency_days_c)), 
			total_revenue = !! sym(as.character(input$rfm_total_revenue_c)),
			analysis_date = input$rfm_analysis_date_c, recency_bins = input$rfm_recency_bins_c,
			frequency_bins = input$rfm_frequency_bins_c, monetary_bins = input$rfm_monetary_bins_c)

}) 

comp_rfm_customer_score_2 <- eventReactive(input$submit_rfm_customer_score_2, {

		rfm_table_customer(data = final_sel$a, customer_id = !! sym(as.character(input$rfm_customer_id_c_2)),
			n_transactions = !! sym(as.character(input$rfm_n_transactions_c_2)),
			recency_days = !! sym(as.character(input$rfm_order_date_c)), 
			total_revenue = !! sym(as.character(input$rfm_total_revenue_c_2)),
			analysis_date = input$rfm_analysis_date_c_2, recency_bins = input$rfm_recency_bins_c_2,
			frequency_bins = input$rfm_frequency_bins_c_2, monetary_bins = input$rfm_monetary_bins_c_2)

}) 

output$rfm_transaction_score_out <- renderDataTable({
	comp_rfm_transaction_score() %>%
	  use_series(rfm) %>%
	  as.data.frame()

})

output$rfm_customer_score_out <- renderDataTable({
	comp_rfm_customer_score() %>%
	  use_series(rfm) %>%
	  as.data.frame()

})

output$rfm_customer_score_out_2 <- renderDataTable({
	comp_rfm_customer_score_2() %>%
	  use_series(rfm) %>%
	  as.data.frame()

})

rfm_final_score <- reactiveValues(a = NULL)

observeEvent(input$submit_rfm_transaction_score, {
  rfm_final_score$a <- comp_rfm_transaction_score()
})

observeEvent(input$submit_rfm_customer_score, {
  rfm_final_score$a <- comp_rfm_customer_score()
})

observeEvent(input$submit_rfm_customer_score_2, {
  rfm_final_score$a <- comp_rfm_customer_score_2()
})

rfm_heatmap_generate <- reactiveValues(a = NULL)

observeEvent(input$submit_rfm_transaction_score, {
  rfm_heatmap_generate$a <- rfm_plot_heatmap(rfm_final_score$a)
})

observeEvent(input$submit_rfm_customer_score, {
  rfm_heatmap_generate$a <- rfm_plot_heatmap(rfm_final_score$a)
})

observeEvent(input$submit_rfm_customer_score_2, {
  rfm_heatmap_generate$a <- rfm_plot_heatmap(rfm_final_score$a)
})

output$plot_heatmap <- renderPlot({
  print(rfm_heatmap_generate$a)
})

rfm_barchart_generate <- reactiveValues(a = NULL)

observeEvent(input$submit_rfm_transaction_score, {
  rfm_barchart_generate$a <- rfm_plot_bar_chart(rfm_final_score$a)
})

observeEvent(input$submit_rfm_customer_score, {
  rfm_barchart_generate$a <- rfm_plot_bar_chart(rfm_final_score$a)
})

observeEvent(input$submit_rfm_customer_score_2, {
  rfm_barchart_generate$a <- rfm_plot_bar_chart(rfm_final_score$a)
})

output$plot_barchart <- renderPlot({
  print(rfm_barchart_generate$a)
})

rfm_histogram_generate <- reactiveValues(a = NULL)

observeEvent(input$submit_rfm_transaction_score, {
  rfm_histogram_generate$a <- rfm_plot_histogram(rfm_final_score$a)
})

observeEvent(input$submit_rfm_customer_score, {
  rfm_histogram_generate$a <- rfm_plot_histogram(rfm_final_score$a)
})

observeEvent(input$submit_rfm_customer_score_2, {
  rfm_histogram_generate$a <- rfm_plot_histogram(rfm_final_score$a)
})

output$plot_histogram <- renderPlot({
  print(rfm_histogram_generate$a)
})