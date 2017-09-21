samp_yes <- eventReactive(input$button_sample_yes, {

	fluidRow(

		column(12, align = 'center',
			tags$div(class = 'header', id = 'samp_div',
				tags$h5('How do you want to sample?')
			)
			# h5('How do you want to sample?')
		),

		br(),
		br(),

		column(6, align = 'right',
				actionButton(
					inputId = 'button_samp_per',
					label = 'Percentage',
					width = '120px'
				)
		),

		column(6, align = 'left',
				actionButton(
					inputId = 'button_samp_n',
					label = 'Observations',
					width = '120px'
				)
		)

	)

})

# samp_no <- eventReactive(input$button_sample_no, {

# 	fluidRow(
# 		br(),
# 		tags$div(class = 'header', id = 'samp_remove_no',
# 			tags$h6('Click on Analyze in the drop down menu to explore the data.')
# 		)
# 	)

# })


output$samp_yes_no <- renderUI({

	samp_yes()

})

# output$samp_no_yes <- renderUI({

# 	samp_no()

# })


samp_per_options <- eventReactive(input$button_samp_per, {

	fluidRow(

		column(12, align = 'center',
			numericInput(
				inputId = 'samp_size_per',
				label = 'Sample Size',
				min = 0,
				max = 1, 
				value = 1,
				step = 0.01,
				width = '120px'
			)
		),

		column(12, align = 'center',
			br(),
			actionButton(inputId = 'submit_samp_per_size', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_samp_per_size", "Click here to select variables.",
                      "bottom", options = list(container = "body"))
		)

	)	


})

samp_obs_options <- eventReactive(input$button_samp_n, {

	fluidRow(

		column(12, align = 'center',
		  numericInput(
				inputId = 'samp_size_n',
				label = 'Sample Size',
				min = 0, 
				value = 0,
				step = 1,
				width = '120px'
			)
		),

		column(12, align = 'center',
			br(),
			actionButton(inputId = 'submit_samp_obs_size', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_samp_obs_size", "Click here to select variables.",
                      "bottom", options = list(container = "body"))
		)

	)		

})

output$samp_per_option <- renderUI({
	samp_per_options()
})

output$samp_obs_option <- renderUI({
	samp_obs_options()
})

observeEvent(input$button_sample_no, {
	removeUI(
		selector = "div:has(> #button_samp_per)"
	)
	removeUI(
		selector = "div:has(> #button_samp_n)"
	)
	removeUI(
		selector = "div:has(> #samp_div)"
	)
	removeUI(
		selector = "div:has(> #samp_size_n)"
	)
	removeUI(
		selector = "div:has(> #submit_samp_obs_size)"
	)
	removeUI(
		selector = "div:has(> #samp_size_per)"
	)
	removeUI(
		selector = "div:has(> #submit_samp_per_size)"
	)
})

observeEvent(input$button_sample_yes, {
	removeUI(
		selector = "div:has(> #samp_remove_no)"
	)
})


observeEvent(input$button_samp_per, {
	removeUI(
		selector = "div:has(> #samp_size_n)"
	)
	removeUI(
		selector = "div:has(> #submit_samp_obs_size)"
	)
})

observeEvent(input$button_samp_n, {
	removeUI(
		selector = "div:has(> #samp_size_per)"
	)
	removeUI(
		selector = "div:has(> #submit_samp_per_size)"
	)
})

observeEvent(input$button_samp_n, {
		updateNumericInput(
			session,
			inputId = 'samp_size_n',
			label = 'Sample Size',
			min = 0, 
			value = nrow(filt_data$p),
			max = nrow(filt_data$p),
			step = 1
		)
})

final_sample <- reactiveValues(d = NULL)

samp1 <- eventReactive(input$submit_samp_per_size, {
	final_sample$d <- dplyr::sample_frac(filt_data$p, size = input$samp_size_per, replace = FALSE)
})

samp2 <- eventReactive(input$submit_samp_obs_size, {
	final_sample$d <- dplyr::sample_n(filt_data$p, size = input$samp_size_n, replace = FALSE)
})

observeEvent(input$submit_samp_per_size, {
	final_sample$d <- samp1()
})

observeEvent(input$submit_samp_obs_size, {
	final_sample$d <- samp2()
})

observeEvent(input$button_sample_no, {
	final_sample$d <- filt_data$p
})

observeEvent(input$button_sample_no, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_partition')
})

observeEvent(input$submit_samp_obs_size, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_partition')
})

observeEvent(input$submit_samp_per_size, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_partition')
})

# output$samp_type <- renderUI({

# 	if (input$data_samp == 'Percentage') {
		
# 		numericInput(
# 			inputId = 'samp_size',
# 			label = 'Sample Size',
# 			min = 0,
# 			max = 1, 
# 			value = 0.7,
# 			step = 0.01
# 		)

# 	} else {

# 		numericInput(
# 			inputId = 'samp_size',
# 			label = 'Sample Size',
# 			min = 0, 
# 			value = 0,
# 			step = 1
# 		)		

# 	}

# })


# observeEvent(input$finalok, {

# 	if (input$data_samp == 'Observations') {

# 		updateNumericInput(
# 			inputId = 'samp_size',
# 			label = 'Sample Size',
# 			min = 0, 
# 			value = nrow(final()),
# 			max = nrow(final()),
# 			step = 1
# 		)

# 	}

# })

# final_sample <- eventReactive(input$submit_samp, {

# 	if (input$data_samp == 'Percentage') {
# 		out <- dplyr::sample_frac(filtered(), size = input$samp_size, replace = FALSE)
# 	} else {
# 		out <- dplyr::sample_n(filtered(), size = input$samp_size, replace = FALSE)
# 	}

# 	out

# })