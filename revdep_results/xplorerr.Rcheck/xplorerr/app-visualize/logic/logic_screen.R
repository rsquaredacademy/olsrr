# output
output$screen <- renderPrint({
    ds_screener(filt_data$p)
})

observeEvent(input$finalok, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_viz_home')
	updateNavlistPanel(session, 'navlist_vizmenu', 'tab_home_viz')
})

final_split <- reactiveValues(train = NULL)

observeEvent(input$finalok, {
	final_split$train <- filt_data$p
})