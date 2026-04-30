# output
output$screen <- renderPrint({
    ds_screener(final_sel$a)
})

observeEvent(input$finalok, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_home_analyze')
	updateNavlistPanel(session, 'navlist_home', 'tab_rfm_home')
})

final_split <- reactiveValues(train = NULL)

observeEvent(input$finalok, {
	final_split$train <- final_sel$a
})
