# output
output$screen <- renderPrint({
    screener(filt_data$p)
})

observeEvent(input$finalok, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_sample')
})