# output
output$screen <- renderPrint({
    ds_screener(filt_data$p)
})

observeEvent(input$finalok, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_sample')
})