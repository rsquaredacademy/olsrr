output$table <- renderDataTable({
 	final_split$train
})

observeEvent(input$view2getdata, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_upload')
  updateNavlistPanel(session, 'navlist_up', 'tab_datasources')
})

observeEvent(input$view2analyze, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_eda')
  updateNavlistPanel(session, 'navlist_eda', 'tab_summary')
})