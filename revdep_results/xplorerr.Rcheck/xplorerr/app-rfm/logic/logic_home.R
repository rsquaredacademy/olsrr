observeEvent(input$click_transaction, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_rfm')
	updateNavlistPanel(session, 'navlist_rfm', 'tab_rfm_transaction_score')
})

observeEvent(input$click_customer_1, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_rfm')
	updateNavlistPanel(session, 'navlist_rfm', 'tab_rfm_customer_score')
})

observeEvent(input$click_customer_2, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_rfm')
	updateNavlistPanel(session, 'navlist_rfm', 'tab_rfm_customer_score_2')
})

observeEvent(input$welcomebutton, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_upload')
	updateNavlistPanel(session, 'navlist_up', 'tab_datasources')
})

