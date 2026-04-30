observeEvent(input$model_bivar_click, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_bivar')
	updateNavlistPanel(session, 'navlist_bivar', 'tab_bivar_analysis')
})

observeEvent(input$model_regress_click, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_reg')
	updateNavlistPanel(session, 'navlist_reg', 'tab_regress')
})

observeEvent(input$model_fitstat_click, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_reg')
	updateNavlistPanel(session, 'navlist_reg', 'tab_model_fit_stats')
})

observeEvent(input$model_varsel_click, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_varsel')
	updateNavlistPanel(session, 'navlist_varsel', 'tab_varsel_forward')
})

observeEvent(input$model_validation_click, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_valid')
	updateNavlistPanel(session, 'navlist_valid', 'tab_conf_matrix')
})

# observeEvent(input$click_visualize, {
# 	updateNavbarPage(session, 'mainpage', selected = 'tab_viz_lib')
# })

observeEvent(input$model_resdiag_click, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_resid')
	updateNavlistPanel(session, 'navlist_resid', 'tab_diag_influence')
})

