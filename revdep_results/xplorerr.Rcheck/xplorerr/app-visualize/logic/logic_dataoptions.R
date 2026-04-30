observeEvent(input$sample_data_yes, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_upload')
  updateNavlistPanel(session, 'navlist_up', 'tab_use_sample')
})


file_upload_options <- eventReactive(input$upload_files_yes, {

	fluidRow(

		column(3, align = 'center',
					actionButton(
						inputId = 'upload_csv_file',
						label = 'CSV',
						width = '120px'
					)
				),
				column(3, align = 'center',
					actionButton(
						inputId = 'upload_xls_file',
						label = 'XLS',
						width = '120px'
					)
				),
				column(3, align = 'center',
					actionButton(
						inputId = 'upload_xlsx_file',
						label = 'XLSX',
						width = '120px'
					)
				)
	)

})

output$upload_file_links <- renderUI({
	file_upload_options()
})

observeEvent(input$upload_csv_file, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_upload')
  updateNavlistPanel(session, 'navlist_up', 'tab_uploadfile')
  updateTabsetPanel(session, 'tab_uploadfile', selected = 'tab_upload_csv')
})

observeEvent(input$upload_xls_file, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_upload')
  updateNavlistPanel(session, 'navlist_up', 'tab_uploadfile')
  updateTabsetPanel(session, 'tabset_upload', selected = 'tab_upload_excel')
})

observeEvent(input$upload_xlsx_file, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_upload')
  updateNavlistPanel(session, 'navlist_up', 'tab_uploadfile')
  updateTabsetPanel(session, 'tabset_upload', selected = 'tab_upload_excel')
})
