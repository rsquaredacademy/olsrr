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
				),
				column(3, align = 'center',
					actionButton(
						inputId = 'upload_json_file',
						label = 'JSON',
						width = '120px'
					)
				),
				column(12, br()),
				column(3, align = 'center',
					actionButton(
						inputId = 'upload_stata_file',
						label = 'STATA',
						width = '120px'
					)
				),
				column(3, align = 'center',
					actionButton(
						inputId = 'upload_spss_file',
						label = 'SPSS',
						width = '120px'
					)
				),
				column(3, align = 'center',
					actionButton(
						inputId = 'upload_sas_file',
						label = 'SAS',
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

observeEvent(input$upload_json_file, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_upload')
  updateNavlistPanel(session, 'navlist_up', 'tab_uploadfile')
  updateTabsetPanel(session, 'tabset_upload', selected = 'tab_upload_json')
})

observeEvent(input$upload_stata_file, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_upload')
  updateNavlistPanel(session, 'navlist_up', 'tab_uploadfile')
  updateTabsetPanel(session, 'tabset_upload', selected = 'tab_upload_stata')
})

observeEvent(input$upload_spss_file, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_upload')
  updateNavlistPanel(session, 'navlist_up', 'tab_uploadfile')
  updateTabsetPanel(session, 'tabset_upload', selected = 'tab_upload_spss')
})

observeEvent(input$upload_sas_file, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_upload')
  updateNavlistPanel(session, 'navlist_up', 'tab_uploadfile')
  updateTabsetPanel(session, 'tabset_upload', selected = 'tab_upload_sas')
})