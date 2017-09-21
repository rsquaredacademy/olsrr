show_but_sel <- eventReactive(input$button_selvar_yes, {

      column(12, align = 'center',
        selectInput(
          inputId = 'dplyr_selvar',
          label = '',
          choices = '',
          selected = '',
          multiple = TRUE,
          selectize = TRUE
        )
      )
})


output$show_sel_button <- renderUI({
  show_but_sel()
})


sel_sub_but <- eventReactive(input$button_selvar_yes, {
 
      column(12, align = 'center',
        br(),
        br(),
        actionButton(inputId = 'submit_dply_selvar', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_seldata", "Click here to select variables.",
                      "bottom", options = list(container = "body"))
      )
  
})

output$sub_sel_button <- renderUI({
  sel_sub_but()
})

observe({
  updateSelectInput(
  	session,
    inputId = "dplyr_selvar",
    choices = names(data()),
    selected = names(data())
  )
})

observeEvent(input$button_selvar_yes, {
	updateSelectInput(
  	session,
    inputId = "dplyr_selvar",
    choices = names(final()),
    selected = names(final())
  )
})

final_sel <- reactiveValues(a = NULL)

finalsel <- eventReactive(input$submit_dply_selvar, {
	k <- final() %>%
		select(input$dplyr_selvar)
	k
})

observeEvent(input$submit_dply_selvar, {
  final_sel$a <- finalsel()
})

observeEvent(input$button_selvar_no, {
  final_sel$a <- final()
})


observeEvent(input$button_selvar_no, {
  removeUI(
    selector = "div:has(> #dplyr_selvar)"
  )
  removeUI(
    selector = "div:has(> #submit_dply_selvar)"
  )
})


observeEvent(input$button_selvar_no, {
  updateNavbarPage(session, 'mainpage', selected = 'tab_trans')
  updateNavlistPanel(session, 'navlist_trans', 'tab_filter')
})

observeEvent(input$submit_dply_selvar, {
  updateNavbarPage(session, 'mainpage', selected = 'tab_trans')
  updateNavlistPanel(session, 'navlist_trans', 'tab_filter')
})