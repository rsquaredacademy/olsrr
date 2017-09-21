library(stringr)

    output$trans_try <- renderUI({
        ncol <- as.integer(ncol(uploadata$t))

        lapply(1:ncol, function(i) {
          fluidRow(
            column(3,
              selectInput(paste("n_col_", i), label = '', width = '150px',
                choices = names(uploadata$t)[i], selected  = names(uploadata$t)[i])
            ),
            column(3,
              textInput(paste("new_name_", i),
                          label = '',  width = '150px',
                          value = names(uploadata$t)[i])
            ),
            column(3,
              selectInput(paste0("data_type_", i),
                          label = '',  width = '150px',
                          choices = c('numeric', 'factor', 'Date', 'character', 'integer'),
                          selected = 'numeric')
            ),
            column(3,
              conditionalPanel(condition = paste(paste0("input.data_type_", i), "== 'Date'"),
                  column(4, br(), tags$h5('Format')),
                  column(8,
                   selectInput(paste("date_type_", i),
                          label = '',  width = '150px',
                          choices = c('%d %m %y', '%d %m %Y', '%y %m %d', '%Y %m %d', '%d %y %m', '%d %Y %m',
                              '%m %d %y', '%m %d %Y', '%y %d %m', '%Y %d %m', '%m %y %d', '%m %Y %d',
                              '%d/%m/%y', '%d/%m/%Y', '%y/m /%d', '%Y/%m/%d', '%d/%y/%m', '%d/%Y/%m',
                              '%m/%d/%y', '%m/%d/%Y', '%y/%d/%m', '%Y/%d/%m', '%m/%y/%d', '%m/%Y/%d',
                              '%d-%m-%y', '%d-%m-%Y', '%y-m -%d', '%Y-%m-%d', '%d-%y-%m', '%d-%Y-%m',
                              '%m-%d-%y', '%m-%d-%Y', '%y-%d-%m', '%Y-%d-%m', '%m-%y-%d', '%m-%Y-%d'
                          ),
                          selected = '%Y %m %d')
                  )
              )
            )
          )

        })
    })

    original <- reactive({
        uploadata$t
    })

    save_names <- reactive({
        names(original())
    })

    n <- reactive({
        length(original())
    })

    data_types <- reactive({
        ncol <- as.integer(ncol(uploadata$t))

        collect <- list(lapply(1:ncol, function(i) {
            input[[paste0("data_type_", i)]]
        }))

        colors <- unlist(collect)
    })

    new_names <- reactive({
        ncol <- as.integer(ncol(uploadata$t))

        collect <- list(lapply(1:ncol, function(i) {
            input[[paste("new_name_", i)]]
        }))

        colors <- unlist(collect)
        colnames <- str_replace(colors, " ", "_")
    })  

    # original <- reactive({
    #     data()
    # })

    # save_names <- reactive({
    #     names(original())
    # })

    # n <- reactive({
    #     length(original())
    # })

    # data_types <- reactive({
    #     ncol <- as.integer(ncol(data()))

    #     collect <- list(lapply(1:ncol, function(i) {
    #         input[[paste0("data_type_", i)]]
    #     }))

    #     colors <- unlist(collect)
    # })

    # new_names <- reactive({
    #     ncol <- as.integer(ncol(data()))

    #     collect <- list(lapply(1:ncol, function(i) {
    #         input[[paste("new_name_", i)]]
    #     }))

    #     colors <- unlist(collect)
    #     colnames <- str_replace(colors, " ", "_")
    # })



        copy <- eventReactive(input$apply_changes, {
        out <- list()

        for (i in seq_len(n())) {

            if (data_types()[i] == 'Date') {
                inp <- eval(parse(text = paste0('input$', paste0('date_type_', i))))
                out[[i]] <- eval(parse(text = paste0("as.", data_types()[i], "(original()$", save_names()[i], ", ", inp, ")")))
            } else {
                out[[i]] <- eval(parse(text = paste0("as.", data_types()[i], "(original()$", save_names()[i], ")")))
            }


        }
        names(out) <- new_names()
        return(out)
        })

        final <- eventReactive(input$apply_changes, {
            data.frame(copy(), stringsAsFactors = F)
        })

observeEvent(input$apply_changes, {
  updateNavbarPage(session, 'mainpage', selected = 'tab_trans')
  updateNavlistPanel(session, 'navlist_trans', 'tab_selvar')
})