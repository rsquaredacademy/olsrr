# importing data
inFile1 <- reactive({
    if(is.null(input$file1)) {
        return(NULL)
    } else {
        input$file1
    }
})

data1 <- reactive({
    if(is.null(inFile1())) {
        return(NULL)
    } else {
        read.csv(inFile1()$datapath,
            header = input$header,
            sep = input$sep,
            quote = input$quote)
    }
})


# importing data
inFile2 <- reactive({
    if(is.null(input$file2)) {
        return(NULL)
    } else {
        input$file2
    }
})

data2 <- reactive({
    if(is.null(inFile2())) {
        return(NULL)
    } else {
        ext <- tools::file_ext(inFile2()$name)

        file.rename(inFile2()$datapath,
                    paste(inFile2()$datapath, ext, sep="."))

        readxl::read_excel(
          path = paste(inFile2()$datapath, ext, sep="."),
          sheet = input$sheet_n
        )
    }
})

# importing data
inFile3 <- reactive({
    if(is.null(input$file3)) {
        return(NULL)
    } else {
        input$file3
    }
})

data3 <- reactive({
    if(is.null(inFile3())) {
        return(NULL)
    } else {
        jsonlite::fromJSON(inFile3()$datapath)
    }
})


# importing data
inFile4 <- reactive({
    if(is.null(input$file4)) {
        return(NULL)
    } else {
        input$file4
    }
})

data4 <- reactive({
    if(is.null(inFile4())) {
        return(NULL)
    } else {
         haven::read_sas(inFile4()$datapath)
    }
})


inFile5 <- reactive({
    if(is.null(input$file5)) {
        return(NULL)
    } else {
        input$file5
    }
})

data5 <- reactive({
    if(is.null(inFile5())) {
        return(NULL)
    } else {
         haven::read_sav(inFile5()$datapath)
    }
})


inFile6 <- reactive({
    if(is.null(input$file6)) {
        return(NULL)
    } else {
        input$file6
    }
})

data6 <- reactive({
    if(is.null(inFile6())) {
        return(NULL)
    } else {
         haven::read_stata(inFile6()$datapath)
    }
})


observe({
  updateSelectInput(
    session,
    inputId = 'sel_data',
    label = '',
    choices = c(input$file1$name, input$file2$name, input$file3$name,
      input$file4$name, input$file5$name, input$file6$name),
    selected = ''
  )
})

ext_type <- reactive({
    ext <- tools::file_ext(input$sel_data)
})


# choosing sample data
sampdata <- reactiveValues(s = NULL)

observeEvent(input$german_data, {
  data("GermanCredit")
  sampdata$s <- GermanCredit
})

observeEvent(input$iris_data, {
  sampdata$s <- iris
})

observeEvent(input$mtcars_data, {
  sampdata$s <- descriptr::mtcarz
})

observeEvent(input$hsb_data, {
  sampdata$s <- hsb
})

observeEvent(input$mpg_data, {
  sampdata$s <- mpg
})

observeEvent(input$diamonds_data, {
  sampdata$s <- diamonds
})

uploadata <- reactiveValues(t = NULL)

observeEvent(input$submit_seldata, {

  if (ext_type() == 'csv') {
    uploadata$t <- data1()
  } else if (ext_type() == 'xls') {
    uploadata$t <- data2()
  } else if (ext_type() == 'xlsx') {
    uploadata$t <- data2()
  } else if (ext_type() == 'json') {
    uploadata$t <- data3()
  } else if (ext_type() == 'sas7bdat') {
    uploadata$t <- data4()
  } else if (ext_type() == 'sav') {
    uploadata$t <- uploadata$t <- data5()
  } else if (ext_type() == 'dta') {
    uploadata$t <- data6()
  }

})

observeEvent(input$use_sample_data, {
  uploadata$t <- sampdata$s
})


  # if (input$sel_data == 'csv') {
  #   data1()
  # } else if (input$sel_data == 'excel') {
  #   data2()
  # } else if (input$sel_data == 'json') {
  #   data3()
  # } else if (input$sel_data == 'sas') {
  #   data4()
  # } else if (input$sel_data == 'spss') {
  #   data5()
  # } else if (input$sel_data == 'stata') {
  #   data6()
  # }


observeEvent(input$use_sample_data, {
  updateNavbarPage(session, 'mainpage', selected = 'tab_trans')
  updateNavlistPanel(session, 'navlist_trans', 'tab_transform')
})

observeEvent(input$submit_seldata, {
  updateNavbarPage(session, 'mainpage', selected = 'tab_trans')
  updateNavlistPanel(session, 'navlist_trans', 'tab_transform')
})

observeEvent(input$csv2datasrc, {
  updateNavbarPage(session, 'mainpage', selected = 'tab_upload')
  updateNavlistPanel(session, 'navlist_up', 'tab_datasources')
})

observeEvent(input$csv2datatrans, {
  updateNavbarPage(session, 'mainpage', selected = 'tab_trans')
  updateNavlistPanel(session, 'navlist_trans', 'tab_seldata')
})

observeEvent(input$excel2datasrc, {
  updateNavbarPage(session, 'mainpage', selected = 'tab_upload')
  updateNavlistPanel(session, 'navlist_up', 'tab_datasources')
})

observeEvent(input$excel2datatrans, {
  updateNavbarPage(session, 'mainpage', selected = 'tab_trans')
  updateNavlistPanel(session, 'navlist_trans', 'tab_seldata')
})

observeEvent(input$json2datasrc, {
  updateNavbarPage(session, 'mainpage', selected = 'tab_upload')
  updateNavlistPanel(session, 'navlist_up', 'tab_datasources')
})

observeEvent(input$json2datatrans, {
  updateNavbarPage(session, 'mainpage', selected = 'tab_trans')
  updateNavlistPanel(session, 'navlist_trans', 'tab_seldata')
})

observeEvent(input$stata2datasrc, {
  updateNavbarPage(session, 'mainpage', selected = 'tab_upload')
  updateNavlistPanel(session, 'navlist_up', 'tab_datasources')
})

observeEvent(input$stata2datatrans, {
  updateNavbarPage(session, 'mainpage', selected = 'tab_trans')
  updateNavlistPanel(session, 'navlist_trans', 'tab_seldata')
})

observeEvent(input$spss2datasrc, {
  updateNavbarPage(session, 'mainpage', selected = 'tab_upload')
  updateNavlistPanel(session, 'navlist_up', 'tab_datasources')
})

observeEvent(input$spss2datatrans, {
  updateNavbarPage(session, 'mainpage', selected = 'tab_trans')
  updateNavlistPanel(session, 'navlist_trans', 'tab_seldata')
})

observeEvent(input$sas2datasrc, {
  updateNavbarPage(session, 'mainpage', selected = 'tab_upload')
  updateNavlistPanel(session, 'navlist_up', 'tab_datasources')
})

observeEvent(input$sas2datatrans, {
  updateNavbarPage(session, 'mainpage', selected = 'tab_trans')
  updateNavlistPanel(session, 'navlist_trans', 'tab_seldata')
})

observeEvent(input$welcomebutton, {
  updateNavbarPage(session, 'mainpage', selected = 'tab_upload')
})












