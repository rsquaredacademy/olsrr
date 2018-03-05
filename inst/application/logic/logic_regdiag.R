d_cprp_mod <- eventReactive(input$submit_cprp_plot, {
  if(input$cprp_use_prev) {
    ols_plot_comp_plus_resid(all_use_n())
  } else {
    k <- lm(input$cprp_fmla, data = final_split$train)
    ols_plot_comp_plus_resid(k)
  }
})

output$cprplot <- renderPlot({
  d_cprp_mod()
})


# added variable plot
d_diag_advar <- eventReactive(input$submit_avplot, {
  if (input$advar_use_prev) {
    model <- all_use_n()
    data <- final_split$train
    xnames <- colnames(attr(model$terms, 'factors'))
    nl <- xnames %>% length()
    resp <- rownames(attr(model$terms, 'factors'))[1]
    myplots <- list()

    for(i in seq_len(nl)) {

        x <- olsrr:::advarx(data, i, xnames)
        y <- olsrr:::advary(data, i, resp, xnames)
        d <- tibble(x, y)
        p <- eval(substitute(ggplot(d, aes(x = x, y = y)) +
                                 geom_point(colour = 'blue', size = 2) +
                                 xlab(paste(xnames[i], " | Others")) +
                                 ylab(paste(resp, " | Others")) +
                                 stat_smooth(method="lm", se=FALSE), list(i = i)))

        # print(p)
        j <- i
        myplots[[j]] <- p

    }

    do.call(grid.arrange, c(myplots, list(ncol = 2)))
  } else {
    model <- lm(input$avplot_fmla, data = final_split$train)
    data <- eval(model$call$data)
    xnames <- colnames(attr(model$terms, 'factors'))
    nl <- xnames %>% length()
    resp <- rownames(attr(model$terms, 'factors'))[1]
    myplots <- list()

    for(i in seq_len(nl)) {

        x <- olsrr:::advarx(data, i, xnames)
        y <- olsrr:::advary(data, i, resp, xnames)
        d <- tibble(x, y)
        p <- eval(substitute(ggplot(d, aes(x = x, y = y)) +
                                 geom_point(colour = 'blue', size = 2) +
                                 xlab(paste(xnames[i], " | Others")) +
                                 ylab(paste(resp, " | Others")) +
                                 stat_smooth(method="lm", se=FALSE), list(i = i)))

        # print(p)
        j <- i
        myplots[[j]] <- p

    }

    do.call(grid.arrange, c(myplots, list(ncol = 2)))
    }
})

output$avplot <- renderPlot({
  print(d_diag_advar())
})


observeEvent(input$button_split_no, {
    updateSelectInput(session,
                      inputId = "resreg_var",
                      choices = names(final_split$train))
})

observeEvent(input$submit_part_train_per, {
    updateSelectInput(session,
                      inputId = "resreg_var",
                      choices = names(final_split$train))
})


d_resreg_mod <- eventReactive(input$submit_resreg_plot, {
  if(input$resreg_use_prev) {
    rvsr_plot_shiny(all_use_n(), final_split$train, as.character(input$resreg_var))
  } else {
    k <- lm(input$resreg_fmla, data = final_split$train)
    rvsr_plot_shiny(k, final_split$train, as.character(input$resreg_var))
  }
})



# d_resreg <- eventReactive(input$submit_resreg_plot, {
#   # validate(need((input$resreg_var != ''), 'Please select a variable.'))
#     data <- tibble::as_data_frame(final_split$train[, c(input$resreg_var)])
#     data
# })

output$rvsrplot <- renderPlot({
  d_resreg_mod()
})


