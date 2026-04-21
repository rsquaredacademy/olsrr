output$norm_shape <- renderPlot({
  vdist_normal_plot(input$norm_m, input$norm_sd)
})

output$nprob_plot <- renderPlot({
  vdist_normal_prob(input$nprob_p, input$nprob_m, input$nprob_sd, input$nprob_tail)
})

output$nperc_plot <- renderPlot({
  vdist_normal_perc(input$nperc_p, input$nperc_m, input$nperc_sd, input$nperc_tail)
})
