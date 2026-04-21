output$chisq_shape <- renderPlot({
  vdist_chisquare_plot(input$chisq_df, as.logical(input$chisq_norm))
})

output$chiprob_plot <- renderPlot({
  vdist_chisquare_prob(input$chiprob_p, input$chiprob_df, input$chiprob_tail)
})

output$chiperc_plot <- renderPlot({
  print(vdist_chisquare_perc(input$chiperc_p, input$chiperc_df, input$chiperc_tail))
})
