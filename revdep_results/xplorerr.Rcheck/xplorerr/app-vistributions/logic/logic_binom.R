output$binom_shape <- renderPlot({
  vdist_binom_plot(input$binom_n, input$binom_p)
})

output$bprob_plot <- renderPlot({
  if (input$bprob_tail != 'interval') {
    vdist_binom_prob(input$bprob_n, input$bprob_p, input$bprob_s, input$bprob_tail)
  } else {
    vdist_binom_prob(input$bprob_n, input$bprob_p,
      c(input$bprob_tail_1, input$bprob_tail_2), input$bprob_tail)
  }

})

output$bperc_plot <- renderPlot({
  vdist_binom_perc(input$bperc_n, input$bperc_p, input$bperc_tp, input$bperc_tail)
})
