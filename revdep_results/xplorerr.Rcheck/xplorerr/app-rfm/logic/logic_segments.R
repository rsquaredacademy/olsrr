library(purrr)
library(ggplot2)

output$segment_prep <- renderUI({

	ncol <- as.integer(input$n_segments)

	lapply(1:ncol, function(i) {

    fluidRow(

      column(3,
        textInput(paste("segment_name_", i),
        label = '',  width = '150px',
        value = "")
      ),
      column(3,
        sliderInput(paste("recency_interval_", i),
          label = '', min = 1, max = 5, value = c(2, 4), step = 1)
      ),
      column(3,
        sliderInput(paste("frequency_interval_", i),
          label = '', min = 1, max = 5, value = c(2, 4), step = 1)
      ),
      column(3,
        sliderInput(paste("monetary_interval_", i),
          label = '', min = 1, max = 5, value = c(2, 4), step = 1)
      )
    )

  })

})

segment_names <- reactive({

  ncol <- as.integer(input$n_segments)

  collect <- list(lapply(1:ncol, function(i) {
    input[[paste("segment_name_", i)]]
  }))

  unlist(collect)

})

recency_lower <- reactive({

	ncol <- as.integer(input$n_segments)

  collect <- list(lapply(1:ncol, function(i) {
    input[[paste("recency_interval_", i)]]
  }))

  collect[[1]] %>%
    map_int(1)

})

recency_upper <- reactive({

	ncol <- as.integer(input$n_segments)

  collect <- list(lapply(1:ncol, function(i) {
    input[[paste("recency_interval_", i)]]
  }))

  collect[[1]] %>%
    map_int(2)

})

frequency_lower <- reactive({

	ncol <- as.integer(input$n_segments)

  collect <- list(lapply(1:ncol, function(i) {
    input[[paste("frequency_interval_", i)]]
  }))

  collect[[1]] %>%
    map_int(1)

})

frequency_upper <- reactive({

	ncol <- as.integer(input$n_segments)

  collect <- list(lapply(1:ncol, function(i) {
    input[[paste("frequency_interval_", i)]]
  }))

  collect[[1]] %>%
    map_int(2)

})

monetary_lower <- reactive({

	ncol <- as.integer(input$n_segments)

  collect <- list(lapply(1:ncol, function(i) {
    input[[paste("monetary_interval_", i)]]
  }))

  collect[[1]] %>%
    map_int(1)

})

monetary_upper <- reactive({

	ncol <- as.integer(input$n_segments)

  collect <- list(lapply(1:ncol, function(i) {

    input[[paste("monetary_interval_", i)]]
  }))

  collect[[1]] %>%

    map_int(2)

})

prep_segment <- eventReactive(input$button_create_segments, {

	rfm_score_table <-
		rfm_final_score$a %>%
	  use_series(rfm)

	for (i in seq_len(input$n_segments)) {
		rfm_score_table$segment[(
			(rfm_score_table$recency_score %>% between(recency_lower()[i], recency_upper()[i])) &
		  (rfm_score_table$frequency_score %>% between(frequency_lower()[i], frequency_upper()[i])) &
		  (rfm_score_table$monetary_score %>% between(monetary_lower()[i], monetary_upper()[i])) &
			!rfm_score_table$segment %in% segment_names)] <- segment_names()[i]
	}

	rfm_score_table$segment[is.na(rfm_score_table$segment)] <- "Others"

	rfm_score_table %>%
	  select(
	    customer_id, segment, rfm_score, transaction_count, recency_days,
	    amount

	  )

})

output$segment_out <- renderDataTable({
	prep_segment()
})

output$segment_size_out <- renderPrint({

	prep_segment() %>%
	  count(segment) %>%
	  arrange(desc(n)) %>%
	  rename(Segment = segment, Count = n) %>%
	  kable() %>%
  	kable_styling(full_width = TRUE, font_size = 30)

})

fill_segments <- reactive({
	input$n_segments + 1
})

output$segment_average_recency <- renderPlot({

	data <-
	  prep_segment() %>%
	  group_by(segment) %>%
	  select(segment, recency_days) %>%
	  summarize(median(recency_days)) %>%
	  rename(segment = segment, avg_recency = `median(recency_days)`) %>%
	  arrange(avg_recency)

	n_fill <- nrow(data)

	ggplot(data, aes(segment, avg_recency)) +
	  geom_bar(stat = "identity", fill = brewer.pal(n = n_fill, name = "Set1")) +
	  xlab("Segment") + ylab("Median Recency") +
	  ggtitle("Median Recency by Segment") +
	  coord_flip() +
	  theme(
	    plot.title = element_text(hjust = 0.5)
	  )

})

output$segment_average_frequency <- renderPlot({

	data <-
		prep_segment() %>%
	  group_by(segment) %>%
	  select(segment, transaction_count) %>%
	  summarize(median(transaction_count)) %>%
	  rename(segment = segment, avg_frequency = `median(transaction_count)`) %>%
	  arrange(avg_frequency)

	n_fill <- nrow(data)

	ggplot(data, aes(segment, avg_frequency)) +
	  geom_bar(stat = "identity", fill = brewer.pal(n = n_fill, name = "Set1")) +
	  xlab("Segment") + ylab("Median Frequency") +
	  ggtitle("Median Frequency by Segment") +
	  coord_flip() +
	  theme(
	    plot.title = element_text(hjust = 0.5)
	  )

})

output$segment_average_monetary <- renderPlot({

	data <-
		prep_segment() %>%
	  group_by(segment) %>%
	  select(segment, amount) %>%
	  summarize(median(amount)) %>%
	  rename(segment = segment, avg_monetary = `median(amount)`) %>%
	  arrange(avg_monetary)

	n_fill <- nrow(data)

	ggplot(data, aes(segment, avg_monetary)) +
	  geom_bar(stat = "identity", fill = brewer.pal(n = n_fill, name = "Set1")) +
	  xlab("Segment") + ylab("Median Monetary Value") +
	  ggtitle("Median Monetary Value by Segment") +
	  coord_flip() +
	  theme(
	    plot.title = element_text(hjust = 0.5)
	  )

})
