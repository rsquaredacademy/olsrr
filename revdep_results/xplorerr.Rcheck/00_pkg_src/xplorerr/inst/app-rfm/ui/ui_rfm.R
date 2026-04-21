tabPanel('RFM', value = 'tab_rfm', icon = icon('sitemap'),
         
    navlistPanel(id = 'navlist_rfm',
        well = FALSE,
        widths = c(2, 10),

        source('ui/ui_rfm_score_transaction.R', local = TRUE)[[1]],
        source('ui/ui_rfm_customer_1.R', local = TRUE)[[1]],
        source('ui/ui_rfm_customer_2.R', local = TRUE)[[1]],
        source('ui/ui_rfm_heat_map.R', local = TRUE)[[1]],
        source('ui/ui_rfm_bar_chart.R', local = TRUE)[[1]],
        source('ui/ui_rfm_histogram.R', local = TRUE)[[1]]
        
    )
)