tabPanel('Segments', value = 'tab_segment', icon = icon('object-ungroup'),
         
    navlistPanel(id = 'navlist_rfm',
        well = FALSE,
        widths = c(2, 10),

        source('ui/ui_segments.R', local = TRUE)[[1]],
        source('ui/ui_segment_size.R', local = TRUE)[[1]],
        source('ui/ui_average_recency.R', local = TRUE)[[1]],
        source('ui/ui_average_frequency.R', local = TRUE)[[1]],
        source('ui/ui_average_monetary.R', local = TRUE)[[1]]
        
    )
)