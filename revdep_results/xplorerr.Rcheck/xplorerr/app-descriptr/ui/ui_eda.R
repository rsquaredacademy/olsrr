tabPanel('EDA', value = 'tab_eda', icon = icon('stats', lib = 'glyphicon'),
         
    navlistPanel(id = 'navlist_eda',
        well = FALSE,
        widths = c(2, 10),

        source('ui/ui_summary.R', local = TRUE)[[1]],
        source('ui/ui_freq_qual.R', local = TRUE)[[1]],
        source('ui/ui_freq_quant.R', local = TRUE)[[1]],
        source('ui/ui_cross.R', local = TRUE)[[1]],
        source('ui/ui_mult1.R', local = TRUE)[[1]],
        source('ui/ui_mult2.R', local = TRUE)[[1]],
        source('ui/ui_group_summary.R', local = TRUE)[[1]]
        
    )
)