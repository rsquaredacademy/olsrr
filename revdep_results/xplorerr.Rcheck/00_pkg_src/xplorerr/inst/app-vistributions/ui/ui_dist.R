tabPanel('Distributions', value = 'tab_dist', icon = icon('area-chart'),
         
    navlistPanel(id = 'navlist_dist',
        well = FALSE,
        widths = c(2, 10),
        
        source('ui/ui_normal.R', local = TRUE)[[1]],
        source('ui/ui_t.R', local = TRUE)[[1]],
        source('ui/ui_chisq.R', local = TRUE)[[1]],
        source('ui/ui_binom.R', local = TRUE)[[1]],
        source('ui/ui_f.R', local = TRUE)[[1]]
        
    )
)