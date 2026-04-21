tabPanel('Bivariate Analysis', value = 'tab_bivar', icon = icon('cubes'),

	navlistPanel(id = 'navlist_bivar',
        well = FALSE,
        widths = c(2, 10),

        
        source('ui/ui_woe_iv.R', local = TRUE)[[1]],
        source('ui/ui_woe_iv_stats.R', local = TRUE)[[1]],
        source('ui/ui_segment_dist.R', local = TRUE)[[1]],
        source('ui/ui_2way_segment.R', local = TRUE)[[1]],
        source('ui/ui_bivar_analysis.R', local = TRUE)[[1]]
				
    )
)
