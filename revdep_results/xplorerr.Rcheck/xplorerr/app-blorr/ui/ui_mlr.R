tabPanel('Regression', value = 'tab_reg', icon = icon('cubes'),

	navlistPanel(id = 'navlist_reg',
        well = FALSE,
        widths = c(2, 10),

        source('ui/ui_regress.R', local = TRUE)[[1]],
        source('ui/ui_model_fit_stats.R', local = TRUE)[[1]],
        source('ui/ui_multi_model_fit_stats.R', local = TRUE)[[1]]
				
    )
)
