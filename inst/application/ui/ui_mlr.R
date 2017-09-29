tabPanel('Linear Regression', value = 'tab_reg', icon = icon('cubes'),

	navlistPanel(id = 'navlist_reg',
        well = FALSE,
        widths = c(2, 10),

        source('ui/ui_regress.R', local = TRUE)[[1]],
				# source('ui/ui_mselection2.R', local = TRUE)[[1]],
				source('ui/ui_resdiagtrial.R', local = TRUE)[[1]],
				source('ui/ui_hetero.R', local = TRUE)[[1]],
				source('ui/ui_collin.R', local = TRUE)[[1]],
				source('ui/ui_inflobs2.R', local = TRUE)[[1]],
				source('ui/ui_mfit2.R', local = TRUE)[[1]],
				source('ui/ui_regdiag.R', local = TRUE)[[1]]
				
    )
)
