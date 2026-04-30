tabPanel('Residual Diagnostics', value = 'tab_resid', icon = icon('cubes'),

	navlistPanel(id = 'navlist_resid',
        well = FALSE,
        widths = c(2, 10),

        source('ui/ui_diag_influence.R', local = TRUE)[[1]],
        source('ui/ui_diag_leverage.R', local = TRUE)[[1]],
        source('ui/ui_diag_fit.R', local = TRUE)[[1]],
        source('ui/ui_dfbetas_panel.R', local = TRUE)[[1]]
				
    )
)
