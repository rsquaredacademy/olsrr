tabPanel('Model Validation', value = 'tab_valid', icon = icon('cubes'),

	navlistPanel(id = 'navlist_valid',
        well = FALSE,
        widths = c(2, 10),

        source('ui/ui_conf_matrix.R', local = TRUE)[[1]],
        source('ui/ui_hoslem_test.R', local = TRUE)[[1]],
        source('ui/ui_gains_table.R', local = TRUE)[[1]],
        source('ui/ui_roc_curve.R', local = TRUE)[[1]],
        source('ui/ui_ks_chart.R', local = TRUE)[[1]],
        source('ui/ui_lorenz_curve.R', local = TRUE)[[1]]
				
    )
)
