tabPanel('Inference', value = 'tab_infer', icon = icon('cogs'),

	navlistPanel(id = 'navlist_infer',
        well = FALSE,
        widths = c(2, 10),

        source('ui/ui_ttest.R', local = TRUE)[[1]],
        source('ui/ui_indttest.R', local = TRUE)[[1]],
        source('ui/ui_ptest.R', local = TRUE)[[1]],
        source('ui/ui_binomtest.R', local = TRUE)[[1]],
				source('ui/ui_osvar.R', local = TRUE)[[1]],
				source('ui/ui_tsvar.R', local = TRUE)[[1]],
				source('ui/ui_osprop.R', local = TRUE)[[1]],
				source('ui/ui_tsprop.R', local = TRUE)[[1]],
				source('ui/ui_anova.R', local = TRUE)[[1]],
				source('ui/ui_levene.R', local = TRUE)[[1]],
				source('ui/ui_chigof.R', local = TRUE)[[1]],
				source('ui/ui_chict.R', local = TRUE)[[1]],
				source('ui/ui_cochran.R', local = TRUE)[[1]],
				source('ui/ui_runs.R', local = TRUE)[[1]],
				source('ui/ui_mcnemar.R', local = TRUE)[[1]]

    )
)
