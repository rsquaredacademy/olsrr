tabPanel('Variable Selection', value = 'tab_varsel', icon = icon('cubes'),

	navlistPanel(id = 'navlist_varsel',
        well = FALSE,
        widths = c(2, 10),

        source('ui/ui_varsel_forward.R', local = TRUE)[[1]],
        source('ui/ui_varsel_backward.R', local = TRUE)[[1]],
        source('ui/ui_varsel_stepwise.R', local = TRUE)[[1]]
				
    )
)
