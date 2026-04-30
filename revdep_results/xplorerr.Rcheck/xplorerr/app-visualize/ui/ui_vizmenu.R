tabPanel('Home', value = 'tab_viz_home', icon = icon('home'),

	navlistPanel(id = 'navlist_vizmenu',
        well = FALSE,
        widths = c(2, 10),

        source('ui/ui_vizhome.R', local = TRUE)[[1]],
        source('ui/ui_viz_base.R', local = TRUE)[[1]],
        source('ui/ui_viz_gg.R', local = TRUE)[[1]],
        source('ui/ui_viz_others.R', local = TRUE)[[1]]
    )
)
