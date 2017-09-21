tabPanel('Screen', value = 'tab_scr', icon = icon('binoculars'),

    navlistPanel(id = 'navlist_scr',
        well = FALSE,
        widths = c(2, 10),

        source('ui/ui_screen.R', local = TRUE)[[1]]

    )
)
