tabPanel('Select', value = 'tab_sel', icon = icon('database'),

    navlistPanel(id = 'navlist_up',
        well = FALSE,
        widths = c(2, 10),

        source('ui/ui_seldata.R', local = TRUE)[[1]]

    )
)
