tabPanel('Others', value = 'tab_others', icon = icon('pie-chart'),

	navlistPanel(id = 'navlist_others',
    well = FALSE,
    widths = c(2, 10),

    source('ui/ui_bar_plot_1.R', local = TRUE)[[1]],
    source('ui/ui_bar_plot_2.R', local = TRUE)[[1]],
    source('ui/ui_box_plot_1.R', local = TRUE)[[1]],
    source('ui/ui_box_plot_2.R', local = TRUE)[[1]],
    source('ui/ui_scatter_prh.R', local = TRUE)[[1]],
    source('ui/ui_line_prh.R', local = TRUE)[[1]],
    source('ui/ui_hist_prh.R', local = TRUE)[[1]],
    source('ui/ui_pie_prh.R', local = TRUE)[[1]]

  )
)
