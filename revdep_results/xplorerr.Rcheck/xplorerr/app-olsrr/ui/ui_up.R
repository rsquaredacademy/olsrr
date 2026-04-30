tabPanel('Get Data', value = 'tab_upload', icon = icon('server'),

    navlistPanel(id = 'navlist_up',
        well = FALSE,
        widths = c(2, 10),

        source('ui/ui_dataoptions.R', local = TRUE)[[1]],
        source('ui/ui_datafiles.R', local = TRUE)[[1]],
        source('ui/ui_datasamples.R', local = TRUE)[[1]]
        # source('ui/ui_upload.R', local = TRUE)[[1]],
        # source('ui/ui_excel.R', local = TRUE)[[1]],
        # source('ui/ui_json.R', local = TRUE)[[1]],
        # source('ui/ui_stata.R', local = TRUE)[[1]],
        # source('ui/ui_spss.R', local = TRUE)[[1]],
        # source('ui/ui_sas.R', local = TRUE)[[1]]

    )
)
