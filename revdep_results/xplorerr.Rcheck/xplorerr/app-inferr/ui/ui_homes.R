tabPanel('Home', value = 'tab_home_analyze', icon = icon('home'),
         
    navlistPanel(id = 'navlist_home',
        well = FALSE,
        widths = c(2, 10),
        
        # source('ui/ui_home.R', local = TRUE)[[1]],
        # source('ui/ui_eda_home.R', local = TRUE)[[1]],
        # source('ui/ui_dist_home.R', local = TRUE)[[1]],
        source('ui/ui_infer_home.R', local = TRUE)[[1]],
        source('ui/ui_infer1_home.R', local = TRUE)[[1]],
        source('ui/ui_infer2_home.R', local = TRUE)[[1]],
        source('ui/ui_infer3_home.R', local = TRUE)[[1]]
        # source('ui/ui_model_home.R', local = TRUE)[[1]]
    )
)