navbarMenu('Data', icon = icon('database'),
  source('ui/ui_up.R', local = TRUE)[[1]],
  source('ui/ui_trans.R', local = TRUE)[[1]],
  source('ui/ui_scr.R', local = TRUE)[[1]],
  # source('ui/ui_sample.R', local = TRUE)[[1]],
  # source('ui/ui_partition.R', local = TRUE)[[1]],
  source('ui/ui_vi.R', local = TRUE)[[1]]
)
