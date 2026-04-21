navbarMenu('Visualize', icon = icon('line-chart'),
	source('ui/ui_vizmenu.R', local = TRUE)[[1]],
	# source('ui/ui_vizlib.R', local = TRUE)[[1]],
  source('ui/ui_base.R', local = TRUE)[[1]],
  source('ui/ui_ggplot.R', local = TRUE)[[1]],
  source('ui/ui_others.R', local = TRUE)[[1]]
)

