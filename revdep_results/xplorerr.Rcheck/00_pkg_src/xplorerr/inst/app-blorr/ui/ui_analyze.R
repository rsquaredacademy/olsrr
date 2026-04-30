navbarMenu('Analyze', icon = icon('search-plus'),
	source('ui/ui_homes.R', local = TRUE)[[1]],
	source('ui/ui_bivar.R', local = TRUE)[[1]],
  source('ui/ui_mlr.R', local = TRUE)[[1]],
	source('ui/ui_varsel.R', local = TRUE)[[1]],
	source('ui/ui_valid.R', local = TRUE)[[1]],
	source('ui/ui_resid_diag.R', local = TRUE)[[1]]
)
