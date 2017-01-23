#' @title Residual Box Plot
#' @description Residual Box Plot
#' @param model an object of class \code{lm}
#' @export
#'
resid_boxplot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	resid <- residuals(model)
	boxplot(resid, xlab = 'Residuals',
	        main = "Residual Box Plot")

}

# studentized residuals vs leverage plot
rstudlev <- function(model) {

	leverage  <- unname(hatvalues(model))
	rstudent  <- unname(rstudent(model))
	n         <- nrow(model.frame(model))
	nam       <- names(model.frame(model))
	k         <- length(model$coefficients)
	levrstud  <- tibble(obs = seq_len(n), leverage, rstudent)
	lev_thrsh <- ((2 * k) + 2) / n

	miny      <- min(rstudent) - 3
	maxy      <- max(rstudent) + 3
	minx      <- min(leverage)
	maxx      <- ifelse((max(leverage) > lev_thrsh), max(leverage), (lev_thrsh + 0.05))

	levrstud$color[(leverage < lev_thrsh & abs(rstudent) < 2)] <- "normal"
	levrstud$color[(leverage > lev_thrsh & abs(rstudent) < 2)] <- "leverage"
	levrstud$color[(leverage < lev_thrsh & abs(rstudent) > 2)] <- "outlier"
	levrstud$color[(leverage > lev_thrsh & abs(rstudent) > 2)] <- "outlier & leverage"

	levrstud$color3 <- factor(levrstud$color)
	levrstud$Observation <- ordered(levrstud$color3,
	                           levels = c("normal", "leverage", "outlier",
	                                      "outlier & leverage"))

	result <- list(levrstud = levrstud, lev_thrsh = lev_thrsh, minx = minx,
		maxx = maxx, miny = miny, maxy = maxy, nam = nam)
	return(result)
	
}
