#' @importFrom stats model.frame residuals lm as.formula
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot aes geom_point xlab ylab stat_smooth
#' @importFrom magrittr %>%
#' @title Added Variable Plot
#' @description Added variable plot provides information about the marginal importance of a 
#' predictor variable, given the other predictor variables already in 
#' the model. It shows the marginal importance of the variable in reducing the
#' residual variability. 
#' @param model an object of class \code{lm}
#' @details The added variable plot was introduced by Mosteller and Tukey (1977). It enables
#' us to visualize the regression coefficient of a new variable being considered to
#' be included in a model. The plot can be constructed for each predictor variable.
#' 
#' Let us assume we want to test the effect of adding/removing variable \emph{X} from a
#' model. Let the response variable of the model be \emph{Y}
#' 
#' Steps to construct an added variable plot:
#' 
#' \itemize{
#'   \item Regress \emph{Y} on all variables other than \emph{X} and store the residuals (\emph{Y} residuals).
#'   \item Regress \emph{X} on all the other variables included in the model (\emph{X} residuals).
#'   \item Construct a scatter plot of \emph{Y} residuals and \emph{X} residuals.
#' }
#' 
#' What do the \emph{Y} and \emph{X} residuals represent? The \emph{Y} residuals represent the part
#' of \strong{Y} not explained by all the variables other than X. The \emph{X} residuals
#' represent the part of \strong{X} not explained by other variables. The slope of the line
#' fitted to the points in the added variable plot is equal to the regression
#' coefficient when \strong{Y} is regressed on all variables including \strong{X}.
#' 
#' A strong linear relationship in the added variable plot indicates the increased
#' importance of the contribution of \strong{X} to the model already containing the
#' other predictors.
#'
#' @references Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th ed. N.p.: John Wiley & Sons, 2012. Print.
#'
#' Kutner, MH, Nachtscheim CJ, Neter J and Li W., 2004, Applied Linear Statistical Models (5th edition). 
#' Chicago, IL., McGraw Hill/Irwin.
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_avplots(model)
#' @export
#'
ols_avplots <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }
	
		 #   m1 <- tibble::as_data_frame(model.frame(model))
	   #   m2 <- tibble::as_data_frame(model.matrix(model)[, c(-1)])
	   # data <- tibble::as_data_frame(cbind(m1[, c(1)], m2))
	 	 # xnames <- colnames(data)

	# data <- eval(model$call$data)
	# xnames <- names(model$coefficients)[-1]
	# # xnames <- colnames(attr(model$terms, 'factors'))
	# nl <- xnames %>% length()
	# resp <- rownames(attr(model$terms, 'factors'))[1]

	    m1 <- tibble::as_data_frame(model.frame(model))
	    m2 <- tibble::as_data_frame(model.matrix(model)[, c(-1)])
	  data <- tibble::as_data_frame(cbind(m1[, c(1)], m2))
	xnames <- colnames(data)
	    nl <- xnames %>% length()
	  resp <- xnames[1]

	myplots <- list()

	for(i in 2:nl) {
	    
	    x <- advarx(data, i)
	    y <- advary(data, i)
	    d <- tibble(x, y)
	    p <- eval(substitute(ggplot(d, aes(x = x, y = y)) +
	                             geom_point(colour = 'blue', size = 2) +
	                             xlab(paste(xnames[i], " | Others")) +
	                             ylab(paste(resp, " | Others")) +
	                             stat_smooth(method="lm", se=FALSE), list(i = i)))
	    
	    # print(p)
	    j <- i - 1
	    myplots[[j]] <- p
	    
	}

	do.call(grid.arrange, c(myplots, list(ncol = 2)))

}
