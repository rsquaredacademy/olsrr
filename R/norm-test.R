corr_test <- function(model) {

		if (!all(class(model) == 'lm')) {
      stop('Please specify a OLS linear regression model.', call. = FALSE)
    }
    
    n       <- nrow(model.frame(model))
    stderr  <- summary(model)$sigma
    expvals <- sapply(1:n, function(k) stderr * qnorm((k - 0.375) / (n + 0.25)))
    out     <- cor(expvals, sort(model$residuals))
    return(out)
    
}


norm_test <- function(y) UseMethod('norm_test')

norm_test.default <- function(y) {

	if (!is.numeric(y)) {
		stop('y must be numeric')
	}
	
	ks  <- ks.test(y, "pnorm", mean(y), sd(y))            
	sw  <- shapiro.test(y)                                
	cvm <- goftest::cvm.test(y, "pnorm", mean(y), sd(y))  
	ad  <- nortest::ad.test(y)                            

	result <- list(kolmogorv = ks, 
								 shapiro   = sw,
								 cramer    = cvm, 
								 anderson  = ad)

	class(result) <- 'norm_test'

	return(result)

}


norm_test.lm <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	norm_test.default(residuals(model))

}


print.norm_test <- function(data) {

	print_norm_test(data)
	
}
