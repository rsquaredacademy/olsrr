poten_resid_plot <- function(model) {
    
    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }
    
    hi  <- hadi(model)
    pot <- unname(hi$potential)
    res <- unname(hi$residual)
    plot(res, pot, col = "blue", xlab = "Residual", ylab = "Potential", 
         main = "Potential-Residual Plot")
    
}