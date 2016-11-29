diag_panel <- function(model) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    op <- par(no.readonly = TRUE)
    on.exit(par(op))

    m  <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), nrow = 3, ncol = 3, byrow = TRUE)
    layout(mat = m, heights = c(2, 2, 2))

    rvsp_plot(model)
    dsrvsp_plot(model)
    studlev_plot(model)
    qqresid(model)
    ovsp_plot(model)
    cooksd_chart(model)
    fm_plot(model)
    rsd_plot(model)    
    hist_resid(model)   
    resid_boxplot(model)
    
}

