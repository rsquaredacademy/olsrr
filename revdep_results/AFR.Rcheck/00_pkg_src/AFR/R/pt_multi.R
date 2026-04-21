#' @title Pluto-Tasche method for multi-year probability of default (PD) analysis
#' @description
#' Calculates the variation inflation factors of all predictors in regression models
#' @param pf unconditional portfolio distribution from the worst to the best credit quality
#' @param num_def number of defaults in a given rating class
#' @param conf_level confidence interval of PD estimates
#' @param num_years number of periods used in the PD estimation
#' @examples
#' pf <- c(10,20,30,40)
#' num_def <- c(1,2,3,4)
#' conf_level = 0.99
#' num_years = 3
#' pt_multi(pf, num_def, conf_level, num_years)
#' @rdname pt_multi
#' @export

pt_multi <- function(pf, num_def, conf_level, num_years) {

  mean <- mean(pf)
  var <- var(pf)


  threshold <- qnorm(1 - conf_level) * sqrt(var * num_years) + mean * num_years


  excess_defaults <- num_def - threshold


  pd <- pnorm(-excess_defaults / sqrt(var * num_years), lower.tail = FALSE)


  pd <- rev(round(pd, 3))

  cat("Estimated probability of default:\n")
  return(pd)
}

