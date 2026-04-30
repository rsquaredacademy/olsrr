#' Generic function for calculating standardized measures
#'
#' `SM_output` is an S3 generic function designed to calculate standardized
#' measures. It dispatches to the appropriate method based on the class of the
#' input (`fit`), ensuring the correct method is applied for different types of models.
#'
#' @param fit the input object, typically a fitted model, for which standardized measures
#' are calculated. The method applied depends on the class of this object.
#' @param ... additional arguments that can be passed to specific methods.
#'
#' @return the return varies depending on the method implemented for the
#' class of the input object.
#'
#' @export

SM_output <- function(fit, ...) {
  UseMethod("SM_output")
}
