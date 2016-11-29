regress <- function(formula, data, conf.level = 0.95, title = 'model') UseMethod('regress')


regress.default <- function(formula, data, conf.level = 0.95, title = 'model') {

  # if (!inherits(formula, 'formula')) {
  # 	stop('Please specify a valid formula.', call. = FALSE)
  # }

  if (missing(data)) {
    stop('data missing', call. = FALSE)
  }

  if(!is.data.frame(data)) {
    stop('data must be a data frame', call. = FALSE)
  }

  if (!is.numeric(conf.level)) {
    stop('conf.level must be numeric', call. = FALSE)
  } 

  if ((conf.level < 0) | (conf.level > 1)) {
    stop('conf.level must be between 0 and 1', call. = FALSE)
  }

  if (!is.character(title)) {
    stop(paste(title, 'is not a string, Please specify a string as title.'), call. = FALSE)
  }

  result        <- reg_comp(formula, data, conf.level, title)
  class(result) <- 'regress'
  return(result)

}


regress.lm <- function(model, ...) {

    formula <- formula(model)
    data    <- model.frame(model)

    regress.default(formula = formula, data = data)

}


print.regress <- function(data) {

    print_reg(data)

}

