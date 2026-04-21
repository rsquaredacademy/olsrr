fl <- function(x, w) {
  x <- as.character(x)
  ret <- format(x, width = w, justify = "left")
  return(ret)
}

fg <- function(x, w) {
  z <- as.character(x)
  y <- format(z, width = w, justify = "right")
  return(y)
}

fsp <- function() {
  x <- rep("  ")
  return(x)
}

fc <- function(x,w){
  x<- as.character (x)
  r<-format(x, width = w, justify = "centre")
  return(r)
}

fr <- function(x,w1){
  x<- as.character (x)
  r<-format(x, width = w1, justify = "right")
  return(r)
}

fs <- function() {
  x <- rep("  ")
  return(x)
}

plot_stepwise <- function (d, title)
{
  a <- NULL
  b <- NULL
  ggplot(d, aes(x = a, y = b)) + geom_line(color = "blue") +
    geom_point(color = "blue", shape = 1, size = 2) + xlab("") +
    ylab("") + ggtitle(title) + theme(axis.ticks = element_blank())
}

print_step_forward <- function(data) {
  n <- length(data$predictors)

  if (n < 1) {
    stop("No variables have been added to the model based on p-values.")
  }

  # width
  w1 <- nchar("Step")
  w2 <- max(nchar("Variable"), nchar(data$predictors))
  w3 <- max(nchar("R-Square"), nchar(format(round(data$rsquare, 4), nsmall = 4)))
  w4 <- max(nchar("R-Square"), nchar(format(round(data$adjr, 4), nsmall = 4)))
  w5 <- max(nchar("C(p)"), nchar(format(round(data$mallows_cp, 4), nsmall = 4)))
  w6 <- max(nchar("AIC"), nchar(format(round(data$aic, 4), nsmall = 4)))
  w7 <- max(nchar("RMSE"), nchar(format(round(data$rmse, 4), nsmall = 4)))
  w <- sum(w1, w2, w3, w4, w5, w6, w7, 24)

  cat("\n")
  cat(format("Selection Summary", justify = "centre", width = w), "\n")
  cat(rep("-", w), sep = "", "\n")
  cat(
    format("", width = w1), fs(), format("Variable", width = w2), fs(),
    format("", width = w3), fs(), format("Adj.", width = w4, justify = "centre"), fs(),
    format("", width = w5), fs(), format("", width = w6), fs(),
    format("", width = w7), fs(), "\n"
  )
  cat(
    format("Step", width = w1, justify = "centre"), fs(), format("Entered", width = w2, justify = "centre"), fs(),
    format("R-Square", width = w3, justify = "centre"), fs(), format("R-Square", width = w4, justify = "centre"), fs(),
    format("C(p)", width = w5, justify = "centre"), fs(), format("AIC", width = w6, justify = "centre"), fs(),
    format("RMSE", width = w7, justify = "centre"), fs(), "\n"
  )
  cat(rep("-", w), sep = "", "\n")

  for (i in seq_len(n)) {
    cat(
      format(i, width = w1), fs(), format(data$predictors[i], width = w2), fs(),
      format(round(data$rsquare[i], 4), width = w3, nsmall = 4), fs(),
      format(round(data$adjr[i], 4), width = w4, nsmall = 4), fs(),
      format(round(data$mallows_cp[i], 4), width = w5, justify = "centre", nsmall = 4), fs(),
      format(round(data$aic[i], 4), width = w6, nsmall = 4), fs(), format(round(data$rmse[i], 4), width = w7, nsmall = 4), fs(), "\n"
    )
  }
  cat(rep("-", w), sep = "", "\n")
}


