#' @title Pluto-Tasche method for one-year probability of default (PD) analysis
#' @description
#' Calculates probability of default according to One-period Pluto and Tasche model
#' @param pf unconditional portfolio distribution from the worst to the best credit quality
#' @param num_def number of defaults in a given rating class
#' @param ci condifence interval of PD estimates
#' @examples
#' pf <- c(10,20,30,40)
#' num_def <- c(1,2,3,4)
#' pt_one(pf, num_def, ci= 0.9)
#' @references Surzhko, Denis. Published 2015-05-21. LDPD package. Archived on 2022-06-20.
#' @rdname pt_one
#' @export

pt_one <-function (pf, num_def, ci = 0.9){
  r.num <- length(pf)
  r.PD <- rep(0, r.num)
  portf.CNum <- rev(cumsum(pf))
  portf.CDef <- rev(cumsum(num_def))

  for (r in seq_len(r.num)) {
    if (portf.CDef[r] == portf.CNum[r]) {
      r.PD[r] <- 1
    } else {
      f <- function(x) pbinom(portf.CDef[r], portf.CNum[r], x) - 1 + ci
      r.PD[r] <- uniroot(f, c(0, 1))$root
    }
  }
  pd <- rev(round(r.PD, 3))

  cat("Estimated probability of default:", pd, "\n")
  return(pd)
}

