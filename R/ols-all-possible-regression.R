#' @importFrom ggplot2 ggtitle scale_shape_manual scale_size_manual scale_color_manual ggtitle geom_text
#' @importFrom utils combn
#' @importFrom dplyr group_by summarise_all
#' @importFrom purrr map_int
#' @importFrom tidyr nest
#' @title All Possible Regression
#' @description Fits all regressions involving one regressor, two regressors, three regressors, and so on.
#' It tests all possible subsets of the set of potential independent variables.
#' @param model an object of class \code{lm}
#' @param x an object of class \code{ols_best_subset}
#' @param ... other arguments
#' @return \code{ols_all_subset} returns an object of class \code{"ols_all_subset"}.
#' An object of class \code{"ols_all_subset"} is a data frame containing the
#' following components:
#'
#' \item{n}{model number}
#' \item{predictors}{predictors in the model}
#' \item{rsquare}{rsquare of the model}
#' \item{adjr}{adjusted rsquare of the model}
#' \item{predrsq}{predicted rsquare of the model}
#' \item{cp}{mallow's Cp}
#' \item{aic}{akaike information criteria}
#' \item{sbic}{sawa bayesian information criteria}
#' \item{sbc}{schwarz bayes information criteria}
#' \item{gmsep}{estimated MSE of prediction, assuming multivariate normality}
#' \item{jp}{final prediction error}
#' \item{pc}{amemiya prediction criteria}
#' \item{sp}{hocking's Sp}
#'
#' @references Mendenhall William and  Sinsich Terry, 2012, A Second Course in Statistics Regression Analysis (7th edition). 
#' Prentice Hall
#' @examples
#' model <- lm(mpg ~ disp + hp, data = mtcars)
#' k <- ols_all_subset(model)
#' k
#'
#' # plot
#' plot(k)
#'
#' @export
#'
ols_all_subset <- function(model, ...) UseMethod('ols_all_subset')

#' @export
#'
ols_all_subset.default <- function(model, ...) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    if (length(model$coefficients) < 3) {
        stop('Please specify a model with at least 2 predictors.', call. = FALSE)
    }

    nam   <- colnames(attr(model$terms, 'factors'))
    n     <- length(nam)
    r     <- seq_len(n)
    combs <- list()

    for (i in seq_len(n)) {
        combs[[i]] <- combn(n, r[i])
    }

    lc        <- length(combs)
    # nam       <- colnames(attr(model$terms, 'factors'))
    # nam       <- names(model$coefficients)[-1]
    varnames  <- names(model.frame(model))
    predicts  <- nam
    len_preds <- length(predicts)
    gap       <- len_preds - 1
    space     <- sum(nchar(predicts)) + gap
    data      <- mod_sel_data(model)
    colas     <- combs %>% map_int(ncol)
    # colas     <- c()
    # for(i in seq_len(lc)) {
    #     colas[i] <- ncol(combs[[i]])
    # }

    response <- varnames[1]
    p        <- colas
    t        <- cumsum(colas)
    q        <- c(1, t[-lc] + 1)
    mcount   <- 0
    rsq      <- list()
    adjrsq   <- list()
    predrsq  <- list()
    cp       <- list()
    aic      <- list()
    sbic     <- list()
    sbc      <- list()
    msep     <- list()
    fpe      <- list()
    apc      <- list()
    hsp      <- list()
    preds    <- list()
    lpreds   <- c()

    for (i in seq_len(lc)) {
        for (j in seq_len(colas[i])) {

                predictors        <- nam[combs[[i]][, j]]
                lp                <- length(predictors)
                out               <- ols_regress(paste(response, '~', paste(predictors, collapse = ' + ')), data = data)
                mcount            <- mcount + 1
                lpreds[mcount]    <- lp
                rsq[[mcount]]     <- out$rsq
                adjrsq[[mcount]]  <- out$adjr
                predrsq[[mcount]] <- ols_pred_rsq(out$model)
                cp[[mcount]]      <- ols_mallows_cp(out$model, model)
                aic[[mcount]]     <- ols_aic(out$model)
                sbic[[mcount]]    <- ols_sbic(out$model, model)
                sbc[[mcount]]     <- ols_sbc(out$model)
                msep[[mcount]]    <- ols_msep(out$model)
                fpe[[mcount]]     <- ols_fpe(out$model)
                apc[[mcount]]     <- ols_apc(out$model)
                hsp[[mcount]]     <- ols_hsp(out$model)
                preds[[mcount]]   <- paste(predictors, collapse = " ")

        }
    }

    ui <- data.frame(n               = lpreds,
                    predictors       = unlist(preds),
                    rsquare          = unlist(rsq),
                    adjr             = unlist(adjrsq),
                    predrsq          = unlist(predrsq),
                    cp               = unlist(cp),
                    aic              = unlist(aic),
                    sbic             = unlist(sbic),
                    sbc              = unlist(sbc),
                    msep             = unlist(msep),
                    fpe              = unlist(fpe),
                    apc              = unlist(apc),
                    hsp              = unlist(hsp),
                    stringsAsFactors = F)

    sorted <- c()

    for (i in seq_len(lc)) {
        temp   <- ui[q[i]:t[i], ]
        temp   <- temp[order(temp$rsquare, decreasing = TRUE), ]
        # temp   <- arrange(temp, desc(rsquare))
        sorted <- rbind(sorted, temp)
    }

    mindex <- seq_len(nrow(sorted))
    sorted <- cbind(mindex, sorted)

    class(sorted) <- c('ols_all_subset', 'tibble', 'data.frame')

    return(sorted)
}


#' @export
#'
print.ols_all_subset <- function(x, ...) {

    n <- max(x$mindex)
    k <- tibble::as_tibble(x)[, c(1:5, 7)]
    k$rsquare <- format(round(k$rsquare, 5), nsmall = 5)
    k$adjr <- format(round(k$adjr, 5), nsmall = 5)
    k$cp <- format(round(k$cp, 5), nsmall = 5)
    names(k) <- c('Index', 'N', 'Predictors', 'R-Square', 'Adj. R-Square', "Mallow's Cp")
    print(k)

}

#' @export
#' @rdname ols_all_subset
#'
plot.ols_all_subset <- function(x, model = NA, ...) {

  maxs  <- tapply(x$rsquare, x$n, max)
  lmaxs <- seq_len(length(maxs))
  # index <- c()

  # suppressWarnings(
  #   for (i in lmaxs) {
  #       index[i] <- which(x$rsquare == maxs[i])
  #   }
  # )
  
  n <- NULL
  rsquare <- NULL
  index <- c()

  d <- tibble(index = x$mindex, n = x$n, rsquare = x$rsquare)

  m <- d %>%
      group_by(n) %>%
      select(n, rsquare) %>%
      summarise_all(max) 

  k <- d %>%
      group_by(n) %>%
      nest()

  suppressWarnings(
      for (i in m$n) {
          index[i] <- k[[2]][[i]]$index[which(m$rsquare[i] == k[[2]][[i]]$rsquare)]
      }
  )

  maxs1  <- tapply(x$adjr, x$n, max)
  lmaxs1 <- seq_len(length(maxs1))
  index1 <- c()

  suppressWarnings(
    for (i in lmaxs1) {
        index1[i] <- which(x$adjr == maxs1[i])
    }
  )

  cps   <- abs(x$n - x$cp)
  mcps  <- tapply(cps, x$n, min)
  lmcps <- seq_len(length(mcps))
  imcps <- c()

  for (i in lmcps) {
    imcps[i] <- which(cps == mcps[i])
  }

  maxs2  <- tapply(x$aic, x$n, min)
  lmaxs2 <- seq_len(length(maxs2))
  index2 <- c()

  for (i in lmaxs2) {
    index2[i] <- which(x$aic == maxs2[i])
  }

  maxs3  <- tapply(x$sbic, x$n, min)
  lmaxs3 <- seq_len(length(maxs3))
  index3 <- c()

  for (i in lmaxs3) {
    index3[i] <- which(x$sbic == maxs3[i])
  }

  maxs4  <- tapply(x$sbc, x$n, min)
  lmaxs4 <- seq_len(length(maxs4))
  index4 <- c()

  for (i in lmaxs4) {
    index4[i] <- which(x$sbc == maxs4[i])
  }

  shape <- NULL
  size <- NULL
  tx <- NULL
  y <- NULL
  k <- NULL

  d1 <- data.frame(x = x$n, y = x$rsquare)
  d2 <- data.frame(x = lmaxs, y = maxs, tx = index, shape = 6, size = 4)
  p1 <- ggplot(d1, aes(x = x, y = y)) +
    geom_point(color = 'blue', size = 2) +
    xlab('') + ylab('') + ggtitle('R-Square') +
    geom_point(data = d2, aes(x = x, y = y, shape = factor(shape),
                              color = factor(shape), size = factor(size))) +
    scale_shape_manual(values = c(2), guide = FALSE) +
    scale_size_manual(values = c(4), guide = FALSE) +
    scale_color_manual(values = c('red'), guide = FALSE) +
    geom_text(data = d2, aes(label = tx), hjust = 0, nudge_x = 0.1)

  d3 <- data.frame(x = x$n, y = x$adjr)
  d4 <- data.frame(x = lmaxs1, y = maxs1, tx = index1, shape = 6, size = 4)
  p2 <- ggplot(d3, aes(x = x, y = y)) +
    geom_point(color = 'blue', size = 2) +
    xlab('') + ylab('') + ggtitle('Adj R-Square') +
    geom_point(data = d4, aes(x = x, y = y, shape = factor(shape),
                              color = factor(shape), size = factor(size))) +
    scale_shape_manual(values = c(2), guide = FALSE) +
    scale_size_manual(values = c(4), guide = FALSE) +
    scale_color_manual(values = c('red'), guide = FALSE) +
    geom_text(data = d4, aes(label = tx), hjust = 0, nudge_x = 0.1)

  d5 <- data.frame(x = x$n, y = x$cp)
  d6 <- data.frame(x = lmcps, y = x$cp[imcps], tx = imcps,  shape = 6, size = 4)
  p3 <- ggplot(d5, aes(x = x, y = y)) +
    geom_point(color = 'blue', size = 2) +
    xlab('') + ylab('') + ggtitle('C(p)') +
    geom_point(data = d6, aes(x = x, y = y, shape = factor(shape),
                              color = factor(shape), size = factor(size))) +
    scale_shape_manual(values = c(2), guide = FALSE) +
    scale_size_manual(values = c(4), guide = FALSE) +
    scale_color_manual(values = c('red'), guide = FALSE) +
    geom_text(data = d6, aes(label = tx), hjust = 0, nudge_x = 0.1)

  d7 <- data.frame(x = x$n, y = x$aic)
  d8 <- data.frame(x = lmaxs2, y = maxs2, tx = index2, shape = 6, size = 4)
  p4 <- ggplot(d7, aes(x = x, y = y)) +
    geom_point(color = 'blue', size = 2) +
    xlab('') + ylab('') + ggtitle('AIC') +
    geom_point(data = d8, aes(x = x, y = y, shape = factor(shape),
                              color = factor(shape), size = factor(size))) +
    scale_shape_manual(values = c(2), guide = FALSE) +
    scale_size_manual(values = c(4), guide = FALSE) +
    scale_color_manual(values = c('red'), guide = FALSE) +
    geom_text(data = d8, aes(label = tx), hjust = 0, nudge_x = 0.1)

  d9 <- data.frame(x = x$n, y = x$sbic)
  d10 <- data.frame(x = lmaxs3, y = maxs3, tx = index3, shape = 6, size = 4)
  p5 <- ggplot(d9, aes(x = x, y = y)) +
    geom_point(color = 'blue', size = 2) +
    xlab('Number of Predictors') + ylab('') + ggtitle('SBIC') +
    geom_point(data = d10, aes(x = x, y = y, shape = factor(shape),
                               color = factor(shape), size = factor(size))) +
    scale_shape_manual(values = c(2), guide = FALSE) +
    scale_size_manual(values = c(4), guide = FALSE) +
    scale_color_manual(values = c('red'), guide = FALSE) +
    geom_text(data = d10, aes(label = tx), hjust = 0, nudge_x = 0.1)

  d11 <- data.frame(x = x$n, y = x$sbc)
  d12 <- data.frame(x = lmaxs4, y = maxs4, tx = index4, shape = 6, size = 4)
  p6 <- ggplot(d11, aes(x = x, y = y)) +
    geom_point(color = 'blue', size = 2) +
    xlab('Number of Predictors') + ylab('') + ggtitle('SBC') +
    geom_point(data = d12, aes(x = x, y = y, shape = factor(shape),
                               color = factor(shape), size = factor(size))) +
    scale_shape_manual(values = c(2), guide = FALSE) +
    scale_size_manual(values = c(4), guide = FALSE) +
    scale_color_manual(values = c('red'), guide = FALSE) +
    geom_text(data = d12, aes(label = tx), hjust = 0, nudge_x = 0.1)

  grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, top = 'All Subset Regression')

  result <- list(rsquare_plot = p1, adj_rsquare_plot = p2, mallows_cp_plot = p3,
                aic_plot = p4, sbic_plot = p5, sbc_plot = p6)
  invisible(result)

}



