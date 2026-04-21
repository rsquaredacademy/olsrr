source('helper/utils.R')
source('helper/output.R')

freq_cont <- function(dframe, x, bins = 5) UseMethod("freq_cont")

freq_cont.default <- function(dframe, x, bins = 5) {

  if(!is.data.frame(dframe)) {
    stop('dframe must be a data frame')
  }

  if(!is.numeric(bins)) {
    stop('bins must be integer value')
  }

  if (!is.character(x)) {
    stop('x must be character')
  }

  if (!x %in% colnames(dframe)) {
    stop('x must be a column in dframe')
  }

  if(is.numeric(bins)) {
    bins <- as.integer(bins)
  }

  var_name <- x
  data <- dframe %>%
          select_(x) %>%
          as.data.frame() %>%
          unlist() %>%
          na.omit()
  n_bins <- bins
  inta <- intervals(data, bins)
  result <- freq(data, bins, inta)
  data_len <- length(data)
  cum <- cumsum(result)
  per <- percent(result, data_len)
  cum_per <- percent(cum, data_len)
  out <- list(breaks = inta,
              frequency = result,
              cumulative = cum,
              percent = per,
              cum_percent = cum_per,
              bins = n_bins,
              data = data,
              varname = var_name)

  class(out) <- "freq_cont"
  return(out)
}


print.freq_cont <- function(x, ...) {
  print_fcont(x)
}


hist.freq_cont <- function(x, col = 'blue', ...) {

  ymax <- max(x$frequency) + 2
  h <-  hist(x$data, breaks = x$breaks,
        main = paste('Histogram of', x$varname),
        xlab = x$varname, ylab = 'Frequency', ylim = c(0, ymax), col = col)
  text(h$mids, h$counts + 1, labels = h$counts, adj = 0.5, pos = 1)

}


