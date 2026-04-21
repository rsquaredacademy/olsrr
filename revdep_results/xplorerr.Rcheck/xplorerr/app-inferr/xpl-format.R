fg <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "centre")
}

fk <- function(x, w) {
  format(x, width = w, justify = "centre", nsmall = 3)
}


fs <- function() {
  rep("  ")
}

fl <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "left")
}

fc <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "centre")
}

formatter_t <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "centre")
}

format_cil <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "centre")
}

format_ciu <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "centre")
}

formats_t <- function() {
  rep("  ")
}

formatter_pair <- function(x, w) {
  x1  <- format(x, nsmall = 2)
  x2  <- as.character(x1)
  ret <- format(x2, width = w, justify = "centre")
  return(ret)
}

fw <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "centre")
}

fn <- function(x, w) {
  x %>%
    as.character() %>%
    format(width = w, justify = "centre")
}

formats <- function() {
  rep("    ")
}
