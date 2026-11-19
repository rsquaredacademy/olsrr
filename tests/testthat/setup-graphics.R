# Plot tests must not open the default file device (Rplots.pdf).
old_device <- getOption("device")
options(device = function(...) grDevices::pdf(NULL))
grDevices::pdf(NULL)
test_device <- grDevices::dev.cur()
withr::defer({
  if (test_device %in% grDevices::dev.list()) {
    grDevices::dev.off(test_device)
  }
  options(device = old_device)
}, testthat::teardown_env())
