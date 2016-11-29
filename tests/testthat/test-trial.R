# library
library(testthat)

# equal
expect_equal(10, 10)
expect_equal(10, 11)
expect_identical(10, 10)
expect_identical(10, 10.00002)

# match
string <- 'hello'
expect_match(string, 'hello')
expect_match(string, 'Hello')
expect_match(string, 'Hello', ignore.case = TRUE)

# output
a <- list(1:10, letters)
expect_output(str(a), 'List of 2')
expect_output(str(a), "int [1:10]", fixed = TRUE)

# message 
expect_message(library(mgcv), 'This is mgcv')

# warning
expect_warning(log(-1))
expect_warning(log(-1), "NaNs produced")
expect_warning(log(0))

# error
expect_error(1 / "a")
expect_error(1 / "a", "non-numeric argument")

# is
expect_is(model, "lm")
expect_is(model, "glm")

