pkgname <- "pprof"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('pprof')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ExampleDataBinary")
### * ExampleDataBinary

flush(stderr()); flush(stdout())

### Name: ExampleDataBinary
### Title: Example data with binary outcomes
### Aliases: ExampleDataBinary
### Keywords: datasets

### ** Examples

data(ExampleDataBinary)
head(ExampleDataBinary$Y)
head(ExampleDataBinary$ProvID)
head(ExampleDataBinary$Z)



cleanEx()
nameEx("ExampleDataLinear")
### * ExampleDataLinear

flush(stderr()); flush(stdout())

### Name: ExampleDataLinear
### Title: Example data with continuous outcomes
### Aliases: ExampleDataLinear
### Keywords: datasets

### ** Examples

data(ExampleDataLinear)
head(ExampleDataLinear$Y)
head(ExampleDataLinear$ProvID)
head(ExampleDataLinear$Z)



cleanEx()
nameEx("SM_output.linear_cre")
### * SM_output.linear_cre

flush(stderr()); flush(stdout())

### Name: SM_output.linear_cre
### Title: Calculate direct/indirect standardized differences from a fitted
###   'linear_cre' object
### Aliases: SM_output.linear_cre

### ** Examples

data(ExampleDataLinear)
outcome <- ExampleDataLinear$Y
covar <- ExampleDataLinear$Z
ProvID <- ExampleDataLinear$ProvID
data <- data.frame(outcome, ProvID, covar)
outcome.char <- colnames(data)[1]
ProvID.char <- colnames(data)[2]
wb.char <- c("z1", "z2")
other.char <- c("z3", "z4", "z5")
fit_cre <- linear_cre(data = data, Y.char = outcome.char, ProvID.char = ProvID.char,
wb.char = wb.char, other.char = other.char)
SM_output(fit_cre)




cleanEx()
nameEx("SM_output.linear_fe")
### * SM_output.linear_fe

flush(stderr()); flush(stdout())

### Name: SM_output.linear_fe
### Title: Calculate direct/indirect standardized differences from a fitted
###   'linear_fe' object
### Aliases: SM_output.linear_fe

### ** Examples

data(ExampleDataLinear)
outcome <- ExampleDataLinear$Y
covar <- ExampleDataLinear$Z
ProvID <- ExampleDataLinear$ProvID
fit_linear <- linear_fe(Y = outcome, Z = covar, ProvID = ProvID)
SM_output(fit_linear)
SM_output(fit_linear, stdz = "direct", null = "mean")




cleanEx()
nameEx("SM_output.linear_re")
### * SM_output.linear_re

flush(stderr()); flush(stdout())

### Name: SM_output.linear_re
### Title: Calculate direct/indirect standardized differences from a fitted
###   'linear_re' object
### Aliases: SM_output.linear_re

### ** Examples

data(ExampleDataLinear)
outcome <- ExampleDataLinear$Y
covar <- ExampleDataLinear$Z
ProvID <- ExampleDataLinear$ProvID
fit_linear <- linear_re(Y = outcome, Z = covar, ProvID = ProvID)
SM_output(fit_linear)




cleanEx()
nameEx("SM_output.logis_cre")
### * SM_output.logis_cre

flush(stderr()); flush(stdout())

### Name: SM_output.logis_cre
### Title: Calculate direct/indirect standardized ratios/rates from a
###   fitted 'logis_cre' object
### Aliases: SM_output.logis_cre

### ** Examples

data(ExampleDataBinary)
outcome <- ExampleDataBinary$Y
covar <- ExampleDataBinary$Z
ProvID <- ExampleDataBinary$ProvID
data <- data.frame(outcome, ProvID, covar)
outcome.char <- colnames(data)[1]
ProvID.char <- colnames(data)[2]
wb.char <- c("z1", "z2")
other.char <- c("z3", "z4", "z5")
fit_cre <- logis_cre(data = data, Y.char = outcome.char, ProvID.char = ProvID.char,
wb.char = wb.char, other.char = other.char)

SR <- SM_output(fit_cre, stdz = "direct", measure = "rate")
SR$direct.rate




cleanEx()
nameEx("SM_output.logis_fe")
### * SM_output.logis_fe

flush(stderr()); flush(stdout())

### Name: SM_output.logis_fe
### Title: Calculate direct/indirect standardized ratios/rates from a
###   fitted 'logis_fe' object
### Aliases: SM_output.logis_fe

### ** Examples

data(ExampleDataBinary)
outcome = ExampleDataBinary$Y
covar = ExampleDataBinary$Z
ProvID = ExampleDataBinary$ProvID
fit_fe <- logis_fe(Y = outcome, Z = covar, ProvID = ProvID, message = FALSE)
SR <- SM_output(fit_fe, stdz = "direct", measure = "rate")
SR$direct.rate




cleanEx()
nameEx("SM_output.logis_re")
### * SM_output.logis_re

flush(stderr()); flush(stdout())

### Name: SM_output.logis_re
### Title: Calculate direct/indirect standardized ratios/rates from a
###   fitted 'logis_re' object
### Aliases: SM_output.logis_re

### ** Examples

data(ExampleDataBinary)
outcome = ExampleDataBinary$Y
covar = ExampleDataBinary$Z
ProvID = ExampleDataBinary$ProvID
fit_re <- logis_re(Y = outcome, Z = covar, ProvID = ProvID)
SR <- SM_output(fit_re, stdz = "direct", measure = "rate")
SR$direct.rate




cleanEx()
nameEx("bar_plot")
### * bar_plot

flush(stderr()); flush(stdout())

### Name: bar_plot
### Title: Get a bar plot for flagging percentage overall and stratified by
###   provider sizes
### Aliases: bar_plot

### ** Examples

data(ExampleDataLinear)
outcome <- ExampleDataLinear$Y
covar <- ExampleDataLinear$Z
ProvID <- ExampleDataLinear$ProvID
fit_linear <- linear_fe(Y = outcome, Z = covar, ProvID = ProvID)
test_linear <- test(fit_linear)
bar_plot(test_linear)

data(ExampleDataBinary)
fit_logis <- logis_fe(Y = ExampleDataBinary$Y,
                      Z = ExampleDataBinary$Z,
                      ProvID = ExampleDataBinary$ProvID, message = FALSE)
test_logis <- test(fit_logis)
bar_plot(test_logis)




cleanEx()
nameEx("caterpillar_plot")
### * caterpillar_plot

flush(stderr()); flush(stdout())

### Name: caterpillar_plot
### Title: Get a caterpillar plot to display confidence intervals for
###   standardized measures
### Aliases: caterpillar_plot

### ** Examples

data(ExampleDataLinear)
outcome <- ExampleDataLinear$Y
covar <- ExampleDataLinear$Z
ProvID <- ExampleDataLinear$ProvID
fit_linear <- linear_fe(Y = outcome, Z = covar, ProvID = ProvID)
CI_linear <- confint(fit_linear)
caterpillar_plot(CI_linear$CI.indirect, use_flag = TRUE,
                 errorbar_width = 0.5, errorbar_size = 1)

data(ExampleDataBinary)
fit_logis <- logis_fe(Y = ExampleDataBinary$Y,
                      Z = ExampleDataBinary$Z,
                      ProvID = ExampleDataBinary$ProvID, message = FALSE)
CI_logis <- confint(fit_logis)
caterpillar_plot(CI_logis$CI.indirect_ratio, use_flag = TRUE,
                 errorbar_width = 0.5, errorbar_size = 1,
                 orientation = "horizontal")




cleanEx()
nameEx("confint.linear_cre")
### * confint.linear_cre

flush(stderr()); flush(stdout())

### Name: confint.linear_cre
### Title: Get confidence intervals for provider effects or standardized
###   measures from a fitted 'linear_cre' object
### Aliases: confint.linear_cre

### ** Examples

data(ExampleDataLinear)
outcome <- ExampleDataLinear$Y
covar <- ExampleDataLinear$Z
ProvID <- ExampleDataLinear$ProvID
data <- data.frame(outcome, ProvID, covar)
outcome.char <- colnames(data)[1]
ProvID.char <- colnames(data)[2]
wb.char <- c("z1", "z2")
other.char <- c("z3", "z4", "z5")
fit_cre <- linear_cre(data = data, Y.char = outcome.char, ProvID.char = ProvID.char,
wb.char = wb.char, other.char = other.char)
confint(fit_cre)




cleanEx()
nameEx("confint.linear_fe")
### * confint.linear_fe

flush(stderr()); flush(stdout())

### Name: confint.linear_fe
### Title: Get confidence intervals for provider effects or standardized
###   measures from a fitted 'linear_fe' object
### Aliases: confint.linear_fe

### ** Examples

data(ExampleDataLinear)
outcome <- ExampleDataLinear$Y
covar <- ExampleDataLinear$Z
ProvID <- ExampleDataLinear$ProvID
fit_linear <- linear_fe(Y = outcome, Z = covar, ProvID = ProvID)
confint(fit_linear)




cleanEx()
nameEx("confint.linear_re")
### * confint.linear_re

flush(stderr()); flush(stdout())

### Name: confint.linear_re
### Title: Get confidence intervals for provider effects or standardized
###   measures from a fitted 'linear_re' object
### Aliases: confint.linear_re

### ** Examples

data(ExampleDataLinear)
outcome <- ExampleDataLinear$Y
ProvID <- ExampleDataLinear$ProvID
covar <- ExampleDataLinear$Z
fit_re <- linear_re(Y = outcome, Z = covar, ProvID = ProvID)
confint(fit_re)




cleanEx()
nameEx("confint.logis_cre")
### * confint.logis_cre

flush(stderr()); flush(stdout())

### Name: confint.logis_cre
### Title: Get confidence intervals for provider effects or standardized
###   measures from a fitted 'logis_cre' object
### Aliases: confint.logis_cre

### ** Examples

data(ExampleDataBinary)
outcome <- ExampleDataBinary$Y
covar <- ExampleDataBinary$Z
ProvID <- ExampleDataBinary$ProvID
data <- data.frame(outcome, ProvID, covar)
outcome.char <- colnames(data)[1]
ProvID.char <- colnames(data)[2]
wb.char <- c("z1", "z2")
other.char <- c("z3", "z4", "z5")
fit_cre <- logis_cre(data = data, Y.char = outcome.char, ProvID.char = ProvID.char,
wb.char = wb.char, other.char = other.char)
confint(fit_cre)




cleanEx()
nameEx("confint.logis_fe")
### * confint.logis_fe

flush(stderr()); flush(stdout())

### Name: confint.logis_fe
### Title: Get confidence intervals for provider effects or standardized
###   measures from a fitted 'logis_fe' object
### Aliases: confint.logis_fe

### ** Examples

data(ExampleDataBinary)
outcome = ExampleDataBinary$Y
covar = ExampleDataBinary$Z
ProvID = ExampleDataBinary$ProvID
fit_fe <- logis_fe(Y = outcome, Z = covar, ProvID = ProvID, message = FALSE)
confint(fit_fe, option = "gamma")
confint(fit_fe, option = "SM")




cleanEx()
nameEx("confint.logis_re")
### * confint.logis_re

flush(stderr()); flush(stdout())

### Name: confint.logis_re
### Title: Get confidence intervals for provider effects or standardized
###   measures from a fitted 'logis_re' object
### Aliases: confint.logis_re

### ** Examples

data(ExampleDataBinary)
outcome <- ExampleDataBinary$Y
ProvID <- ExampleDataBinary$ProvID
covar <- ExampleDataBinary$Z
fit_re <- logis_re(Y = outcome, Z = covar, ProvID = ProvID)
confint(fit_re)




cleanEx()
nameEx("data_check")
### * data_check

flush(stderr()); flush(stdout())

### Name: data_check
### Title: Data quality check function
### Aliases: data_check

### ** Examples

data(ExampleDataBinary)
outcome = ExampleDataBinary$Y
covar = ExampleDataBinary$Z
ProvID = ExampleDataBinary$ProvID
data_check(outcome, covar, ProvID)




cleanEx()
nameEx("ecls_data")
### * ecls_data

flush(stderr()); flush(stdout())

### Name: ecls_data
### Title: Early Childhood Longitudinal Study Dataset
### Aliases: ecls_data
### Keywords: datasets

### ** Examples

data(ecls_data)
formula_FE <- as.formula("Math_Score ~ Income + id(School_ID) + Child_Sex")
fit_FE <- linear_fe(formula = formula_FE, data = ecls_data)

formula_RE <- as.formula("Math_Score ~ Income + (1|School_ID) + Child_Sex")
fit_RE <- linear_re(formula = formula_RE, data = ecls_data)



cleanEx()
nameEx("linear_cre")
### * linear_cre

flush(stderr()); flush(stdout())

### Name: linear_cre
### Title: Main Function for fitting correlated random effect linear model
### Aliases: linear_cre

### ** Examples

data(ExampleDataLinear)
outcome <- ExampleDataLinear$Y
covar <- ExampleDataLinear$Z
ProvID <- ExampleDataLinear$ProvID
data <- data.frame(outcome, ProvID, covar)
outcome.char <- colnames(data)[1]
ProvID.char <- colnames(data)[2]
wb.char <- c("z1", "z2")
other.char <- c("z3", "z4", "z5")

# Fit a correlated random effect linear model
fit_cre <- linear_cre(data = data, Y.char = outcome.char, ProvID.char = ProvID.char,
wb.char = wb.char, other.char = other.char)




cleanEx()
nameEx("linear_fe")
### * linear_fe

flush(stderr()); flush(stdout())

### Name: linear_fe
### Title: Main function for fitting the fixed effect linear model
### Aliases: linear_fe

### ** Examples

data(ExampleDataLinear)
outcome <- ExampleDataLinear$Y
covar <- ExampleDataLinear$Z
ProvID <- ExampleDataLinear$ProvID
data <- data.frame(outcome, ProvID, covar)
covar.char <- colnames(covar)
outcome.char <- colnames(data)[1]
ProvID.char <- colnames(data)[2]
formula <- as.formula(paste("outcome ~", paste(covar.char, collapse = " + "), "+ id(ProvID)"))

# Fit fixed linear effect model using three input formats
fit_fe1 <- linear_fe(Y = outcome, Z = covar, ProvID = ProvID)
fit_fe2 <- linear_fe(data = data, Y.char = outcome.char,
Z.char = covar.char, ProvID.char = ProvID.char)
fit_fe3 <- linear_fe(formula, data)




cleanEx()
nameEx("linear_re")
### * linear_re

flush(stderr()); flush(stdout())

### Name: linear_re
### Title: Main Function for fitting the random effect linear model
### Aliases: linear_re

### ** Examples

data(ExampleDataLinear)
outcome <- ExampleDataLinear$Y
covar <- ExampleDataLinear$Z
ProvID <- ExampleDataLinear$ProvID
data <- data.frame(outcome, ProvID, covar)
covar.char <- colnames(covar)
outcome.char <- colnames(data)[1]
ProvID.char <- colnames(data)[2]
formula <- as.formula(paste("outcome ~", paste(covar.char, collapse = " + "), "+ (1|ProvID)"))

# Fit random effect linear model using three input formats
fit_re1 <- linear_re(Y = outcome, Z = covar, ProvID = ProvID)
fit_re2 <- linear_re(data = data, Y.char = outcome.char,
Z.char = covar.char, ProvID.char = ProvID.char)
fit_re3 <- linear_re(formula, data)




cleanEx()
nameEx("logis_cre")
### * logis_cre

flush(stderr()); flush(stdout())

### Name: logis_cre
### Title: Main Function for fitting correlated random effect logistic
###   model
### Aliases: logis_cre

### ** Examples

data(ExampleDataBinary)
outcome <- ExampleDataBinary$Y
covar <- ExampleDataBinary$Z
ProvID <- ExampleDataBinary$ProvID
data <- data.frame(outcome, ProvID, covar)
outcome.char <- colnames(data)[1]
ProvID.char <- colnames(data)[2]
wb.char <- c("z1", "z2")
other.char <- c("z3", "z4", "z5")

# Fit a correlated random effect linear model
fit_cre <- logis_cre(data = data, Y.char = outcome.char, ProvID.char = ProvID.char,
wb.char = wb.char, other.char = other.char)




cleanEx()
nameEx("logis_fe")
### * logis_fe

flush(stderr()); flush(stdout())

### Name: logis_fe
### Title: Main function for fitting the fixed effect logistic model
### Aliases: logis_fe

### ** Examples

data(ExampleDataBinary)
outcome <- ExampleDataBinary$Y
covar <- ExampleDataBinary$Z
ProvID <- ExampleDataBinary$ProvID
data <- data.frame(outcome, ProvID, covar)
covar.char <- colnames(covar)
outcome.char <- colnames(data)[1]
ProvID.char <- colnames(data)[2]
formula <- as.formula(paste("outcome ~", paste(covar.char, collapse = " + "), "+ id(ProvID)"))

# Fit logistic linear effect model using three input formats
fit_fe1 <- logis_fe(Y = outcome, Z = covar, ProvID = ProvID)
fit_fe2 <- logis_fe(data = data, Y.char = outcome.char,
Z.char = covar.char, ProvID.char = ProvID.char)
fit_fe3 <- logis_fe(formula, data)




cleanEx()
nameEx("logis_firth")
### * logis_firth

flush(stderr()); flush(stdout())

### Name: logis_firth
### Title: Main function for fitting the fixed effect logistic model using
###   firth correction
### Aliases: logis_firth

### ** Examples

data(ExampleDataBinary)
outcome <- ExampleDataBinary$Y
covar <- ExampleDataBinary$Z
ProvID <- ExampleDataBinary$ProvID
data <- data.frame(outcome, ProvID, covar)
covar.char <- colnames(covar)
outcome.char <- colnames(data)[1]
ProvID.char <- colnames(data)[2]
formula <- as.formula(paste("outcome ~", paste(covar.char, collapse = " + "), "+ id(ProvID)"))

# Fit logistic linear effect model using three input formats
fit_fe1 <- logis_firth(Y = outcome, Z = covar, ProvID = ProvID)
fit_fe2 <- logis_firth(data = data, Y.char = outcome.char,
Z.char = covar.char, ProvID.char = ProvID.char)
fit_fe3 <- logis_firth(formula, data)




cleanEx()
nameEx("logis_re")
### * logis_re

flush(stderr()); flush(stdout())

### Name: logis_re
### Title: Main Function for fitting the random effect logistic model
### Aliases: logis_re

### ** Examples

data(ExampleDataBinary)
outcome <- ExampleDataBinary$Y
covar <- ExampleDataBinary$Z
ProvID <- ExampleDataBinary$ProvID
data <- data.frame(outcome, ProvID, covar)
covar.char <- colnames(covar)
outcome.char <- colnames(data)[1]
ProvID.char <- colnames(data)[2]
formula <- as.formula(paste("outcome ~", paste(covar.char, collapse = " + "), "+ (1|ProvID)"))

# Fit logistic linear effect model using three input formats
fit_re1 <- logis_re(Y = outcome, Z = covar, ProvID = ProvID)




cleanEx()
nameEx("plot.linear_fe")
### * plot.linear_fe

flush(stderr()); flush(stdout())

### Name: plot.linear_fe
### Title: Get funnel plot from a fitted 'linear_fe' object for
###   institutional comparisons
### Aliases: plot.linear_fe

### ** Examples

data(ExampleDataLinear)
outcome <- ExampleDataLinear$Y
covar <- ExampleDataLinear$Z
ProvID <- ExampleDataLinear$ProvID
fit_fe <- linear_fe(Y = outcome, Z = covar, ProvID = ProvID)
plot(fit_fe)




cleanEx()
nameEx("plot.logis_fe")
### * plot.logis_fe

flush(stderr()); flush(stdout())

### Name: plot.logis_fe
### Title: Get funnel plot from a fitted 'logis_fe' object for
###   institutional comparisons
### Aliases: plot.logis_fe

### ** Examples

data(ExampleDataBinary)
outcome <- ExampleDataBinary$Y
covar <- ExampleDataBinary$Z
ProvID <- ExampleDataBinary$ProvID
fit_fe <- logis_fe(Y = outcome, Z = covar, ProvID = ProvID)
plot(fit_fe)




cleanEx()
nameEx("summary.linear_fe")
### * summary.linear_fe

flush(stderr()); flush(stdout())

### Name: summary.linear_cre
### Title: Result Summaries of Covariate Estimates from a fitted
###   'linear_fe', 'linear_re' or 'linear_cre' object
### Aliases: summary.linear_cre summary.linear_fe summary.linear_re

### ** Examples

data(ExampleDataLinear)
outcome <- ExampleDataLinear$Y
covar <- ExampleDataLinear$Z
ProvID <- ExampleDataLinear$ProvID
data <- data.frame(outcome, ProvID, covar)
outcome.char <- colnames(data)[1]
ProvID.char <- colnames(data)[2]
wb.char <- c("z1", "z2")
other.char <- c("z3", "z4", "z5")
fit_cre <- linear_cre(data = data, Y.char = outcome.char, ProvID.char = ProvID.char,
wb.char = wb.char, other.char = other.char)
summary(fit_cre)

data(ExampleDataLinear)
outcome <- ExampleDataLinear$Y
covar <- ExampleDataLinear$Z
ProvID <- ExampleDataLinear$ProvID
fit_fe <- linear_fe(Y = outcome, Z = covar, ProvID = ProvID)
summary(fit_fe)

data(ExampleDataLinear)
outcome <- ExampleDataLinear$Y
covar <- ExampleDataLinear$Z
ProvID <- ExampleDataLinear$ProvID
fit_re <- linear_fe(Y = outcome, Z = covar, ProvID = ProvID)
summary(fit_re)




cleanEx()
nameEx("summary.logis_fe")
### * summary.logis_fe

flush(stderr()); flush(stdout())

### Name: summary.logis_fe
### Title: Result Summaries of Covariate Estimates from a fitted 'logis_fe'
###   object
### Aliases: summary.logis_fe

### ** Examples

data(ExampleDataBinary)
outcome = ExampleDataBinary$Y
covar = ExampleDataBinary$Z
ProvID = ExampleDataBinary$ProvID

fit_fe <- logis_fe(Y = outcome, Z = covar, ProvID = ProvID, message = FALSE)
summary.wald <- summary(fit_fe, level = 0.95, test = "wald")
summary.wald




cleanEx()
nameEx("summary.logis_re")
### * summary.logis_re

flush(stderr()); flush(stdout())

### Name: summary.logis_cre
### Title: Result Summaries of Covariate Estimates from a fitted 'logis_re'
###   or 'logis_cre' object
### Aliases: summary.logis_cre summary.logis_re

### ** Examples

data(ExampleDataBinary)
outcome <- ExampleDataBinary$Y
covar <- ExampleDataBinary$Z
ProvID <- ExampleDataBinary$ProvID
data <- data.frame(outcome, ProvID, covar)
outcome.char <- colnames(data)[1]
ProvID.char <- colnames(data)[2]
wb.char <- c("z1", "z2")
other.char <- c("z3", "z4", "z5")
fit_cre <- logis_cre(data = data, Y.char = outcome.char, ProvID.char = ProvID.char,
wb.char = wb.char, other.char = other.char)
summary(fit_cre)





cleanEx()
nameEx("test.linear_cre")
### * test.linear_cre

flush(stderr()); flush(stdout())

### Name: test.linear_cre
### Title: Conduct hypothesis testing for provider effects from a fitted
###   'linear_cre' object
### Aliases: test.linear_cre

### ** Examples

data(ExampleDataLinear)
outcome <- ExampleDataLinear$Y
covar <- ExampleDataLinear$Z
ProvID <- ExampleDataLinear$ProvID
data <- data.frame(outcome, ProvID, covar)
outcome.char <- colnames(data)[1]
ProvID.char <- colnames(data)[2]
wb.char <- c("z1", "z2")
other.char <- c("z3", "z4", "z5")
fit_cre <- linear_cre(data = data, Y.char = outcome.char, ProvID.char = ProvID.char,
wb.char = wb.char, other.char = other.char)
test(fit_cre)




cleanEx()
nameEx("test.linear_fe")
### * test.linear_fe

flush(stderr()); flush(stdout())

### Name: test.linear_fe
### Title: Conduct hypothesis testing for provider effects from a fitted
###   'linear_fe' object
### Aliases: test.linear_fe

### ** Examples

data(ExampleDataLinear)
outcome <- ExampleDataLinear$Y
covar <- ExampleDataLinear$Z
ProvID <- ExampleDataLinear$ProvID
fit_linear <- linear_fe(Y = outcome, Z = covar, ProvID = ProvID)
test(fit_linear)




cleanEx()
nameEx("test.linear_re")
### * test.linear_re

flush(stderr()); flush(stdout())

### Name: test.linear_re
### Title: Conduct hypothesis testing for provider effects from a fitted
###   'linear_re' object
### Aliases: test.linear_re

### ** Examples

data(ExampleDataLinear)
outcome <- ExampleDataLinear$Y
ProvID <- ExampleDataLinear$ProvID
covar <- ExampleDataLinear$Z
fit_re <- linear_re(Y = outcome, Z = covar, ProvID = ProvID)
test(fit_re)




cleanEx()
nameEx("test.logis_cre")
### * test.logis_cre

flush(stderr()); flush(stdout())

### Name: test.logis_cre
### Title: Conduct hypothesis testing for provider effects from a fitted
###   'logis_cre' object
### Aliases: test.logis_cre

### ** Examples

data(ExampleDataBinary)
outcome <- ExampleDataBinary$Y
covar <- ExampleDataBinary$Z
ProvID <- ExampleDataBinary$ProvID
data <- data.frame(outcome, ProvID, covar)
outcome.char <- colnames(data)[1]
ProvID.char <- colnames(data)[2]
wb.char <- c("z1", "z2")
other.char <- c("z3", "z4", "z5")
fit_cre <- logis_cre(data = data, Y.char = outcome.char, ProvID.char = ProvID.char,
wb.char = wb.char, other.char = other.char)
test(fit_cre)




cleanEx()
nameEx("test.logis_fe")
### * test.logis_fe

flush(stderr()); flush(stdout())

### Name: test.logis_fe
### Title: Conduct hypothesis testing for provider effects from a fitted
###   'logis_fe' object
### Aliases: test.logis_fe

### ** Examples

data(ExampleDataBinary)
outcome = ExampleDataBinary$Y
covar = ExampleDataBinary$Z
ProvID = ExampleDataBinary$ProvID
fit_fe <- logis_fe(Y = outcome, Z = covar, ProvID = ProvID, message = FALSE)
test(fit_fe, test = "score")




cleanEx()
nameEx("test.logis_re")
### * test.logis_re

flush(stderr()); flush(stdout())

### Name: test.logis_re
### Title: Conduct hypothesis testing for provider effects from a fitted
###   'logis_re' object
### Aliases: test.logis_re

### ** Examples

data(ExampleDataBinary)
outcome <- ExampleDataBinary$Y
ProvID <- ExampleDataBinary$ProvID
covar <- ExampleDataBinary$Z
fit_re <- logis_re(Y = outcome, Z = covar, ProvID = ProvID)
test(fit_re)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
