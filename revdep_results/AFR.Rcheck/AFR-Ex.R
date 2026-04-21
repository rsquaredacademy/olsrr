pkgname <- "AFR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('AFR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("HP")
### * HP

flush(stderr()); flush(stdout())

### Name: HP
### Title: Hodrick-Prescott filter for time series data
### Aliases: HP

### ** Examples

data(macroKZ)
HP(macroKZ[,2])



cleanEx()
nameEx("bg")
### * bg

flush(stderr()); flush(stdout())

### Name: bg
### Title: Breusch-Godfrey test [BG test]
### Aliases: bg

### ** Examples

model <- lm(real_gdp ~ imp + exp + poil + eurkzt + tonia_rate, data = macroKZ)
bg(model)



cleanEx()
nameEx("bp")
### * bp

flush(stderr()); flush(stdout())

### Name: bp
### Title: Breusch-Pagan test
### Aliases: bp

### ** Examples

model <- lm(real_gdp ~ imp + exp + poil + eurkzt + tonia_rate, data = macroKZ)
bp(model)



cleanEx()
nameEx("check_betas")
### * check_betas

flush(stderr()); flush(stdout())

### Name: check_betas
### Title: All possible regression variable coefficients.
### Aliases: check_betas

### ** Examples

model <- lm(real_gdp~imp+exp+usdkzt+eurkzt, data = macroKZ)
check_betas(model)



cleanEx()
nameEx("checkdata")
### * checkdata

flush(stderr()); flush(stdout())

### Name: checkdata
### Title: Preliminary data check for errors
### Aliases: checkdata

### ** Examples

data(macroKZ)
checkdata(macroKZ)



cleanEx()
nameEx("corsel")
### * corsel

flush(stderr()); flush(stdout())

### Name: corsel
### Title: Multicollinearity test
### Aliases: corsel

### ** Examples

data(macroKZ)
corsel(macroKZ,num=FALSE,thrs=0.65)



cleanEx()
nameEx("dec_plot")
### * dec_plot

flush(stderr()); flush(stdout())

### Name: dec_plot
### Title: Decomposition plot
### Aliases: dec_plot

### ** Examples

data(macroKZ)
model <- lm(real_gdp ~ usdkzt + eurkzt + imp+exp, data = macroKZ)
dec_plot(model, macroKZ)



cleanEx()
nameEx("difflog")
### * difflog

flush(stderr()); flush(stdout())

### Name: difflog
### Title: Transforming time-series data to stationary
### Aliases: difflog

### ** Examples

data (macroKZ)
new<-pct1(macroKZ)



cleanEx()
nameEx("gq")
### * gq

flush(stderr()); flush(stdout())

### Name: gq
### Title: Godfrey-Quandt test
### Aliases: gq

### ** Examples

model <- lm(real_gdp ~ imp + exp + poil + eurkzt + tonia_rate, data = macroKZ)
gq(model)



cleanEx()
nameEx("ols_test_normality")
### * ols_test_normality

flush(stderr()); flush(stdout())

### Name: ols_test_normality
### Title: Test for normality Test for detecting violation of normality
###   assumption.
### Aliases: ols_test_normality

### ** Examples

data(macroKZ)
model <- lm(real_gdp ~ imp + exp + usdkzt + poil, data = macroKZ)
ols_test_normality(model)




cleanEx()
nameEx("opt_size")
### * opt_size

flush(stderr()); flush(stdout())

### Name: opt_size
### Title: Necessary size of the time-series dataset
### Aliases: opt_size

### ** Examples

data(macroKZ)
model <- lm(real_gdp ~ imp + exp + poil + eurkzt + tonia_rate, data = macroKZ)
opt_size(model)



cleanEx()
nameEx("pct1")
### * pct1

flush(stderr()); flush(stdout())

### Name: pct1
### Title: Transforming time-series data to stationary
### Aliases: pct1

### ** Examples

data (macroKZ)
new<-pct1(macroKZ)



cleanEx()
nameEx("pct4")
### * pct4

flush(stderr()); flush(stdout())

### Name: pct4
### Title: Transforming time-series data to stationary
### Aliases: pct4

### ** Examples

data (macroKZ)
new<-pct4(macroKZ)



cleanEx()
nameEx("pt_multi")
### * pt_multi

flush(stderr()); flush(stdout())

### Name: pt_multi
### Title: Pluto-Tasche method for multi-year probability of default (PD)
###   analysis
### Aliases: pt_multi

### ** Examples

pf <- c(10,20,30,40)
num_def <- c(1,2,3,4)
conf_level = 0.99
num_years = 3
pt_multi(pf, num_def, conf_level, num_years)



cleanEx()
nameEx("pt_one")
### * pt_one

flush(stderr()); flush(stdout())

### Name: pt_one
### Title: Pluto-Tasche method for one-year probability of default (PD)
###   analysis
### Aliases: pt_one

### ** Examples

pf <- c(10,20,30,40)
num_def <- c(1,2,3,4)
pt_one(pf, num_def, ci= 0.9)



cleanEx()
nameEx("reg_plot")
### * reg_plot

flush(stderr()); flush(stdout())

### Name: reg_plot
### Title: Regression forecast plot
### Aliases: reg_plot

### ** Examples

data(macroKZ)
model <- lm(real_gdp ~ usdkzt + eurkzt + imp + exp, data = macroKZ)
reg_plot(model, macroKZ)



cleanEx()
nameEx("reg_test")
### * reg_test

flush(stderr()); flush(stdout())

### Name: reg_test
### Title: Test for detecting violation of Gauss-Markov assumptions.
### Aliases: reg_test

### ** Examples

data(macroKZ)
model <- lm(real_gdp~ imp + exp + poil + eurkzt + usdkzt, macroKZ)
reg_test(model)



cleanEx()
nameEx("regsel_f")
### * regsel_f

flush(stderr()); flush(stdout())

### Name: regsel_f
### Title: Regressors selection
### Aliases: regsel_f

### ** Examples

data(macroKZ)
model <- lm(real_gdp ~ imp + exp + poil + eurkzt + tonia_rate, data = macroKZ)
regsel_f(model)



cleanEx()
nameEx("vif_reg")
### * vif_reg

flush(stderr()); flush(stdout())

### Name: vif_reg
### Title: VIF by variable
### Aliases: vif_reg

### ** Examples

data(macroKZ)
model <- lm(real_gdp ~ imp + exp + poil + eurkzt + tonia_rate, data = macroKZ)
vif_reg(model)



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
