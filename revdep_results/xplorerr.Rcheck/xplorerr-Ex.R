pkgname <- "xplorerr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('xplorerr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("app_descriptive")
### * app_descriptive

flush(stderr()); flush(stdout())

### Name: app_descriptive
### Title: Descriptive Statistics
### Aliases: app_descriptive

### ** Examples

## Not run: 
##D app_descriptive()
## End(Not run)



cleanEx()
nameEx("app_inference")
### * app_inference

flush(stderr()); flush(stdout())

### Name: app_inference
### Title: Inferential Statistics
### Aliases: app_inference

### ** Examples

## Not run: 
##D app_inference()
## End(Not run)



cleanEx()
nameEx("app_linear_regression")
### * app_linear_regression

flush(stderr()); flush(stdout())

### Name: app_linear_regression
### Title: Linear Regression
### Aliases: app_linear_regression

### ** Examples

## Not run: 
##D app_linear_regression()
## End(Not run)



cleanEx()
nameEx("app_logistic_regression")
### * app_logistic_regression

flush(stderr()); flush(stdout())

### Name: app_logistic_regression
### Title: Logistic Regression
### Aliases: app_logistic_regression

### ** Examples

## Not run: 
##D app_logistic_regression()
## End(Not run)



cleanEx()
nameEx("app_rfm_analysis")
### * app_rfm_analysis

flush(stderr()); flush(stdout())

### Name: app_rfm_analysis
### Title: RFM Analysis
### Aliases: app_rfm_analysis

### ** Examples

## Not run: 
##D app_rfm_analysis()
## End(Not run)



cleanEx()
nameEx("app_vistributions")
### * app_vistributions

flush(stderr()); flush(stdout())

### Name: app_vistributions
### Title: Visualize distributions
### Aliases: app_vistributions

### ** Examples

## Not run: 
##D app_vistributions()
## End(Not run)



cleanEx()
nameEx("app_visualizer")
### * app_visualizer

flush(stderr()); flush(stdout())

### Name: app_visualizer
### Title: Visualization
### Aliases: app_visualizer

### ** Examples

## Not run: 
##D app_visualizer()
## End(Not run)



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
