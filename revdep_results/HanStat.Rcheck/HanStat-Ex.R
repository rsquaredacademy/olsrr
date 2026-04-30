pkgname <- "HanStat"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('HanStat')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("HansStat-Data")
### * HansStat-Data

flush(stderr()); flush(stdout())

### Name: HansStat Data
### Title: Radomized data for testing models
### Aliases: 'HansStat Data'
### Keywords: datasets

### ** Examples

data(data)




cleanEx()
nameEx("LinReg")
### * LinReg

flush(stderr()); flush(stdout())

### Name: LinReg
### Title: LinReg
### Aliases: LinReg
### Keywords: Linear Regression

### ** Examples

m<-LinReg('dv',c('iv_1','iv_2','iv_3'),data=data,BS=FALSE,NBS=1000,OC=FALSE,plot=TRUE)
print(m$Results)
print(m$Require)
print(m$Plots)



cleanEx()
nameEx("data")
### * data

flush(stderr()); flush(stdout())

### Name: data
### Title: Randomized data for testing models Contains 5 Variables, one
###   dependent, 4 independent. The fourth independent is correlated with
###   the dependent
### Aliases: data
### Keywords: datasets

### ** Examples

data(data)



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
