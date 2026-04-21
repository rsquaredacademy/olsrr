pkgname <- "SQI"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('SQI')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("PCAIndex")
### * PCAIndex

flush(stderr()); flush(stdout())

### Name: PCAIndex
### Title: Soil Quality Index Based on Regression
### Aliases: PCAIndex

### ** Examples

library("SQI")
OP<-c(7,1111,9999,9999,9999,9999,9999,9999,9999,9999,1111)
PIndex<-PCAIndex(DataFrame = Data,OptimumValue = OP)



cleanEx()
nameEx("RegIndex")
### * RegIndex

flush(stderr()); flush(stdout())

### Name: RegIndex
### Title: Soil Quality Index Based on Regression
### Aliases: RegIndex

### ** Examples

library("SQI")
OP<-c(7,1111,9999,9999,9999,9999,9999,9999,9999,9999,1111)
RIndex<-RegIndex(DataFrame = Data,Dep_col=7,OptimumValue = OP)



cleanEx()
nameEx("ScoingIndex")
### * ScoingIndex

flush(stderr()); flush(stdout())

### Name: ScoingIndex
### Title: Soil Quality Index Based on Linear Scoring
### Aliases: ScoingIndex

### ** Examples

library("SQI")
OP<-c(7,1111,9999,9999,9999,9999,9999,9999,9999,9999,1111)
ScoreIndex<-ScoingIndex(DataFrame = Data,OptimumValue = OP)



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
