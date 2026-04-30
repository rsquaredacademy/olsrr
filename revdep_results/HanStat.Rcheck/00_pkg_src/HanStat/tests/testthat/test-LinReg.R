testthat::test_that("Output gets created", {
  data<-data.frame(dv=rnorm(100,0,2),iv_1=rnorm(100,0,2),iv_2=rnorm(100,0,2),iv_3=rnorm(100,0,2));
  m<-LinReg('dv',c('iv_1','iv_2','iv_3'),data=data,OC=FALSE,BS=FALSE,NBS=1000,plot=FALSE);
  testthat::expect_identical((length(m)>1),TRUE)
})
