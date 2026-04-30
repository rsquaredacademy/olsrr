library(vistributions)
library(tibble)
library(magrittr)
library(ggplot2)

shinyServer(function(input, output, session) {

    source("logic/logic_binom.R", local = T)
    source("logic/logic_f.R", local = T)
    source("logic/logic_t.R", local = T)
    source("logic/logic_norm.R", local = T)
    source("logic/logic_chisq.R", local = T)
    source("logic/logic_home.R", local = T)
    source("logic/logic_exit_button.R", local = T)

})


