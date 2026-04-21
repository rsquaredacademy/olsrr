library(rfm)
library(descriptr)
library(dplyr)
library(tibble)
library(readxl)
library(haven)
library(readr)
library(jsonlite)
library(magrittr)
library(tools)
library(lubridate)
library(scales)
library(stringr)
library(rlang)
library(ggplot2)
library(forcats)
library(purrr)
library(RColorBrewer)
library(knitr)


shinyServer(function(input, output, session) {

    source("logic/logic_dataoptions.R", local = T)
    source("logic/logic_upload.R", local = T)
    source("logic/logic_transform2.R", local = T)
    source("logic/logic_select.R", local = T)
    source("logic/logic_screen.R", local = T)
    source("logic/logic_view.R", local = T)
    source("logic/logic_home.R", local = T)
    source("logic/logic_exit_button.R", local = T)
    source("logic/logic_rfm_score.R", local = T)
    source("logic/logic_segments.R", local = T)

})


