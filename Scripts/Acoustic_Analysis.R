



### SYMBIOSIS Data Summarisation ###

library("dplyr")                                                
library("plyr")                                               
library("readr")  



UKDATA <- list.files(path = "N:/Project work/SYMBIOSIS/data/UK", pattern = "*.csv", all.files = TRUE,
           full.names = TRUE, recursive = TRUE,
           ignore.case = FALSE, include.dirs = TRUE, no.. = FALSE) %>%
  lapply(read_csv) %>%                                          
  bind_rows  













