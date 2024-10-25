#*******************************************************************************
#* Created 16/10/2024
#* Creator N.P
#* Main program to analyse J2000 output
#*******************************************************************************

#### IMPORT LIBRARY ####
library(dplyr)
library(tidyr)
library(stringr)
library(reshape2)
library(ggplot2)
library(sf)
library(XML)
library(gridExtra)
library(chron)
library('lubridate')
library('stringr')
library('purrr')
library('generics')
library(plotly)
library(tibble)
library(htmlwidgets)
library(rlang)






simdir <- "/home/npellerin/JAMS/jamsModelData/J2K_Loire/output/J2K_LOIRE_models_comparaison/results_J2K_Loire/"

#### ReachLoop ####
fp <- paste0(simdir, "ReachLoop.dat")
reach_block<- j2kReadReachLoop(fp, verbose=TRUE) %>% 
  as_tibble() %>% 
  rename(., "ReachID" = "ID")


