#*******************************************************************************
#* Created 16/11/2022
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


# sources
source("./pellerin/working_directory/code/old_code/input.R")
source("./pellerin/working_directory/code/old_code/plotingFunctions.R")
source("./pellerin/working_directory/code/old_code/fromReachLooptoTibble.R")

#### INPUTS information concerning interesting station or reach ####
table_description_barrage = table_description_barrage()
table_description_affluents = table_description_affluents()
table_description_reach_soutien_etiage = table_description_reach_soutien_etiage()
table_description_B_SE = full_join(table_description_barrage, 
                                   table_description_reach_soutien_etiage,
                                   by = c("ReachID", "Nom", "type"))
nom_Affluents_barrages = nom_Affluents_barrages(table_description_barrage, 
                                                table_description_affluents)
###############################################################

#### FO reading ####
FO <- FO_2023("1960-01-01", "2100-12-31") %>%
  left_join(., table_description_barrage %>% select(ReachID, Nom), by = "Nom")
# FO <- FO_2023_s2("1960-01-01", "2100-12-31") %>% 
#   left_join(., table_description_barrage %>% select(ReachID, Nom), by = "Nom")
################


#### SELECT DIRECTORY ####
################### VALIDATION TEMPS PRESENT ###################################
      # # simulation folder
      #   simdir <- "/home/npellerin/CONTINUUMMES/WORKING_DIRECTORY/1-JAMS/data/J2K_Rhone_v2_withMAGE_withBarrage/output/LYON-MED/VALIDATION_TEMPS_PRESENT/1960-2019_LYON-MED_REF/"
      # # target folder for re-organized data
      #   tardir <- "/home/npellerin/CONTINUUMMES/WORKING_DIRECTORY/4-TRAITEMENT_RESULTATS/VALIDATION_MODELE_TEMPS_PRESENT/DATA/"

#################### PROJECTIONS CLIMATIQUES ###################################
scenario = "3"
PROJECTIONS_NAME = c("CNRM-CM5-LR_ALADIN63_1960-2100", "EC-EARTH_RACMO22E_1960-2100","HadGEM2-ES_CCLM4-8-17_1960-2100",
                     "HadGEM2-ES_HadREM3-GA7-05_1960-2100", "IPSL-CM5A-MR_WRF381P_1960-2100", "MPI-ESM-LR_CCLM4-8-17_1960-2100")
        
for(i in 1:6){
  projection_repertory = paste0(scenario,"_",PROJECTIONS_NAME[i])
  # simulation folder
  simdir <- paste0("/home/npellerin/CONTINUUMMES/WORKING_DIRECTORY/1-JAMS/data/J2K_Rhone_v2_withMAGE_withBarrage/output/LYON-MED/SIMULATIONS_PROJECTIONS/",projection_repertory,"/")
  # target folder for re-organized data
  tardir <- paste0("/home/npellerin/CONTINUUMMES/WORKING_DIRECTORY/4-TRAITEMENT_RESULTATS/PROJECTIONS_CLIMATIQUES_SCENARIOS/SCENARIO_", scenario, "/")
  #####################
  
  #### TimeLoop ####
  readTime <- readLines(paste0(simdir,'TimeLoop.dat'))
  simulationTime <- length(readTime)- 1
  # yearday = c(1:366)
  time_block <- readTime[seq(from = 11, to = simulationTime)]
  
  time_block <- time_block %>%
    strsplit(., split ='\t') %>%
    unlist()
  colnames = c("Date","TERNAY","VALENCE","VIVIERS","BEAUCAIRE","AI_tvRD1","AI_tvRD2","AI_tvRG1","AI_tvRG2","IA_tvRD1","IA_tvRD2","IA_tvRG1","IA_tvRG2")
  time_block = as_tibble(matrix(time_block, ncol = 13, byrow =TRUE, dimnames = list(NULL,colnames)))
  time_block$Date = as.Date(time_block$Date)
  for(i in 2:13){
    time_block[i] = as.numeric(unlist(time_block[i]))
  }
  time_block = add_column(time_block, "737601" = time_block$AI_tvRD1 + time_block$AI_tvRD2 + time_block$AI_tvRG1 + time_block$AI_tvRG2) 
  time_block = add_column(time_block, "727200" = time_block$IA_tvRD1 + time_block$IA_tvRD2 + time_block$IA_tvRG1 + time_block$IA_tvRG2) 
  time_block = time_block[-c(6:13)]
  # time_block = time_block[time_block$Date >= "1980-01-01",]
  #############
  
  
  #### ReachLoop ####
  fp <- paste0(simdir, "ReachLoop.dat")
  reach_block<- j2kReadReachLoop(fp, verbose=TRUE) %>% 
    as_tibble() %>% 
    rename(., "ReachID" = "ID")
  # reach_block = reach_block[reach_block$Date >= "1980-01-01",]
  ##############
  
  #### Group TimeLoop & ReachLoop in different tibble depending of the type of information ####
  # DERIVATION
  DERIVATIONS <- function(){
    DERIVATIONS = reach_block %>% 
      .[-c(2,3)]
    AI = time_block[-c(2,3,4,5,7)] %>% 
      mutate(., "ReachID" = 737601) %>% 
      rename(., FO_fin_AI = "737601")
    IA = time_block[-c(2,3,4,5,6)] %>% 
      mutate(., "ReachID" = 727200)%>% 
      rename(., FO_fin_IA = "727200")
    DERIVATIONS_tmp = full_join(AI, IA, by = c("Date","ReachID"))
    DERIVATIONS = full_join(DERIVATIONS, DERIVATIONS_tmp, by = c("Date","ReachID")) 
    DERIVATIONS$FO_fin[is.na(DERIVATIONS$FO_fin)] = 0
    DERIVATIONS$FO_fin_AI[is.na(DERIVATIONS$FO_fin_AI)] = 0
    DERIVATIONS$FO_fin_IA[is.na(DERIVATIONS$FO_fin_IA)] = 0
    DERIVATIONS = mutate (DERIVATIONS, FO_fin = FO_fin + FO_fin_AI + FO_fin_IA ) %>% 
      left_join(., table_description_B_SE, by = "ReachID")
    DERIVATIONS = DERIVATIONS[DERIVATIONS$type == "transfert",] %>% 
      .[-c(4:9)] 
  }
  DERIVATIONS = DERIVATIONS()
  
  # BARRAGES
  BARRAGES <- function(table_description_B_SE){
    BARRAGES = reach_block %>% 
      .[-3] %>% 
      left_join(., table_description_B_SE, by = "ReachID") 
    BARRAGES = BARRAGES[BARRAGES$type == "barrage",] %>% 
      .[-c(5:8)]
  }
  BARRAGES = BARRAGES(table_description_B_SE)
  
  # SOUTIEN ETIAGE
  SOUTIEN_ETIAGE = function(table_description_B_SE){
    SOUTIEN_ETIAGE = reach_block %>% 
      .[-c(2,4)] %>% 
      left_join(., table_description_B_SE, by = "ReachID") 
    SOUTIEN_ETIAGE = SOUTIEN_ETIAGE[SOUTIEN_ETIAGE$type == "soutien_etiage",] %>% 
      .[-c(5:7)] %>% 
      relocate(Date, .before = ReachID)
  }
  SOUTIEN_ETIAGE = SOUTIEN_ETIAGE(table_description_B_SE)
  
  #### VALEUR DEBIT STATION HISTORIQUE J2000 ####
  STATION_HIST_J2000 <- function(){
    STATION_HIST_J2000 = time_block %>% 
      .[-c(6,7)]
    for(i in 2:5){
      STATION_HIST_J2000[i] = STATION_HIST_J2000[i]/(1000*24*3600)
    }
    return(STATION_HIST_J2000)
  }
  STATION_HIST_J2000 = STATION_HIST_J2000()
  
  
  
  # DERIVATIONS = mutate(DERIVATIONS, yearday = yday(DERIVATIONS$Date))
  DERIVATIONS_FO = left_join(DERIVATIONS, FO, by = c('Date', 'ReachID'))
  DERIVATIONS_FO = mutate(DERIVATIONS_FO, ratio_FO = FO_fin/Volume_FO)
  DERIVATIONS_FO$ratio_FO[is.na(DERIVATIONS_FO$ratio_FO)] = 0
  DERIVATIONS_FO$ratio_FO[DERIVATIONS_FO$ratio_FO == Inf] = 0
  # DERIVATIONS_FO = left_join(DERIVATIONS_FO, table_description_barrage %>% select(ReachID, Nom), by = "ReachID")
  
  # BARRAGES = mutate(BARRAGES, yearday = yday((BARRAGES$Date))) 
  BARRAGES_FO = left_join(BARRAGES, FO, by = c('Date', 'ReachID'))
  BARRAGES_FO = mutate(BARRAGES_FO, ratio_FO = FO_fin/Volume_FO)
  BARRAGES_FO$ratio_FO[is.na(BARRAGES_FO$ratio_FO)] = 0
  # BARRAGES_FO = left_join(BARRAGES_FO, table_description_barrage %>% select(ReachID, Nom), by = "ReachID")
  
  #*******************************************************************************
  #*******************************************************************************
  #*                          SAVING J2K RDATA
  #*******************************************************************************
  #* TEMPS PRESENT
  # simname = "Temps_présent"
  # tibble_list = c("DERIVATIONS_FO", "BARRAGES_FO", "SOUTIEN_ETIAGE","STATION_HIST_J2000")
  # save(list = tibble_list, file = paste0(tardir, simname,"_FO",".Rdata"))
  
  #* PROJECTIONS CLIMATIQUES 
  tibble_list = c("DERIVATIONS_FO", "BARRAGES_FO", "SOUTIEN_ETIAGE","STATION_HIST_J2000")
  save(list = tibble_list, file = paste0(tardir,projection_repertory,".Rdata"))
  ##################
}


  
#*******************************************************************************
#*******************************************************************************
#*                      PLOTTING BARRAGES & DERIVATIONS
#*******************************************************************************
  
# load("/home/npellerin/CONTINUUMMES/WORKING_DIRECTORY/4-TRAITEMENT_RESULTATS/PROJECTIONS_CLIMATIQUES_SCENARIOS/SCENARIO_2/2_CNRM-CM5-LR_ALADIN63_1960-2100.Rdata")
# tardir ="/home/npellerin/CONTINUUMMES/WORKING_DIRECTORY/4-TRAITEMENT_RESULTATS/PROJECTIONS_CLIMATIQUES_SCENARIOS/SCENARIO_2/"

# Plot graph for each barrage to compare real and theoretical FO 
for(name in table_description_barrage$Nom){
  if(table_description_barrage$type[table_description_barrage$Nom == name] == "barrage"){
    data_tmp = BARRAGES_FO[BARRAGES_FO$Nom == name,] %>% 
      mutate(Volume_FO = Volume_FO/1e9,
             Storage = Storage/1e9,
             FO_fin = FO_fin/1e9) #To get Mm³

    ax <- list(title = "",zeroline = FALSE,showline = FALSE,showticklabels = FALSE)  
    # timelimit = "1975-12-31"
    timelimit = "2070-01-01"
    fig1 <- plot_ly(data_tmp[data_tmp$Date>=timelimit,], type = 'scatter', mode = 'lines', fill = 'tonexty')%>%
      add_trace(x = ~Date, y = ~Storage, name = 'Storage')%>%
      layout(xaxis =ax, yaxis = list(title = 'Volume (Mm³)'))
    fig2 <- plot_ly(data_tmp[data_tmp$Date>=timelimit,], type = 'scatter', mode = 'lines')%>%
      add_trace(x = ~Date, y = ~FO_fin, name = 'FO réelle', line = list(color = 'purple'))%>%
      add_trace(x = ~Date, y = ~Volume_FO, name = 'FO théorique', line = list(color = 'red', dash = 'dot'))%>%
      layout(title = name,
             xaxis = list(tickformat="%b<br>%Y"),
             yaxis = list(title = 'Volume (Mm³)'))
    fig <- subplot(fig1, fig2, nrows = 2, 
                   titleY = TRUE, titleX = TRUE, shareX = TRUE) %>% 
      layout(xaxis = list(zerolinecolor = '#ffff',
                          zerolinewidth = 2,
                          gridcolor = 'ffff'),
             yaxis = list(zerolinecolor = '#ffff',
                          zerolinewidth = 2,
                          gridcolor = 'ffff'),
             plot_bgcolor='#e5ecf6')
  }
  
  if(table_description_barrage$type[table_description_barrage$Nom == name] == "transfert"){
    data_tmp = DERIVATIONS_FO[DERIVATIONS_FO$Nom == name,] %>% 
      mutate(Volume_FO = Volume_FO/1e9,
             FO_fin = FO_fin/1e9) #To get Mm³
    
    
    ax <- list(title = "",zeroline = FALSE,showline = FALSE,showticklabels = FALSE)  
    # timelimit = "1975-12-31"
    timelimit = "2070-01-01"
    fig <- plot_ly(data_tmp[data_tmp$Date>=timelimit,], type = 'scatter', mode = 'lines')%>%
      add_trace(x = ~Date, y = ~FO_fin, name = 'FO réelle', line = list(color = 'purple'))%>%
      add_trace(x = ~Date, y = ~Volume_FO, name = 'FO théorique', line = list(color = 'red', dash = 'dot'))%>%
      layout(title = name,
             xaxis = list(tickformat="%b<br>%Y"),
             yaxis = list(title = 'Volume (Mm³)'))
    fig <- fig %>% 
      layout(xaxis = list(zerolinecolor = '#ffff',
                          zerolinewidth = 2,
                          gridcolor = 'ffff'),
             yaxis = list(zerolinecolor = '#ffff',
                          zerolinewidth = 2,
                          gridcolor = 'ffff'),
             plot_bgcolor='#e5ecf6')
  }
  
  # tardirplot = "/home/npellerin/CONTINUUMMES/WORKING_DIRECTORY/4-TRAITEMENT_RESULTATS/VALIDATION_MODELE_TEMPS_PRESENT/Plot_evolution_barrage_REF/"
  tardirplot = paste0(tardir,"/Plot_evolution_barrage/")
  saveWidget(fig, paste0(tardirplot,name,".html"), selfcontained = T)
}


