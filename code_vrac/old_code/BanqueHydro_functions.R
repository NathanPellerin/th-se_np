#####################################################################
#
# Banque Hydro: fonctions utiles
# 
#####################################################################
# Flora Branger, Inrae (flora.branger@inrae.fr), 2020-04-27
#####################################################################

###############################################################################
# Required packages
###############################################################################

library(chron)
library(dplyr)
library(tidyr)
library(reshape2)
library(sf)


# -------------------------------------------------------------------
# Extract Banque Hydro QJO
# fonction qui récupère le fichier QJO d'une station donnée et extrait
# la série temporelle de débit.
# -------------------------------------------------------------------
# folder:     répertoire où se trouvent les fichiers avec les chroniques
# stationID:  code HYDRO de la station de forme X1130010
#             rows containing the data to actually aggregate
# timewindow: fenêtre temporelle (date début / date fin) sur laquelle on 
#             souhaite extraire la série; format "1981-01-01/2010-12-31"
# shpBV:      adresse complète d'un shapefile contenant la surface du BV dans la table attributaire. Par défaut = NULL
#             si présent: convertit le débit en mm/j en divisant par la
#             surface du BV
# areaname:   nom du champ ds le shapefile qui contient la surface en km2!!
#-------------------------------------------------------------------
# output: renvoie un data frame à 2 colonnes: Time = la date en format chron
# et Q_obs = les valeurs de débit en L/s ou mm/j selon l'option choisie
# folder <- "~/Travail/Projets/2018_MDR_EauxSouterraines/Donnees"
# stationID <- "X1130010"
# timewindow <- "1981-01-01/2010-12-31"
# shpBV<-"~/Travail/SIG/Rhone/MDR_EauxSout/contours45BV.shp"


ExtractBanqueHydro<-function(folder, stationID,timewindow, shpBV=NULL,areaname=NULL){
  # ****** TODO: rechercher le code station ID dans le nom de fichier (ne présuppose pas la syntaxe exacte du nom)******
  # recherche et lecture du fichier
  tmp <- read.table(paste(folder,"/",stationID, "_HYDRO_QJM.txt",sep=""),header = TRUE, sep=";",skip=41, fill=TRUE, stringsAsFactors=FALSE,quote="", na.strings= " ") #comment.char = "F"
    # On ne conserve que les QJO (les fichiers sont trop en bazar)
  #tmp <- filter(tmp,V1=="QJO")
  
  #remove poor quality
  ##     Val_H       : code de validité BANQUE HYDRO ([5] estimé (anciennement douteux), [I] inconnu faible, [S] inconnu fort, [8] reconstitué bon, [9] bon)
  ##     Val_I       : code de validité INRAE ([0] anomalie présumée : valeur interpolée, [1] pas d anomalie détectée, [2] débit négatif)
  ##     Val_E2      : code de validité Explore2 ([0] pas d'anomalie ou donnée manquante, [1] autres, [2] bruit, [3] décrochement, [4] erreur ponctuelle, [5] interpolation, [6] désaccord (2 anomalies différentes identifiées), [-99] période non critiquée)
  
  tmp$Q_obs[tmp$val_I == 9 ] <- NA # remove negative values
  tmp$Q_obs[tmp$val_E2 != 0 ] <- NA # remove error E2
  tmp <- tmp[,1:2] # recupere la date et le debit
  colnames(tmp) <- c("Time", "Q_obs") # renomme les colonnes
  tmp$Q_obs <- as.numeric(tmp$Q_obs)
  tmp$Q_obs[tmp$Q_obs < 0 ] <- NA # remove negative values
  summary(tmp)
  # formatage des dates en format chron
  tmp <- mutate(tmp, Time= chron(as.character(strptime(tmp$Time,"%Y%m%d")),format="y-m-d"))
  # extraction selon la time window
  timesequence <- seq.dates(from=chron(strsplit(timewindow,"/")[[1]][1],format="y-m-d"), to=chron(strsplit(timewindow,"/")[[1]][2],format="y-m-d"),by="days")
  tmp <- filter(tmp, Time %in% timesequence)
  # Use merge to recreate Nas in case they are not properly indicated in the banque hydro original files (unfortunatey it does happen)
  tmp <- merge(data.frame(Time=timesequence),tmp,by="Time",all.x=TRUE)
  # # find any pbs in the time sequence ### DBG ONLY
  # if(setequal(tmp$Time,timesequence)==FALSE){
  #   print (paste("error: missing dates station ", stationID, " ",subset(timesequence, !(timesequence %in% tmp$Time))))
  #   break
  #     }
  
  # récupération shapefile si shpBV != NULL
  if(is.null(shpBV)==FALSE){
    # lecture du shapefile
    
    
    # shp_BV <- st_read(shpBV)
    shp_BV <- shpBV %>% as_tibble()
    
    
    
        #st_crs(shp_BV)$epsg<-2154
    # récupération de la surface
    # **** TODO = récupération de la surface à partir du shapefile et non de la table attributaire!!!!***************
    if ("Code" %in% names(shp_BV)){
    surface <- shp_BV %>% 
      filter(CODE==stationID[1]) %>% 
      select(areaname) %>% 
      as.numeric()
    }  
    if ("CODE" %in% names(shp_BV)){
      surface <- shp_BV %>% 
        filter(CODE==stationID[1]) %>% 
        select(areaname) %>% 
        as.numeric()
    }
            #conversion du débit de L/s en mm/j
    tmp <- mutate(tmp, Q_obs=Q_obs*86400/(surface*1000000))
    
  }
  
  print(paste0('Data from Station ',stationID,': OK'))
  
  
  # else do nothing
  summary(tmp)
  return(tmp)
}


ExtractBanqueHydro_m3s<-function(folder, stationID,timewindow, shpBV=NULL,areaname=NULL){
  # ****** TODO: rechercher le code station ID dans le nom de fichier (ne présuppose pas la syntaxe exacte du nom)******
  # recherche et lecture du fichier
  tmp <- read.table(paste(folder,"/",stationID, "_HYDRO_QJM.txt",sep=""),header = TRUE, sep=";",skip=41, fill=TRUE, stringsAsFactors=FALSE,quote="", na.strings= " ") #comment.char = "F"
  # On ne conserve que les QJO (les fichiers sont trop en bazar)
  #tmp <- filter(tmp,V1=="QJO")
  
  #remove poor quality
  ##     Val_H       : code de validité BANQUE HYDRO ([5] estimé (anciennement douteux), [I] inconnu faible, [S] inconnu fort, [8] reconstitué bon, [9] bon)
  ##     Val_I       : code de validité INRAE ([0] anomalie présumée : valeur interpolée, [1] pas d anomalie détectée, [2] débit négatif)
  ##     Val_E2      : code de validité Explore2 ([0] pas d'anomalie ou donnée manquante, [1] autres, [2] bruit, [3] décrochement, [4] erreur ponctuelle, [5] interpolation, [6] désaccord (2 anomalies différentes identifiées), [-99] période non critiquée)
  
  tmp$Q_obs[tmp$val_I == 9 ] <- NA # remove negative values
  tmp$Q_obs[tmp$val_E2 != 0 ] <- NA # remove error E2
  tmp <- tmp[,1:2] # recupere la date et le debit
  colnames(tmp) <- c("Time", "Q_obs") # renomme les colonnes
  tmp$Q_obs <- as.numeric(tmp$Q_obs)
  tmp$Q_obs[tmp$Q_obs < 0 ] <- NA # remove negative values
  summary(tmp)
  # formatage des dates en format chron
  tmp <- mutate(tmp, Time= chron(as.character(strptime(tmp$Time,"%Y%m%d")),format="y-m-d"))
  # extraction selon la time window
  timesequence <- seq.dates(from=chron(strsplit(timewindow,"/")[[1]][1],format="y-m-d"), to=chron(strsplit(timewindow,"/")[[1]][2],format="y-m-d"),by="days")
  tmp <- filter(tmp, Time %in% timesequence)
  # Use merge to recreate Nas in case they are not properly indicated in the banque hydro original files (unfortunatey it does happen)
  tmp <- merge(data.frame(Time=timesequence),tmp,by="Time",all.x=TRUE)
  # # find any pbs in the time sequence ### DBG ONLY
  # if(setequal(tmp$Time,timesequence)==FALSE){
  #   print (paste("error: missing dates station ", stationID, " ",subset(timesequence, !(timesequence %in% tmp$Time))))
  #   break
  #     }
  # 
  # # récupération shapefile si shpBV != NULL
  # if(is.null(shpBV)==FALSE){
  #   # lecture du shapefile
  #   
  #   
  #   # shp_BV <- st_read(shpBV)
  #   shp_BV <- shpBV %>% as_tibble()
  #   
  #   
  #   
  #   #st_crs(shp_BV)$epsg<-2154
  #   # récupération de la surface
  #   # **** TODO = récupération de la surface à partir du shapefile et non de la table attributaire!!!!***************
  #   if ("Code" %in% names(shp_BV)){
  #     surface <- shp_BV %>% 
  #       filter(CODE==stationID[1]) %>% 
  #       select(areaname) %>% 
  #       as.numeric()
  #   }  
  #   if ("CODE" %in% names(shp_BV)){
  #     surface <- shp_BV %>% 
  #       filter(CODE==stationID[1]) %>% 
  #       select(areaname) %>% 
  #       as.numeric()
  #   }
#    tmp <- mutate(tmp, Q_obs=Q_obs*86400/(surface*1000000))
    
  # }

  tmp <- mutate(tmp, Q_obs=Q_obs/1000)
  
  print(paste0('Data from Station ',stationID,': OK'))
  
  
  # else do nothing
  summary(tmp)
  return(tmp)
}



ExtractBanqueHydro2020<-function(folder, stationID, timewindow, shpBV=NULL,areaname=NULL){
  # ****** TODO: rechercher le code station ID dans le nom de fichier (ne présuppose pas la syntaxe exacte du nom)******
  # recherche et lecture du fichier
  
  tmp <- read.table(paste(folder,"/",stationID, "_qj_hydro2.txt",sep=""),header = FALSE, sep=";",skip=3, fill=TRUE, stringsAsFactors=FALSE,quote="", na.strings= " ") #comment.char = "F"
  # On ne conserve que les QJO (les fichiers sont trop en bazar)
  
  tmp <- filter(tmp,V1=="QJO")
  tmp <- tmp[,c(3:4)] # recupere la date et le debit
  
  colnames(tmp) <- c("Time", "Q_obs") # renomme les colonnes
  tmp$Q_obs <- as.numeric(tmp$Q_obs)
  tmp$Q_obs[tmp$Q_obs < 0 ] <- NA # remove negative values
  summary(tmp)
  # formatage des dates en format chron
  tmp <- mutate(tmp, Time= chron(as.character(strptime(tmp$Time,"%Y%m%d")),format="y-m-d"))
  # extraction selon la time window
  timesequence <- seq.dates(from=chron(strsplit(timewindow,"/")[[1]][1],format="y-m-d"), to=chron(strsplit(timewindow,"/")[[1]][2],format="y-m-d"),by="days")
  tmp <- filter(tmp, Time %in% timesequence)
  # Use merge to recreate Nas in case they are not properly indicated in the banque hydro original files (unfortunatey it does happen)
  tmp <- merge(data.frame(Time=timesequence),tmp,by="Time",all.x=TRUE)
  # # find any pbs in the time sequence ### DBG ONLY
  # if(setequal(tmp$Time,timesequence)==FALSE){
  #   print (paste("error: missing dates station ", stationID, " ",subset(timesequence, !(timesequence %in% tmp$Time))))
  #   break
  #     }
  
  # récupération shapefile si shpBV != NULL
  if(is.null(shpBV)==FALSE){
    # lecture du shapefile
    shp_BV <- st_read(shpBV)
    #st_crs(shp_BV)$epsg<-2154
    # récupération de la surface
    # **** TODO = récupération de la surface à partir du shapefile et non de la table attributaire!!!!***************
    if ("Code" %in% names(shp_BV)){
      surface <- subset(shp_BV, Code == stationID[1])[[areaname]]
    }  
    if ("CODE" %in% names(shp_BV)){
      surface <- subset(shp_BV, CODE==stationID[1])[[areaname]]
    }
    #conversion du débit de L/s en mm/j
    tmp <- mutate(tmp, Q_obs=Q_obs*86400/(surface*1000000))
  }
  
  print(paste0('Data from Station',stationID,': OK'))
  # else do nothing
  summary(tmp)
  return(tmp)
}

# -------------------------------------------------------------------
# Format MDR Obs
# fonction qui récupère les fichiers QJO obs formatés par Thomas Cipriani dans le projet MDR
# -------------------------------------------------------------------
# folder:     répertoire où se trouvent les fichiers avec les chroniques
# stationID:  code HYDRO de la station de forme X1130010
#             rows containing the data to actually aggregate
# timewindow: fenêtre temporelle (date début / date fin) sur laquelle on 
#             souhaite reformater la série; format "1981-01-01/2010-12-31"
# shpST:      adresse complète d'un shapefile contenant en table attributaire la surface du BV de la station. Par défaut = NULL
#             si présent: convertit le débit en mm/j en divisant par la
#             surface du BV
#-------------------------------------------------------------------
# output: renvoie un data frame à 2 colonnes: Time = la date en format chron
# et Q_obs = les valeurs de débit en L/s ou mm/j selon l'option choisie
# folder <- "~/Travail/Projets/2018_MDR_EauxSouterraines/Donnees/obs_MDR"
# stationID <- "U0104010"
# timewindow <- "1987-01-01/2010-12-31"
# shpST<-"~/Travail/SIG/Rhone/gauges_Rhone_retenues.shp"


FormatMDRObs<-function(folder, stationID,timewindow, shpST=NULL){
  # ****** TODO: rechercher le code station ID dans le nom de fichier (ne présuppose pas la syntaxe exacte du nom)******
  # recherche et lecture du fichier
  tmp<-read.table(paste(folder,"/",stationID, "_OBS_ls.txt",sep=""),sep="\t",header=TRUE,fill=T, stringsAsFactors=FALSE,quote="",comment.char = "F", na.strings= "NA")
  # formatage des dates en format chron
  tmp<- mutate(tmp, Time= chron(as.character(as.Date(paste(an,mois,jour,sep="/"),"%Y/%m/%d")),format="y-m-d"))

  # extraction selon la time window
  timesequence<-seq.dates(from=chron(strsplit(timewindow,"/")[[1]][1],format="y-m-d"), to=chron(strsplit(timewindow,"/")[[1]][2],format="y-m-d"),by="days")
  tmp<-filter(tmp, Time %in% timesequence)
  # Use merge to recreate Nas in case they are not properly indicated in the banque hydro original files (unfortunatey it does happen)
  tmp<-merge(data.frame(Time=timesequence),tmp,by="Time",all.x=TRUE)
  # # find any pbs in the time sequence ### DBG ONLY
  # if(setequal(tmp$Time,timesequence)==FALSE){
  #   print (paste("error: missing dates station ", stationID, " ",subset(timesequence, !(timesequence %in% tmp$Time))))
  #   break
  #     }
  
  # elimination des colonnes inutiles et renommage de la colonne de débits (merci dplyr)
  tmp<- tmp %>% 
    select(-jour,-mois, -an) %>% 
    rename(Q_obs = debit)
  
  # récupération shapefile si shpBV != NULL
  if(is.null(shpST)==FALSE){
    # lecture du shapefile
    station_shp<-st_read(shpST)
    st_crs(station_shp)$epsg<-2154
    # récupération de la surface / attention la surface est en km2
    surface<-subset(station_shp, CODE==stationID)$S_BH
    #conversion du débit de L/s en mm/j
    tmp<-mutate(tmp, Q_obs=Q_obs*86400/(surface*1000000))
  }
  
  # else do nothing
  return(tmp)
}


# # # Test
# test<-ExtractBanqueHydro(folder= "~/Travail/Projets/2018_MDR_EauxSouterraines/Donnees/HYDRO",
#                          stationID="U0415010",
#                          timewindow= "1981-01-01/2010-12-31",
#                          shpBV = "~/Travail/SIG/Rhone/maillage_rhone/J2K_Rhone_v2/stations_selection.shp",
#                          areaname="S_BH")
# # pour appliquer en batch avec un lapply
# Q_Hydro<-lapply(contours_BV$Code, ExtractBanqueHydro,folder= folder, timewindow=timewindow)
# names(Q_Hydro)<-contours_BV$Code
# test<-FormatMDRObs(folder = "~/Travail/Projets/2018_MDR_EauxSouterraines/Donnees/obs_MDR",
#                          stationID = "U1224010",
#                          timewindow = "1987-01-01/2010-12-31",
#                          shpST="~/Travail/SIG/Rhone/gauges_Rhone_retenues.shp")
# test<-lapply(station_names, FormatMDRObs,folder= "~/Travail/Projets/2018_MDR_EauxSouterraines/Donnees/obs_MDR", 
#              timewindow="1987-01-01/2010-12-31", shpST="~/Travail/SIG/Rhone/gauges_Rhone_retenues.shp")
# 
# for(i in station_names){
#   test<-FormatMDRObs(folder = "~/Travail/Projets/2018_MDR_EauxSouterraines/Donnees/obs_MDR",
#                                               stationID = i,
#                                              timewindow = "1987-01-01/2010-12-31",
#                                                shpST="~/Travail/SIG/Rhone/gauges_Rhone_retenues.shp")
# }
# # names(Q_Hydro)<-contours_BV$Code

