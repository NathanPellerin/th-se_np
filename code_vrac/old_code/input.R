#*******************************************************************************
#* Created 16/11/2022
#* Creator N.P
#* INPUT variables: description table, FO
#*******************************************************************************

table_description_barrage <- function(){
  table_description_barrage <- 
    tibble(ReachID = c(380601, 384001, 580400, 609400, 702400, 712401, 
                       714000,724600, 727200, 730200, 737601, 740400, 
                       744600, 810200, 819000, 862600, 866200),
           Nom = c('LoireArdeche', 'Chassezac', 'Vouglans', 'Leman', 'Monteynard', 
                   'GrdMaison', 'Chambon', 'Roselend', 'IsereArc','Tignes', 'ArcIsere', 
                   'Bissorte', 'MontCenis', 'DuranceCanaux', 'SerrePoncon', 'VerdonCanaux', 
                   'SteCroix'),
           type = c('transfert','barrage','barrage','barrage','barrage','barrage',
                    'barrage','barrage','transfert','barrage','transfert','barrage',
                    'barrage','transfert','barrage','transfert','barrage'),
           Smax = c(1.00e+06, 5.80e+01, 6.00e+02, 8.90e+04, 4.40e+02, 1.40e+02, 5.00e+01, 
                    2.10e+02, 1.00e+06,2.30e+02, 1.00e+06, 5.00e+01, 3.30e+02, 1.00e+06, 
                    1.04e+03, 1.00e+06, 7.60e+02),
           V0 = c(1.000000e+06, 7.800000e+00, 4.833000e+02, 8.897090e+04, 3.161000e+02, 
                  1.005800e+02, 2.810000e+01, 1.312200e+02,0.000000e+00, 1.437145e+02, 
                  0.000000e+00, 3.517000e+01, 2.631500e+02, 0.000000e+00, 5.610400e+02, 
                  0.000000e+00,6.898000e+02))
  
  return(table_description_barrage)
}

################################################################################
table_description_affluents <- function(){
  table_description_affluents <- 
    tibble(ReachID = c(354801,369201,374601,390801,392001,399201,
                       404401,412001,415001,573401,679801,691601,
                       693201,759001,769802,778201,787001,799401),
           Nom = c('Gardon', 'Aygues', 'Ardèche', 'Ouvèze RD', 'Eyrieux', 'Doux', 
                   'Cance', 'Gier', 'Saône','Rhône', 'Yzeron', 'Galaure',
                   'Isère', 'Drôme', 'Roubion', 'Cèze', 'Ouvèze RG','Durance'),
           type = c('affluent','affluent','affluent','affluent','affluent','affluent',
                    'affluent','affluent','affluent','affluent','affluent','affluent',
                    'affluent','affluent','affluent','affluent','affluent','affluent'),
           code_station = c('V7194005','V5354010','V5064010','V4305010','V4174015',
                            'V3744010','V3524010','V3124010','U4720010','V3000015',
                            'V3015020','V3614010','W3540010','V4264021',
                            'V4414021','V5474020','OUVEZE','X3500010'),
           Nom_lieu = c('à Remoulins', 'à Orange', "à Saint-Martin-d'Ardèche", 
                        'à Pouzin', 'à Saint-Fortunat-sur-Eyrieux', 
                        'à Tournon-sur-Rhône', 'à Sarras', 'à Givors', 
                        'à Lyon','à Lyon Perrache', 'à Francheville', 
                        'à Saint-Uze', 'à Beaumont-Monteux','à Loriol-sur-Drôme',
                        'à Montélimar','à Chusclan','à Sorgues','à Caumont-sur-Durance'),
           ordre_Amont_Aval = c("r. Gardon","o. Aygues","m. Ardèche","k. Ouvèze RD","i. Eyrieux",
                                "g. Doux","e. Cance","d. Gier","b. Saône","a. Rhône","c. Yzeron",
                                "f. Galaure","h. Isère","j. Drôme",
                                "l. Roubion","n. Cèze","p. Ouvèze RG","q. Durance"))
  
  table_description_affluents = arrange(table_description_affluents, ordre_Amont_Aval)
  
  return(table_description_affluents)
}

################################################################################
table_description_reach_soutien_etiage<- function(){
  table_description_reach_soutien_etiage <- 
    tibble(ReachID = c(728200, 723400, 740000, 383200, 578001, 580200, 712201, 702200, 816000, 862800),
           Nom = c('Tignes', 'Roselend', 'MontCenis et Bissorte', 'Chassezac', 'Vouglans', 'Vouglans', 'GrdMaison et Chambon', 
                   'Monteynard', 'SerrePoncon', 'SteCroix'),
           type = c('soutien_etiage','soutien_etiage','soutien_etiage','soutien_etiage','soutien_etiage','soutien_etiage',
                    'soutien_etiage','soutien_etiage','soutien_etiage','soutien_etiage'))
  
  table_description_reach_soutien_etiage = arrange(table_description_reach_soutien_etiage, ReachID)
  
  return(table_description_reach_soutien_etiage)
}
################################################################################
nom_Affluents_barrages <- function(table_description_barrage, table_description_affluents){
  
  dim_table_affluent <- dim(table_description_affluents)[1]
  dim_table_barrage <- dim(table_description_barrage)[1]
  name <- table_description_barrage[,1:3] %>% 
    full_join(., table_description_affluents, by = 'ReachID')
  
  for (i  in (dim_table_barrage + 1):(dim_table_affluent + dim_table_barrage)){
    name[i,2] =  name[i,4]
    name[i,3] =  name[i,5]
  }
  nom_Affluents_barrages <- name[,-(4:5)]%>%
    rename(Nom = Nom.x, type = type.x)
  rm(name)
 
  return(nom_Affluents_barrages)
}

################################################################################
FO_2023 <- function(limit1, limit2){
  #### FO reading ####
  readFO = readLines("./home/JAMS/jamsModelData/J2K_Rhone_v2/input/local/FO_2023.dat")
  
  nameFO = readFO[seq(from = 9, to = 9)]
  nameFO = unlist(strsplit(nameFO, split ='\t'))
  nameFO[1] = "Date"
  
  
  #### Fonction Objectif ####
  FO = readFO[seq(from = 16, to = length(readFO))]
  FO = unlist(strsplit(FO, split ='\t'))
  FO = matrix(FO, ncol = 19, byrow =TRUE)
  FO = as_tibble(FO)
  FO = FO [,-2]
  colnames(FO) = nameFO
  FO = mutate(FO, 
              Date = as.Date(Date, "%Y.%m.%d"),
              Tignes=as.numeric(Tignes),
              Roselend=as.numeric(Roselend),
              MontCenis=as.numeric(MontCenis),
              Bissorte=as.numeric(Bissorte),
              Chassezac=as.numeric(Chassezac),
              Vouglans=as.numeric(Vouglans),
              GrdMaison=as.numeric(GrdMaison),
              Monteynard=as.numeric(Monteynard),
              SerrePoncon=as.numeric(SerrePoncon),
              SteCroix=as.numeric(SteCroix),
              ArcIsere=as.numeric(ArcIsere),
              IsereArc=as.numeric(IsereArc),
              LoireArdeche=as.numeric(LoireArdeche),
              DuranceCanaux=as.numeric(DuranceCanaux),
              VerdonCanaux=as.numeric(VerdonCanaux),
              Chambon=as.numeric(Chambon),
              Leman=as.numeric(Leman))
  
  # Long form :
  FO = pivot_longer(FO, !Date, names_to = "Nom", values_to = "Volume_FO")
  FO = arrange(FO, Nom)
  
  FO = filter(FO, Date >= as.Date(limit1), Date <= as.Date(limit2))
  
  #Complete missing values for the 29th February
  # FO$Volume_FO[FO$date == '1960-02-29'] = (FO$Volume_FO[FO$date == '1960-02-28'] + FO$Volume_FO[FO$date == '1960-03-01'])/2
  
  #Get FO in m³/j 
  # FO$Volume_FO =FO$Volume_FO/1000
  
  return(FO)
}

FO_2023_s2 <- function(limit1, limit2){
  #### FO reading ####
  readFO = readLines("/home/npellerin/CONTINUUMMES/WORKING_DIRECTORY/0-DATA/FO/FO_scenarios/Scenario_2/FO_2023_s2.dat")
  
  nameFO = readFO[seq(from = 9, to = 9)]
  nameFO = unlist(strsplit(nameFO, split ='\t'))
  nameFO[1] = "Date"
  
  
  #### Fonction Objectif ####
  FO = readFO[seq(from = 16, to = length(readFO))]
  FO = unlist(strsplit(FO, split ='\t'))
  FO = matrix(FO, ncol = 19, byrow =TRUE)
  FO = as_tibble(FO)
  FO = FO [,-2]
  colnames(FO) = nameFO
  FO = mutate(FO, 
              Date = as.Date(Date, "%Y.%m.%d"),
              Tignes=as.numeric(Tignes),
              Roselend=as.numeric(Roselend),
              MontCenis=as.numeric(MontCenis),
              Bissorte=as.numeric(Bissorte),
              Chassezac=as.numeric(Chassezac),
              Vouglans=as.numeric(Vouglans),
              GrdMaison=as.numeric(GrdMaison),
              Monteynard=as.numeric(Monteynard),
              SerrePoncon=as.numeric(SerrePoncon),
              SteCroix=as.numeric(SteCroix),
              ArcIsere=as.numeric(ArcIsere),
              IsereArc=as.numeric(IsereArc),
              LoireArdeche=as.numeric(LoireArdeche),
              DuranceCanaux=as.numeric(DuranceCanaux),
              VerdonCanaux=as.numeric(VerdonCanaux),
              Chambon=as.numeric(Chambon),
              Leman=as.numeric(Leman))
  
  # Long form :
  FO = pivot_longer(FO, !Date, names_to = "Nom", values_to = "Volume_FO")
  FO = arrange(FO, Nom)
  
  FO = filter(FO, Date >= as.Date(limit1), Date <= as.Date(limit2))
  
  #Complete missing values for the 29th February
  # FO$Volume_FO[FO$date == '1960-02-29'] = (FO$Volume_FO[FO$date == '1960-02-28'] + FO$Volume_FO[FO$date == '1960-03-01'])/2
  
  #Get FO in m³/j 
  # FO$Volume_FO =FO$Volume_FO/1000
  
  return(FO)
}



