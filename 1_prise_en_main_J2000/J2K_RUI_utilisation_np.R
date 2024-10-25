#*******************************************************************************
#* Created 23/10/2024
#* Creator N.P ~
#* Main program to test myself on J2K_RUI
#*******************************************************************************

source("./working_directory/0_SOURCE/J2K-RUI/J2K_RUI.R")
getwd()

# 2/ Loading shapefiles and hru and reach parameter files
shp <- j2kReadGIS(hrufp=NULL,
                  reachfp=NULL,
                  stationfp="./J2000/SIG/GIS_Explore2/GIS_Rhone/shp_rhone_v1/hru/hru",
                  subcatchfp=NULL)


reachpar <- j2kReadPar("/home/npellerin/JAMS/jamsModelData/J2K_Rhone_v2/parameter/reach_full.par")
hrupar <- j2kReadPar("/home/npellerin/JAMS/jamsModelData/J2K_Rhone_v2/parameter/hru_full.par")
hrufarmdamspar  <- j2kReadPar("/home/npellerin/JAMS/jamsModelData/J2K_Rhone_v2/parameter/hru_farmdams.par")
farmdamspar <- j2kReadPar("/home/npellerin/JAMS/jamsModelData/J2K_Rhone_v2/parameter/farmdams.par")

summary(reachpar)
summary(hrupar)




