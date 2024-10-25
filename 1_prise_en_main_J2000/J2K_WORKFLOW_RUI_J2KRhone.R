source("./working_directory/0_SOURCE/J2K-RUI/J2K_RUI.R")


#####################################################################
# Catchment and sub-catchments definition
#####################################################################

# -------------------------------------------------------------------
# HRU-Delin results
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# 1/ folders and filenames
 hrudelin_fp <- "/home/jeremie.bonneau/Explore2/hrudelinRhone/restitution2/"
# 
 hrudelin_main <- "out_results"
 hrudelin_intermediate <- "out_files"
# 
 shp_fn_hru <- "hru"
 shp_fn_reach <- "reach"
 shp_fn_station <- "stations"
 shp_fn_subcatch <- "step2_step2_watersheds"
# 
 par_fn_hru <- "hru.par"
 par_fn_reach <- "reach.par"

 # ------------------------------------------------------------------- 
 # Travail sur les stations to check that they are indeed the same between all models :
 
 # 1 load current stations in J2K Rhone v2 
 
 
 # 2 load 
 
# -------------------------------------------------------------------
# 2/ Loading shapefiles and hru and reach parameter files

## il faut prendre le shp généré par hru-delin complet sinon la détermination des sous-bassins est incomplete
 # shp <- j2kReadGIS(hru = NULL,
 #                   reach = NULL,
 #                   station = '/home/jeremie.bonneau/Explore2/GIS_Loire/Stations/StationsHydro_BVRhone_V2_BV',
 #                   subcatch = NULL)
 
 shp <- j2kReadGIS(hru = NULL,
                   reach = NULL,
                   station = '/home/jeremie.bonneau/Explore2/GIS_Rhone/shp_rhone_v1/stations/stations',
                   subcatch = NULL)

reachpar <- j2kReadPar("/home/jeremie.bonneau/JAMS/data/J2K_Rhone_Natural_Hydrology_v2/parameter/reach.par")
hrupar <- j2kReadPar("/home/jeremie.bonneau/JAMS/data/J2K_Rhone_Natural_Hydrology_v2/parameter/hru.par")

# If you get a warning here (incomplete rows), it means that HRU-Delin failed at some point...#
# Best workaround is to delete the corresponding rows 
# (it's likely very small HRUs that cause the problem... which, for some reason, don't have values for the 'to_poly' and 'to_reach' column)
summary(reachpar)
summary(hrupar)

# Notes:
#  * the reach and hru parameter files have to be copied manually into the
# parameter folder of J2K 
#  * Reach.par likely needs some editing:
# (1) make sure to have realistic reach width
# (2) make sure none of the slopes are zero, it will make J2K crash or behave strangely

# -------------------------------------------------------------------
# Create sub-catchment object for j2k
# -------------------------------------------------------------------
# 
# # only take explore 2 diagnostic stations
# # gauges_explore2 <- readOGR('~/Explore2/stations_Explore2_final.shp')
# gauges_explore2 <- read.table('/home/jeremie.bonneau/Explore2/points_simulations_all_mai22.csv',
#                               sep = ',',
#                               header = TRUE)
# 
# shp$station <- shp$station[which(!is.na(match(shp$station@data$CODE,
#                                               gauges_explore2$CODE[gauges_explore2$inEvaluation ==1]))),]



catchmentids <- as.numeric(shp$station$ID)
length(catchmentids)
# remove catchment 531 (Verdon à Castillon)
catchmentids<-catchmentids[-match(531,catchmentids)]

# get the index of the catchments to keep: all_catchment[, "id"] returns all the catchments ids as if all_catchment was a dataframe
# (it is not a data.frame though, it is a list with an S3 class of "j2ksubcatch")

all_catchments <- j2kGetSubcatchment(shp=shp, 
                                     reachpar=reachpar, 
                                     stationid="ID")
print(all_catchments)


#manually select some catchments to develop functions
#catchmentids=c(427,542,517,500,414,383,368,358,468,445,406,319,315,232,309,218,4,11,16,286,166,155,151,91,85,41,51,24)
#sort(c(427,542,517,500,414,383,368,358,468,445,406,319,315,232,309,218,4,11,16,286,166,155,151,91,85,41,51,24))
catchmentids <- as.numeric(shp$station$ID)

index_of_catchments_to_keep <- match(catchmentids, all_catchments[, "id"])
index_of_catchments_to_keep <- index_of_catchments_to_keep[!is.na(index_of_catchments_to_keep)]

#subset the original j2ksubcatch object using the index of the catchments you want to keep (as if all_catchments was a list, which it is)
my_catchments <- all_catchments[index_of_catchments_to_keep]

print(my_catchments)



# #####################################################################
# # Meteorological forcings: SAFRAN to J2K (only if needed)
# #####################################################################
# 
# # where the ncdf files are ... NOTE: the order in 'ncfilenames' matter! 
# safDataDir <- "C:/Users/ivan.horner/Documents/Data/SAFRAN/SAFRAN_2019/"
# ncfilenames <- c("safran_new_Rainf.nc", "safran_new_Snowf.nc", "safran_new_ET0.nc", "safran_new_Tair.nc")
# 
# # J2K forcing files names and header comment
# header_comments <- paste("j2k-RUI", Sys.getenv("USERNAME"), format(Sys.time(), format="%Y-%m-%d %H:%M", tz="UTC"), sep=" | ")  # header comment of forcing files
# forcing_filenames <- c(P="P_2019", ET0="ETRef_2019", T="T_2019")
# 
# # Modeling time range
# time_range <- as.POSIXct(strptime(c("1970-01-01", "2019-09-01"), format="%Y-%m-%d", tz="UTC"))
# 
# # Get needed cell ids, and the corresponding X, Y (in Lambert 93) and elevation 
# # Get spatial extend from hrus shapefile to retrieve the needed Safran cells
# # hrusextend <- j2kGetSpatialExtend(hrufolder=hrushp_folder, hrufilename=hrushp_name, fact=0.1)
# hrusextend <- j2kGetSpatialExtent(shp=shp, fact=0.1)
# cell_info <- j2kGetCellsFromExtend(ncdffp=paste0(safDataDir, ncfilenames[1]), extend=hrusextend)
# cell_info <- cbind(cell_info, "Elevation"=rep(1250, nrow(cell_info)))
# head(cell_info)
# dim(cell_info)
# 
# # fetching data and writing j2k input files
# j2kProjDir <- "C:/Users/ivan.horner/Documents/Forges/jams/modeldata/J2K_Ardeche_IH/"
# j2kFromSafranToJ2K(
#   safDataDir=safDataDir,
#   j2kProjDir=j2kProjDir,
#   time_range=time_range,
#   cell_info=cell_info,
#   forcing_filenames=forcing_filenames,
#   header_comments=header_comments,
#   ncfilename=ncfilenames)

#####################################################################
# J2K Configuration
#####################################################################

# -------------------------------------------------------------------
# Loading source project
# -------------------------------------------------------------------

# parameter file names
par_fn_landuse <- "landuse.par"
par_fn_soil <- "soils.par"
par_fn_hgeo <- "hgeo_v1.par"
par_fn_hru <- "hru_v1_geo_lu_soil.par"
par_fn_reach <- "reach.par"

# Project folder
j2kProjDir <- "~/JAMS/data/J2K_Rhone_Natural_Hydrology_v2/"
j2kProjOutDir <- "~/JAMS/data/J2K_Rhone_Natural_Hydrology_v2/"

# Base model file path
j2kprojectFile <- paste0(j2kProjDir, "J2K_Rhone_Natural_Hydrology_v2_restitution.jam")

# Output model file path
j2kprojectOutFile <- paste0(j2kProjOutDir, "J2K_Rhone_Natural_Hydrology_v2_restitution_snow.jam")

rm('j2kjamproj')
# read base model (or an already existing model created using J2K-RUI)
j2kjamproj <- j2kLoadJAMSProject(j2kprojectFile)




#################################################################################


fp = j2kprojectFile


  j2kproject = xmlRoot(xmlTreeParse(fp))
  contextnames = .j2kGetContextNames(xml = j2kproject)
  # j2kproject = xmlTreeParse(fp)$doc$children$model
  ds = .j2kXMLgetDataStores(j2kproject)
  dbls = .j2kXMLgetDoubleSetter(j2kproject)
  sc = .j2kXMLgetSubcatchments(j2kproject)
  # removing subcatchement output variables from TimeLoop datastore and from the double setter component
  outvar = as.character(outer(unlist(sc$outvariables), unlist(lapply(sc$subcatch, function(x){x[["id"]]})), paste, sep="_"))
  if (length(outvar) > 0) {
    matchvar = match(outvar, ds[[get(".j2k.timeContext", envir=j2k.env)]])
    
    matchvar = 1:length(outvar)
    
    if (length(which(is.na(matchvar))) > 0){warning("Some variables were missing from the '", get(".j2k.timeContext", envir=j2k.env), "' datastore.")}
    ds[[get(".j2k.timeContext", envir=j2k.env)]] = ds[[get(".j2k.timeContext", envir=j2k.env)]][matchvar]
    # removing subcatchement varables from DoubleSetter component
    matchvar = match(outvar, dbls)
    if (length(which(is.na(matchvar)))>0){warning("Some variables were missing from the '", get(".j2k.component.doubleSetter", envir=j2k.env), "' component.")}
    dbls = dbls[-matchvar]
  }
  # removing subcatchement output parameters from MODEL datastore
  outpar = as.character(outer(unlist(sc$outparameters), unlist(lapply(sc$subcatch, function(x){x[["id"]]})), paste, sep="_"))
  # print(unlist(lapply(sc$subcatch, function(x){x[["id"]]})))
  # print(unlist(sc$outparameters))
  # print(outpar)
  if (length(outpar) > 0) {
    matchpar = match(outpar, ds[[get(".j2k.modelName", envir=j2k.env)]])
    # print(matchpar)
    if (length(which(is.na(matchpar)))>0){warning("Some variables were missing from the '", get(".j2k.modelName", envir=j2k.env), "' datastore.")}
    ds[[get(".j2k.modelName", envir=j2k.env)]] = ds[[get(".j2k.modelName", envir=j2k.env)]][-matchpar]
  }
  # FIX ME: should also do the same thing for Area_xx in J2K_RHONE datastore???
  pfn = .j2kXMLgetParameterFileNames(j2kproject)
  ffn = .j2kXMLgetForcingFileNames(j2kproject)
  mtr = .j2kXMLgetTimeInformation(j2kproject)
  J2KJAMSproject = list(
    "filename"=basename(fp),
    "directory"=dirname(fp),
    "XML"=j2kproject,
    "datastores"=ds,
    "doublesetter"=dbls,
    "outputvariables"=sc$outvariables,
    "outputparameters"=sc$outparameters,
    "subcatchments"=sc$subcatch,
    "parameterfiles"=pfn,
    "forcingfiles"=ffn,
    "timeinformation"=mtr,
    "contextnames"=contextnames
  )
  # print(sc)
  class(J2KJAMSproject) = append(class(J2KJAMSproject), "j2kproject")
  return(J2KJAMSproject)



  j2kjamproj <- J2KJAMSproject




#################################################################################





j2kjamproj$datastores$TimeLoop <- NULL

# add sub-catchments
j2kjamproj <- j2kSetSubcatchments(j2kjamproj, my_catchments)
print(j2kjamproj)

# set parameters files (they must be in the 'parameter' folder)
j2kjamproj<- j2kSetParameterFiles(j2kjamproj, hru=paste0("parameter/", par_fn_hru),
                                  reach=paste0("parameter/", par_fn_reach),
                                  landuse=paste0("parameter/", par_fn_landuse),
                                  soil=paste0("parameter/", par_fn_soil),
                                  hgeol=paste0("parameter/", par_fn_hgeo))

# set the forcing file names (no extensions!)
j2kjamproj <- j2kSetForcingFiles(j2kjamproj, P="P_safran", ET0="ETRef_1959_2019", T="T_safran")

# set the modeling time range (it might be more relevant to do this in JAMS rather than here)
timerange <- as.POSIXct(strptime(c("1959-01-01", "2019-12-31"), format="%Y-%m-%d", tz="UTC"))
j2kjamproj <- j2kSetModelingTimeRange(j2kjamproj, timerange=timerange)

# set desired output variables and parameters (parameter values aggregated for each sub-catchment)
# here nothing will be outputed. 

j2kjamproj <- j2kSetOutputParameters(j2kjamproj, misc=NULL, liter=NULL, area=FALSE)
j2kjamproj <- j2kSetOutputVariables(j2kjamproj, misc=NULL, liter=NULL, reach=NULL)
# or ...
j2kjamproj <- j2kSetOutputParameters(j2kjamproj, misc=NULL, liter=NULL, area=FALSE)
j2kjamproj <- j2kSetOutputVariables(j2kjamproj, misc=c("precip", "tmean"), liter=NULL, reach='Runoff')
# or ...
j2kjamproj <- j2kSetOutputParameters(j2kjamproj,
                                     misc=c('RG1_k', "slope", "sealedGrade",  "MaxInfSummer", "MaxInfWinter", "MaxInfSnow",
                                            paste0("Kc_", 1:12), paste0("LAI_", 1:12)),
                                     liter=c('maxRG2','maxRG1', "maxMPS", "maxLPS"), area=TRUE)
j2kjamproj <- j2kSetOutputVariables(j2kjamproj,
                                    misc= c('precip', 'tmean', 'etref', 'etpot'),
                                    liter=c('etact', "storedInterceptedWater", "TotSWE", "actDPS", "actMPS", "actLPS", "actRG1", "Percolation", "LPSoutflow", "LateralFlow"),
                                    reach=c('outRD1', 'outRD2', 'outRG1', 'Runoff'))
# or ...
j2kjamproj <- j2kSetOutputParameters(j2kjamproj, misc=NULL, liter=NULL, area=FALSE)
j2kjamproj <- j2kSetOutputVariables(j2kjamproj,
                                    misc= c('precip', 'tmean', 'etref', 'etpot'),
                                    liter= c('rain', 'snow'),
                                    reach=c('outRD1', 'outRD2', 'outRG1', 'Runoff'))


# Explore 2 restitution full
j2kjamproj <- j2kSetOutputParameters(j2kjamproj, misc=NULL, liter=NULL, area=FALSE)
j2kjamproj <- j2kSetOutputVariables(j2kjamproj,
                                    misc = c('precip','tmean', 'etref', 'etpot'),
                                    liter = c('snow','etact',"actMPS", "actLPS","actRG1"),
                                    reach = c( 'Runoff'))



# SNOW
j2kjamproj <- j2kSetOutputParameters(j2kjamproj, misc=NULL, liter=NULL, area=FALSE)
j2kjamproj <- j2kSetOutputVariables(j2kjamproj, 
                                    misc=c('SnowDepth'), 
                                    liter=c('TotSWE'), 
                                    reach=NULL)
# # Spécial pour J2K_Rhone_v2 avec irrigation
# j2kjamproj <- j2kSetOutputVariables(j2kjamproj,
#                                     misc= c('precip', 'tmean', 'etref', 'etpot'),
#                                     liter=c('etact', "TotSWE",'irrigationTotal'), # special pour J2K_Rhone_v2
#                                     reach=c('outRD1', 'outRD2', 'outRG1', 'Runoff'))

# have a look at the project before saving it
print(j2kjamproj)

# save the resulting project (that's where the magic is done to create the new model)
j2kSaveJAMSProject(j2kjamproj, fp = j2kprojectOutFile, verbose=TRUE)


#####################################################################
# Get 
#####################################################################
# 
# readjam <- readLines('~/JAMS/data/J2K_Rhone_Natural_Hydrology_v2/J2K_Rhone_Natural_Hydrology_v2_restitution_full.jam')
# 
# i = 1
# for (i in 1:600){
#   
#   texttoreplace <- paste0('precip_',i,';')
#   to_replaced_by <- paste0('precip_',i,';snow_',i,';')
#   readjam_replaced <- gsub(pattern = texttoreplace,
#                            replacement = to_replaced_by,
#                            x = readjam)
#   
#   
# }
# 
# write.table(file = '~/JAMS/data/J2K_Rhone_Natural_Hydrology_v2/J2K_Rhone_Natural_Hydrology_v2_restitution_full_test.jam', 
#             x = readjam_replaced, row.names = FALSE, col.names = FALSE, quote = FALSE)
# 

                     
