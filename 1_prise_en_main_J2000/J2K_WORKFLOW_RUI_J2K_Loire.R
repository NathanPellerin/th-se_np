source("./working_directory/0_SOURCE/J2K-RUI/J2K_RUI.R")
#source("C:/Users/jeremie.bonneau/Documents/Explore2/J2K-RUI/J2K_RUI.R")


#####################################################################
# Catchment and sub-catchments definition
#####################################################################

# -------------------------------------------------------------------
# HRU-Delin results
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# 1/ folders and filenames
 hrudelin_fp <- "/home/jeremie.bonneau/Explore2/GIS_Loire/hru_delin/restitution3/"
 #hrudelin_fp <- "C:/Users/jeremie.bonneau/Documents/Explore2/GIS_Loire/hru_delin/15000hrus_2022jan/"
 
 # 
 hrudelin_main <- "out_results"
 hrudelin_intermediate <- "out_files"
# 
 shp_fn_hru <- "hru"
 shp_fn_reach <- "reach"
 shp_fn_station <- "step2_gauges_for_watersheds"
 shp_fn_subcatch <- "step2_step2_watersheds"
# 
 par_fn_hru <- "hru_e2.par"
 par_fn_reach <- "reach_e2.par"

# -------------------------------------------------------------------
# 2/ Loading shapefiles and hru and reach parameter files

## il faut prendre le shp généré par hru-delin complet sinon la détermination des sous-bassins est incomplete
shp <- j2kReadGIS(hrufp = NULL,
                  reachfp = NULL,
                  stationfp = "//home/jeremie.bonneau/Explore2/GIS_Loire/hru_delin/restitution3/out_files/step2_gauges_for_watersheds",
                  subcatchfp = NULL)

 
 
# read .par
#reachpar <- j2kReadPar("/home/jeremie.bonneau/Explore2/GIS_Loire/hru_delin/restitution3/out_results/reach.par")
#hrupar <- j2kReadPar("/home/jeremie.bonneau/Explore2/GIS_Loire/hru_delin/restitution3/out_results/hru.par")

reachpar <- j2kReadPar("/home/jeremie.bonneau/JAMS/data/J2K_Loire/parameter/reach_e2.par")
hrupar <- j2kReadPar("/home/jeremie.bonneau/JAMS/data/J2K_Loire/parameter/hru_e2.par")


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
all_catchments <- j2kGetSubcatchment(shp = shp, 
                                     reachpar = reachpar, 
                                     stationid = "ID")
print(all_catchments)


# 12/7/2022
# The station outlet reach was not found in the reach topology for the following station: 212, 295, 317, 600, 707
# 212 295 317 600 707 mal située
# 212 is also doublon 



## ----------------------------
# # only take explore 2 diagnostic stations
# gauges_explore2 <- readOGR('~/Explore2/stations_Explore2_final.shp')
# gauges_explore2 <- read.table(file = '/home/jeremie.bonneau/Explore2/Selection/synthese_decision_stations_explore2.csv',
#                               header = TRUE,
#                               sep = ',',
#                               stringsAsFactors = FALSE) %>%
#   as_tibble() %>%
#   filter(Decision == 1)
# #keep
# shp$station <- shp$station[is.element(shp$station@data$CODE,gauges_explore2$Code),]

# ----------------------------
#catchment id
catchmentids <- as.numeric(shp$station$ID)

# get the index of the catchments to keep: all_catchment[, "id"] returns all the catchments ids as if all_catchment was a dataframe
# (it is not a data.frame though, it is a list with an S3 class of "j2ksubcatch")
# index_of_catchments_to_keep <- match(catchmentids, all_catchments[, "id"])
# index_of_catchments_to_keep <- index_of_catchments_to_keep[!is.na(index_of_catchments_to_keep)]
index_of_catchments_to_keep <- is.element(catchmentids, all_catchments[, "id"])
index_of_catchments_to_keep <- catchmentids[index_of_catchments_to_keep]

#subset the original j2ksubcatch object using the index of the catchments you want to keep (as if all_catchments was a list, which it is)
my_catchments <- all_catchments[index_of_catchments_to_keep]
my_catchments <- all_catchments
print(my_catchments)


#####################################################################
# Meteorological forcings: SAFRAN to J2K (only if needed)
#####################################################################
 
 # where the ncdf files are ... NOTE: the order in 'ncfilenames' matter! 
 safDataDir <- "//ly-data/LY-Unites/Riverly/Hhly/Entrepothh_depot/SAFRAN/NEW/daily/"
 ncfilenames <- c("safran_new_Rainf.nc", "safran_new_Snowf.nc", "safran_new_ET0.nc", "safran_new_Tair.nc")
 
 # J2K forcing files names and header comment
 header_comments <- paste("j2k-RUI", Sys.getenv("USERNAME"), format(Sys.time(), format="%Y-%m-%d %H:%M", tz="UTC"), sep=" | ")  # header comment of forcing files
 forcing_filenames <- c(P="rain_safran", ET0="refet", T="tmean_safran")
 
 # Modeling time range
 time_range <- as.POSIXct(strptime(c("1958-01-01", "2019-12-31"), format="%Y-%m-%d", tz="UTC"))
 
 # Get needed cell ids, and the corresponding X, Y (in Lambert 93) and elevation 
 # Get spatial extend from hrus shapefile to retrieve the needed Safran cells
 hrusextend <- j2kGetSpatialExtend(hrufolder="C:/Users/jeremie.bonneau/Documents/Explore2/GIS_Loire/hru_delin/hru_delin_loire_michael/out_results", 
                                   hrufilename = "hru", fact=0.1)
 #hrusextend <- j2kGetSpatialExtent(shp=shp, fact=0.1)
 cell_info <- j2kGetCellsFromExtend(ncdffp=paste0(safDataDir, ncfilenames[1]), extend=hrusextend)
 cell_info <- cbind(cell_info, "Elevation"=rep(1250, nrow(cell_info)))
 head(cell_info)
 dim(cell_info)
 
 # fetching data and writing j2k input files
 j2kProjDir <- "C:/Users/jeremie.bonneau/Documents/JAMS/data/J2K_Loire_NaturalHydrology/"
 j2kFromSafranToJ2K(
   safDataDir = safDataDir,
   j2kProjDir = j2kProjDir,
   time_range = time_range,
   cell_info = cell_info,
   forcing_filenames = forcing_filenames,
   header_comments = header_comments,
   ncfilename = ncfilenames)

 
 
 
# Select three randoms days and plot rainfall to check you export the right data

 
 mydates <- as.POSIXct(c("1990-05-20","1990-05-21","1990-05-22"),tz = 'UTC')
 #mydates <- as.POSIXct(c("2050-12-24"),tz = 'UTC')
 
 R3days <- j2kGetSafranNcdfData(fp=paste0(safDataDir, ncfilenames[1]),
                               from = min(mydates), 
                               to =  max(mydates), 
                               extend = hrusextend)
 
 
 R3days$data[1:10]
 

 #as tibble
 myvarR <- R3days$data %>% 
   as_tibble() %>%  #tibble
   mutate(time = R3days$time)
 
 cell_info$ID <- as.character(cell_info$ID)
 dim(cell_info)
 
 #long format
 myvar_longR <- myvarR %>% 
   pivot_longer(cols = !time,
                names_to = "ID", 
                values_to = "R") %>% 
   full_join(cell_info,by = "ID")
   
 #check
 summary(myvar_longR)
 str(myvar_longR)
 
 #plot
 ggplot(myvar_longR) + geom_point(aes(x = X, y = Y, col = R),size = 3) + theme_bw() + scale_fill_gradient(na.value = 'red') + facet_wrap(~as.character(time))
 

#####################################################################
# J2K Configuration
#####################################################################

# -------------------------------------------------------------------
# Loading source project
# -------------------------------------------------------------------

# parameter file names
par_fn_landuse <- "landuse_e2.par"
par_fn_soil <- "soils_e2.par"
par_fn_hgeo <- "hgeo_e2.par"
par_fn_hru <- "hru_e2.par"
par_fn_reach <- "reach_e2.par"

# Project folder
j2kProjDir <- "~/JAMS/data/J2K_Loire/"
j2kProjOutDir <- j2kProjDir

# Base model file path
j2kprojectFile <- paste0(j2kProjDir, "J2K_Loire_diagnostic_base.jam")

# Output model file path
j2kprojectOutFile <- paste0(j2kProjOutDir, "J2K_Loire_restitution_snow.jam")


# read base model (or an already existing model created using J2K-RUI)
#rm('j2kjamproj')
j2kjamproj <- j2kLoadJAMSProject(j2kprojectFile)
j2kjamproj$datastores$TimeLoop <- NULL

# add sub-catchments
j2kjamproj <- j2kSetSubcatchments(j2kjamproj, my_catchments)
print(j2kjamproj)

# add all subcatchments to data store
names(j2kjamproj$datastores) <- c("J2K_LOIRE")
#j2kjamproj$datastores$J2K_LOIRE <- paste0("Area_", unique(c(855,sample(unique(my_catchments[,"id"]),29))))


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


# Explore 2
j2kjamproj <- j2kSetOutputParameters(j2kjamproj, misc=NULL, liter=NULL, area=FALSE)
j2kjamproj <- j2kSetOutputVariables(j2kjamproj,
                                    misc= c('precip', 'tmean', 'etref', 'etpot'),
                                    liter= c('rain','snow'),
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



                     
