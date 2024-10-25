###############################################################################
#
# J2000 R user interface (J2K-RUI)
#
# -----------------------------------------------------------------------------
#
# A set of functions to set up a J2000 model from HRU-Delin results, create
# sub models, read results, format SAFRAN data, etc.
#
###############################################################################
# -----------------------------------------------------------------------------
#
# -----------------------------------------------------------------------------
# Author: Ivan Horner (ivan.horner@irstea.fr), Inrae
# -----------------------------------------------------------------------------
# Last modified: 2020-02-14
# -----------------------------------------------------------------------------
###############################################################################


# J2K-RUI main file: it only loads all the R Script of J2K-RUI (to make life easier as long as no J2KRUI package exists)
j2kRUIfolder <- getSrcDirectory(function(x) x)
j2kRUIfolder <- paste0(j2kRUIfolder, "/")

message("Sourcing J2K-RUI from folder: \n ", j2kRUIfolder); flush.console()

message(" --> Sourcing J2K_Optas.R"); flush.console()
source(paste0(j2kRUIfolder, "J2K_Optas.R"))

message(" --> Sourcing j2kproject.R"); flush.console()
source(paste0(j2kRUIfolder, "j2kproject.R"))

message(" --> Sourcing j2ksubcatch.R"); flush.console()
source(paste0(j2kRUIfolder, "j2ksubcatch.R"))

message(" --> Sourcing j2kvaroutpar.R"); flush.console()
source(paste0(j2kRUIfolder, "j2kvaroutpar.R"))

message(" --> Sourcing J2K_Editing.R"); flush.console()
source(paste0(j2kRUIfolder, "J2K_Editing.R"))

message(" --> Sourcing J2K_InOutPar.R"); flush.console()
source(paste0(j2kRUIfolder, "J2K_InOutPar.R"))

message(" --> Sourcing j2k_GlobalVariables.R"); flush.console()
source(paste0(j2kRUIfolder, "j2k_GlobalVariables.R"))

message(" --> Sourcing J2K_Subcatchments.R"); flush.console()
source(paste0(j2kRUIfolder, "J2K_Subcatchments.R"))

message(" --> Sourcing J2K_Components.R"); flush.console()
source(paste0(j2kRUIfolder, "J2K_Components.R"))

message(" --> Sourcing J2K_HRUDelin.R"); flush.console()
source(paste0(j2kRUIfolder, "J2K_HRUDelin.R"))

message("Loading J2K-RUI's dependencies."); flush.console()
library(XML)
library(ncdf4)
library(rgdal)
library(rgeos)
library(shiny)
