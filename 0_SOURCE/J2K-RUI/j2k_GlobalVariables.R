# -------------------------------------------------------------------
# Required packages
# -------------------------------------------------------------------
# if (!require(XML)){
#     install.packages("XML")
#     if (!require(XML)){stop("the XML package is required")}
# }
# if (!require(ncdf4)){
#     install.packages("ncdf4")
#     if (!require(ncdf4)){stop("The ncdf4 package is required")}
# }
# if (!require(shapefiles)){ # FIX ME: only use rgdal library instead
#     install.packages("shapefiles")
#     if (!require(shapefiles)){stop("The shapefiles package is required")}
# }
# if (!require(rgdal)){
#     install.packages("rgdal")
#     if (!require(rgdal)){stop("The rgdal package is required")}
# }
# 
# if (!require(XML)){
#   install.packages("XML")
#   if (!require(XML)){stop("The XML package is required")}
# }
# if (!require(rgdal)){
#   install.packages("rgdal")
#   if (!require(rgdal)){stop("The rgdal package is required")}
# }
# if (!require(rgeos)){
#   install.packages("rgeos")
#   if (!require(rgeos)){stop("The rgeos package is required")}
# }
# if (!require(shiny)){
#   install.packages("shiny")
#   if (!require(shiny)){stop("The shiny package is required")}
# }

j2k.env = new.env(parent=emptyenv())

assign(".j2k.xmlprefix", '<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n', envir = j2k.env)


# prefix to all XML documents
# .j2k.xmlprefix = '<?xml version="1.0" encoding="UTF-8" standalone="no"?>\n'


assign(".j2k.modelName", "J2K", envir = j2k.env)
assign(".j2k.paramContext", "ParameterInput", envir = j2k.env)
assign(".j2k.initContext", "Initialization", envir = j2k.env)
assign(".j2k.timeContext", "TimeLoop", envir = j2k.env)
assign(".j2k.inputContext", "TSInput", envir = j2k.env)
assign(".j2k.hruContext", "HRULoop", envir = j2k.env)
assign(".j2k.reachContext", "ReachLoop", envir = j2k.env)
assign(".j2k.timeInterval", "timeInterval", envir = j2k.env)

# context names
# .j2k.modelName="J2K"
# .j2k.paramContext="ParameterInput"
# .j2k.initContext="Initialization"
# .j2k.timeContext="TimeLoop"
# .j2k.inputContext="TSInput"
# .j2k.hruContext="HRULoop"
# .j2k.reachContext="ReachLoop"
# .j2k.timeInterval="timeInterval"


# THE FOLLOWING 'global' variables has
# to be added to the objects that can be modified
# using a function such as the base options() or par() functions.

# outputdatastore related tags: -------------------------------------
assign(".j2k.tag.datastores", "datastores", envir = j2k.env)
assign(".j2k.tag.outputdatastore", "outputdatastore", envir = j2k.env)
assign(".j2k.tag.trace", "trace", envir = j2k.env)
# .j2k.tag.datastores = "datastores"
# .j2k.tag.outputdatastore = "outputdatastore"
# .j2k.tag.trace = "trace"

# init context and component names: ---------------------------------
assign(".j2k.context.InitWatershedsAreas", "InitWatershedsAreas", envir = j2k.env)
assign(".j2k.context.InitArea_", "InitArea_", envir = j2k.env)
assign(".j2k.context.InitWeight_", "InitWeight_", envir = j2k.env)
assign(".j2k.context.Init_", "Init_", envir = j2k.env)
assign(".j2k.component.AreaWeight_", "AreaWeight_", envir = j2k.env)
assign(".j2k.component.WeightedSumAggregator_", "WeightedSumAggregator_", envir = j2k.env)
# .j2k.context.InitWatershedsAreas = "InitWatershedsAreas"
# .j2k.context.InitArea_ = "InitArea_"
# .j2k.context.InitWeight_ = "InitWeight_"
# .j2k.context.Init_ = "Init_"
# .j2k.component.AreaWeight_ = "AreaWeight_"
# .j2k.component.WeightedSumAggregator_ = "WeightedSumAggregator_"
# .j2k.context.AggregatedDistribuedParameters = "AggregatedDistribuedParameters"
assign(".j2k.context.WatershedsDistributedParameters", "WatershedsDistributedParameters", envir = j2k.env)
assign(".j2k.context.DistributedPar_", "DistributedPar_", envir = j2k.env)
# .j2k.context.WatershedsDistributedParameters = "WatershedsDistributedParameters"
# .j2k.context.DistributedPar_= "DistributedPar_"

# input/state context and component names: --------------------------
assign(".j2k.context.WatershedsInputAndState", "WatershedsInputAndState", envir = j2k.env)
assign(".j2k.context.ISOvar_", "ISOvar_", envir = j2k.env)
assign(".j2k.component.WeightedSumAggregator_na_", "WeightedSumAggregator_na_", envir = j2k.env)
assign(".j2k.component.WeightedSumAggregator_L_", "WeightedSumAggregator_L_", envir = j2k.env)
assign(".j2k.component.WeightedSumAggregator.var.sum", "sum", envir = j2k.env)
# .j2k.context.WatershedsInputAndState = "WatershedsInputAndState"
# .j2k.context.ISOvar_ = "ISOvar_"
# .j2k.component.WeightedSumAggregator_na_ = "WeightedSumAggregator_na_"
# .j2k.component.WeightedSumAggregator_L_ = "WeightedSumAggregator_L_"
# .j2k.component.WeightedSumAggregator.var.sum = "sum"

# reach context and component names: --------------------------------
assign(".j2k.context.ReachesOutputs", "ReachesOutputs", envir = j2k.env)
# .j2k.context.ReachesOutputs = "ReachesOutputs"

# TSinput context: --------------------------------------------------
assign(".j2k.component.Dam_DataReader", "Dam_DataReader", envir = j2k.env)
assign(".j2k.component.Temp_DataReader", "Temp_DataReader", envir = j2k.env)
assign(".j2k.component.Precip_DataReader", "Precip_DataReader", envir = j2k.env)
assign(".j2k.component.ETRef_DataReader", "ETRef_DataReader", envir = j2k.env)
assign(".j2k.component.DataReader.id", "id", envir = j2k.env)
# .j2k.component.Dam_DataReader = "Dam_DataReader"
# .j2k.component.Temp_DataReader = "Temp_DataReader"
# .j2k.component.Precip_DataReader = "Precip_DataReader"
# .j2k.component.ETRef_DataReader = "ETRef_DataReader"
# .j2k.component.DataReader.id = "id"

# parameterInput context: -------------------------------------------
assign(".j2k.component.StandardEntityReader", "StandardEntityReader", envir = j2k.env)
assign(".j2k.component.StandardEntityReader.class", "org.unijena.j2k.io.StandardEntityReader", envir = j2k.env)
assign(".j2k.component.StandardEntityReader.reachFileName", "reachFileName", envir = j2k.env)
assign(".j2k.component.StandardEntityReader.hruFileName", "hruFileName", envir = j2k.env)

assign(".j2k.component.StandardLUReader", "StandardLUReader", envir = j2k.env)
assign(".j2k.component.StandardLUReader.class", "org.unijena.j2k.io.StandardLUReader", envir = j2k.env)
assign(".j2k.component.StandardLUReader.luFileName", "luFileName", envir = j2k.env)

assign(".j2k.component.StandardSoilParaReader", "StandardSoilParaReader", envir = j2k.env)
assign(".j2k.component.StandardSoilParaReader.class", "org.unijena.j2k.io.StandardSoilParaReader", envir = j2k.env)
assign(".j2k.component.StandardSoilParaReader.stFileName", "stFileName", envir = j2k.env)

assign(".j2k.component.StandardGroundwaterParaReader", "StandardGroundwaterParaReader", envir = j2k.env)
assign(".j2k.component.StandardGroundwaterParaReader.class", "org.unijena.j2k.io.StandardGroundwaterParaReader", envir = j2k.env)
assign(".j2k.component.StandardGroundwaterParaReader.gwFileName", "gwFileName", envir = j2k.env)

# .j2k.component.StandardEntityReader = "StandardEntityReader"
# .j2k.component.StandardEntityReader.class = "org.unijena.j2k.io.StandardEntityReader"
# .j2k.component.StandardEntityReader.reachFileName = "reachFileName"
# .j2k.component.StandardEntityReader.hruFileName = "hruFileName"

# .j2k.component.StandardLUReader = "StandardLUReader"
# .j2k.component.StandardLUReader.class = "org.unijena.j2k.io.StandardLUReader"
# .j2k.component.StandardLUReader.luFileName = "luFileName"

# .j2k.component.StandardSoilParaReader = "StandardSoilParaReader"
# .j2k.component.StandardSoilParaReader.class = "org.unijena.j2k.io.StandardSoilParaReader"
# .j2k.component.StandardSoilParaReader.stFileName = "stFileName"

# .j2k.component.StandardGroundwaterParaReader = "StandardGroundwaterParaReader"
# .j2k.component.StandardGroundwaterParaReader.class = "org.unijena.j2k.io.StandardGroundwaterParaReader"
# .j2k.component.StandardGroundwaterParaReader.gwFileName = "gwFileName"

# time context components: ------------------------------------------
assign(".j2k.component.doubleSetter", "DoubleSetter", envir = j2k.env)
# .j2k.component.doubleSetter = "DoubleSetter"

# constant for variable names in J2K model: -------------------------
# FIX ME: the names below are not consistent with how the other variables (above) are named
assign(".j2k.variable.hru.name", "HRUS", envir = j2k.env)
assign(".j2k.variable.hru.area", "area", envir = j2k.env)
assign(".j2k.variable.hru.watershed", "watershed", envir = j2k.env)
assign(".j2k.variable.hru.ID", "ID", envir = j2k.env)
assign(".j2k.variable.prefix.area", "Area_", envir = j2k.env)
assign(".j2k.variable.prefix.areaweight", "AreaWeight_", envir = j2k.env)
# .j2k.variable.hru.name = "HRUS"
# .j2k.variable.hru.area = "area"
# .j2k.variable.hru.watershed = "watershed"
# .j2k.variable.hru.ID = "ID"
# .j2k.variable.prefix.area = "Area_"
# .j2k.variable.prefix.areaweight = "AreaWeight_"


# constant for GIS layers (attribute table fields): -----------------
assign(".j2k.gis.whatershed", "watershed", envir = j2k.env)
assign(".j2k.gis.hruid", "value", envir = j2k.env)
# .j2k.gis.whatershed = "watershed"
# .j2k.gis.hruid = "value"


# time series datastore files (XML): --------------------------------
assign(".j2k.timeSeriesDataStore", "j2ktsdatastore", envir = j2k.env)
assign(".j2k.timeSeriesDataStore.parsetime", "parsetime", envir = j2k.env)
assign(".j2k.timeSeriesDataStore.timeformat", "dumptimeformat", envir = j2k.env)
assign(".j2k.timeSeriesDataStore.timeformat.code", "yyyy-MM-dd HH:mm", envir = j2k.env)  # unclear... not consistent with .dat files... "dd.MM.yyyy HH:mm"?
# .j2k.timeSeriesDataStore = "j2ktsdatastore"
# .j2k.timeSeriesDataStore.parsetime = "parsetime"
# .j2k.timeSeriesDataStore.timeformat = "dumptimeformat"
# .j2k.timeSeriesDataStore.timeformat.code = "yyyy-MM-dd HH:mm"

# markdown in .dat file: --------------------------------------------
assign(".j2k.dataValueAttribs", "dataValueAttribs", envir = j2k.env)
assign(".j2k.dataSetAttribs", "dataSetAttribs", envir = j2k.env)
assign(".j2k.dataSetAttribs.missingDataVal", "missingDataVal", envir = j2k.env)
assign(".j2k.dataSetAttribs.dataStart", "dataStart", envir = j2k.env)
assign(".j2k.dataSetAttribs.dataEnd", "dataEnd", envir = j2k.env)
assign(".j2k.dataSetAttribs.timeRes", "tres", envir = j2k.env)
# .j2k.dataValueAttribs = "dataValueAttribs"
# .j2k.dataSetAttribs = "dataSetAttribs"
# .j2k.dataSetAttribs.missingDataVal = "missingDataVal"
# .j2k.dataSetAttribs.dataStart = "dataStart"
# .j2k.dataSetAttribs.dataEnd = "dataEnd"
# .j2k.dataSetAttribs.timeRes = "tres"

assign(".j2k.statAttribVal", "statAttribVal", envir = j2k.env)
assign(".j2k.statAttribVal.name", "name", envir = j2k.env)
assign(".j2k.statAttribVal.id", "id", envir = j2k.env)
assign(".j2k.statAttribVal.elevation", "elevation", envir = j2k.env)
assign(".j2k.statAttribVal.x", "x", envir = j2k.env)
assign(".j2k.statAttribVal.y", "y", envir = j2k.env)
assign(".j2k.statAttribVal.dataColumn", "datacolumn", envir = j2k.env)
# .j2k.statAttribVal = "statAttribVal"
# .j2k.statAttribVal.name = "name"
# .j2k.statAttribVal.id = "id"
# .j2k.statAttribVal.elevation = "elevation"
# .j2k.statAttribVal.x = "x"
# .j2k.statAttribVal.y = "y"
# .j2k.statAttribVal.dataColumn = "datacolumn"

assign(".j2k.dataVal", "dataVal", envir = j2k.env)
assign(".j2k.dataSec", "data", envir = j2k.env)
# .j2k.dataVal = "dataVal"
# .j2k.dataSec = "data"

assign(".j2k.output.sectionChar", "@", envir = j2k.env)
assign(".j2k.output.context", "context", envir = j2k.env)
assign(".j2k.output.attributes", "attributes", envir = j2k.env)
assign(".j2k.output.data", "data", envir = j2k.env)
# .j2k.output.sectionChar = "@"
# .j2k.output.context = "context"
# .j2k.output.attributes = "attributes"
# .j2k.output.data = "data"

assign(".j2k.outputs.attributes", "attributes", envir = j2k.env)
assign(".j2k.outputs.data", "start", envir = j2k.env) # the @data section is empty, the data is actually within the @start section (which may be an embedded section of @data... but this is unclear.)
# .j2k.outputs.attributes = "attributes"
# .j2k.outputs.data = "start" 

assign(".j2k.inputs.data", "dataVal", envir = j2k.env)
assign(".j2k.inputs.valueAttributes", "dataValueAttribs", envir = j2k.env)
assign(".j2k.inputs.setAttributes", "dataSetAttribs", envir = j2k.env)
assign(".j2k.inputs.stateAttributes", "statAttribVal", envir = j2k.env)
# .j2k.inputs.data = "dataVal"
# .j2k.inputs.valueAttributes = "dataValueAttribs"
# .j2k.inputs.setAttributes = "dataSetAttribs"
# .j2k.inputs.stateAttributes = "statAttribVal"

assign(".j2k.sectionChar", "@", envir = j2k.env)
assign(".j2k.commentChar", "#", envir = j2k.env)
assign(".j2k.nacode", "-9999", envir = j2k.env)
# .j2k.sectionChar = "@"
# .j2k.commentChar = "#"
# .j2k.nacode = "-9999"

# useless so far
assign(".j2k.parameters.alias", list(
  c("LAI", "LAI_aAF", "LAI_mAF"),
  c("Kc", "CropCoef", "CropCoef_aAF", "CropCoef_mAF"),
  c("sealedGrade", "sealedGrade_aAF", "sealedGrade_mAF"),
  c("MaxInfSummer", "soilMaxInfSummer", "soilMaxInfSummer_aAF", "soilMaxInfSummer_mAF"),
  c("MaxInfWinter", "soilMaxInfWinter", "soilMaxInfWinter_aAF", "soilMaxInfWinter_mAF"),
  c("MaxInfSnow", "soilMaxInfSnow", "soilMaxInfSnow_aAF", "soilMaxInfSnow_mAF"),
  c("maxMPS", "maxMPS_aAF", "maxMPS_mAF"),
  c("maxLPS", "maxLPS_aAF", "maxLPS_mAF"),
  c("kRG1", "RG1_k", "kRG1_aAF", "kRG1_mAF"),
  c("RG1_max", "RG1_max_aAF", "RG1_max_mAF"),
  c("roughness", "rough", "roughness_aAF", "roughness_mAF"),
  c("width",  "width_aAF", "width_mAF")
), envir = j2k.env)