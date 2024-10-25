#####################################################################
### ------------------------------------------------------------- ###
### Functions to edit the project XML file of J2K model created   ###
### under the JAMS builder interface.                             ###
### These function are intended to make it easy to add sub-       ###
### catchment into the J2K project and to easily edit wanted      ###
### outputs of simulations.                                       ###
###                                                               ###
### ------------------------------------------------------------- ###
### Author: Ivan Horner, Irstea (ivan.horner@irstea.fr)           ###
### Created: 2017-03-29                                           ###
### Updated: 2017-07-24                                           ###
### ------------------------------------------------------------- ###
#####################################################################

# -------------------------------------------------------------------
# Required packages
# -------------------------------------------------------------------
# XML

# Below is the supposed layout of a JAMS J2K XML project:
#	 >Model
#	   >ParameterInput
#	   >Initialization
#	   >TimeLoope
#	     >TSInput
#	     >HRULoop
#	     >ReachLoop
# adding components and context within a JAMS project require to have an
# a priori layout of some key context (those listed above).
# If there is any change in this layout, the following function will be affected:
#  - j2kSetContextsNames() # and all the .j2k.*Context variables with their default values at the end of the document
#  - j2kInitWatershedsAreas()
#  - j2kWatershedInputAndState()
#  - j2kReachesOutputs()
#  - j2kAddVariablesInOutputs() <-- check if it is the case for that one.


# FIX ME: -----------------------------------------------------------
# arguments should be .dots. , many more argument should be modify-able
# actually, all the variables in j2k.env should be readable and modifiable
# -------------------------------------------------------------------
# (Re)set the names of contexts stored in .j2k.*Context and model
# name stored in .j2k.modelName
# -------------------------------------------------------------------
# I. Horner / 2017-07-24 / 2018-06-25
# -------------------------------------------------------------------
j2kSetContextsNames = function(modelName=NULL, paramContext=NULL, initContext=NULL, 
	timeContext=NULL, inputContext=NULL, hruContext=NULL, reachContext=NULL){
	if (!is.null(modelName)){assign(".j2k.modelName", modelName, pos=j2k.env)}
	if (!is.null(paramContext)){assign(".j2k.paramContext", paramContext, pos=j2k.env)}
	if (!is.null(initContext)){assign(".j2k.initContext", initContext, pos=j2k.env)}
	if (!is.null(timeContext)){assign(".j2k.timeContext", timeContext, pos=j2k.env)}
	if (!is.null(inputContext)){assign(".j2k.inputContext", inputContext, pos=j2k.env)}
	if (!is.null(hruContext)){assign(".j2k.hruContext", hruContext, pos=j2k.env)}
	if (!is.null(reachContext)){assign(".j2k.reachContext", reachContext, pos=j2k.env)}
}


# -------------------------------------------------------------------
# In 'Initialization', add the two contexts to compute the total area
# of subcatchments (each corresponding to a station) as well as the 
# weight of the area in the total area of the whole catchment
# => It adds the corresponding contexts in the XML J2K project that is 
# then returned.
# -------------------------------------------------------------------
# I. Horner / 2017-03-31 / 2017-08-04
# -------------------------------------------------------------------
.j2kInitWatershedsAreas = function(j2kproject, stationIDs, watershedIDs){
	children = list()
	for (k in 1:length(stationIDs)){
		children[[k]] = .j2kWatershedAreaCalculators(stationID=stationIDs[k] , watershedID=watershedIDs[[k]])
	}
	context = .j2kContext(name = get(".j2k.context.InitWatershedsAreas", envir=j2k.env), CHILDREN=children)
	# then, this must be added just before the TimeLoop Context
	# see supposed layout at the begining of this function script
	return(context)
}

# -------------------------------------------------------------------
# Between the HRUloop and ReachLoop contexts, two sumAggregator are
# added for each station in order to aggregate state variables either
# with liter units or other units.
# => It adds a context containing all the components for all station 
# in between the HRULoop and ReachLoops context in the XML project
# that is then returned.
# -------------------------------------------------------------------
# I. Horner / 2017-03-31 / 2017-11-27 / 2018-10-12
# -------------------------------------------------------------------
.j2kWatershedInputAndState = function(j2kproject, stationIDs, watershedIDs, variable_na, variable_L){
	children = list()
	for (k in 1:length(stationIDs)){
        children[[k]] = .j2kWatershedVariableAggregator(stationID=stationIDs[k], watershedID=watershedIDs[[k]], suffix="ISO",
            contextPrefix=get(".j2k.context.ISOvar_", envir=j2k.env), weightContext=get(".j2k.modelName", envir=j2k.env), 
            TargetContext=get(".j2k.timeContext", envir=j2k.env), values_na=variable_na, values_L=variable_L)
	}
	context = .j2kContext(name = get(".j2k.context.WatershedsInputAndState", envir=j2k.env), CHILDREN=children)
	# then, this must be added just before the ReachLoop Context and after the HRULoop Context
	# see supposed layout at the begining of this function script
	return(context)
}

# -------------------------------------------------------------------
# At the end of the initialization context, two sumAggregator are
# added for each station in order to aggregate distributed parameters either
# with liter units or other units.
# => It adds a context containing all the components for all station 
# in between the HRULoop and ReachLoops context in the XML project
# that is then returned.
# -------------------------------------------------------------------
# I. Horner / 2017-03-31 / 2017-11-27
# -------------------------------------------------------------------
.j2kWatershedDistributedParameters = function(stationIDs, watershedIDs, variable_na, variable_L){
	children = list()
	for (k in 1:length(stationIDs)){
        children[[k]] = .j2kWatershedVariableAggregator(stationID=stationIDs[k], watershedID=watershedIDs[[k]], suffix="DP", 
            contextPrefix=get(".j2k.context.DistributedPar_", envir=j2k.env), weightContext=get(".j2k.modelName", envir=j2k.env),
            TargetContext=get(".j2k.modelName", envir=j2k.env), values_na=variable_na, values_L=variable_L)
	}
	context = .j2kContext(name = get(".j2k.context.WatershedsDistributedParameters", envir=j2k.env), CHILDREN=children)
	# then, this must be added just at then of the Initialization context
	# see supposed layout at the begining of this function script
	return(context)
}

# -------------------------------------------------------------------
# In 'ReachLoop', add the contexts that compute the outputs of each
# subcatchments
# => It adds the corresponding contexts in the XML J2K project that is 
# then returned.
# -------------------------------------------------------------------
# I. Horner / 2017-07-24 / 2017-10-25
# -------------------------------------------------------------------
.j2kReachesOutputs = function(j2kproject, stationID, reachID, variable_reach, unit_mm=FALSE){
	children = list()
	if (length(variable_reach)>0) {
  	for (k in 1:length(variable_reach)){
  		children[[k]] = .j2kReachOutputs(stationID=stationID, reachID=reachID, outputVar=variable_reach[k],
  			reachIDattribute="ID", ReachLoopContext=get(".j2k.reachContext", envir=j2k.env), TimeLoopContext=get(".j2k.timeContext", envir=j2k.env), weight=unit_mm)
  	}
	}
	context = .j2kContext(name = get(".j2k.context.ReachesOutputs", envir=j2k.env), CHILDREN=children)
	# then, this must be added at the end of the reach context
	# see supposed layout at the begining of this function script
	return(context)
}

# -------------------------------------------------------------------
# For a given station 'stationID', create two spatialFilteredContext
# where (1) the subcatchment area is computed and (2) its corresponding
# weight in the total catchment area is also computed.
# => It returns a simple context node containing two spatialFilterdContext
# with the necessary component within
# -------------------------------------------------------------------
# I. Horner / 2017-03-31 / 2017-07-24
# -------------------------------------------------------------------
.j2kWatershedAreaCalculators = function(stationID, watershedID){
	# context names
	context_name_area = paste0(get(".j2k.context.InitArea_", envir=j2k.env), stationID)
	context_name_weight = paste0(get(".j2k.context.InitWeight_", envir=j2k.env), stationID)
	# SumAggregator to compute total area of watershed
	component_name_area = paste0(get(".j2k.component.WeightedSumAggregator_", envir=j2k.env), stationID)
	variable_name_area = paste0(get(".j2k.variable.prefix.area", envir=j2k.env), stationID)
	value = list(attribute=get(".j2k.variable.hru.area", envir=j2k.env), context=context_name_area)
	sum = list(attribute=variable_name_area, context=get(".j2k.modelName", envir=j2k.env))	
	n1 = .j2kComponentWeightedSumAggregator(name=component_name_area, value=value, weight=NULL, sum=sum, divideByWeight=NULL)
	# Area weight calculator to compute area weight of each HRU in regard to total watershed area
	component_name_weight = paste0(get(".j2k.component.AreaWeight_", envir=j2k.env), stationID)
	variable_name_weight = paste0(get(".j2k.variable.prefix.areaweight", envir=j2k.env), stationID)
	entityArea = list(attribute=get(".j2k.variable.hru.area", envir=j2k.env), context=context_name_weight)
	catchmentArea = list(attribute=variable_name_area, context=get(".j2k.modelName", envir=j2k.env))
	areaWeight = list(attribute=variable_name_weight, context=context_name_weight)
	n2 = .j2kComponentAreaWeight(name=component_name_weight, entityArea=entityArea, catchmentArea=catchmentArea, areaWeight=areaWeight)
	# setup of filtered spatial contexts
	attributeName = list(value=get(".j2k.variable.hru.watershed", envir=j2k.env))
	attributeValues = list(value=paste(watershedID, collapse=";"))
	entities = list(attribute=get(".j2k.variable.hru.name", envir=j2k.env), context=get(".j2k.modelName", envir=j2k.env))
	c1 = .j2kContextFilteredSpatial(name=context_name_area, attributeName=attributeName, attributeValues=attributeValues,
		attributeValuesAlternative=NULL, entities=entities, current=NULL, CHILDREN=list(n1))
	c2 = .j2kContextFilteredSpatial(name=context_name_weight, attributeName=attributeName, attributeValues=attributeValues,
		attributeValuesAlternative=NULL, entities=entities, current=NULL, CHILDREN=list(n2))
	# add it to a general context
	context_name = paste0(get(".j2k.context.Init_", envir=j2k.env), stationID)
	context = .j2kContext(name = context_name, CHILDREN=list(c1, c2))
	return(context)
}

# -------------------------------------------------------------------
# OBSOLETE: to be removed?, the function '.j2kWatershedAreaCalculators'
# seems to have replaced this one... As it is more general.
# -------------------------------------------------------------------
# For a given station 'stationID', create two weightedSumAggregator
# component to compute the state variables
# => It returns a SpatialFilteredContext node containing two 
# weightedSumAggregator components within
# -------------------------------------------------------------------
# I. Horner / 2017-03-31 / 2017-11-28
# -------------------------------------------------------------------
.j2kWatershedInputAndState_old = function(stationID, watershedID, JAMSModel, TimeLoopContext, values_na, values_L){
	# context names
	context_name = paste0(get(".j2k.context.ISOvar_", envir=j2k.env), stationID)
	values_L_out = paste0(values_L, "_", stationID)
	values_na_out = paste0(values_na, "_", stationID)
	# miscellaneous -------------------------------------------------
	# SumAggregator to aggregate variables according to weight of the current HRU in the total sub-catchment area (gives result in original unit)
	component_name_na = paste0(get(".j2k.component.WeightedSumAggregator_na_", envir=j2k.env), stationID)
	value = list(attribute=paste(values_na, collapse=";"), context=context_name)
	weight = list(attribute=paste0(get(".j2k.variable.prefix.areaweight", envir=j2k.env), stationID), context=context_name)
	sum = list(attribute=paste(values_na_out, collapse=";"), context=TimeLoopContext)	
	n1 = .j2kComponentWeightedSumAggregator(name=component_name_na, value=value, weight=weight, sum=sum, divideByWeight=NULL)
	# liter ---------------------------------------------------------
	# SumAggregator to aggregate variables according to the total sub-catchment area (gives result in mm as it is divided by a surface)
	component_name_L = paste0(get(".j2k.component.WeightedSumAggregator_L_", envir=j2k.env), stationID)
	value = list(attribute=paste(values_L, collapse=";"), context=context_name)
	weight = list(attribute=paste0(get(".j2k.variable.prefix.area", envir=j2k.env), stationID), context=JAMSModel)
	sum = list(attribute=paste(values_L_out, collapse=";"), context=TimeLoopContext)
	n2 = .j2kComponentWeightedSumAggregator(name=component_name_L, value=value, weight=weight, sum=sum, divideByWeight=NULL)
	# Filtered spatial contexts
	attributeName = list(value=get(".j2k.variable.hru.watershed", envir=j2k.env))
	attributeValues = list(value=paste(watershedID, collapse=";"))
	entities = list(attribute=get(".j2k.variable.hru.name", envir=j2k.env), context=JAMSModel)
	context = .j2kContextFilteredSpatial(name=context_name, attributeName=attributeName, attributeValues=attributeValues,
		attributeValuesAlternative=NULL, entities=entities, current=NULL, CHILDREN=list(n1, n2))
	return(context)
}

.j2kWatershedVariableAggregator = function(stationID, watershedID, suffix, contextPrefix, weightContext, TargetContext, values_na, values_L){
	# context names
	# context_name = paste0(.j2k.context.ISOvar_, stationID)
	context_name = paste0(contextPrefix, stationID)
	CHILDREN = list()
	# miscellaneous -------------------------------------------------
	# SumAggregator to aggregate variables according to weight of the current HRU in the total sub-catchment area (gives result in original unit)
	if (!is.null(values_na)) {
	values_na_out = paste0(values_na, "_", stationID)
	component_name_na = paste0(get(".j2k.component.WeightedSumAggregator_na_", envir=j2k.env), stationID, "_", suffix)
	value = list(attribute=paste(values_na, collapse=";"), context=context_name)
	weight = list(attribute=paste0(get(".j2k.variable.prefix.areaweight", envir=j2k.env), stationID), context=context_name)
	sum = list(attribute=paste(values_na_out, collapse=";"), context=TargetContext)	
	n1 = .j2kComponentWeightedSumAggregator(name=component_name_na, value=value, weight=weight, sum=sum, divideByWeight=NULL)
	CHILDREN[[length(CHILDREN)+1]] = n1
	}
	# liter ---------------------------------------------------------
	# SumAggregator to aggregate variables according to the total sub-catchment area (gives result in mm as it is divided by a surface)
	if (!is.null(values_L)) {
	values_L_out = paste0(values_L, "_", stationID)
	component_name_L = paste0(get(".j2k.component.WeightedSumAggregator_L_", envir=j2k.env), stationID, "_", suffix)
	value = list(attribute=paste(values_L, collapse=";"), context=context_name)
	weight = list(attribute=paste0(get(".j2k.variable.prefix.area", envir=j2k.env), stationID), context=weightContext)
	sum = list(attribute=paste(values_L_out, collapse=";"), context=TargetContext)
	n2 = .j2kComponentWeightedSumAggregator(name=component_name_L, value=value, weight=weight, sum=sum, divideByWeight=NULL)
	CHILDREN[[length(CHILDREN)+1]] = n2
	}
	# Filtered spatial contexts
	attributeName = list(value=get(".j2k.variable.hru.watershed", envir=j2k.env))
	attributeValues = list(value=paste(watershedID, collapse=";"))
	entities = list(attribute=get(".j2k.variable.hru.name", envir=j2k.env), context=weightContext)
	context = .j2kContextFilteredSpatial(name=context_name, attributeName=attributeName, attributeValues=attributeValues,
		attributeValuesAlternative=NULL, entities=entities, current=NULL, CHILDREN=CHILDREN)
	return(context)
}


# -------------------------------------------------------------------
# For a given output variable 'outputVar' station create a switchContext
# containing a weightedSumAggregator for each station in 'stationID' and
# their corresponding reaches 'reachID' which only store the variable
# value of the current time step within the time loop context
# => It returns a SwitchContext node containing weightedSumAggregator,
# one for each station components
# -------------------------------------------------------------------
# I. Horner / 2017-07-24 / 2017-08-04
# -------------------------------------------------------------------
.j2kReachOutputs = function(stationID, reachID, outputVar, reachIDattribute="ID", ReachLoopContext, TimeLoopContext, weight=FALSE){
	# context names:
	context_name = outputVar
	# weigthedSumAggregator to store variable in the time loop context
	children = list()
	for (k in 1:length(reachID)){
		component_name = paste0(outputVar, "_", stationID[k], "_", reachID[k])
		variable_name =  paste0(outputVar, "_", stationID[k])
		weight_name = NULL #  paste0(.j2k.variable.prefix.area,  stationID[k])
		if (weight){
			weight_name = list(attribute=paste0(get(".j2k.variable.prefix.area", envir=j2k.env), stationID[k]), context=get(".j2k.modelName", envir=j2k.env))
		}
		value = list(attribute=outputVar, context=ReachLoopContext)
		sum = list(attribute=variable_name, context=TimeLoopContext)	
		children[[k]] = .j2kComponentWeightedSumAggregator(name=component_name, value=value, weight=weight_name, sum=sum, divideByWeight=NULL)
	}
	# Switch context
	attribute = list(attribute=reachIDattribute, context=ReachLoopContext)
	values = list(value=paste(reachID, collapse=";"))
	context = .j2kContextSwitch(name=context_name, attribute=attribute, values=values, CHILDREN=children)
	return(context)
}


# TO COMMENT AND TEST
.j2kDataStores = function(variables, datastorecontexts) {
	n = length(variables) # get the number of datastores
	if (n>0) {
  	children = list()
  	for (k in 1:n){ # for each datastores
  		children[[k]] = .j2kOutputDataStore(context=datastorecontexts[k], name=datastorecontexts[k], attribute_ids=variables[[k]])
  	}
  	ds = xmlNode(get(".j2k.tag.datastores", envir=j2k.env), .children=children)
	} else {
	  ds = xmlNode(get(".j2k.tag.datastores", envir=j2k.env))
	}
	return(ds)
}



j2kGetModelParametersValues = function(j2kproj) {
  .j2kGetNodeVar = function(node) {
    results = list()
    cc_name = xmlAttrs(node)["name"]
    i_var = which(names(node)=="var")
    for (j in i_var) {
      xmlAttrs(node[[j]])["name"]
      xmlAttrs(node[[j]])["context"]
      xmlAttrs(node[[j]])["attribute"]
      if (!is.na(xmlAttrs(node[[j]])["value"])) {
        results[[length(results) + 1]] = list("cc_name"=cc_name,
                                              "var_name"=xmlAttrs(node[[j]])["name"],
                                              "var_value"=xmlAttrs(node[[j]])["value"])
      }
    }
    i_context = which(names(node)=="contextcomponent")
    for (j in i_context) {
      results = c(results, .j2kGetNodeVar(node[[j]]))
    }
    i_component = which(names(node)=="component")
    for (j in i_component) {
      results = c(results, .j2kGetNodeVar(node[[j]]))
    }
    return(results)
  }
  return(do.call(rbind, .j2kGetNodeVar(j2kproj$XML)))
}



if (FALSE) {
message("This function should be deleted. It is an outdated duplicate!")
.j2kWatershedVariableAggregator = function(stationID, watershedID, suffix, contextPrefix, weightContext, TargetContext, values_na, values_L){
  # context names
  # context_name = paste0(.j2k.context.ISOvar_, stationID)
  context_name = paste0(contextPrefix, stationID)
  values_L_out = paste0(values_L, "_", stationID)
  values_na_out = paste0(values_na, "_", stationID)
  # miscellaneous -------------------------------------------------
  # SumAggregator to aggregate variables according to weight of the current HRU in the total sub-catchment area (gives result in original unit)
  component_name_na = paste0(get(".j2k.component.WeightedSumAggregator_na_", envir=j2k.env), stationID, "_", suffix)
  value = list(attribute=paste(values_na, collapse=";"), context=context_name)
  weight = list(attribute=paste0(get(".j2k.variable.prefix.areaweight", envir=j2k.env), stationID), context=context_name)
  sum = list(attribute=paste(values_na_out, collapse=";"), context=TargetContext)	
  n1 = .j2kComponentWeightedSumAggregator(name=component_name_na, value=value, weight=weight, sum=sum, divideByWeight=NULL)
  # liter ---------------------------------------------------------
  # SumAggregator to aggregate variables according to the total sub-catchment area (gives result in mm as it is divided by a surface)
  component_name_L = paste0(get(".j2k.component.WeightedSumAggregator_L_", envir=j2k.env), stationID, "_", suffix)
  value = list(attribute=paste(values_L, collapse=";"), context=context_name)
  weight = list(attribute=paste0(get(".j2k.variable.prefix.area", envir=j2k.env), stationID), context=weightContext)
  sum = list(attribute=paste(values_L_out, collapse=";"), context=TargetContext)
  n2 = .j2kComponentWeightedSumAggregator(name=component_name_L, value=value, weight=weight, sum=sum, divideByWeight=NULL)
  # Filtered spatial contexts
  attributeName = list(value=get(".j2k.variable.hru.watershed", envir=j2k.env))
  attributeValues = list(value=paste(watershedID, collapse=";"))
  entities = list(attribute=get(".j2k.variable.hru.name", envir=j2k.env), context=weightContext)
  context = .j2kContextFilteredSpatial(name=context_name, attributeName=attributeName, attributeValues=attributeValues,
                                       attributeValuesAlternative=NULL, entities=entities, current=NULL, CHILDREN=list(n1, n2))
  return(context)
}

}


