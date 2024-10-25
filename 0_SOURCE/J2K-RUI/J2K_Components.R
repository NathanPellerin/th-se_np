# FIX ME: some class names of JAMS are hard-coded! Should not be the case

#####################################################################
### ------------------------------------------------------------- ###
### Context component node                                        ###
### ------------------------------------------------------------- ###
### Last updated: 2017-03-31                                      ###
### ------------------------------------------------------------- ###
### IN:                                                           ###
###    - name:          component name                            ###
###    - CHILDREN:      children nodes                            ###
### OUT:                                                          ###
###    contextcomponent node "<contextcomponent ...> ...          ###
###    </contextcomponent>"                                       ###
### ------------------------------------------------------------- ###
### Required: XML package                                         ###
#####################################################################
.j2kContext = function(name, CHILDREN=NULL){
    xmln = xmlNode("contextcomponent", attrs=c("class"="jams.components.core.Context", enabled="true", name=name, version="1.0_0"))
    for (k in CHILDREN){
        xmln = addChildren(xmln, k)
    }
    return(xmln)
}


#####################################################################
### ------------------------------------------------------------- ###
### FilteredSpatialContext context component node                 ###
### ------------------------------------------------------------- ###
### Last updated: 2017-03-31                                      ###
### ------------------------------------------------------------- ###
### IN:                                                           ###
###    - name:			component name                            ###
###    - attributeName:	"Double attribute to filter"              ###
###    - attributeValues:	                                      ###
###						"Attribute values to match"               ###
###    - attributeValuesAlternative:	                          ###
###						"Attribute values to match"               ###
###    - entities:		"List of spatial entities"                ###
###    - current:		"Current entity"                          ###
###    - CHILDREN:      children nodes                            ###
### OUT:                                                          ###
###    contextcomponent node "<contextcomponent ...> ...          ###
###    </contextcomponent>"                                       ###
### ------------------------------------------------------------- ###
### Required: XML package                                         ###
#####################################################################
.j2kContextFilteredSpatial = function(name, attributeName, attributeValues, attributeValuesAlternative, entities, current, CHILDREN=NULL){
	xmln = xmlNode("contextcomponent", attrs=c("class"="jams.components.conditional.FilteredSpatialContext", enabled="true", name=name, version="1.2_0"))
	xmln = addChildren(xmln, .j2kVar(name="attributeName", attribute=attributeName$attribute, context=attributeName$context, value=attributeName$value))
	xmln = addChildren(xmln, .j2kVar(name="attributeValues", attribute=attributeValues$attribute, context=attributeValues$context, value=attributeValues$value))
	xmln = addChildren(xmln, .j2kVar(name="attributeValuesAlternative", attribute=attributeValuesAlternative$attribute,
		context=attributeValuesAlternative$context, value=attributeValuesAlternative$value))
	xmln = addChildren(xmln, .j2kVar(name="entities", attribute=entities$attribute, context=entities$context, value=entities$value))
	xmln = addChildren(xmln, .j2kVar(name="current", attribute=current$attribute, context=current$context, value=current$value))
	for (k in CHILDREN){
		xmln = addChildren(xmln, k)
	}
	return(xmln)
}

#####################################################################
### ------------------------------------------------------------- ###
### SwitchContext context component node                          ###
### ------------------------------------------------------------- ###
### Last updated: 2017-07-24                                      ###
### ------------------------------------------------------------- ###
### IN:                                                           ###
###    - name:			component name                            ###
###    - attribute:		"Double attribute to be compared with     ###
###                     values"                                   ###
###    - attributeValues:	                                      ###
###						"Double values to be compared which       ###
###                     attribute"                                ###
###    - CHILDREN:      children nodes                            ###
### OUT:                                                          ###
###    contextcomponent node "<contextcomponent ...> ...          ###
###    </contextcomponent>"                                       ###
### ------------------------------------------------------------- ###
### Required: XML package                                         ###
#####################################################################
.j2kContextSwitch = function(name, attribute, values, CHILDREN=NULL){
	xmln = xmlNode("contextcomponent", attrs=c("class"="jams.components.conditional.SwitchContext", enabled="true", name=name, version="1.0_1"))
	xmln = addChildren(xmln, .j2kVar(name="attribute", attribute=attribute$attribute, context=attribute$context, value=attribute$value))
	xmln = addChildren(xmln, .j2kVar(name="values", attribute=values$attribute, context=values$context, value=values$value))
	for (k in CHILDREN){
		xmln = addChildren(xmln, k)
	}
	return(xmln)
}

#####################################################################
### ------------------------------------------------------------- ###
### CalcAreaWeight component node                                 ###
### ------------------------------------------------------------- ###
### Last updated: 2017-03-31                                      ###
### ------------------------------------------------------------- ###
### IN:                                                           ###
###    - name:			component name                            ###
###    - entityArea:	"the area of the single entity"           ###
###    - catchmentArea:	"the area of the catchment"               ###
###    - areaWeight:	"the relative area weight of the entity"  ###
### OUT:                                                          ###
###    component node "<component ...> ... </component>"          ###
### ------------------------------------------------------------- ###
### Required: XML package                                         ###
#####################################################################
.j2kComponentAreaWeight = function(name, entityArea, catchmentArea, areaWeight){
	xmln = xmlNode("component", attrs=c("class"="org.unijena.j2k.CalcAreaWeight", "enabled"="true", "name"=name, "version"="1.0_0"))
	xmln = addChildren(xmln, .j2kVar(name="entityArea", attribute=entityArea$attribute, context=entityArea$context, value=entityArea$value))
	xmln = addChildren(xmln, .j2kVar(name="catchmentArea", attribute=catchmentArea$attribute, context=catchmentArea$context, value=catchmentArea$value))
	xmln = addChildren(xmln, .j2kVar(name="areaWeight", attribute=areaWeight$attribute, context=areaWeight$context, value=areaWeight$value))
	return(xmln)
}


#####################################################################
### ------------------------------------------------------------- ###
### SumAggregator component node                                  ###
### IT SEEMS THAT IT IS EXACTLY THE SAME CODE AS THE WEIGTH SUM   ###
### AGGREGATOR. In Java code, this class only extent the other one###
### without adding anything!                                      ###
### ------------------------------------------------------------- ###
### Last updated: 2017-03-31                                      ###
### ------------------------------------------------------------- ###
### IN:                                                           ###
###    - name:			component name                            ###
###    - value:			"The value(s) to be summed up"            ###
###    - weight:		"A weight that the value(s) are being     ###
###						devided by during aggregation"            ###
###    - sum:			"The resulting weighted sum(s) of the     ###
###						given values"                             ###
###    - divideByWeight:"How should the weighting be done? If     ###
###						divideByWeight is true (default),the value###
###						will be divided by the weight, otherwise  ###
###						multiplied."                              ###
### OUT:                                                          ###
###    component node "<component ...> ... </component>"          ###
### ------------------------------------------------------------- ###
### Required: XML package                                         ###
#####################################################################
.j2kComponentSumAggregator = function(name, value, weight, sum, divideByWeight){
	xmln = xmlNode("component", attrs=c("class"="org.unijena.j2k.aggregate.SumAggregator", "enabled"="true", "name"=name, "version"="1.0_0"))
	xmln = addChildren(xmln, .j2kVar(name="value", attribute=value$attribute, context=value$context, value=value$value))
	xmln = addChildren(xmln, .j2kVar(name="weight", attribute=weight$attribute, context=weight$context, value=weight$value))
	xmln = addChildren(xmln, .j2kVar(name="sum", attribute=sum$attribute, context=sum$context, value=sum$value))
	xmln = addChildren(xmln, .j2kVar(name="divideByWeight", attribute=divideByWeight$attribute, context=divideByWeight$context, value=divideByWeight$value))
	return(xmln)
}


#####################################################################
### ------------------------------------------------------------- ###
### WeightedSumAggregator component node                          ###
### ------------------------------------------------------------- ###
### Last updated: 2017-03-31                                      ###
### ------------------------------------------------------------- ###
### IN:                                                           ###
###    - name:			component name                            ###
###    - value:			"The value(s) to be summed up"            ###
###    - weight:		"A weight that the value(s) are being     ###
###						devided by during aggregation"            ###
###    - sum:			"The resulting weighted sum(s) of the     ###
###						given values"                             ###
###    - divideByWeight:"How should the weighting be done? If     ###
###						divideByWeight is true (default),the value###
###						will be divided by the weight, otherwise  ###
###						multiplied."                              ###
### OUT:                                                          ###
###    component node "<component ...> ... </component>"          ###
### ------------------------------------------------------------- ###
### Required: XML package                                         ###
#####################################################################
.j2kComponentWeightedSumAggregator = function(name, value, weight, sum, divideByWeight){
	xmln = xmlNode("component", attrs=c("class"="org.unijena.j2k.aggregate.WeightedSumAggregator", "enabled"="true", "name"=name, "version"="1.0"))
	xmln = addChildren(xmln, .j2kVar(name="value", attribute=value$attribute, context=value$context, value=value$value))
	xmln = addChildren(xmln, .j2kVar(name="weight", attribute=weight$attribute, context=weight$context, value=weight$value))
	xmln = addChildren(xmln, .j2kVar(name="sum", attribute=sum$attribute, context=sum$context, value=sum$value))
	xmln = addChildren(xmln, .j2kVar(name="divideByWeight", attribute=divideByWeight$attribute, context=divideByWeight$context, value=divideByWeight$value))
	return(xmln)
}

# TO TEST AND COMMENT
.j2kOutputDataStore = function(context, name, attribute_ids){
	xmlnods = xmlNode(get(".j2k.tag.outputdatastore", envir=j2k.env), attrs=c("context"=context, "enabled"="true", "name"=name))
	xmltrace = xmlNode(get(".j2k.tag.trace", envir=j2k.env))
	for (k in 1:length(attribute_ids)){
		xmltrace = addChildren(xmltrace, .j2kAttribute(id=attribute_ids[k]))
	}
	xmlnods = addChildren(xmlnods, xmltrace)
	return(xmlnods)
}


#####################################################################
### ------------------------------------------------------------- ###
### Variable node                                                 ###
### ------------------------------------------------------------- ###
### Last updated: 2017-03-31                                      ###
### ------------------------------------------------------------- ###
### IN:                                                           ###
###    - name:			variable name                                 ###
###    - attribute:		attribute of variable                       ###
###    - context:		context of attribute                          ###
###    - value:			value of variable                             ###
### OUT:                                                          ###
###    variable node "<var ... />"                                ###
### ------------------------------------------------------------- ###
### Required: XML package                                         ###
#####################################################################
.j2kVar = function(name, attribute=NULL, context=NULL, value=NULL){
	if (sum(c(is.null(attribute), is.null(context)))==1){
		stop("if variable attribute is provided, context must be provided (vice versa)!")
	}
	xmlNode("var", attrs=c("name"=name, "attribute"=attribute, "context"=context, "value"=value))
}


# -------------------------------------------------------------------
# Create an attribute "node"
# in ... arguments must be named!!
# -------------------------------------------------------------------
# I. Horner / 2017-08-04 / 2017-08-04 / 2019-02-06
# -------------------------------------------------------------------
.j2kAttribute = function(...){
	xmlNode("attribute", attrs=list(...))
}


#####################################################################
### ------------------------------------------------------------- ###
### UniversalEfficiencyCalculator component node                  ###
### ------------------------------------------------------------- ###
### Last updated: 2019-02-06                                      ###
### ------------------------------------------------------------- ###
### IN:                                                           ###
### OUT:                                                          ###
###    component node "<component ...> ... </component>"          ###
### ------------------------------------------------------------- ###
### Required: XML package                                         ###
#####################################################################
.j2kComponentUniversalEfficiencyCalculator = function(name, mainContext, simContext, simVariable, obsContext, obsVariable, timeContext, timeVariable){
  xmln = xmlNode("component", attrs=c("class"="optas.efficiencies.UniversalEfficiencyCalculator", "enabled"="true", "name"=name, "version"="1.0_1"))
  name_eff = c("r2", "bias", "e1", "e2", "le1", "le2", "ave")
  name_eff = c(name_eff, paste0(name_eff, "_normalized"))
  attr_eff = paste0(name, "_", name_eff)
  for (k in 1:length(name_eff)) {
    xmln = addChildren(xmln, .j2kVar(name=name_eff[k], attribute=attr_eff[k], context=mainContext))
  }
  xmln = addChildren(xmln, .j2kVar(name="measurementAttributeName", value="measurement"))
  xmln = addChildren(xmln, .j2kVar(name="simulationAttributeName", value="simulation"))
  xmln = addChildren(xmln, .j2kVar(name="measurement", attribute=obsVariable, context=obsContext))
  xmln = addChildren(xmln, .j2kVar(name="simulation", attribute=simVariable, context=simContext))
  xmln = addChildren(xmln, .j2kVar(name="time", attribute=timeVariable, context=timeContext))
  return(xmln)
}

