
j2kSetParameterFiles = function(j2kproject, hru=NULL, reach=NULL, landuse=NULL, soil=NULL, hgeol=NULL) {
  if (!is.null(hru)) j2kproject$parameterfiles["hrufile"] = hru
  if (!is.null(reach)) j2kproject$parameterfiles["reachfile"] = reach
  if (!is.null(landuse)) j2kproject$parameterfiles["landufile"] = landuse
  if (!is.null(soil)) j2kproject$parameterfiles["soilfile"] = soil
  if (!is.null(hgeol)) j2kproject$parameterfiles["groundwfile"] = hgeol
  
  return(j2kproject)
}

j2kGetParameterFiles = function(j2kproject) {
  return(j2kproject$parameterfiles)
}

j2kSetForcingFiles = function(j2kproject, P=NULL, ET0=NULL, T=NULL) {
  if (!is.null(P)) j2kproject$forcingfiles['precipitation'] = P
  if (!is.null(ET0)) j2kproject$forcingfiles['ETRef'] = ET0
  if (!is.null(T)) j2kproject$forcingfiles['temperature'] = T
  return(j2kproject)
}

j2kSetModelingTimeRange = function(j2kproject, timerange) {
  j2kproject$timeinformation$timerange = timerange
  return(j2kproject)
}

j2kSetOutputParameters = function(j2kproject, misc, liter, area=FALSE) {
  if (!missing(misc)) {
    if (is.null(misc)) warning("'misc' set to NULL.")
    j2kproject$outputparameters[["misc"]] = misc
  }
  if (!missing(liter)) {
    if (is.null(liter)) warning("'liter' set to NULL.")
    j2kproject$outputparameters[["liter"]] = liter
  }
  if (!is.logical(area)) {
    warning("'area' must be logical. Set to default (FALSE).")
    area = FALSE
  }
  if (area) {
    if (!is.null(j2kproject$subcatchments)) {
      j2kproject$datastores[['J2K_RHONE']]=c(paste0("Area_", j2kproject$subcatchments[, "id"]))
      
    } else {
      warning("Cannot add 'Area' as an output parameter if no subcatchments were added to the project.")
    }
  }
  return(j2kproject)
}


j2kSetOutputVariables = function(j2kproject, misc, liter, reach) {
  if (!missing(misc)) {
    if (is.null(misc)) warning("'misc' set to NULL.")
    j2kproject$outputvariables[["misc"]] = misc
  }
  if (!missing(liter)) {
    if (is.null(liter)) warning("'liter' set to NULL.")
    j2kproject$outputvariables[["liter"]] = liter
  }
  if (!missing(reach)) {
    if (is.null(reach)) warning("'reach' set to NULL.")
    j2kproject$outputvariables[["reach"]] = reach
  }
  return(j2kproject)
}


j2kSetSubcatchments = function(j2kproject, subcatchments) {
  j2kproject$subcatchments = subcatchments
  return(j2kproject)
}

j2kGetSubcatchments = function(j2kproject) {
  return(j2kproject$subcatchments)
}


# -------------------------------------------------------------------
# FIX ME:  
# This function should extract relevant information from the XML file
# returning a list with:
# > subcatchments: a list (one element for each station) containing all 
#   the required information to (re-)construct the XML object afterwards:
# * ID:              station id as in the GIS station layer
# * watersheds:      ids of all upstream watersheds
# * variable_na:     the desired output subcatchment variables (misc unit)
# * variable_L:      the desired output subcatchment variables (L unit)
# * reachid:         the corresponding reach id
# * variable_reach:  the desired output reach variables 
# > output variables: a list with one element for each 'datastore'
#   containing character vectors with all the variables 
# > parameter files: a character vector with the parameter file names
# > forcing files: a character vector with the forcing file names
# > modeling time range: a character (POSIX ?) vector of size 2 
# > any other information that could be easily implemented.
# -------------------------------------------------------------------
# Load J2K XML project file created in JAMS
# => It returns the XML R Object
# -------------------------------------------------------------------
# I. Horner / 2017-03-31 / 2017-11-27
# -------------------------------------------------------------------
j2kLoadJAMSProject = function(fp){
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
		if (length(which(is.na(matchvar)))>0){warning("Some variables were missing from the '", get(".j2k.timeContext", envir=j2k.env), "' datastore.")}
		ds[[get(".j2k.timeContext", envir=j2k.env)]] = ds[[get(".j2k.timeContext", envir=j2k.env)]][-matchvar]
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
}

print.j2kproject = function(x){
	sep = paste0(paste(rep("-", 70), collapse=""), "\n")
	cat(sep)
	cat("Filename:            ", x$filename, "\n")
	cat("Directory:           ", x$directory, "\n")
	cat("Modeling time range: ", paste(x$timeinformation$timerange, collapse=" -> "), "\n")
	cat(sep)
	cat("Datastores: \n")
	nds = names(x$datastores)
	if (length(nds)>0) {
  	for (k in 1:length(nds)){
  		cat(" > ", nds[k], "\n")
  		print(x$datastores[[k]])
  	}
	} else {
	  cat("<no custom datastores>\n")
	}
	cat(sep)
	cat("Parameter files: \n")
	cat(" > Reaches parameters:     ", x$parameterfiles["reachfile"], "\n")
	cat(" > HRUs parameters:        ", x$parameterfiles["hrufile"], "\n")
	cat(" > Land-use parameters:    ", x$parameterfiles["landufile"], "\n")
	cat(" > Soil parameters:        ", x$parameterfiles["soilfile"], "\n")
	cat(" > Groundwater parameters: ", x$parameterfiles["groundwfile"], "\n")
	cat(sep)
	cat("Forcing files: \n")
	cat(" > Precipitations: ", x$forcingfiles["precipitation"], "\n")
	cat(" > ET0:            ", x$forcingfiles["ETRef"], "\n")
	cat(" > Temperature:    ", x$forcingfiles["temperature"], "\n")
	cat(" > Dams:           ", x$forcingfiles["dam"], "\n")
	cat(sep)
	cat("Sub-catchments: \n")
	print(x$subcatchments)
	cat(sep)
	cat("Sub-catchments output variables: \n")
	print(x$outputvariables)
	cat(sep)
	cat("Sub-catchments output parameters: \n")
	print(x$outputparameters)
	cat(sep)
}


# -------------------------------------------------------------------
# Save a XML R Object of a J2K JAMS project
# -------------------------------------------------------------------
# I. Horner / 2017-03-31 / 2017-07-24
# -------------------------------------------------------------------
j2kSaveJAMSProject = function(j2kproject, fp, verbose=TRUE){
  contextnames = j2kproject$contextnames
	j2kSetContextsNames(modelName=contextnames[1], paramContext=contextnames[2], initContext=contextnames[3], 
		timeContext=contextnames[4], inputContext=contextnames[5], hruContext=contextnames[6], reachContext=contextnames[7])
	xml = j2kproject$XML
	if (verbose) message("Setting time information..."); flush.console()
	xml = .j2kXMLsetTimeInformation(j2kproject=xml, timeinfo=j2kproject$timeinformation)
	if (verbose) message("Setting forcing files..."); flush.console()
	xml = .j2kXMLsetForcingFileNames(j2kproject=xml, forcingfiles=j2kproject$forcingfiles)
	if (verbose) message("Setting parameter files..."); flush.console()
	xml = .j2kXMLsetParameterFileNames(j2kproject=xml, parameterfiles=j2kproject$parameterfiles)
	if (verbose) message("Setting sub-catchments..."); flush.console()
	xml = .j2kXMLsetSubcatchments(j2kproject=xml, subcatchments=j2kproject$subcatchments,
			variables=j2kproject$outputvariables, parameters=j2kproject$outputparameters, datastores=j2kproject$datastores, 
			doublesetter=j2kproject$doublesetter)
	if (verbose) message("Saving XML JAMS project file..."); flush.console()
	save_dir = dirname(fp)
	dir.create(save_dir, showWarnings=FALSE, recursive=TRUE)
	suppressWarnings({saveXML(xml, file=fp, prefix=get(".j2k.xmlprefix", envir=j2k.env))})
	invisible()
}

# -------------------------------------------------------------------
# In the J2K xml file, given the time information, reset the time
# information.
# => It returns the updated 'j2kproject' XML JAMS project.
# -------------------------------------------------------------------
# I. Horner / 2017-11-24 / 2017-11-24
# -------------------------------------------------------------------
.j2kXMLsetTimeInformation = function(j2kproject, timeinfo) {
	timestepcode = c("year"=1, "month"=2, "day"=6, "hour"=11, "minute"=12, "second"=13)
	trg = timeinfo$timerange
	tst = timeinfo$timestep
	# --
	ti = paste(paste(format(trg, format="%Y-%m-%d %H:%M"), collapse=" "), timestepcode[tst[1]], tst[2], collapse=" ")
	xmlAttrs(j2kproject[[.j2kGetContextLoc(j2kproject, name=get(".j2k.timeInterval", envir=j2k.env))]]) = c("value"=ti)
	return(j2kproject)
}

# -------------------------------------------------------------------
# In the J2K xml file, extract the information on modeling time range
# and time steps.
# => It returns a named list containing the time range in POSIXlt and
# the time steps information (unit interval and number of units).
# -------------------------------------------------------------------
# I. Horner / 2017-11-22 / 2017-11-22
# -------------------------------------------------------------------
.j2kXMLgetTimeInformation = function(j2kproject) {
	timeinfo = xmlAttrs(j2kproject[[.j2kGetContextLoc(j2kproject, name=get(".j2k.timeInterval", envir=j2k.env))]])[["value"]]
	timeinfo = strsplit(timeinfo, " ")[[1]]
	# time steps codes: 1=Year, 2=Month, 6=Day, 11=Hour, 12=Minute, 13=Seconds
	timestepcode = c("year"=1, "month"=2, "day"=6, "hour"=11, "minute"=12, "second"=13)
	timerange = strptime(c(paste(timeinfo[1:2], collapse=" "), paste(timeinfo[3:4], collapse=" ")), format="%Y-%m-%d %H:%M", tz="GMT")
	timestep = names(timestepcode)[which(timestepcode== as.numeric(timeinfo[5]))]
	timestepby = as.numeric(timeinfo[6])
	return(list("timerange"=timerange, "timestep"=c(timestep, timestepby)))
}

# -------------------------------------------------------------------
# In the J2K xml file, given the forcing files names, reset the name
# of the forcing files to use.
# => It returns the updated 'j2kproject' XML JAMS project.
# -------------------------------------------------------------------
# I. Horner / 2017-11-24 / 2017-11-27
# -------------------------------------------------------------------
.j2kXMLsetForcingFileNames = function(j2kproject, forcingfiles) {
	timecontext = j2kproject[[.j2kGetContextLoc(j2kproject, name=get(".j2k.timeContext", envir=j2k.env))]]
	inputcontext = timecontext[[.j2kGetContextLoc(timecontext, name=get(".j2k.inputContext", envir=j2k.env))]]
	dam = inputcontext[[.j2kGetContextLoc(inputcontext, name=get(".j2k.component.Dam_DataReader", envir=j2k.env))]]
	if (!is.na(forcingfiles['dam'])) {
		if (!is.null(.j2kGetContextLoc(dam, name=get(".j2k.component.DataReader.id", envir=j2k.env)))) {
			xmlAttrs(dam[[.j2kGetContextLoc(dam, name=get(".j2k.component.DataReader.id", envir=j2k.env))]]) = c("value"=forcingfiles[['dam']])
		} else {
			warning("The 'dam' data reader component wasn't found.")
		}
	}
	temp = inputcontext[[.j2kGetContextLoc(inputcontext, name=get(".j2k.component.Temp_DataReader", envir=j2k.env))]]
	if (!is.na(forcingfiles['temperature'])) xmlAttrs(temp[[.j2kGetContextLoc(temp, name=get(".j2k.component.DataReader.id", envir=j2k.env))]]) = c("value"=forcingfiles[['temperature']])
	precip = inputcontext[[.j2kGetContextLoc(inputcontext, name=get(".j2k.component.Precip_DataReader", envir=j2k.env))]]
	if (!is.na(forcingfiles['precipitation'])) xmlAttrs(precip[[.j2kGetContextLoc(precip, name=get(".j2k.component.DataReader.id", envir=j2k.env))]]) = c("value"=forcingfiles[['precipitation']])
	etref = inputcontext[[.j2kGetContextLoc(inputcontext, name=get(".j2k.component.ETRef_DataReader", envir=j2k.env))]]
	if (!is.na(forcingfiles['ETRef'])) xmlAttrs(etref[[.j2kGetContextLoc(etref, name=get(".j2k.component.DataReader.id", envir=j2k.env))]]) = c("value"=forcingfiles[['ETRef']])

	inputcontext[[.j2kGetContextLoc(inputcontext, name=get(".j2k.component.Dam_DataReader", envir=j2k.env))]] = dam
	inputcontext[[.j2kGetContextLoc(inputcontext, name=get(".j2k.component.Temp_DataReader", envir=j2k.env))]] = temp
	inputcontext[[.j2kGetContextLoc(inputcontext, name=get(".j2k.component.Precip_DataReader", envir=j2k.env))]] = precip
	inputcontext[[.j2kGetContextLoc(inputcontext, name=get(".j2k.component.ETRef_DataReader", envir=j2k.env))]] = etref
	
	timecontext[[.j2kGetContextLoc(timecontext, name=get(".j2k.inputContext", envir=j2k.env))]] = inputcontext
	j2kproject[[.j2kGetContextLoc(j2kproject, name=get(".j2k.timeContext", envir=j2k.env))]] = timecontext

	return(j2kproject)
}

# -------------------------------------------------------------------
# In the J2K xml file, extract the forcing file names for dams,
# temperature, precipitation and reference evapotranspiration.
# => It returns a named character vector containing the forcing
# file names.
# -------------------------------------------------------------------
# I. Horner / 2017-11-22 / 2017-11-22
# -------------------------------------------------------------------
.j2kXMLgetForcingFileNames = function(j2kproject){
	timecontext = j2kproject[[.j2kGetContextLoc(j2kproject, name=get(".j2k.timeContext", envir=j2k.env))]]
	inputcontext = timecontext[[.j2kGetContextLoc(timecontext, name=get(".j2k.inputContext", envir=j2k.env))]]
	dam = inputcontext[[.j2kGetContextLoc(inputcontext, name=get(".j2k.component.Dam_DataReader", envir=j2k.env))]]
	if (!is.null(.j2kGetContextLoc(dam, name=get(".j2k.component.DataReader.id", envir=j2k.env)))) {
		dam = xmlAttrs(dam[[.j2kGetContextLoc(dam, name=get(".j2k.component.DataReader.id", envir=j2k.env))]])[["value"]]
	} else {
		warning("The 'dam' data reader component is empty.")
		dam = NULL
	}
	temp = inputcontext[[.j2kGetContextLoc(inputcontext, name=get(".j2k.component.Temp_DataReader", envir=j2k.env))]]
	if (!is.null(.j2kGetContextLoc(temp, name=get(".j2k.component.DataReader.id", envir=j2k.env)))) {
		temp = xmlAttrs(temp[[.j2kGetContextLoc(temp, name=get(".j2k.component.DataReader.id", envir=j2k.env))]])[["value"]]
	} else {
		warning("The 'temperature' data reader component is empty.")
		temp = NULL
	}
	precip = inputcontext[[.j2kGetContextLoc(inputcontext, name=get(".j2k.component.Precip_DataReader", envir=j2k.env))]]
	if (!is.null(.j2kGetContextLoc(precip, name=get(".j2k.component.DataReader.id", envir=j2k.env)))) {
		precip = xmlAttrs(precip[[.j2kGetContextLoc(precip, name=get(".j2k.component.DataReader.id", envir=j2k.env))]])[["value"]]
	} else {
		warning("The 'precipitation' data reader component is empty.")
		precip = NULL
	}
	etref = inputcontext[[.j2kGetContextLoc(inputcontext, name=get(".j2k.component.ETRef_DataReader", envir=j2k.env))]]
	if (!is.null(.j2kGetContextLoc(etref, name=get(".j2k.component.DataReader.id", envir=j2k.env)))) {
		etref = xmlAttrs(etref[[.j2kGetContextLoc(etref, name=get(".j2k.component.DataReader.id", envir=j2k.env))]])[["value"]]
	} else {
		warning("The 'ETRef' data reader component is empty.")
		etref = NULL
	}
	return(c("dam"=dam, "temperature"=temp, "precipitation"=precip, "ETRef"=etref))
}

# -------------------------------------------------------------------
# In the J2K xml file, given the parameter files names, reset the name
# of the parameter files to use.
# => It returns the updated 'j2kproject' XML JAMS project.
# -------------------------------------------------------------------
# I. Horner / 2017-11-25 / 2017-11-27
# -------------------------------------------------------------------
.j2kXMLsetParameterFileNames  = function(j2kproject, parameterfiles){
	paramcontext = j2kproject[[.j2kGetContextLoc(j2kproject, get(".j2k.paramContext", envir=j2k.env))]]
	er = paramcontext[[.j2kGetContextLoc(paramcontext, class=get(".j2k.component.StandardEntityReader.class", envir=j2k.env))]]
	xmlAttrs(er[[.j2kGetContextLoc(er, name=get(".j2k.component.StandardEntityReader.reachFileName", envir=j2k.env))]]) = c('value'=parameterfiles[['reachfile']])
	xmlAttrs(er[[.j2kGetContextLoc(er, name=get(".j2k.component.StandardEntityReader.hruFileName", envir=j2k.env))]]) = c('value'=parameterfiles[['hrufile']])
	
	lur = paramcontext[[.j2kGetContextLoc(paramcontext, class=get(".j2k.component.StandardLUReader.class", envir=j2k.env))]]
	xmlAttrs(lur[[.j2kGetContextLoc(lur, name=get(".j2k.component.StandardLUReader.luFileName", envir=j2k.env))]]) = c('value'=parameterfiles[['landufile']])
	
	sr = paramcontext[[.j2kGetContextLoc(paramcontext, class=get(".j2k.component.StandardSoilParaReader.class", envir=j2k.env))]]
	xmlAttrs(sr[[.j2kGetContextLoc(sr, name=get(".j2k.component.StandardSoilParaReader.stFileName", envir=j2k.env))]]) = c('value'=parameterfiles[['soilfile']])
	
	gwr = paramcontext[[.j2kGetContextLoc(paramcontext, class=get(".j2k.component.StandardGroundwaterParaReader.class", envir=j2k.env))]]
	xmlAttrs(gwr[[.j2kGetContextLoc(gwr, name=get(".j2k.component.StandardGroundwaterParaReader.gwFileName", envir=j2k.env))]]) = c('value'=parameterfiles[['groundwfile']])
	
	paramcontext[[.j2kGetContextLoc(paramcontext, class=get(".j2k.component.StandardEntityReader.class", envir=j2k.env))]] = er
	paramcontext[[.j2kGetContextLoc(paramcontext, class=get(".j2k.component.StandardLUReader.class", envir=j2k.env))]] = lur
	paramcontext[[.j2kGetContextLoc(paramcontext, class=get(".j2k.component.StandardSoilParaReader.class", envir=j2k.env))]] = sr
	paramcontext[[.j2kGetContextLoc(paramcontext, class=get(".j2k.component.StandardGroundwaterParaReader.class", envir=j2k.env))]] = gwr
	
	j2kproject[[.j2kGetContextLoc(j2kproject, get(".j2k.paramContext", envir=j2k.env))]] = paramcontext
	return(j2kproject)
 }

# -------------------------------------------------------------------
# In the J2K xml file, extract the parameter file names for reaches,
# hrus, land use, soil and groundwater.
# => It returns a named character vector containing the parameters
# file names.
# -------------------------------------------------------------------
# I. Horner / 2017-11-22 / 2017-11-22
# -------------------------------------------------------------------
.j2kXMLgetParameterFileNames = function(j2kproject){
	paramcontext = j2kproject[[.j2kGetContextLoc(j2kproject, get(".j2k.paramContext", envir=j2k.env))]]
	er = paramcontext[[.j2kGetContextLoc(paramcontext, class=get(".j2k.component.StandardEntityReader.class", envir=j2k.env))]]
	reachfile = xmlAttrs(er[[.j2kGetContextLoc(er, name=get(".j2k.component.StandardEntityReader.reachFileName", envir=j2k.env))]])[["value"]]
	hrufile = xmlAttrs(er[[.j2kGetContextLoc(er, name=get(".j2k.component.StandardEntityReader.hruFileName", envir=j2k.env))]])[["value"]]
	lur = paramcontext[[.j2kGetContextLoc(paramcontext, class=get(".j2k.component.StandardLUReader.class", envir=j2k.env))]]
	landufile = xmlAttrs(lur[[.j2kGetContextLoc(lur, name=get(".j2k.component.StandardLUReader.luFileName", envir=j2k.env))]])[["value"]]
	sr = paramcontext[[.j2kGetContextLoc(paramcontext, class=get(".j2k.component.StandardSoilParaReader.class", envir=j2k.env))]]
	soilfile = xmlAttrs(sr[[.j2kGetContextLoc(sr, name=get(".j2k.component.StandardSoilParaReader.stFileName", envir=j2k.env))]])[["value"]]
	gwr = paramcontext[[.j2kGetContextLoc(paramcontext, class=get(".j2k.component.StandardGroundwaterParaReader.class", envir=j2k.env))]]
	groundwfile = xmlAttrs(gwr[[.j2kGetContextLoc(gwr, name=get(".j2k.component.StandardGroundwaterParaReader.gwFileName", envir=j2k.env))]])[["value"]]
	return(c("reachfile"=reachfile, "hrufile"=hrufile, "landufile"=landufile, "soilfile"=soilfile, "groundwfile"=groundwfile))
}

# -------------------------------------------------------------------
# FIX ME: 
# manage the case where there is no sub-catchments
# manage the case the various context specific to subcatchment don't exist.
# j2kproject no longer needs to be send to the various function!
# -------------------------------------------------------------------
# In the J2K xml JAMS project, insert the subcatchment
# -------------------------------------------------------------------
# I. Horner | 2018-01-01
# -------------------------------------------------------------------
.j2kXMLsetSubcatchments = function(j2kproject, subcatchments, variables, parameters, datastores, doublesetter, verbose=TRUE){
	
	if (length(subcatchments) == 0) {
	  # Deal with the case where there's no subcatchment.
    warning("No sub-catchments found. No sub-catchment were added in the model.")
		# message("<case not coded>")
	} else {
	  ##=========================================================================================
		# 1/ Let's (re-)create the nodes for the Init context
		initsubcatch = .j2kInitWatershedsAreas(j2kproject=j2kproject, stationIDs=subcatchments[, "id"], watershedIDs=subcatchments[, "watersheds"])
		initcontext = j2kproject[[.j2kGetContextLoc(j2kproject, get(".j2k.initContext", envir=j2k.env))]]
		iiwa = .j2kGetContextLoc(initcontext, get(".j2k.context.InitWatershedsAreas", envir=j2k.env))
		# if iiwa is null, message + insertion at the end of the initialization context
		if (is.null(iiwa)) {
			if (verbose) message(". no initialization context found. Created."); flush.console()
			initcontext = append.xmlNode(initcontext, initsubcatch)
		} else { 
			initcontext[[iiwa]] = initsubcatch
		}
		j2kproject[[.j2kGetContextLoc(j2kproject, get(".j2k.initContext", envir=j2k.env))]] = initcontext
        
    ##=========================================================================================
		# 2/ MAJOR UPDATE let's (re-)create the nodes where distributed parameters will be aggregated for each sub-catchment
    # filtered spatial context (as in InputState context)
    initdistribvarsubcatch = .j2kWatershedDistributedParameters(stationIDs=subcatchments[, "id"], 
                                                                watershedIDs=subcatchments[, "watersheds"],
                                                                variable_na=parameters[["misc"]],
                                                                variable_L=parameters[["liter"]])
        
		iiwd = .j2kGetContextLoc(initcontext, get(".j2k.context.WatershedsDistributedParameters", envir=j2k.env))
		# if iiwa is null, message + insertion at the end of the initialization context
		if (is.null(iiwd)) {
			if (verbose) message(". no distributed parameters aggregation context found. Created."); flush.console()
			initcontext = append.xmlNode(initcontext, initdistribvarsubcatch)
		} else { 
			initcontext[[iiwd]] = initdistribvarsubcatch
		}
		j2kproject[[.j2kGetContextLoc(j2kproject, get(".j2k.initContext", envir=j2k.env))]] = initcontext
		
    ##=========================================================================================
		# 3/ let's (re-)create the nodes for the InputState context
		inputstatesubcatch = .j2kWatershedInputAndState(j2kproject=j2kproject, stationIDs=subcatchments[, "id"], 
		                                                watershedIDs=subcatchments[, "watersheds"],
		                                                variable_na=variables[["misc"]],
		                                                variable_L=variables[["liter"]])
		timecontext = j2kproject[[.j2kGetContextLoc(j2kproject, get(".j2k.timeContext", envir=j2k.env))]]
		iwis = .j2kGetContextLoc(timecontext, get(".j2k.context.WatershedsInputAndState", envir=j2k.env))
		# if iwis is null, message + insertion  at the end of the initialization context
		if (is.null(iwis)) {
			if (verbose) message(". no input/state context found. Created."); flush.console()
			timecontext = .j2kInsertChild(xml=timecontext, child=inputstatesubcatch, loc=.j2kGetContextLoc(timecontext, get(".j2k.reachContext", envir=j2k.env)))
		} else { 
			timecontext[[iwis]] = inputstatesubcatch
		}
		
		##=========================================================================================
		# 4/ let's (re-)create the nodes for the ReachOutput context
		reachoutputsubcatch = .j2kReachesOutputs(j2kproject=j2kproject, stationID=subcatchments[, "id"], 
		                                         reachID=subcatchments[, "reach"], variable_reach=variables[["reach"]], unit_mm=FALSE)
		irea = .j2kGetContextLoc(timecontext, get(".j2k.reachContext", envir=j2k.env))
		reachcontext = timecontext[[irea]]
		iout = .j2kGetContextLoc(reachcontext, get(".j2k.context.ReachesOutputs", envir=j2k.env))
		if (is.null(iout)) {
			if (verbose) message(". no reach output context found. Created."); flush.console()
			# timecontext = .j2kInsertChild(xml=timecontext, child=inputstatesubcatch, loc=.j2kGetContextLoc(timecontext, .j2k.reachContext))
			reachcontext = append.xmlNode(reachcontext, reachoutputsubcatch)
		} else {
			reachcontext[[iout]] = reachoutputsubcatch
		}
		timecontext[[irea]] = reachcontext
		j2kproject[[.j2kGetContextLoc(j2kproject, get(".j2k.timeContext", envir=j2k.env))]] = timecontext
		
		##=========================================================================================
		# 5/ create the TimeLoop datastore for output variables
		varsubcatch = as.vector(outer(unlist(variables), subcatchments[, "id"], paste, sep="_"))
		
		othervartimeloop = datastores[[get(".j2k.timeContext", envir=j2k.env)]] # get already existing variables
		vartimeloop = c(othervartimeloop, varsubcatch)
		# no need of this condition
		# if (!is.null(othervartimeloop)) vartimeloop = c(othervartimeloop, varsubcatch) else vartimeloop = varsubcatch
		if (length(vartimeloop)>0) datastores[[get(".j2k.timeContext", envir=j2k.env)]] = vartimeloop # update  datastore
		# outdatastores = .j2kDataStores(variables=datastores, datastorecontexts=names(datastores))
		# j2kproject[[which(names(j2kproject)==.j2k.tag.datastores)]] = outdatastores
		
		vardoublesetter = c(doublesetter, varsubcatch)
		# if (!is.null(othervartimeloop)) vardoublesetter = c(doublesetter, varsubcatch) else vardoublesetter = varsubcatch
		j2kproject = .j2kXMLsetDoubleSetter(j2kproject, dblsetvar=vardoublesetter)
		
    # FIX-ME: should also include the addition of Area_xx in J2K datastore. 
		#    THOUGHT? the object j2kvarout should manage: timeloop variable (it is the case, implicitly) and other context variables
		#    including J2K context.
		##=========================================================================================
    # 6/ create the MODEL datastore for output parameters
		parsubcatch = as.vector(outer(unlist(parameters), subcatchments[, "id"], paste, sep="_"))
		otherparmodel = datastores[[get(".j2k.modelName", envir=j2k.env)]] # get already existing variables
		parmodel = c(otherparmodel, parsubcatch)
		# if (!is.null(otherparmodel)) parmodel = c(otherparmodel, parsubcatch) else parmodel = parsubcatch
		if (length(parmodel)>0) datastores[[get(".j2k.modelName", envir=j2k.env)]] = parmodel # update  datastore
		
		outdatastores = .j2kDataStores(variables=datastores, datastorecontexts=names(datastores))
		##=========================================================================================
    # 6/ update datastore in XML object
		j2kproject[[which(names(j2kproject)==get(".j2k.tag.datastores", envir=j2k.env))]] = outdatastores
	}
	return(j2kproject)
}




# -------------------------------------------------------------------
# In the J2K xml file, extract the station ids, the upstream watershed
# ids and the corresponding reach ids. The computed variables in the 
# InputAndState and ReachOutput sections are also extracted.
# => A list of three element: (1) 'stations', a data.frame with two
# columns with stations and reachs ids, (2) 'variables', a three element 
# list with miscellaneous, liters and reach variables and (3) 'watersheds',
# a list with as many element as stations containing numeric vectors of the 
# upstream watersheds.
# -------------------------------------------------------------------
# I. Horner / 2017-11-21 / 2018-02-22
# -------------------------------------------------------------------
# 2018-02-22: minor bug fix when retrieving reach ids
.j2kXMLgetSubcatchments = function(j2kproject){
  .cleanparam = function(para) {
    para = sapply(para, function(x){ strsplit(x, ";") })
    para = lapply(para, function(x) {
      sapply(x, function(e) {
        ecut = strsplit(e, "_")[[1]]
        paste(ecut[-length(ecut)], collapse="_")
      }, USE.NAMES=FALSE)
    })
    unique(para)
  }
	# various context have to be screened to check consistency 
	# and get all the needed data/information:
	# InitWatershedsAreas / WatershedsInputAndState / ReachesOutputs
	
	# 1/ let's extract data/information from the Init context
	initcontext = j2kproject[[.j2kGetContextLoc(j2kproject, get(".j2k.initContext", envir=j2k.env))]]
	iiwa = .j2kGetContextLoc(initcontext, get(".j2k.context.InitWatershedsAreas", envir=j2k.env))
	if (!is.null(iiwa)){
		# get the station ids
		initcontextnames = unlist(lapply(initcontext[[iiwa]]$children, FUN=function(x){ x$attribute["name"] }))
		idstation = as.numeric(sapply(initcontextnames, function(x){ strsplit(x, "_")[[1]][2] }))
		# get the upstream watershed ids
		watersheds = lapply(initcontext[[iiwa]]$children, function(x){ xmlAttrs(x$children[[1]]$children[[2]])["value"] })
		watersheds = lapply(watersheds, function(x){ as.numeric(unlist(strsplit(x, ";"))) })
		names(watersheds) = paste0("station_", idstation)
	}else{
		idstation = NULL
		watersheds = NULL
	}
	# 1.2/ let's extract aggregated parameters from the Init context
	iiwd = .j2kGetContextLoc(initcontext, get(".j2k.context.WatershedsDistributedParameters", envir=j2k.env))
	if (!is.null(iiwa)){
    # make sure the station ids order match with the contexts order
		o = match(paste0(get(".j2k.context.DistributedPar_", envir=j2k.env), idstation), unlist(lapply(initcontext[[iiwd]]$children, function(x){xmlAttrs(x)[["name"]]})))
		if (!identical(o, seq_len(length(o)))){
			warning("In context '", get(".j2k.context.WatershedsDistributedParameters", envir=j2k.env),
			"', subcatchments order is inconsistent with '", get(".j2k.context.InitWatershedsAreas", envir=j2k.env), "' context")
		}
		
		# get the misc aggregated parameters computed here
		par_na = c()
		for (k in o){
			ci = .j2kGetContextLoc(initcontext[[iiwd]]$children[[k]], name=paste0(get(".j2k.component.WeightedSumAggregator_na_", envir=j2k.env), idstation[k], "_DP"))
			if (!is.null(ci)) {
			  si = .j2kGetContextLoc(initcontext[[iiwd]]$children[[k]]$children[[ci]], get(".j2k.component.WeightedSumAggregator.var.sum", envir=j2k.env))
			  par_na = c(par_na, xmlAttrs(initcontext[[iiwd]]$children[[k]]$children[[ci]]$children[[si]])[['attribute']])
			}
		}
		if (!is.null(par_na)) {
		  par_na = .cleanparam(par_na)
		  print(par_na)
		  if (length(par_na) != 1){ stop("in context '", get(".j2k.context.WatershedsDistributedParameters", envir=j2k.env), "', different 'misc' variables found depending on the station") }
		  par_na = par_na[[1]]
		}
		# get the liter variables computed here # FIX-ME: should not use absolute index value for children!
		par_L = c()
		for (k in o){
			ci = .j2kGetContextLoc(initcontext[[iiwd]]$children[[k]], name=paste0(get(".j2k.component.WeightedSumAggregator_L_", envir=j2k.env), idstation[k], "_DP"))
			if (!is.null(ci)) {
  			si = .j2kGetContextLoc(initcontext[[iiwd]]$children[[k]]$children[[ci]], get(".j2k.component.WeightedSumAggregator.var.sum", envir=j2k.env))
  			par_L = c(par_L, xmlAttrs(initcontext[[iiwd]]$children[[k]]$children[[ci]]$children[[si]])[['attribute']])
			}
		}
		if (!is.null(par_L)) {
  		par_L = .cleanparam(par_L)
  		if (length(par_L) != 1){ stop("in context '", get(".j2k.context.WatershedsDistributedParameters", envir=j2k.env), "', different 'liter' variables found depending on the station") }
  		par_L = par_L[[1]]
		}
	} else {
		par_na = NULL
		par_L = NULL
	}

	# 2/ let's extract data/information from the InputState context
	timecontext = j2kproject[[.j2kGetContextLoc(j2kproject, get(".j2k.timeContext", envir=j2k.env))]]
	iwis = .j2kGetContextLoc(timecontext, get(".j2k.context.WatershedsInputAndState", envir=j2k.env))
	if (!is.null(iiwa)){
		# make sure the station ids order match with the contexts order
		o = match(paste0(get(".j2k.context.ISOvar_", envir=j2k.env), idstation), unlist(lapply(timecontext[[iwis]]$children, function(x){xmlAttrs(x)[["name"]]})))
		if (!identical(o, seq_len(length(o)))){
			warning("In context '", get(".j2k.context.WatershedsInputAndState", envir=j2k.env),
			"', subcatchments order is inconsistent with '", get(".j2k.context.InitWatershedsAreas", envir=j2k.env), "' context")
		}
		# get the misc variables computed here
		var_na = c()
		for (k in o){
			ci = .j2kGetContextLoc(timecontext[[iwis]]$children[[k]], name=paste0(get(".j2k.component.WeightedSumAggregator_na_", envir=j2k.env), idstation[k], "_ISO"))
			if (!is.null(ci)) {
  			si = .j2kGetContextLoc(timecontext[[iwis]]$children[[k]]$children[[ci]], get(".j2k.component.WeightedSumAggregator.var.sum", envir=j2k.env))
  			var_na = c(var_na, xmlAttrs(timecontext[[iwis]]$children[[k]]$children[[ci]]$children[[si]])[['attribute']])
			}
		}
		if (!is.null(var_na)) {
		  var_na = sapply(var_na, function(x){ strsplit(x, ";") })
		  var_na = unique(lapply(var_na, function(x){ as.character(sapply(x, function(y){ strsplit(y, "_")[[1]][1] })) }))
		  if (length(var_na) != 1){ stop("in context '", get(".j2k.context.WatershedsInputAndState", envir=j2k.env), "', different 'misc' variables found depending on the station") }
		  var_na = var_na[[1]]
		}
		# get the liter variables computed here # FIX-ME: should not use absolute index value for children!
		var_L = c()
		for (k in o){
			ci = .j2kGetContextLoc(timecontext[[iwis]]$children[[k]], name=paste0(get(".j2k.component.WeightedSumAggregator_L_", envir=j2k.env), idstation[k], "_ISO"))
			if (!is.null(ci)) {
  			si = .j2kGetContextLoc(timecontext[[iwis]]$children[[k]]$children[[ci]], get(".j2k.component.WeightedSumAggregator.var.sum", envir=j2k.env))
  			var_L = c(var_L, xmlAttrs(timecontext[[iwis]]$children[[k]]$children[[ci]]$children[[si]])[['attribute']])
			}
		}
		if (!is.null(var_L)) {
		  var_L = sapply(var_L, function(x){ strsplit(x, ";") })
		  var_L = unique(lapply(var_L, function(x){ as.character(sapply(x, function(y){ strsplit(y, "_")[[1]][1] })) }))
		  if (length(var_L) != 1){ stop("in context '", get(".j2k.context.WatershedsInputAndState", envir=j2k.env), "', different 'liter' variables found depending on the station") }
		  var_L = var_L[[1]]
		}
	} else {
		var_na = NULL
		var_L = NULL
	}
	# 3/ let's extract data/information from the ReachOutput context
	reachcontext = timecontext[[.j2kGetContextLoc(timecontext, get(".j2k.reachContext", envir=j2k.env))]]
	iout = .j2kGetContextLoc(reachcontext, get(".j2k.context.ReachesOutputs", envir=j2k.env))
	if (!is.null(iout)){
		# get the computed variables names
		var_reach = as.character(unlist(lapply(reachcontext[[iout]]$children, function(x){ xmlAttrs(x)['name'] })))
		# get the reach id of each station
		reach_id = unique(lapply(reachcontext[[iout]]$children, function(x){
			i = .j2kGetContextLoc(x, "values")
			xmlAttrs(x$children[[i]])['value'] 
		}))
		if (length(reach_id) != 1){ stop("in context '", get(".j2k.context.ReachesOutputs", envir=j2k.env), "', different 'reach id' found depending on the variable") }
		reach_id = as.numeric(strsplit(reach_id[[1]], ";")[[1]])	
	} else {
		var_reach = NULL
		reach_id = NULL
	}
	# 4/ create final object
	if (!is.null(idstation)){
		subcatchs = list()
		for (k in 1:length(idstation)){
			subcatchs[[k]] = list("id"=idstation[k], "reach"=reach_id[k], "watersheds"=watersheds[[k]])
		}
		subcatchs = as.j2ksubcatch(subcatchs)
		outvariables = as.j2koutvar(list("misc"=var_na, "liter"=var_L, "reach"=var_reach))
        outparameters = as.j2koutpar(list("misc"=par_na, "liter"=par_L))
	} else {
		subcatchs = as.j2ksubcatch(NULL)
		outvariables = as.j2koutvar(list("misc"=c(), "liter"=c(), "reach"=c()))
		outparameters = as.j2koutpar(list("misc"=c(), "liter"=c()))
	}
	return(list("subcatch"=subcatchs, "outvariables"=outvariables, "outparameters"=outparameters))
}

# FIX ME: -----------------------------------------------------------
# ITS RECIPROCAL TO CODE
# -------------------------------------------------------------------
# In the J2K xml file, extract the variables listed in the datastore
# sections. These variables are those that will be printed to an
# output text file when running a simulation.
# =>  If any datastore section is found a named list (one element for
# each datastore) of character vector is returned. NULL if no datastore
# section are found.
# -------------------------------------------------------------------
# I. Horner / 2017-11-21 / 2017-11-22
# -------------------------------------------------------------------
.j2kXMLgetDataStores = function(j2kproject){
	ds = j2kproject[[which(names(j2kproject)==get(".j2k.tag.datastores", envir=j2k.env))]]
	n = length(names(ds))
	if (n>0) {
		dscontent = list()
		for (k in 1:length(names(ds))) {
			 tmp = unlist(xmlApply(ds[[k]][[1]], xmlAttrs))
			 names(tmp)=NULL
			 dscontent[[k]] = tmp
		}
		names(dscontent) = unlist(xmlApply(ds, function(x){xmlAttrs(x)["name"]}))
	} else {
		dscontent=NULL
	}
	return(dscontent)
}

# -------------------------------------------------------------------
# In the J2K xml file, extract the variables listed in the doubleSetter
# component. These variables are those that are re-setted to 0 at the
# beginning of each time step.
# =>  It returns a character vector containing all the variables listed
# in the doubleSetter component 'attribute' variable.
# -------------------------------------------------------------------
# I. Horner / 2017-12-03 / 2017-12-04
# -------------------------------------------------------------------
.j2kXMLgetDoubleSetter = function(j2kproject){
	timecontext = j2kproject[[.j2kGetContextLoc(j2kproject, name=get(".j2k.timeContext", envir=j2k.env))]]
	doublesetter = timecontext[[.j2kGetContextLoc(timecontext, name=get(".j2k.component.doubleSetter", envir=j2k.env))]]
	iattributes = .j2kGetContextLoc(doublesetter, 'attributes')
	dblsetvar = xmlAttrs(doublesetter[[iattributes]])["attribute"]
	dblsetvar = strsplit(dblsetvar, ";")[[1L]]
	return(dblsetvar)
}

# -------------------------------------------------------------------
# In the J2K xml file, given a character vector containing the variables
# names which have to be reset to 0 at each time step, edit the attribute
# of the attributes variable of the DoubleSetter component.
# => It returns the updated 'j2kproject' XML JAMS project.
# -------------------------------------------------------------------
# I. Horner / 2017-12-04 / 2017-12-04
# -------------------------------------------------------------------
.j2kXMLsetDoubleSetter = function(j2kproject, dblsetvar){
	timecontext = j2kproject[[.j2kGetContextLoc(j2kproject, name=get(".j2k.timeContext", envir=j2k.env))]]
	doublesetter = timecontext[[.j2kGetContextLoc(timecontext, name=get(".j2k.component.doubleSetter", envir=j2k.env))]]
	iattributes = .j2kGetContextLoc(doublesetter, 'attributes')
	dblsetvar = paste(dblsetvar, collapse=";")
	xmlAttrs(doublesetter[[iattributes]]) = c("attribute"=dblsetvar)
	timecontext[[.j2kGetContextLoc(timecontext, name=get(".j2k.component.doubleSetter", envir=j2k.env))]] = doublesetter
	j2kproject[[.j2kGetContextLoc(j2kproject, name=get(".j2k.timeContext", envir=j2k.env))]] = timecontext
	return(j2kproject)
}


# -------------------------------------------------------------------
# Given an XML (piece) of a J2K project, find the localization of an
# element where an attribute is equal to the named arguments given in
# .dots. The name of the argument in dots will be used as the attribute
# name to match in the XML file. Note that a default attribute 'name' 
# will be used in case the argument given in .dots. has no name.
# => It return the index of the localization or NULL.
# -------------------------------------------------------------------
# I. Horner / 2017-03-31 / 2017-11-22
# -------------------------------------------------------------------
.j2kGetContextLoc=function(xml, ...){ # FIXME: I should rename it .j2kGetLoc() or .j2kXMLgetLoc
    args = list(...)
    n = names(args)
    if (length(args)>1) {
        warning("More than one arguments given in .dots. Only first one is used.")
        n = n[1]
    }
    args = args[[1]]
    if (is.null(n)) n = "name" # default value, mainly for back compatibility
    atxml = lapply(xml$children, function(x, n) x$attribute[n], n=n)
    ind = which(atxml==args)
    if (length(ind)!=1) ind = NULL # FIXME: should I rather return the complete vector with a warning? A w
    return(ind)
}


# -------------------------------------------------------------------
# FIXME: check is it is really necessary. As only node creation is done
# by the functions in this script, it may be more relevant to move the
# the function to the j2kproject.R file. Moreover, there might be cleaner 
# way of editing the source XML file.
# 2017-11-27: 7 occurrences of '.j2kInsertChild'
#             2 occurrences of '.j2kInsertChildren'
# -------------------------------------------------------------------
# Insert a child in an xml object at a given location 'loc' where 'loc' 
# is the index of desired position, i.e. everything located after is
# moved downward, everything located before is unchanged.
# => It returns the updated XML object
# -------------------------------------------------------------------
# I. Horner / 2017-08-04 / 2017-08-04
# -------------------------------------------------------------------
.j2kInsertChild = function(xml, child, loc){
	if (class(child)[1] != "XMLNode"){stop("'child' must be a XML Node i.e. class(child)==\"XMLNode\"")}
	n = length(xml)
	if (loc > n){ # if the desired position is actually after all children
		tmp = xml$children
		tmp[[n+1]] = child
	}else{
		tmp = list()
		j = 1
		for (i in 1:n){
			if (i==loc){
				tmp[[j]] = child
				j = j + 1
			}
			tmp[[j]] = xml[[i]]
			j = j + 1
		}
	}
	xml$children = tmp
	return(xml)
}

# -------------------------------------------------------------------
# This function returns the names of important contexts of a J2K model.
# These names are necessary  to properly edit the XML JAMS project.
# It calls the j2kSetContextsNames() (in Editing.R file) function 
# to store the names in the global environment.
# -------------------------------------------------------------------
# FIX ME: glabal variables should be stored in a specific environment
# -------------------------------------------------------------------
# I. Horner | 2017-11-27
# -------------------------------------------------------------------
.j2kGetContextNames = function(xml, verbose=TRUE)
{
	xml_model = xml$children
	imc = which(as.logical(match(names(xml_model), "contextcomponent", nomatch=0)))
	if (length(imc) < 3) stop("Missing context in model... 
		'ParameterInput', 'Initialization' and 'TimeLoop' context should exist (their names don't matter).")
	if (length(imc) > 3) warning("Too many contexts in model... Three context were expected: 
		'ParameterInput', 'Initialization' and 'TimeLoop' (their names don't matter)")
	xml_time = xml_model[[imc[3]]]
	itc = which(as.logical(match(names(xml_time), "contextcomponent", nomatch=0)))
	if (length(itc) == 4) {
		itc = itc[c(1, 2, 4)] # there, contexts inputContext, hruContext,  watershedInputAndState and reachContext are expected
	} else if (length(itc) < 3) {
		stop("Missing context in time loop... 'TSInput', 'HRULoop' and 'ReachLoop' context should exist (their names don't matter).")
	} else if (length(itc) > 3) {
		warning("Too many contexts in time loop... Three or four contexts are expected: 
			'TSInput', 'HRULoop', 'watershedInputAndState'[Optional] and 'ReachLoop' (their names don't matter).")
		itc = itc[c(1, 2, 4)]
	}
	contextnames = rep(NA, 7)
	# retrieve ".j2k.modelName"
	contextnames[1] = xmlAttrs(xml)["name"]
	# retrieve ".j2k.paramContext"
	contextnames[2] = xmlAttrs(xml_model[[imc[1]]])["name"]
	# retrieve ".j2k.initContext"
	contextnames[3] = xmlAttrs(xml_model[[imc[2]]])["name"]
	# retrieve ".j2k.timeContext"
	contextnames[4] = xmlAttrs(xml_model[[imc[3]]])["name"]
	# retrieve ".j2k.inputContext"
	contextnames[5] = xmlAttrs(xml_time[[itc[1]]])["name"]
	# retrieve ".j2k.hruContext"
	contextnames[6] = xmlAttrs(xml_time[[itc[2]]])["name"]
	# retrieve ".j2k.reachContext"
	contextnames[7] = xmlAttrs(xml_time[[itc[3]]])["name"]
	contextdesc = c("Model", "Parameter files", "Initialization", "Time loop", "Forcing files", "HRU loop", "Reach loop")
	if (verbose) {
		message(".j2kGetContextNames: The following context names were found: ")
		for (k in 1:7) message(contextdesc[k], ":", rep(" ", 16-nchar(contextdesc[k])), contextnames[k])
		flush.console()
		message(".j2kGetContextNames: saving the context names...")
	}
	j2kSetContextsNames(modelName=contextnames[1], paramContext=contextnames[2], initContext=contextnames[3], 
		timeContext=contextnames[4], inputContext=contextnames[5], hruContext=contextnames[6], reachContext=contextnames[7])
    return(contextnames)
}



