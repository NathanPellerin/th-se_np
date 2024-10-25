
# -------------------------------------------------------------------
# Given a filepath to the HRU shapefile (folder and filename)
# > reproject data (Lamber93->LamberIIe) to fit SAFRAN data
# > retrieve the shapefile extend and apply an "increase" factor
# -------------------------------------------------------------------
# FIX ME: easier management of file name and file path
# easier management of projections.
# J2K projection code should be a global variable
# SAFRAN projection code can still be provided as an argument
# TYPO: EXTEND ==> EXTENT
# -------------------------------------------------------------------
# Ivan Horner | 2018-06-22
# -------------------------------------------------------------------
j2kGetSpatialExtend = function(hrufolder, hrufilename="hrus",
    projection="+init=epsg:2154", reprojection="+init=epsg:27572", fact=0.1, verbose=TRUE)
{

    # step 1: load and re-project hrus sp
    if (verbose) {message("Reading shapefile..."); flush.console()}
    hrus = readOGR(dsn=hrufolder, layer=hrufilename, p4s=projection, verbose=FALSE)
    if (!is.null(reprojection)) {
        if (verbose) {message("Re-projecting shapefile..."); flush.console()}
        hrus = spTransform(hrus, CRS(reprojection))
    }
    # step 2: get extent of hrus, increase extend using factor
    if (verbose) {message("Computing shapefile extend..."); flush.console()}
    extend = t(bbox(hrus))
    increasextend = apply(extend, 2, diff)*fact
    increasextend = matrix(c(-increasextend, increasextend), 2, 2, byrow=TRUE)
    extend = extend + increasextend
    # step 3: return extend
    return(extend)
}

# -------------------------------------------------------------------
# Given the object shp created by the j2kReadGIS() function:
# > reproject data (Lamber93->LamberIIe) to fit SAFRAN data
# > retrieve the shapefile extend and apply an "increase" factor
# -------------------------------------------------------------------
# FIX ME: easier management of file name and file path
# easier management of projections.
# J2K projection code should be a global variable
# SAFRAN projection code can still be provided as an argument
# -------------------------------------------------------------------
# Ivan Horner | 2018-06-22 | 2019-04-25
# -------------------------------------------------------------------
j2kGetSpatialExtent = function(shp, reprojection="+init=epsg:27572", fact=0.1, verbose=TRUE)
{

    # step 1: load and re-project hrus sp
    # if (verbose) {message("Reading shapefile..."); flush.console()}
    hrus = shp$hru
    if (!is.null(reprojection)) {
        if (verbose) {message("Re-projecting shapefile..."); flush.console()}
        hrus = spTransform(hrus, CRS(reprojection))
    }
    # step 2: get extent of hrus, increase extent using factor
    if (verbose) {message("Computing shapefile extent..."); flush.console()}
    extent = t(bbox(hrus))
    increasextend = apply(extent, 2, diff)*fact
    increasextend = matrix(c(-increasextend, increasextend), 2, 2, byrow=TRUE)
    extent = extent + increasextend
    # step 3: return extent
    return(extent)
}

# -------------------------------------------------------------------
# Given the filepath to one of the NCDF files and the extend box of 
# the given model area (retrieved with j2kGetSpatialExtend), it
# returns a data.frame with SAFRAN cell IDs and their corresponding
# X and Y coordinates in Lambert93
# -------------------------------------------------------------------
# FIX ME: easier management of file name and file path
# TYPO: EXTEND ==> EXTENT
# -------------------------------------------------------------------
# Ivan Horner | 2018-06-22
# -------------------------------------------------------------------
j2kGetCellsFromExtend = function(ncdffp, extend, verbose=TRUE)
{
    if (verbose) { message("Opening NCDF file ..."); flush.console() }
    # open NCDF file
    ncdf = nc_open(ncdffp)
    # get cell ids that within the provided extend
    if (verbose) { message("Getting cells ids from spatial extend ..."); flush.console() }
    XY = data.frame("x"=ncvar_get(ncdf, varid="LambXg"), "y"=ncvar_get(ncdf, varid="LambYg"))
    ids = which(XY[, 1]>=extend[1, 1] & XY[, 1]<=extend[2, 1] & XY[, 2]>=extend[1, 2] & XY[, 2]<=extend[2, 2])
    cells = ncdf$dim[["Cell number"]]$vals[ids]
    if (length(cells)==0) {
        stop("No cells are within the provided spatial extend. Check projection.")
    }
    # cells = data.frame(ID=cells, X=XY[ids, 1], Y=XY[ids, 2])
    # create the shapefile of cell centroids
    if (!require(rgdal)){
        install.packages("rgdal")
        if (!require(rgdal)) stop("The rgdal package is required")
    }
    XY = XY[ids, ]
    coordinates(XY) = c("x", "y")
    proj4string(XY) = CRS("+init=epsg:27572")
    XY = spTransform(XY, CRS("+init=epsg:2154"))
    XY = coordinates(XY)
    cells = data.frame(ID=cells, X=XY[, 1], Y=XY[, 2])
    return(cells)
}

# -------------------------------------------------------------------
# Function to retrieve data from a NCDF file. NCDF format is as 
# J.P. Vidal created the NCDF file in June 2018.
# -------------------------------------------------------------------
# Ivan Horner | 2018-06-22
# -------------------------------------------------------------------
j2kGetSafranNcdfData = function(fp, from, to, cells, extend, verbose=TRUE)
{
    if (verbose) { message("Opening NCDF file ..."); flush.console() }
    # open NCDF file
    ncdf = nc_open(fp)

    # if extend is provided
    if (!missing(extend)) {
        if (verbose) { message("Getting cells ids from spatial extend ..."); flush.console() }
        XY = data.frame("x"=ncvar_get(ncdf, varid="LambXg"), "y"=ncvar_get(ncdf, varid="LambYg"))
        ids = which(XY[, 1]>=extend[1, 1] & XY[, 1]<=extend[2, 1] & XY[, 2]>=extend[1, 2] & XY[, 2]<=extend[2, 2])
        cells = ncdf$dim[["Cell number"]]$vals[ids]
        if (length(cells)==0) stop("No cells are within the provided spatial extend. Check projection.")
    }
    
    # get time reference
    ref_time = ncdf$dim[["Time"]]$units
    ref_time = unlist(strsplit(ref_time, " "))
    ref_time = ref_time[length(ref_time)]
    ref_time = as.POSIXct(strptime(ref_time, format=c("%Y-%m-%d"), tz="UTC"))

    # compute time vector
    time_dim_length = ncdf$dim[[3]]$len
    time_vector = seq(from=ref_time, length.out=time_dim_length+1, by="days")
    time_vector = time_vector[-1]
    time_start <- time_vector[1]
    time_end <- time_vector[length(time_vector)]
    

    # Get 'start' and 'count' from specified wanted time range
    if (missing(from)) from = time_start
    i_start = which(time_vector == from)
    if (length(i_start) == 0) {
        message("'from' is invalid. First date found in SAFRAN taken instead: ", format(time_start, format=c("%Y-%m-%d")))
        i_start = 1
    }
    if (missing(to)) to = time_end
    i_end = which(time_vector == to)
    if (length(i_end) == 0) {
      message("'to' is invalid. Last date found in SAFRAN taken instead: ", format(time_end, format=c("%Y-%m-%d")))
        i_end = length(time_vector)
    }
    count = i_end - i_start + 1
    time_vector = time_vector[i_start:i_end]

    # check provided cell ids
    # if (missing(cells)) cells = 1:ncdf$dim$cell$len
    if (missing(cells)) cells = 1:ncdf$dim$`Cell number`$len
    cell_ids = ncdf$dim[["Cell number"]]$vals
    matching_cells = intersect(cells, cell_ids)
    i_nomatch = which(is.na(match(cells, matching_cells)))
    if (length(i_nomatch)>0) {
      message(paste0("Some wanted cells were not found. Please check provided cell ids.\nThe unvalid id(s) is/are: ",
            paste(cells[i_nomatch], collapse=", ")))
    }

    # retrieve data
    myvar = matrix(NA, count, length(matching_cells))
    if (verbose) {
        # cat("\n")
        for (k in 1:length(matching_cells)) {
            cat(paste0("\r[", round(k/length(matching_cells)*100, 0), " %] Retrieving cell ", matching_cells[k], "...     ")); flush.console()
            myvar[, k] = ncvar_get(ncdf, start=c(matching_cells[k], i_start), count=c(1, count))
        }
        cat("\n")
    } else { # if verbose is False, it will increase retrieval speed in some cases (if number of cell is large and time range short)
        for (k in 1:length(matching_cells)) {
            myvar[, k] = ncvar_get(ncdf, start=c(matching_cells[k], i_start), count=c(1, count))
        }
    }
    colnames(myvar) = matching_cells

    # close ncdf and return time and values
    nc_close(ncdf)

    return(list("time"=time_vector, "data"=myvar))
}

# -------------------------------------------------------------------
# FIX ME: add 'verbose' argument
# -------------------------------------------------------------------
# Wrapper function to create the J2K forcing files from the NCDF files
# It needs the directoru to the SAFRAN data, the SAFRAN file names, 
# the directory of the J2K project, the time range, the required 
# SAFRAN cells, the output forcing filenames and a header comment as 
# arguments.
# -------------------------------------------------------------------
# Ivan Horner | 2018-06-22
# -------------------------------------------------------------------
j2kFromSafranToJ2K = function(safDataDir, j2kProjDir, time_range, cell_info, forcing_filenames, header_comments, 
    ncfilename=c("safran_new_Rainf.nc", "safran_new_Snowf.nc", "safran_new_ET0.nc", "safran_new_Tair.nc"))
{
    # test if packages were loaded (XML and ncdf)
    if (!require(XML)) stop("The 'XML' package is required. Please run 'install.packages(\"XML\")' to install it.")
    if (!require(ncdf4)) stop("The 'ncdf4' package is required. Please run 'install.packages(\"ncdf4\")' to install it.")  

    # Precipitation: ----------------------------------------------------
    message("Reading Rain and Snow NCDF files..."); flush.console()
    R = j2kGetSafranNcdfData(fp=paste0(safDataDir, ncfilename[1]), from=time_range[1], to=time_range[2], cells=cell_info[, 1])
    S = j2kGetSafranNcdfData(fp=paste0(safDataDir, ncfilename[2]), from=time_range[1], to=time_range[2], cells=cell_info[, 1])
    P = list(time=R$time, data=R$data+S$data)

    # Evapotranspiration of reference: ----------------------------------
    message("Reading Reference evapotranspiration NCDF files..."); flush.console()
    E = j2kGetSafranNcdfData(fp=paste0(safDataDir, ncfilename[3]), from=time_range[1], to=time_range[2], cells=cell_info[, 1])

    # Air temperature: --------------------------------------------------
    message("Reading Air temperature NCDF files..."); flush.console()
    T = j2kGetSafranNcdfData(fp=paste0(safDataDir, ncfilename[4]), from=time_range[1], to=time_range[2], cells=cell_info[, 1])
    T$data = T$data - 273.15

    # Writting J2K input files: -----------------------------------------
    INPUT_DATA = list(P, E, T)
    variables = list(
    list(unit="mm", maxrange=c(0, 9999)),
    list(unit="Deg", maxrange=c(-273, 9999)),
    list(unit="mm", maxrange=c(0, 9999))
    )
    for (k in 1:3) {
        message("Writting J2K forcing file \"", forcing_filenames[k], "\""); flush.console()
        j2kWriteInputs(	folder=paste0(j2kProjDir, "input/"), 	# folder where the forcing file is to be written
                        fileName=forcing_filenames[k],			# filename of the forcing file
                        data=INPUT_DATA[[k]],								# a list containing elements 'time' (vector POSIX) and 'data' (matrix/data.frame)
                        dataAttributes=cell_info,				# a list containing with columns "X", "Y" and "Elevation" (a row per mesh)
                        na="-9999",								# na code
                        variableName=forcing_filenames[k],		# name of the variable (metadata of the forcing file)
                        variableUnit=variables[[k]]$unit,		# unit of the variable (metadata of the forcing file)
                        variableMaxRange=variables[[k]]$maxrange, # value range of the variable (metadata of the forcing file)
                        timeResolution="d",						# resolution ("d" stand for days)
                        header=header_comments)					# header comment of the forcing file
    }
}


# -------------------------------------------------------------------
# Read a parameter file of J2K i.e. .par files
# => It returns a data.frame
# -------------------------------------------------------------------
# I. Horner / 2017-03-29 / 2017-07-24
# -------------------------------------------------------------------
j2kReadPar = function(fp, ignore_incomplete=TRUE){
	# typical layout of .par file:
	#  - comment lines '#'
	#  - variable names
	#  - ?
	#  - variable n/a code
	#  - variable unit
	#  - values ...
	#  - comment lines '#'
	rawdata = readLines(fp, warn=FALSE)
	start_end = which(substr(rawdata, 1, 1)=="#")
	start_end = start_end[which(diff(start_end)!=1)[1]+c(0,1)]+c(1, -1)
	if (diff(start_end)<4) stop("Error while reading .par file. No data found.")
	var_names=unlist(strsplit(rawdata[start_end[1]], "\t"))
	var_units=unlist(strsplit(rawdata[start_end[1]+3], "\t"))
	var_units[which(var_units=="n/a")]=NA
	var_values=strsplit(rawdata[(start_end[1]+4):start_end[2]], "\t")
    # check if all lines are complete
    iuncomplete = which(unlist(lapply(var_values, length))!=length(var_names))
    if (length(iuncomplete)>0) {
        action = ifelse(ignore_incomplete, "They were ignored. ", "They were filled with NA. ")
        action = paste0(action, 'see argument \'ignore_incomplete\'.\n')
        warning(paste0("Some rows were found to be incomplete. ", action, "\nRow(s):", paste(iuncomplete, collapse=", ")))
        if (ignore_incomplete) {
            var_values = var_values[-iuncomplete]
        } else {
            var_values[iuncomplete] = lapply(var_values[iuncomplete], function(e, n) c(e, rep(NA, n-length(e))), n=length(var_names))
        }
    }
	var_values=as.data.frame(matrix(as.numeric(unlist(var_values)), length(var_values), length(var_names), byrow=TRUE))
	colnames(var_values)=var_names
	return(var_values)
}

# -------------------------------------------------------------------
# Write a parameter file of J2K i.e. .par files
# -------------------------------------------------------------------
# I. Horner / 2017-08-09 / 2018-06-19
# -------------------------------------------------------------------
j2kWritePar = function(fp, data, comment_line=paste0("edited with j2k-rui | ", format(Sys.time(), format="%Y-%m-%d %H:%M")), units=NULL){
    if (is.null(dim(data))) data = matrix(data, 1, length(data))
	comment_line = paste0(get(".j2k.commentChar", envir=j2k.env), comment_line)
	varname_line = paste(colnames(data), collapse="\t")
	unknown_line_1 = paste(rep(0, ncol(data)), collapse="\t")
	unknown_line_2 = paste(rep(999999, ncol(data)), collapse="\t")
	if (is.null(units)){units = rep("n/a", ncol(data))}
	if (length(units)!=ncol(data)){stop("Length of 'units' must match the number of variables in 'data'")}
	units_line = paste(units, collapse="\t")
	end_comment_line = "# end of reach.par"
	data_lines = apply(data, 1, function(x){paste(x, collapse="\t")})
	writeLines(c(comment_line, varname_line, unknown_line_1, unknown_line_2, units_line, data_lines, end_comment_line), con = fp)
}

# -------------------------------------------------------------------
# General function to read .dat file. It finds all the sections of the file
# and print the comment lines. 
# => It returns a list of list containing the .dat file content
# -------------------------------------------------------------------
# I. Horner / 2017-08-10 / 2017-08-10
# -------------------------------------------------------------------
.j2kReadDatFile = function(fp, sep="\t", verbose=TRUE){
	if (verbose){message(".j2kReadDatFile: reading '", basename(fp), "' file ...\nIn: ", dirname(fp)); flush.console()}
	# 0/ read file
	raw_lines = readLines(con=fp)
	if (verbose){message(".j2kReadDatFile: formatting ..."); flush.console()}
	# 1/ split each line according to sep
	raw_lines = strsplit(raw_lines, sep)
	# 2/ fetch first elements
	first_elem = unlist(lapply(raw_lines, function(x){x[1]}))
	# 3/ fetch first character of each fisrt element
	first_char = substr(first_elem, 1, 1)
	# 4 / find comment lines and print them
	ind_com = which(first_char==get(".j2k.commentChar", envir=j2k.env))
	if (length(ind_com)){
		comments = raw_lines[ind_com]
		if (verbose){message(".j2kReadDatFile: comments found:"); lapply(comments, print); flush.console()}
	}
	if (verbose){message(".j2kReadDatFile: fetching file sections ..."); flush.console()}
	# 5/ find sections: start of section, length of section, name of section
	ind_sec = which(first_char==get(".j2k.sectionChar", envir=j2k.env))
	name_sec = unlist(lapply(strsplit(first_elem[ind_sec], get(".j2k.sectionChar", envir=j2k.env)), function(x){x[[2]]}))
	if (ind_sec[length(ind_sec)]!=length(raw_lines)){ind_sec=c(ind_sec,length(raw_lines))}
	len_sec = diff(ind_sec)
	# 6/ create the output list
	out_dat = list()
	for (k in 1:length(len_sec)){
		if (len_sec[k]>1){
			out = raw_lines[(ind_sec[k]+1):(ind_sec[k]+len_sec[k]-1)]
		}else{
			out = NULL
		}
		out_dat[[name_sec[k]]] = out
	}
	return(out_dat)
}


# -------------------------------------------------------------------
# Read an output .dat file of J2K
# => It returns a data.frame
# -------------------------------------------------------------------
# I. Horner / 2017-08-03 / 2017-11-08
# -------------------------------------------------------------------
j2kReadOutputs = function(fp, verbose=TRUE){
	dat = .j2kReadDatFile(fp)
	# get variable names
	var_names = dat[[get(".j2k.outputs.attributes", envir=j2k.env)]][[1]]
	# get data
	raw_data = dat[[get(".j2k.outputs.data", envir=j2k.env)]]
	# format into a matrix
	mat_data = do.call(rbind, raw_data)
	# extract time (if first colunm is time)
	data_time = mat_data[, 1]
	if (nchar(data_time[1])==16){ # This is a workaround. A tryCatch() might be a better option.
		if (verbose){message("Converting time ..."); flush.console()}
		# data_time = chron(substr(data_time, 1, 10), paste0(substr(data_time, 12, 16), ":00"), format=c("y-m-d", "h:m:s"))
		data_time = strptime(data_time, format="%Y-%m-%d %H:%M", tz="UTC")
		if (verbose){message("Creating final data.frame ..."); flush.console()}
		df_data = data.frame("Time"=data_time, t(apply(mat_data[,-1], 1, as.numeric)))
		colnames(df_data) = c("Time", var_names[-1])
	}else{
		if (verbose){message("Creating final data.frame ..."); flush.console()}
		df_data = data.frame(t(apply(array(mat_data[-1], dim=dim(mat_data)-c(0,1)), 1, as.numeric)))
		colnames(df_data) = var_names[-1]
	}
	return(df_data)
}


# -------------------------------------------------------------------
# Read an input .dat file of J2K
# => It returns a data.frame
# -------------------------------------------------------------------
# I. Horner / 2017-08-10 / 2017-08-10
# -------------------------------------------------------------------
j2kReadInputs = function(fp, verbose=TRUE){
	# .timeformatting=function(dates, times){chron(dates, paste0(times, ":00"), format=c("d.m.y", "h:m:s"))}
	dat = .j2kReadDatFile(fp)
	# get information on variable and data content
	if (verbose){message("Fetching information on data ..."); flush.console()}
	info_var=dat[[get(".j2k.inputs.valueAttributes", envir=j2k.env)]][[1]]
	info=list("Name"=info_var[1], "Range"=as.numeric(info_var[2:3]), "Unit"=info_var[4])
	info_file=dat[[get(".j2k.inputs.setAttributes", envir=j2k.env)]]
	info[["NAcode"]]=info_file[[1]][2]
	info[["TimeRange"]]=c(info_file[[2]][2], info_file[[3]][2])
	# info[["TimeRange"]]=.timeformatting(substr(info[["TimeRange"]], 1, 10), substr(info[["TimeRange"]], 12, 16))
	info[["TimeRange"]]=strptime(info[["TimeRange"]], format="%d.%m.%Y %H:%M")
	info[["TimeResolution"]]=info_file[[4]][2]
	# get information on variable and data content
	if (verbose){message("Fetching columns attributes ..."); flush.console()}
	attrs_col=do.call(rbind, dat[[get(".j2k.inputs.stateAttributes", envir=j2k.env)]])
	rownames(attrs_col) = attrs_col[,1]
	attrs_col = t(apply(attrs_col[,-1], 1, as.numeric))
	# get data and time
	mat_data = do.call(rbind, dat[[get(".j2k.inputs.data", envir=j2k.env)]])
	data_time = mat_data[,c(1,2)]
	# data_time = .timeformatting(data_time[,1], data_time[,2])
	data_time = strptime(apply(data_time, 1, paste), format="%d.%m.%Y %H:%M")
	mat_data =  t(apply(mat_data[,-c(1,2)], 1, as.numeric))
	if (verbose){message("Creating final data.frame ..."); flush.console()}
	df_data = data.frame("Time"=data_time, mat_data)
	colnames(df_data) = c("Time", attrs_col["name", ])
	return(list("info"=info, "attributes"=attrs_col, "data"=df_data))
}

# -------------------------------------------------------------------
# Write the .dat and .xml file of input of j2k for a given variable
# taking as inputs the outputs of functions 'j2kSelectSafranMesh()'
# and 'j2kPreprocessData()'.
# -------------------------------------------------------------------
# I. Horner / 2017-08-03 / 2017-08-03
# -------------------------------------------------------------------
j2kWriteInputs = function(folder, fileName, data, dataAttributes, variableName, variableUnit, variableMaxRange, na="-9999",
	timeResolution="d", header="j2k-RUI", parseTime=NULL, timeFormat=NULL, dataFolderName="local", verbose=TRUE)
{
	if (!("time"%in%names(data))) stop("No element 'time' was found in data")
	if (!("data"%in%names(data))) stop("No element 'data' was found in data")
	if (ncol(data$data)!=nrow(dataAttributes)) stop("Number of attribute values doesn't match the number of mesh in 'data'")
	if (!("X"%in%colnames(dataAttributes))) stop("No column named 'X' was found in 'dataAttributes'")
	if (!("Y"%in%colnames(dataAttributes))) stop("No column named 'Y' was found in 'dataAttributes'")
	if (!("Elevation"%in%colnames(dataAttributes))) stop("No column named 'Elevation' was found in 'dataAttributes'")
	if (is.null(colnames(data$data))) stop("Element 'data' in data must have column names (corresponding to mesh id)")
	if (!dir.exists(paste0(folder, dataFolderName, "/"))){
		warning(paste0("'", dataFolderName, "' directory was created in '", folder, "'."))
		dir.create(paste0(folder, dataFolderName, "/"))
	}
	if (verbose){message("Writing data and attributes in .dat file '", fileName, "' ..."); flush.console()}
    .j2kWriteDatFile(data=data, fp=paste0(folder, dataFolderName, "/", fileName, ".dat"), variable_name=variableName,
		variable_unit=variableUnit, variable_maxrange=variableMaxRange, x=dataAttributes[,"X"], y=dataAttributes[,"Y"], 
		elevation=dataAttributes[,"Elevation"], missing_data_code=na, time_resolution=timeResolution, header_comment=header)
	if (verbose){message("Creating associated XML file ..."); flush.console()}
    .j2kWriteDatXMLfile(fp=paste0(folder, fileName, ".xml"), parsetime=ifelse(is.null(parseTime),
		TRUE, parseTime), timeformat=ifelse(is.null(timeFormat), get(".j2k.timeSeriesDataStore.timeformat.code", envir=j2k.env), timeFormat))
}

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# I. Horner / 2017-08-01 / 2017-08-03
# -------------------------------------------------------------------
.j2kWriteDatFile = function(data, fp, variable_name, variable_unit, variable_maxrange, x, y, elevation,
		missing_data_code="-9999", time_resolution="d", header_comment="j2k-Rui / ih"){
	x = round(x)
	y = round(y)
	data$data = round(data$data, 2)
	col_names = colnames(data$data)
	col_id = 1:length(col_names)
	attribs = list()
	attribs[[get(".j2k.dataValueAttribs", envir=j2k.env)]] = list("name"=variable_name, "range"=variable_maxrange, "unit"=variable_unit)
	attribs[[get(".j2k.dataSetAttribs", envir=j2k.env)]] = list("na_code"=missing_data_code, "date_start"=min(data$time), "date_end"=max(data$time), "time_res"=time_resolution)
	attribs[[get(".j2k.statAttribVal", envir=j2k.env)]] = list("name"=col_names, "ID"=col_id, "elevation"=elevation, "x"=x, "y"=y, "dataColumn"=col_id)
	res = list()
	res[["data"]] = cbind(data.frame("Time"=data$time), data$data)
	res[["attributes"]] = attribs

	cat(paste0(get(".j2k.commentChar", envir=j2k.env), header_comment, "\n"), file=fp, sep="\t", append=FALSE)
	cat(paste0(get(".j2k.sectionChar", envir=j2k.env), get(".j2k.dataValueAttribs", envir=j2k.env), "\n"), file=fp, sep="\t", append=TRUE)
	cat(unlist(res[["attributes"]][[get(".j2k.dataValueAttribs", envir=j2k.env)]]), file=fp, sep="\t", append=TRUE)
	cat("\n", file=fp, sep="\t", append=TRUE)
	cat(paste0(get(".j2k.sectionChar", envir=j2k.env), get(".j2k.dataSetAttribs", envir=j2k.env), "\n"), file=fp, sep="\t", append=TRUE)
	cat(c(get(".j2k.dataSetAttribs.missingDataVal", envir=j2k.env), res[["attributes"]][[get(".j2k.dataSetAttribs", envir=j2k.env)]]$na_code), file=fp, sep="\t", append=TRUE)
	cat("\n", file=fp, sep="\t", append=TRUE)
	cat(c(get(".j2k.dataSetAttribs.dataStart", envir=j2k.env), strftime(res[["attributes"]][[get(".j2k.dataSetAttribs", envir=j2k.env)]]$date_start, format="%d.%m.%Y\t%H:%M", tz="UTC")), file=fp, sep="\t", append=TRUE)
	cat("\n", file=fp, sep="\t", append=TRUE)
	cat(c(get(".j2k.dataSetAttribs.dataEnd", envir=j2k.env), strftime(res[["attributes"]][[get(".j2k.dataSetAttribs", envir=j2k.env)]]$date_end, format="%d.%m.%Y\t%H:%M", tz="UTC")), file=fp, sep="\t", append=TRUE)
	cat("\n", file=fp, sep="\t", append=TRUE)
	cat(c(get(".j2k.dataSetAttribs.timeRes", envir=j2k.env), time_resolution), file=fp, sep="\t", append=TRUE)
	cat("\n", file=fp, sep="\t", append=TRUE)
	cat(paste0(get(".j2k.sectionChar", envir=j2k.env), get(".j2k.statAttribVal", envir=j2k.env), "\n"), file=fp, sep="\t", append=TRUE)
	cat(c(get(".j2k.statAttribVal.name", envir=j2k.env), res[["attributes"]][[get(".j2k.statAttribVal", envir=j2k.env)]]$name), file=fp, sep="\t", append=TRUE)
	cat("\n", file=fp, sep="\t", append=TRUE)
	cat(c(get(".j2k.statAttribVal.id", envir=j2k.env), res[["attributes"]][[get(".j2k.statAttribVal", envir=j2k.env)]]$ID), file=fp, sep="\t", append=TRUE)
	cat("\n", file=fp, sep="\t", append=TRUE)
	cat(c(get(".j2k.statAttribVal.elevation", envir=j2k.env), res[["attributes"]][[get(".j2k.statAttribVal", envir=j2k.env)]]$elevation), file=fp, sep="\t", append=TRUE)
	cat("\n", file=fp, sep="\t", append=TRUE)
	cat(c(get(".j2k.statAttribVal.x", envir=j2k.env), res[["attributes"]][[get(".j2k.statAttribVal", envir=j2k.env)]]$x), file=fp, sep="\t", append=TRUE)
	cat("\n", file=fp, sep="\t", append=TRUE)
	cat(c(get(".j2k.statAttribVal.y", envir=j2k.env), res[["attributes"]][[get(".j2k.statAttribVal", envir=j2k.env)]]$y), file=fp, sep="\t", append=TRUE)
	cat("\n", file=fp, sep="\t", append=TRUE)
	cat(c(get(".j2k.statAttribVal.dataColumn", envir=j2k.env), res[["attributes"]][[get(".j2k.statAttribVal", envir=j2k.env)]]$dataColumn), file=fp, sep="\t", append=TRUE)
	cat("\n", file=fp, sep="\t", append=TRUE)
	cat(paste0(get(".j2k.sectionChar", envir=j2k.env), get(".j2k.dataVal", envir=j2k.env), "\n"), file=fp, sep="\t", append=TRUE)

	tv = strftime(res[["data"]][,1], format="%d.%m.%Y\t%H:%M", tz="UTC")
	y = cbind(tv, res[["data"]][,-1])
	write.table(y, file=fp, append=TRUE, quote=FALSE, sep="\t", na=missing_data_code, row.names=FALSE, col.names=FALSE)

}

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# I. Horner / 2017-08-01 / 2017-08-03
# -------------------------------------------------------------------
.j2kWriteDatXMLfile = function(fp, parsetime=TRUE, timeformat=get(".j2k.timeSeriesDataStore.timeformat.code", envir=j2k.env)){
	datxml = xmlNode(get(".j2k.timeSeriesDataStore", envir=j2k.env))
	datxml = addChildren(datxml, xmlNode(get(".j2k.timeSeriesDataStore.parsetime", envir=j2k.env), attrs=c("value"=parsetime)))
	datxml = addChildren(datxml, xmlNode(get(".j2k.timeSeriesDataStore.timeformat", envir=j2k.env), attrs=c("value"=timeformat)))
	saveXML(datxml, file=fp, prefix=get(".j2k.xmlprefix", envir=j2k.env))
}


# this function re-organize the result by catchment and create, in a given folder
# one .RData by catchment. It requires to have both the static 'J2K_RHONE.dat'
# and the time series 'TimeLoop.dat' J2K result files; in particular, in
# 'J2K_RHONE.dat', areas of each sub-catchment must be stored!
j2kReOrganizeOutputs = function(simdir, simname, tardir, catchmentids,
                                areavarname="Area", reachvarname=c('outRD1', 'outRD2', 'outRG1', 'Runoff'),
                                timevar="Time") {
  
  # -------------------------------------------------------------------
  # step 1: read result files
  rawres_ts = j2kReadOutputs(paste0(simdir, "TimeLoop.dat"))
  rawres_st = j2kReadOutputs(paste0(simdir, "J2K_RHONE.dat"))
  
  # -------------------------------------------------------------------
  # step 2: create the static element of the output list
  catchment_area = c()
  catchment_para = list()
  
  # get colnames
  cn = colnames(rawres_st)
  scn = strsplit(cn, "_")
  idcn = unlist(lapply(scn, function(e) if (length(e)>1) e[length(e)] else NA))
  vrcn = unlist(lapply(scn, function(e) if (length(e)>2) paste(e[-length(e)], collapse="_") else e[1]))
  for (k in 1:length(catchmentids)) {
    catchid = catchmentids[[k]]
    # get area of current catchment
    areacolid = which(cn==paste0(areavarname, "_", catchid))
    if (length(areacolid) != 1) {
      stop("Area value of catchment ", catchid, " not found (", 
           paste0(areavarname, "_", catchid), " not found).")
    }
    catchment_area[k] = rawres_st[, areacolid]
    # get all static variables / parameters of current catchment
    icurrent = which(idcn==catchid)
    allvar = rawres_st[, icurrent]
    names(allvar)=vrcn[icurrent]
    catchment_para[[k]] = allvar
  }
  names(catchment_area) = names(catchment_para) = catchmentids
  
  # -------------------------------------------------------------------
  # step 3: create the time series element of the output list
  timseries = list()
  
  # get colnames
  # rawres_ts_tmp = rawres_ts
  # rawres_ts = rawres_ts_tmp
  
  cn = colnames(rawres_ts)
  scn = strsplit(cn, "_")
  idcn = unlist(lapply(scn, function(e) if (length(e)>1) e[length(e)] else NA))
  vrcn = unlist(lapply(scn, function(e) if (length(e)>2) paste(e[-length(e)], collapse="_") else e[1]))
  timeid = which(cn == timevar)
  for (k in 1:length(catchmentids)) {
    catchid = catchmentids[[k]]
    # get reach variables
    reachvarids = paste0(reachvarname, "_", catchid)
    reachvarids = match(reachvarids, cn)
    if (sum(is.na(reachvarids))!=0) {
      if (sum(is.na(reachvarids))==length(reachvarids)) {
        stop("All reach variables were not found for catchment ", catchid)
      } else {
        warning("The reach variables ",  paste(reachvarname[is.na(reachvarids)], collapse=", "), " were not found for catchment ", catchid)
        reachvarids = reachvarids[!is.na(reachvarids)]
      }
    }
    # divide these variable by catchment area
    # VAR[mm] = (((VAR[L] / 1000)[m3] / AREA[m2])[m] * 1000)[mm] = VAR_L / AREA
    rawres_ts[, reachvarids] = rawres_ts[, reachvarids] / catchment_area[k]
    # get all variables of current catchment
    icurrent = which(idcn==catchid)
    allvar = rawres_ts[, c(timeid, icurrent)]
    names(allvar)=c(timevar, vrcn[icurrent])
    timseries[[k]] = allvar
  }
  names(timseries) = catchmentids
  
  
  # summary(timseries)
  # summary(timseries[[5]])
  
  
  # -------------------------------------------------------------------
  # step 4: create the final list
  allcatchment = list()
  for (k in 1:length(catchmentids)) {
    allcatchment[[k]] = list("Timeseries"=timseries[[k]], "Static"=catchment_para[[k]])
  }
  
  
  # -------------------------------------------------------------------
  # step 5: write data to file (RData)
  tarfp = paste0(tardir, simname, "/")
  dir.create(tarfp, recursive=TRUE)
  for (k in 1:length(catchmentids)) {
    simdata = allcatchment[[k]]
    save(simdata, file=paste0(tarfp, catchmentids[[k]], ".RData"))
  }
  
}


