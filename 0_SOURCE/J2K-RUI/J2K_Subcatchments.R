
# -------------------------------------------------------------------
# Required packages
# -------------------------------------------------------------------
# Shapefiles # FIX ME: only use rgdal library instead... 
# -------------------------------------------------------------------
# Reading GIS data
# => return a list of shapefile R object with HRUs, reach and station
# -------------------------------------------------------------------
# I. Horner / 2017-03-29 / 2017-07-24 / 2019-04-16
# -------------------------------------------------------------------
# FIX ME: use of missing() is more relevant here instead of testing NULL
# Use the rgdal library instead. Make this function an internal function
# The user only has to call the j2kGetSubcatchment function
# -------------------------------------------------------------------
j2kReadGIS = function(hrufp=NULL, reachfp=NULL, stationfp=NULL, subcatchfp=NULL, verbose=TRUE){
	if (!is.null(hrufp)){
		if (verbose){message("Reading hru shapefile..."); flush.console()}
		hru =  readOGR(dirname(hrufp), layer=basename(hrufp), verbose=verbose, integer64="allow.loss", stringsAsFactors=FALSE)
	}else{hru=NULL}
	if (!is.null(reachfp)){
  	if (verbose){message("Reading reach shapefile..."); flush.console()}
  	reach = readOGR(dirname(reachfp), layer=basename(reachfp), verbose=verbose, integer64="allow.loss", stringsAsFactors=FALSE)
	}else{reach=NULL}
	if (!is.null(stationfp)){
  	if (verbose){message("Reading hydrometric station shapefile..."); flush.console()}
  	station =  readOGR(dirname(stationfp), layer=basename(stationfp), verbose=verbose, integer64="allow.loss", stringsAsFactors=FALSE)
	}else{station=NULL}
	if (!is.null(subcatchfp)){
  	if (verbose){message("Reading subcatchment (watershed) shapefile..."); flush.console()}
  	  subcatch =  readOGR(dirname(subcatchfp), layer=basename(subcatchfp), verbose=verbose, integer64="allow.loss", stringsAsFactors=FALSE)
	}else{subcatch=NULL}
	return(list("hru"=hru, "reach"=reach, "station"=station, "subcatch"=subcatch))
}

# -------------------------------------------------------------------
# FIX ME: should take a HRU-delin result directory as input
# -------------------------------------------------------------------
# From the GIS data (HRUs, reach and station), and the topology file
# reach.par, find upstream HRU ids, watershed ids and outlet  reach ids
# for all stations.
# => return a list of 'j2ksubcatch' objects
# -------------------------------------------------------------------
# I. Horner / 2017-03-29 / 2018-08-03 / 2019-04-16
# -------------------------------------------------------------------
j2kGetSubcatchment = function(shp, reachpar, stationid="IDALL", watershed="watershed", verbose=TRUE){
	stationid = shp$station@data[, stationid]
	if (verbose){message("Fetching outlet reach ids..."); flush.console()}
	reachids = .j2kGetOutletReachesFromShapefile(shp$station, reachidfield="ReachID")
	# Here, it is necessary to check if the outlet are within the reach.par file.
	# If not, the corresponding station are removed with a warning
	# If there is no station left, an error is sent
	valid_station = !is.na(match(reachids, reachpar[, "ID"]))
	if (sum(valid_station)==0) stop("For all station: the station outlet reach was not found in the reach topology.")
	if (sum(valid_station)!=length(stationid)) warning(paste0("The station outlet reach was not found in the reach topology for the following station: ", 
	                                                         paste(stationid[!valid_station], collapse=", ")))
	stationid = stationid[valid_station]
	reachids = reachids[valid_station]
	n = length(stationid)

	if (n != length(reachids)) stop("Number of outlet reach id doesn't match the number of hydrometric stations")
	if (verbose){message("Analyzing topology to find upstream stations..."); flush.console()}
	# upreachids = .j2kGetUpstreamReach(reachpar, reachids) # get upstream reaches
	upreachids = sapply(reachids, function(e, tbl) tbl[.j2kGetUpstreamElements(tbl, e), 1], tbl=reachpar[, c("ID", "to-reach")])
	usc = list()
	for (k in 1:length(upreachids)){ # match with stations
		ur = unique(c(reachids[k], upreachids[[k]]))
		usc[[k]] = stationid[which(reachids%in%ur)]
	}
	# create a list of 'j2ksubcatch' object 
	subcatchs = list()
	for (k in  1:length(reachids)){
		subcatch = list("id"=stationid[k], "reach"=reachids[k], "watersheds"=usc[[k]])
		subcatchs[[k]] = subcatch
	}
	subcatchs = as.j2ksubcatch(subcatchs)
	return(subcatchs)
}

# -------------------------------------------------------------------
# FIX ME: should take a HRU-delin result directory as input
# -------------------------------------------------------------------
# From the GIS data (HRUs, reach and station), and the topology file
# reach.par, find upstream HRU ids, watershed ids and outlet  reach ids
# for all stations.
# Another possibility is to provide the reach ids and station ids manually.
# => return a list of 'j2ksubcatch' objects
# -------------------------------------------------------------------
# I. Horner / 2017-03-29 / 2018-08-03 / 2019-04-16 / 2019-12-10
# -------------------------------------------------------------------
j2kGetSubcatchment = function(shp=NULL, reachpar, stationid="IDALL",station_ids=NULL, reach_ids=NULL, verbose=TRUE){
  if (is.null(shp)) {
    if (is.null(reach_ids) || is.null(station_ids)) stop("if 'shp' is null, arguments 'reach_ids' and 'station_ids' must be provided!")
  } else {
    station_ids = shp$station@data[, stationid]
  	if (verbose){message("Fetching outlet reach ids..."); flush.console()}
    reach_ids = .j2kGetOutletReachesFromShapefile(shp$station, reachidfield="ReachID")
  }
	# Here, it is necessary to check if the outlet are within the reach.par file.
	# If not, the corresponding station are removed with a warning
	# If there is no station left, an error is sent
	valid_station = !is.na(match(reach_ids, reachpar[, "ID"]))
	if (sum(valid_station)==0) stop("For all station: the station outlet reach was not found in the reach topology.")
	if (sum(valid_station)!=length(station_ids)) warning(paste0("The station outlet reach was not found in the reach topology for the following station: ", 
	                                                         paste(station_ids[!valid_station], collapse=", ")))
	station_ids = station_ids[valid_station]
	reach_ids = reach_ids[valid_station]
	n = length(station_ids)

	if (n != length(reach_ids)) stop("Number of outlet reach id doesn't match the number of hydrometric stations")
	if (verbose){message("Analyzing topology to find upstream stations..."); flush.console()}
	upreachids = sapply(reach_ids, function(e, tbl) tbl[.j2kGetUpstreamElements(tbl, e), 1], tbl=reachpar[, c("ID", "to-reach")])
	usc = list()
	for (k in 1:length(upreachids)){ # match with stations
		ur = unique(c(reach_ids[k], upreachids[[k]]))
		usc[[k]] = station_ids[which(reach_ids%in%ur)]
	}
	# create a list of 'j2ksubcatch' object 
	subcatchs = list()
	for (k in  1:length(reach_ids)){
		subcatch = list("id"=station_ids[k], "reach"=reach_ids[k], "watersheds"=usc[[k]])
		subcatchs[[k]] = subcatch
	}
	subcatchs = as.j2ksubcatch(subcatchs)
	return(subcatchs)
}


# -------------------------------------------------------------------
# FIX ME: adapt this function to the rgdal shapefile R object.
# -------------------------------------------------------------------
# if a column in the attribute table is named 'reachidfield', the
# reach id is taken there. Returns either NULL (if no field found)
# or a vector containing the reach id.
# -------------------------------------------------------------------
# I. Horner / 2018-03-29 / 2018-03-29
# -------------------------------------------------------------------
.j2kGetOutletReachesFromShapefile = function(stationsshp, reachidfield="ReachID") {
    dbf = stationsshp@data
    if (reachidfield %in% colnames(dbf)) {
        return(dbf[, reachidfield])
    } else {
        return(NULL)
    }
}

# -------------------------------------------------------------------
# FIX ME: this function will be unecessary if the new version of 
# HRU-delin is used. I should only use ".j2kGetOutletReachesFromShapefile'
# -------------------------------------------------------------------
# For each station, find the reach that has the closest vertices
# => It returns a vector of reach index (one index for each station)
# -------------------------------------------------------------------
# I. Horner / 2017-03-29 / 2018-03-29
# OBSOLETE !! No longer supported
# the rgdal and sp library are now used instead of the shapefile library
# Will be rem
# -------------------------------------------------------------------
.j2kGetOutletReaches = function(shp, verbose=TRUE){
    warning("OBSOLETE!! This function should no longer be used! It will be removed in the next version of J2KRUI.")
    # try and see if "ReachID" column exists in attribute table of shapefile station
    reachIDs = .j2kGetOutletReachesFromShapefile(shp$station, reachidfield="ReachID")
    if (!is.null(reachIDs)) {
        if (verbose){message(".j2kGetOutletReaches: reach ids retrieved from station shapefile."); flush.console()}
        return(reachIDs)
    } else {
        if (verbose){message(".j2kGetOutletReaches: reach ids not found in station shapefile"); flush.console()}
    }
	# function to compute distance between point Axy and point Bxy
	ihdist = function(A, B){sqrt((B[1]-A[1])^2+(B[2]-A[2])^2)}
	# get reach shapes and attribute table
	reachshp = shp$reach$shp$shp
	pts = lapply(reachshp, FUN=function(x){x$points}) # extract vertices of all reaches
	reachdbf = shp$reach$dbf$dbf # extract reach attribute table
	# get station shapes
	stationshp = shp$station$shp$shp
	reachIDs = c()
	for (k in 1:nrow(stationshp)){ # for each station
		if (verbose){cat("\r.j2kGetOutletReaches: processing station ", k, " / ", nrow(stationshp)); flush.console()}
		ptsref = stationshp[k, c(2,3)] # get station coordinates
		# compute distance with all vertices of reaches
		d = +Inf # initialize distance
		mind = 1
		for (i in 1:length(pts)){ # for each reache
			nd = min(apply(pts[[i]], 1, ihdist, A=ptsref)) # find minimum distance
			if (nd<d){ # if the distance is below the min found so far
				d=nd; mind=i # the reach is set as the closest reach
			}else if (nd==d){ # if the distance is equal to the min found so far
				mind = c(mind, i) # we add the reach to the closest reach list
			}
		}
		reachID = reachdbf[mind,1] # the closest reach is extracted (one or more than one)
		if (length(reachID)>1){ # if more than one reach was found
			warning(".j2kGetOutletReaches: for station ", k, ", multiple possible outlet reaches were found: ", paste(reachID, collapse=", "))
			reachID=max(reachID) # the reach with the maximum ID is selected (ARBITRARY BUT SEEMS TO WORK)
			# FIX ME: there, it could be possible to search the topology in reach.par in order to find
            #		the most upstream reach which is arguably the most relevant choice but maybe not
		}
		reachIDs[k] = reachID
	}
	if (verbose){cat("\n"); flush.console()}
	return(reachIDs)
}

# -------------------------------------------------------------------
# FIX ME: I should use the function '.j2kGetUpstreamElements' in
# J2K_InOutPar.R file. It is a very similar function. At least, a wrapper
# function should be implemented.
# -------------------------------------------------------------------
# For each reach in 'reachids' (reaches that are linked with stations)
# find all upstream reach that are in 'reachids'
# => It returns a list with upstream reach index
# -------------------------------------------------------------------
# I. Horner / 2017-07-24 / 2017-07-24
# -------------------------------------------------------------------
.j2kGetUpstreamReach = function(reachpar, reachids){
  warning("OBSOLETE!!! The function '.j2kGetUpstreamElements' should be used instead! It will be removed in the next version of J2KRUI.")
	.upreachrec = function(reachpar, reachids, curid, reachbool){
		ind = which(reachpar[,2]==curid)
		if (length(ind)==0){
			return(reachbool)
		}else{
			upids = reachpar[ind,1]
			for (k in upids){
				if (k%in%reachids){
					reachbool[which(reachids==k)]=TRUE
				}
			}
			for (k in upids){
				reachbool=.upreachrec(reachpar, reachids, curid=k, reachbool)
			}
			return(reachbool)
		}
	}
	upstreamReaches = list()
	for (k in 1:length(reachids)){
		reachbool=.upreachrec(reachpar, reachids, curid=reachids[k], reachbool=rep(FALSE, length(reachids)))
		upstreamReaches[[k]] = reachids[which(reachbool)]
	}
	return(upstreamReaches)
}



# -------------------------------------------------------------------
# Analyze the topology given in a data.frame or matrix from a given 
# start index:
#  - topology: a two column data.frame ["id", "connexion"]
#  - start:    outlet id
# -------------------------------------------------------------------
# I. Horner / 2018-06-19 / 2018-06-19
# -------------------------------------------------------------------
.j2kGetUpstreamElements = function(topology, start){
    .getUpstreamElem = function(topology, i){
        index = c(i)
        ind = which(topology[,2]==topology[i, 1])
        if (length(ind)!=0){
            for (k in ind){
                index=c(index, .getUpstreamElem(topology, k))
            }
            return(index)
        }else{
            return(index)
        }
    }
    index = c()
    for (i in start){
        j =  which(topology[,2]==i)
        if (length(j)!=0) for (k in j) index = c(index, .getUpstreamElem(topology, k)) 
    }
    return(index)
}


# -------------------------------------------------------------------
# Analyze the topology given in a data.frame or matrix from a given 
# start index:
#  - topology: a two column data.frame ["id", "connexion"]
#  - start:    source id
# The difference with the previous function is that it search all the
# elements before reaching an element that is connected to nothing
# -------------------------------------------------------------------
# I. Horner / 2019-04-16
# -------------------------------------------------------------------
.j2kGetDownstreamElements = function(topology, start){
    .getDownstreamElem = function(topology, i){
        index = c(i)
        ind = which(topology[,1]==topology[i, 2])
        if (length(ind)!=0){
            for (k in ind){
                index=c(index, .getDownstreamElem(topology, k))
            }
            return(index)
        }else{
            return(index)
        }
    }
    index = c()
    for (i in start){
        j =  which(topology[, 1]==i)
        index = c(index, .getDownstreamElem(topology, j)) 
    }
    return(index)
}


# -------------------------------------------------------------------
# FIX ME: I should not have the shapefile names hardcoded.
# -------------------------------------------------------------------
# It reads hru.par and reach.par. Given an outlet reach id and an
# output directory, it creates a new hru.par and reach.par corresponding
# to the sub-catchment defined by the provided outlet reach id.
# Optionally, it is possible to provide the function with the
# hru and reach shapefile containing folders. If provided, two new
# shapefiles are created in the outout directory.
# -------------------------------------------------------------------
# I. Horner / 2018-06-19 / 2018-08-03
# -------------------------------------------------------------------
j2kExtractSubcatchment = function(hrufp, reachfp, reachoutlet, outputdir, hrushpfolder=NULL, hrushpname=NULL, reachshpfolder=NULL, reachshpname=NULL, verbose=TRUE)
{
    warning("OBSOLETE! You should not have been able to use this function as it should be overwritten by another one loaded afterwards!")
    # 0/ reading output files
    if (verbose) { message("Reading topology paramter files ..."); flush.console()}
    hrupar = j2kReadPar(hrufp)
    reachpar = j2kReadPar(reachfp)
    
    # 1/ analyze the reach topology to extract all the upstream reach ids
    if (verbose) { message("Analyzing topology ..."); flush.console()}
    ioutlet = which(reachpar[, "ID"]==reachoutlet)
    rids = c(ioutlet, .j2kGetUpstreamElements(topology=reachpar[, c("ID", "to-reach")], start=reachoutlet))
    rids
    reachpar[rids, "ID"]
    # 2/ for each of the found upstream reach, find all the connected hrus
    conhru = c()
    for (k in reachpar[rids, "ID"]) conhru = c(conhru, which(hrupar[, "to_reach"]==k))
    conhru = hrupar[conhru, "ID"]
    
    # 3/ for each connected hrus, find all the upstream hrus.
    hids = list()
    for (k in 1:length(conhru)) {
        hids[[k]] = c(conhru[k], .j2kGetUpstreamElements(topology=hrupar[, c("ID", "to_poly")], start=conhru[k]))
    }
    hids = unlist(hids)
    
    # 4/ construct the new reach.par and hrus.par from the gathered information
    if (verbose) { message("Create and write new topology parameter files ..."); flush.console()}
    nreachpar = matrix(reachpar[sort(rids), ], length(rids), ncol(reachpar), byrow=TRUE, dimnames=list(NULL, colnames(reachpar)))
    
    # need to add the 9999 row and the connexion to it
    # FIXME: can I put anything in this column? should not be hardcoded...
    end_topo = rbind(c(9999, 0, 1.0, 0.0, 1.0, 30, 1), c(reachpar[ioutlet, "to-reach"], 9999, 1.0, 0.0, 1.0, 30, 1))
    nreachpar = rbind(nreachpar, end_topo)
    
    nhrupar = hrupar[sort(hids), ]
    dir.create(outputdir, recursive=TRUE)
    
    j2kWritePar(fp=paste0(outputdir, "/hru.par"), data=nhrupar, comment_line="edited with j2k-rui / ivan.horner@irstea.fr", units=NULL)
    j2kWritePar(fp=paste0(outputdir, "/reach.par"), data=nreachpar, comment_line="edited with j2k-rui / ivan.horner@irstea.fr", units=NULL)
    
    # 5/ create new reach and hrus shapefiles
    # I think this step should be optional
    if (!is.null(hrushpfolder) & !is.null(reachshpfolder)) {
        if (verbose) { message("Create new reach and hru shapefiles..."); flush.console()}
        if (!require(rgdal)){
            install.packages("rgdal")
            if (!require(rgdal)){stop("The rgdal package is required")}
        }
        hrusp = readOGR(hrushpfolder, layer=hrushpname, verbose=FALSE, stringsAsFactors=FALSE)
        hruspids = as.numeric(hrusp$cat)
        tokeep = unlist(sapply(nhrupar[, "ID"], function(e, sp) which(sp==e), sp=hruspids))
        hrusp = hrusp[tokeep, ]
        writeOGR(obj=hrusp, dsn=outputdir, layer=hrushpname, driver="ESRI Shapefile", overwrite_layer=TRUE)
        
        reachsp = readOGR(reachshpfolder, layer=reachshpname, verbose=FALSE, stringsAsFactors=FALSE)
        reachspids = as.numeric(reachsp$cat)
        tokeep = unlist(sapply(nreachpar[, "ID"], function(e, sp) which(sp==e), sp=reachspids))
        reachsp = reachsp[tokeep, ]
        writeOGR(obj=reachsp, dsn=outputdir, layer=reachshpname, driver="ESRI Shapefile", overwrite_layer=TRUE)
    }
}


j2kGetUpstreamHRUandReach = function(reachpar, hrupar, st_reach_ids) {
  
  reach_topology = reachpar[, c("ID", "to-reach")]
  hru_reach_topology = hrupar[, c("ID", "to_reach")]
  hru_reach_topology[hru_reach_topology==0] = NA
  hru_topology = hrupar[, c("ID", "to_poly")]
  hru_topology[hru_topology==0] = NA
  upstream = list()
  for (k in 1:length(st_reach_ids)) {
    # get all the reaches of the catchment
    i = .j2kGetUpstreamElements(topology=reach_topology, start=st_reach_ids[k])
    selected_reach = c(st_reach_ids[k], reach_topology[i, 1])
    # get all the hrus of the catchment
    selected_hru = c()
    for (k1 in 1:length(selected_reach)) {
      i = .j2kGetUpstreamElements(topology=hru_reach_topology, start=selected_reach[k1])
      if (length(i)>0) {
        sel_hru = hru_reach_topology[i, 1]
        up_hru = c()
        for (k2 in 1:length(sel_hru)) {
          i = .j2kGetUpstreamElements(topology=hru_topology, start=sel_hru[k2])
          if (length(i)>0) {
            up_hru = c(up_hru, hru_topology[i, 1])
          }
        }
        selected_hru = c(selected_hru, c(sel_hru, up_hru))
      }
    }
    upstream[[k]] = list('reach'=unname(selected_reach), 'hru'=unname(selected_hru))
  }
  names(upstream) = names(st_reach_ids)
  return(upstream)
}


# -------------------------------------------------------------------
# FIX ME: I should not have the shapefile names hardcoded.
# -------------------------------------------------------------------
# It reads hru.par and reach.par. Given an outlet reach id and an
# output directory, it creates a new hru.par and reach.par corresponding
# to the sub-catchment defined by the provided outlet reach id.
# Optionally, it is possible to provide the function with the
# hru and reach shapefile containing folders. If provided, two new
# shapefiles are created in the outout directory.
# -------------------------------------------------------------------
# I. Horner / 2018-06-19 / 2018-08-03 / 2019-04-18 / 2018-04-25
# -------------------------------------------------------------------
j2kExtractSubcatchment = function(outlet_reach_id, hrupar, reachpar, shp=NULL)
{
    # find all the upstream reach and hru ids
    up_reach_hru_ids = j2kGetUpstreamHRUandReach(reachpar=reachpar, hrupar=hrupar, st_reach_ids=outlet_reach_id)[[1]]
  
    # construct new reachpar
    
    last_reach_rows = reachpar[which(reachpar[, "ID"] == up_reach_hru_ids$reach[1]), , drop=FALSE]
    last_reach_rows[, "to-reach"] = 9999
    last_reach_rows = rbind(last_reach_rows, c(9999, 0, 1.0, 0.0, 1.0, 30, 1))

    i = match(up_reach_hru_ids$reach[-1], reachpar[, "ID"])
    new_reachpar = rbind(reachpar[i, ], last_reach_rows)
    
    # construct new hrupar
    i = match(up_reach_hru_ids$hru, hrupar[, "ID"])
    new_hrupar = hrupar[i, ]
    
    # construct new shapefiles
    if (!is.null(shp)) {
      multimatch = function(x, table) as.list(sapply(x, function(e, tbl) which(tbl==e), tbl=table))
      # note: here I cannot use match() as only the first element would be returned
      i = unlist(multimatch(up_reach_hru_ids$hru, shp$hru@data[, "cat"]))
      new_hrushp = shp$hru[i, ]
      i = unlist(multimatch(up_reach_hru_ids$reach, shp$reach@data[, "cat"]))
      new_reachshp = shp$reach[i, ]
      shp$hru = new_hrushp
      shp$reach = new_reachshp
    }
    return(list(hrupar=new_hrupar, reachpar=new_reachpar, shp=shp))
}

j2kWriteGIS = function(shp, outdir, recursive=FALSE) {
  dir.create(outdir, recursive=recursive, showWarnings=FALSE)
  fn = names(shp)
  if (is.null(fn)) stop("shp must be a named list!")
  fp = paste0(outdir, fn)
  for (k in 1:length(fp)) writeOGR(obj=shp[[k]], dsn=dirname(fp[k]), layer=basename(fp[k]), driver="ESRI Shapefile", overwrite_layer=TRUE)
  invisible(fn)
}

