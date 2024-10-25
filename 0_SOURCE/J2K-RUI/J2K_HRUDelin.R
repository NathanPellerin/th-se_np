

# -------------------------------------------------------------------
# Compute the Strahler order based on the "ID" and "to-reach" columns 
# of a reach.par file provided in the 'x' argument. The 'val' argument
# is a vector filled of zero with a length equal to the number of reaches.
# The 'i' argument is the index of the reach corresponding to the
# catchment outlet.
# => It returns a vector containing the Strahler order for each reach.
# -------------------------------------------------------------------
# I. Horner / 2017-11-03 / 2017-11-03
# -------------------------------------------------------------------
j2kStrahler=function(x, val=rep(0, nrow(x)), i=which(x[,1]==9999)){
	# get the current id
	v=x[i, 1]
	# see if there is any source
	isource=which(x[,2]==v)
	if (length(isource)>0){
		# if any source is found, see if the Strahler order is known
		for (k in 1:length(isource)){
			if (val[isource[k]]==0){ # the Strahler order is not known
				val=j2kStrahler(x, val, isource[k])
			}
			# here, all the source Strahler orders should be known
			m=max(val[isource])
			if (length(which(val[isource]==m))>=2){# if at least two of them are the same, the Strahler order is incremented
				val[i]=m+1
			}else{# if not, the maximum Strahler order of the source is taken
				val[i]=m
			}
		}
	}else{
		# if there is no source, the Srahler order is 1
		val[i]=1
	}
	return(val)
}


# -------------------------------------------------------------------
# Change the to_reach column of specific rows
# => It returns the update reach.par table
# -------------------------------------------------------------------
# I. Horner / 2017-08-09 / 2017-08-09
# -------------------------------------------------------------------
j2kUpdateReachParFile = function(reachpar, from_reach, new_to_reach){
	i = sapply(from_reach, function(x, y){which(y==x)}, y=reachpar[,1])
	reachpar[i, 2] = new_to_reach
	return(reachpar)
}


j2kGetTopology = function(reach_shp, hru_shp, reachpar, hrupar) {
  # reaches ---------------------------------------------------------
  # get reach ids from shapefile
  reach_data = reach_shp@data[, 1]
  # get "centroid" coordinates of each reach (found in reach.par)
  reach_coord_from = matrix(NA, nrow(reachpar), 2)
  for (k in 1:nrow(reachpar)) {
    i = which(reach_data == reachpar[k, "ID"])
    if (length(i)>0) {
      l = reach_shp[i, ]
      j = which(unlist(lapply(coordinates(l), function(e) nrow(e[[1]])))>1)
      if (length(j)>0) { # Solution 1: get centroid and then look for closest point on reach
        centroid = gCentroid(l[j, ])
        reach_coord_from[k, ] = coordinates(gNearestPoints(l[j, ], centroid)[1, ])
      } else { # Solution 2: mean of x and y of each reach used
        warning("An error occured ... but you should be fine")
        reach_coord_from[k, ] = apply(l@bbox, 1, mean)
      }
    }
  }
  
  # an additional check
  i1 = which(!unique(reachpar[, "ID"])%in%unique(reach_data))
  i2 = which(!unique(reach_data)%in%unique(reachpar[, "ID"]))
  if (length(i1)>1) {
    message("The following reach of 'reach.par' were not found in reach shapefile:")
    print(reachpar[i1, "ID"])
    message("No geometry corresponding to these reaches are found in the reach shapefile.")
    ok = c()
    for (k in i1) {
      fi = .j2kGetDownstreamElements(reachpar[, c("ID", "to-reach")], start=reachpar[k, "ID"])
      if (reachpar[fi[length(fi)], "ID"] !=9999) {
        ok = c(ok, k)
      }
    }
    if (length(ok)>0) {
      message("The following reaches are not routed to the catchment outlet (9999):")
      print(reachpar[ok, "ID"])
    } else {
      message("However, all these reaches have been checked to be (eventually) routed to the catchment outlet (9999).")
    }
  }
  if (length(i2)>1) {
    message("The following reach id in shapefile were not found in reach.par:")
    print(reachpar[i2, "ID"])
  }
  
  # get target reach of each reach
  i_to = match(reachpar[, "to-reach"], reachpar[, "ID"])
  reach_topo = cbind(reach_coord_from, reach_coord_from[i_to, ])
  
  # HRUs ------------------------------------------------------------
  # get HRU ids from shapefile
  hru_data = hru_shp@data[, "cat"] # or "value"?
  # get a point within each HRU and use it as centroid
  hru_coord_from = matrix(NA, nrow(hrupar), 2)
  for (k in 1:nrow(hrupar)) {
    i = which(hru_data == hrupar[k, "ID"])
    p = hru_shp[i, ]
    hru_coord_from[k, ] = coordinates(gPointOnSurface(p))
  }
  # see which HRU connects to which HRU or Reach
  i_to_hru = match(hrupar[, "to_poly"], hrupar[, "ID"])
  i_to_reach = match(hrupar[, "to_reach"], reachpar[, "ID"])
  i_to_hru_valid = which(!is.na(i_to_hru))
  i_to_reach_valid = which(!is.na(i_to_reach))
  # get target reach or HRU of each HRU
  hru_coord_to = matrix(NA, nrow(hru_coord_from), 2)
  hru_coord_to[i_to_hru_valid, ] = hru_coord_from[i_to_hru[i_to_hru_valid], ]
  hru_coord_to[i_to_reach_valid, ] = reach_coord_from[i_to_reach[i_to_reach_valid], ]
  types = rep(NA, nrow(hru_coord_from))
  types[i_to_hru_valid] = 1
  types[i_to_reach_valid] = 2
  hru_topo = cbind(hru_coord_from, hru_coord_to, types)
  
  # Return ----------------------------------------------------------
  return(list(reach_topo=reach_topo, hru_topo=hru_topo))
}


createGIStopology = function(upstream_xy, downstream_xy, attributes, p4s, outdir, outlayer) {
  if (identical(substr(outdir, nchar(outdir), nchar(outdir)), "/")) outdir = substr(outdir, 1, nchar(outdir)-1)
  
  data = list()
  poly = list()
  k = 1
  for (i in 1:nrow(attributes)) {
    if (sum(is.na(upstream_xy[i, ]))==0 & sum(is.na(downstream_xy[i, ]))==0) {
      poly[[k]] = Lines(slinelist=list(Line(coords=matrix(as.numeric(c(upstream_xy[i, ], downstream_xy[i, ])), 2, 2, byrow=TRUE))), ID=k)
      data[[k]] = attributes[i, ]
      k = k + 1
    }
  }
  data = do.call(rbind, data)
  
  splines = SpatialLines(LinesList=poly, proj4string=CRS(p4s))
  sptopo = SpatialLinesDataFrame(sl=splines, data=as.data.frame(data), match.ID=FALSE)
  
  writeOGR(sptopo, dsn=outdir, layer=outlayer, driver="ESRI Shapefile", overwrite_layer=TRUE)
}









# below: my attempt at creating a shiny app to explore HRU-Delin results
# honestly, it's just usefull to have a quick look... 
# I recommend computing the topology from R (see workflow in J2KRUI_hrudelin.R)
# and then use a GIS software to explore the results.




plot_poly = function(polygons_obj, plot=TRUE, lwd=1, col="black", lty=1, bgcol="transparent", ...) {
  lines_coords = lapply(polygons_obj@polygons, function(e) {
    poly_list = e@Polygons
    do.call(rbind, lapply(poly_list, function(e) e@coords))
  })
  if (plot) {
    n = length(lines_coords)
    if (length(lwd)!=n) lwd = rep(lwd, n)
    if (length(lty)!=n) lty = rep(lty, n)
    if (length(col)!=n) col = rep(col, n)
    if (length(bgcol)!=n) bgcol = rep(bgcol, n)
    for (k in 1:n) {
      polygon(lines_coords[[k]][, 1], lines_coords[[k]][, 2], col=bgcol[k], border=col[k], lwd=lwd[k], lty=lty[k], ...)
    }
    # for (k in 1:n) {
    # lines(lines_coords[[k]], col=col[k], lwd=lwd[k], lty=lty[k], ...)
    # }
  }
  invisible(lines_coords)
}

plot_line = function(lines_obj, plot=TRUE, plot_end=TRUE,  lwd=1, col="black", lty=1, ...){
  lines_coords = lapply(lines_obj@lines, function(e) {
    line_list = e@Lines
    do.call(rbind, lapply(line_list, function(e) e@coords))
  })
  if (plot) {
    n = length(lines_coords)
    if (length(lwd)!=n) lwd = rep(lwd, n)
    if (length(lty)!=n) lty = rep(lty, n)
    if (length(col)!=n) col = rep(col, n)
    for (k in 1:n) {
      lines(lines_coords[[k]], col=col[k], lwd=lwd[k], lty=lty[k], ...)
    }
    if (plot_end) lapply(lines_coords, function(e, ...) points(e[c(1, nrow(e)), ], pch=16, cex=0.5, ...), ...)
  }
  invisible(lines_coords)
}

plot_gis = function(xlim=NULL, ylim=NULL, hru_shp=NULL, subcatch_shp=NULL, station_shp=NULL, reach_shp=NULL, reach_topo=NULL, hru_topo=NULL, station_lbl=NULL,
                    lwds_reach=1, cols_hru="transparent") {
  bbox = c(NA, NA)
  if (!is.null(hru_shp)) bbox = cbind(bbox, hru_shp@bbox)
  if (!is.null(subcatch_shp)) bbox = cbind(bbox, subcatch_shp@bbox)
  if (!is.null(station_shp)) bbox = cbind(bbox, station_shp@bbox)
  if (!is.null(reach_shp)) bbox = cbind(bbox, reach_shp@bbox)
  if (!is.null(reach_topo)) bbox = cbind(bbox, c(min(reach_topo[, c(1, 3)], na.rm=TRUE), min(reach_topo[, c(2, 4)], na.rm=TRUE)), 
                                         c(max(reach_topo[, c(1, 3)], na.rm=TRUE), max(reach_topo[, c(2, 4)], na.rm=TRUE)))
  if (!is.null(hru_topo)) bbox = cbind(bbox, c(min(hru_topo[, c(1, 3)], na.rm=TRUE), min(hru_topo[, c(2, 4)], na.rm=TRUE)), 
                                       c(max(hru_topo[, c(1, 3)], na.rm=TRUE), max(hru_topo[, c(2, 4)], na.rm=TRUE)))
  if (is.null(xlim)) xlim = range(bbox[1, -1])
  if (is.null(ylim)) ylim = range(bbox[2, -1])
  par("mai"=c(0.5, 0.5, 0.5, 0.5))
  plot(xlim, ylim, type="n", asp=1, bty="n", xlab="", ylab="", xaxt="n", yaxt="n")
  grid()
  if (!is.null(hru_shp)) plot_poly(hru_shp, col="grey", bgcol=cols_hru)
  if (!is.null(subcatch_shp)) plot_poly(subcatch_shp, col="red", lwd=2)
  
  if (!is.null(station_shp)) {
    points(station_shp, col="darkred", pch=16, cex=2)
    text(coordinates(station_shp), labels=station_lbl, cex=0.8)
  }
  if (!is.null(reach_shp)) plot_line(reach_shp, col="blue", lwd=lwds_reach)
  if (!is.null(reach_topo)) {
    points(reach_topo[, 1:2], col="orange", cex=0.5)
    arrows(reach_topo[, 1], reach_topo[, 2], reach_topo[, 3], reach_topo[, 4], col="orange", length=0.1, lwd=2)
  }
  
  if (!is.null(hru_topo)) {
    points(hru_topo[, 1:2], col="darkgreen", cex=0.5)
    arrows(hru_topo[, 1], hru_topo[, 2], hru_topo[, 3], hru_topo[, 4], col=c("darkgreen", "green")[hru_topo[, 5]], length=0.05, lwd=0.5)
  }
  axis(1); axis(2); axis(3); axis(4)
}

j2kHRUDelinExplorer = function(hru_shp, reach_shp, station_shp, subcatch_shp, hru_topo, reach_topo, lwds_reach, upstream, stationname) {
  
  ih.shiny.plot = new.env()
  
  cb_layer_choices_names = list("HRUs", "Subcatchments", "Stations", "Reaches", "Reach topology", "HRU topology")
  cb_layer_choices_value = list("hru_shp", "subcatch_shp", "station_shp", "reach_shp", "reach_topo", "hru_topo")
  
  
  cb_station_choices_names = unname(stationname)
  cb_station_choices_value = 1:length(upstream)
  
  
  ui = shinyUI(fluidPage(
    titlePanel(title="HRU Delin Result Explorer", windowTitle="HRU Delin Result Explorer"),
    sidebarLayout(
      
      sidebarPanel(style="background-color: #f7fbff",
                   h4("Layers selector: "),
                   checkboxGroupInput(inputId="cb_layer", label=NULL, selected=unlist(cb_layer_choices_value),
                                      choiceNames=cb_layer_choices_names, choiceValues=cb_layer_choices_value),
                   actionButton("update", "Update plot"), 
                   h4("Highlight elements corresponding to sub-catchments: "),
                   wellPanel(style = "overflow-y:scroll; max-height: 600px; background-color: #deebf7",
                             checkboxGroupInput(inputId="cb_station", label=NULL,
                                                choiceNames=cb_station_choices_names, choiceValues=cb_station_choices_value), # selected=cb_station_choices_value),
                             uiOutput('station_highlight')
                   )
      ),mainPanel(
        plotOutput("plot1", width="100%", height="800px", dblclick = "plot_dblclick", brush = brushOpts("plot_brush", resetOnNew=TRUE, delayType="debounce", delay=10000))
      ))
  ))
  
  server <- function(input, output, session) {
    
    observe({
      input$update
      assign("station", isolate(input$cb_station), pos=ih.shiny.plot)
      
    })
    
    output$plot1 <- renderPlot({
      if (!is.null(input$plot_brush)){
        xlim = c(input$plot_brush$xmin, input$plot_brush$xmax)
        ylim = c(input$plot_brush$ymin, input$plot_brush$ymax)
        assign("xlim", xlim, pos=ih.shiny.plot)
        assign("ylim", ylim, pos=ih.shiny.plot)
      } else  if (!is.null(input$plot_dblclick)){
        assign("xlim", NULL, pos=ih.shiny.plot)
        assign("ylim", NULL, pos=ih.shiny.plot)
      } else {
        assign("layer", input$cb_layer, pos=ih.shiny.plot)
        input$update
        
        eval(get("plot_expression", pos=ih.shiny.plot))
      }
    })
    

  }
  
  assign("ui", ui, pos=ih.shiny.plot)
  assign("server", server, pos=ih.shiny.plot)
  
  assign("ylim", NULL, pos=ih.shiny.plot)
  assign("xlim", NULL, pos=ih.shiny.plot)
  assign("station", NULL, pos=ih.shiny.plot)
  assign("upstream", upstream, pos=ih.shiny.plot)
  assign("stationname", stationname, pos=ih.shiny.plot)
  
  arg_layer = list(hru_shp=hru_shp, subcatch_shp=subcatch_shp,
                   station_shp=station_shp, reach_shp=reach_shp, 
                   reach_topo=reach_topo, hru_topo=hru_topo)
  arg_look = list(lwds_reach=lwds_reach, cols_hru="transparent")
  assign("arg_layer", arg_layer, pos=ih.shiny.plot)
  assign("arg_look", arg_look, pos=ih.shiny.plot)
  plot_expr = expression({
    arg_layer = get("arg_layer", pos=ih.shiny.plot)
    arg_look = get("arg_look", pos=ih.shiny.plot)
    layer = get("layer", pos=ih.shiny.plot)
    station = as.numeric(get("station", pos=ih.shiny.plot))
    upstream = get("upstream", pos=ih.shiny.plot)
    us = upstream[station]
    if (length(us)>0) {
      us = unique(unlist(lapply(us, function(e) e$hru)))
      hru_ids = hru_shp@data[, "cat"]
      selected_hru_i = which(hru_ids%in%us)
      cols_hru = rep("transparent", length(arg_layer$hru_shp))
      cols_hru[selected_hru_i] = "pink"
      stationname = unname(unlist(get("stationname", pos=ih.shiny.plot)))
      station_lbl = rep(NA, length(stationname))
      station_lbl[station] = stationname[station]
      print(station)
      print(station_lbl)
    } else {
      cols_hru = "transparent"
      station_lbl = NULL
    }
    
    active_layer = names(arg_layer)%in%layer
    arg_layer = arg_layer[active_layer]
    
    if (sum(active_layer)==0) {
      plot(0:1, 0:1, type="n", xaxt="n", yaxt="n", bty="n", xlab="", ylab="")
      text(0.5, 0.5, "No Data", font=2, cex=2)
    } else {
      plot_gis(xlim=get("xlim", pos=ih.shiny.plot), ylim=get("ylim", pos=ih.shiny.plot), 
               hru_shp=arg_layer$hru_shp, subcatch_shp=arg_layer$subcatch_shp, station_shp=arg_layer$station_shp,
               reach_shp=arg_layer$reach_shp, reach_topo=arg_layer$reach_topo, hru_topo=arg_layer$hru_topo, 
               lwds_reach=arg_look$lwds_reach, cols_hru=cols_hru, station_lbl=station_lbl)
    }
  })
  assign("plot_expression", plot_expr, pos=ih.shiny.plot)
  
  shinyApp(get("ui", pos=ih.shiny.plot), get("server", pos=ih.shiny.plot))
  
}

j2kHRUDelinExplorerWrapper = function(shp,  stationname) {
  hrudelin_topo = j2kGetTopology(reach_shp=shp$reach, hru_shp=shp$hru, reachpar=reachpar, hrupar=hrupar)
  st_reach_ids = shp$station@data[, "ReachID"]
  upstream = j2kGetUpstreamHRUandReach(reachpar=reachpar, hrupar=hrupar,
                                       st_reach_ids=st_reach_ids)
  j2kHRUDelinExplorer(hru_shp=shp$hru, reach_shp=shp$reach, station_shp=shp$station, subcatch_shp=shp$subcatch,
                      hru_topo=hrudelin_topo[["hru_topo"]], reach_topo=hrudelin_topo[["reach_topo"]],
                      lwds_reach=1, upstream=upstream, stationname=stationname)
}

.j2kHRUdelinPlotResults = function(reachpar, reachshp, stationshp=NULL, idstationname=NULL, station_reach_id=NULL, col_var="slope", xlim, ylim){
	if (is.null(idstationname)){idstationname=1}
	# for each reach (in reach.par), find the coordinated by looking in the reach shapefile
	reachids = reachpar[,1]
	shpdbf = reachshp$dbf$dbf
	reach_shp = reachshp$shp$shp
	reach_xy = matrix(NA, length(reachids), 2)
	for (k in 1:length(reachids)){
		i_in_dbf = which(shpdbf[,1]==reachids[k])
		if (length(i_in_dbf)!=0){
			shp_pts = lapply(i_in_dbf, function(x, y){y[[x]]$points}, y=reach_shp)
			shp_pts = do.call(rbind, shp_pts)
			reach_xy[k, ] = c(mean(shp_pts[,1]), mean(shp_pts[,2]))
		}
	}
	reach_xy[which(is.na(reach_xy[,1])),] = c(mean(reach_xy[,1], na.rm=TRUE), min(reach_xy[,2], na.rm=TRUE))
	# get the index of the target reach of each reach
	i_to_reach = unlist(sapply(reachpar[,2], function(x, y){i=which(y==x);ifelse(length(i)==0, NA, i)}, y=reachids))
	# get the stations coordinates
	if (!is.null(stationshp)){
		station_xy = stationshp$shp$shp[, c(2,3)]
		stationnames = stationshp$dbf$dbf[, idstationname]
		# get the index in reach_xy of the connected reach
		if (!is.null(station_reach_id)){
			i_station_reach = unlist(sapply(station_reach_id, function(x, y){which(y==x)}, y=reachpar[,1]))
		}
	}
	# look for slopes with a zero values to specify arrows with different colors
	v = as.numeric(reachpar[,col_var])
	arrows_col = hsv((v-min(v))/(max(v)-min(v))*0.7, 1, 1)
	i_null_slope = which(reachpar[,"slope"]==0)
	# arrows_col = rep("darkgreen", nrow(reachpar))
	# arrows_col[i_null_slope] = "darkred"
	arrow_lty = rep(1, nrow(reachpar))
	arrow_lty[i_null_slope] = 2
	# plot
	if(is.null(xlim)){xlim=range(reach_xy[,1])}
	if(is.null(ylim)){ylim=range(reach_xy[,2])}
	parmai=par("mai")
	par("mai"=c(1,1,0,0))
	plot(NA, xlim=xlim, ylim=ylim, bty="n", las=1, xlab="", ylab="")#, xaxt="n", yaxt="n")
	s=lapply(lapply(reach_shp, FUN=function(x){x$points}), FUN=function(x, ...){lines(x=x[,1], y=x[,2], ...)}, col="grey")
	if (!is.null(stationshp)){
		points(x=station_xy[,1], y=station_xy[,2], pch=16, col="darkorange", cex=2)
		text(x=station_xy[,1], y=station_xy[,2], labels=stationnames, col="darkorange", pos=1, cex=0.9)
		if (!is.null(station_reach_id)){
			segments(station_xy[,1], station_xy[,2], reach_xy[i_station_reach,1], reach_xy[i_station_reach,2], col="darkorange")
		}
	}
	points(x=reach_xy[,1], y=reach_xy[,2], pch=16, col="blue")
	points(x=reach_xy[,1], y=reach_xy[,2], col="lightblue", cex=3)
	text(x=reach_xy[,1], y=reach_xy[,2], labels=reachids, col="blue", pos=1, cex=0.9)
	arrows(reach_xy[,1], reach_xy[,2], reach_xy[i_to_reach,1], reach_xy[i_to_reach,2], col=arrows_col, length=0.1, lwd=2, lty=arrow_lty)
	par("mai"=parmai)
}

j2kHRUdelinShinyPlot = function(reachpar, reachshp, stationshp=NULL, idstationname=NULL, station_reach_id=NULL, col_var="slope"){
  message("OBSOLETE!! please use 'j2kHRUDelinExplorer' function instead.")
	assign("ylim", NULL, pos=ih.shiny.plot)
	assign("xlim", NULL, pos=ih.shiny.plot)
	arguments = list(reachpar=reachpar, reachshp=reachshp,
		stationshp=stationshp, idstationname=idstationname, 
		station_reach_id=station_reach_id, col_var=col_var)
	assign("arg", arguments, pos=ih.shiny.plot)
	plot_expr = expression({
		arg = get("arg", pos=ih.shiny.plot)
		.j2kHRUdelinPlotResults(reachpar=arg$reachpar, reachshp=arg$reachshp,
			stationshp=arg$stationshp, idstationname=arg$idstationname, station_reach_id=arg$station_reach_id,
			col_var=arg$col_var, xlim=get("xlim", pos=ih.shiny.plot), ylim=get("ylim", pos=ih.shiny.plot))
	})
	assign("plot_expression", plot_expr, pos=ih.shiny.plot)
	shinyApp(get("ui", pos=ih.shiny.plot), get("server", pos=ih.shiny.plot))
}

# -------------------------------------------------------------------
# Expression containing the code to pre-process the HRU Delin results
# before plotting
# -------------------------------------------------------------------
# I. Horner / 2017-08-09 / 2017-08-09
# -------------------------------------------------------------------
# init_hrudelinres = expression({
	# needed: reachpar, reachshp, stationshp, idstationname



# })

# -------------------------------------------------------------------
# Expression containing the code to plot pre-processed HRU Delin results
# -------------------------------------------------------------------
# I. Horner / 2017-08-09 / 2017-08-09
# -------------------------------------------------------------------
# plot_hrudelinres = expression({
	# this expression must have xlim/ylim variables with default value setting if NULL
	# needed: xlim, ylim, shpshp, stationcoords, coords, i_to_reach, arrow_col

# })


# -------------------------------------------------------------------
# Variable the user may want to change...
# -------------------------------------------------------------------
# I. Horner / 2017-08-10 / 2017-08-10
# -------------------------------------------------------------------
# ih.shiny.plot.title = "HRU-Delin results"
# ih.shiny.plot.plot = plot_hrudelinres
# ih.shiny.plot.init = init_hrudelinres
# ih.shiny.plot = new.env()


# -------------------------------------------------------------------
# Defining the ui object and server function for a shiny app
# -------------------------------------------------------------------
# I. Horner / 2017-08-09 / 2017-08-09
# -------------------------------------------------------------------
# ui <- shinyUI(fluidPage(
#       plotOutput("plot1", width="100%", height="800px", dblclick = "plot_dblclick", brush = brushOpts("plot_brush", resetOnNew=TRUE))
# ))
# server <- function(input, output) {
# 	output$plot1 <- renderPlot({
# 		if (!is.null(input$plot_brush)){
# 			xlim = c(input$plot_brush$xmin, input$plot_brush$xmax)
# 			ylim = c(input$plot_brush$ymin, input$plot_brush$ymax)
# 			assign("xlim", xlim, pos=ih.shiny.plot)
# 			assign("ylim", ylim, pos=ih.shiny.plot)
# 		}
# 		if (!is.null(input$plot_dblclick)){
# 			assign("xlim", NULL, pos=ih.shiny.plot)
# 			assign("ylim", NULL, pos=ih.shiny.plot)
# 		}
# 		xlim = get("xlim", pos=ih.shiny.plot)
# 		ylim = get("ylim", pos=ih.shiny.plot)
# 		eval(get("plot_expression", pos=ih.shiny.plot))
# 	})
# }
# assign("ui", ui, pos=ih.shiny.plot)
# assign("server", server, pos=ih.shiny.plot)
# 
