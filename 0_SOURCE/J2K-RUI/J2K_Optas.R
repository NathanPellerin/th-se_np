
# read "optimizer.dat" and get the template of the file:
# get the parameter names in "optas" formatting
# get the parameter names
# add a column to know wether or not we're dealing with a parameter or an efficiency value/argument/parameter
j2kGetOptimizerTemplate <- function(fp, effValue = NULL) {
    optas_template <- scan(file = fp, what = "character", nlines = 1, sep = "\t", skip = 5, quiet = TRUE)
    optas_names <- optas_template[-c(1, length(optas_template))]
    optas_names_split <- strsplit(optas_names, split = "___", fixed = TRUE)
    optas_par_names <- vapply(optas_names_split, function(e) e[2], character(1L))
    optas_types <- factor(rep("parameter", length(optas_names)), levels = c("parameter", "efficiency"))
    if (!is.null(effValue)) {
      iseff <- optas_names == effValue
      optas_types[iseff] <- "efficiency"
    }
    data.frame(optas = optas_names, name = optas_par_names, type = optas_types, stringsAsFactors = FALSE)
}

# correct optas model:
# 1/ there is a bug in the parameter order in the parametrization, more importantly, some parameters are duplicated
#    FIX-ME: I should see if there is no mistakes in how the parameters are read from the optimizer.dat!!!
#            as this could be the source of huge mistakes in my analysis!!!
# 2/ change the type of "optimizer" to 'optas.sampler.FileListSampler'
# 3/ add a file name in the parametrization of the optimizer: 'fileName=optimizer.dat'
# 4/ write the new model to file
j2kOptasModelCorrection <- function(fp_in, fp_out = fp_in, optas_parameters) {
  optas_j2k_src <- xmlRoot(xmlTreeParse(fp_in))
  optas_j2k <- xmlChildren(optas_j2k_src[["contextcomponent"]])
  i_var <- which(names(optas_j2k) == "var")
  optas_j2k <- optas_j2k[i_var]
  optas_j2k_names <- vapply(optas_j2k, function(e) xmlAttrs(e)["name"], character(1L))
  names(optas_j2k) <- optas_j2k_names
  j_var <- c(
      which(optas_j2k_names == "parameterIDs"), 
      which(optas_j2k_names == "parameterNames"), 
      which(optas_j2k_names == "boundaries"),
      which(optas_j2k_names == "optimizationClassName"),
      which(optas_j2k_names == "parameterization")
  )
  par_ids <- strsplit(xmlAttrs(optas_j2k[["parameterIDs"]])["attribute"], split = ";", fixed = TRUE)[[1]]
  par_names <- strsplit(xmlAttrs(optas_j2k[["parameterNames"]])["value"], split = ";", fixed = TRUE)[[1]]
  par_bounds <- strsplit(xmlAttrs(optas_j2k[["boundaries"]])["value"], split = ";", fixed = TRUE)[[1]]
  
  under_selection <- match(optas_parameters, par_ids)
  isna <- is.na(under_selection)
  if (any(isna)) warning("The following parameters were not found in the model:\n", paste(optas_parameters[isna], collapse = ", "))
  under_selection <- under_selection[!is.na(under_selection)]
  under_selection
  
  xmlAttrs(optas_j2k_src[[7]][[i_var[1] + j_var[1] - 1]])[["attribute"]] <- paste(par_ids[under_selection], collapse = ";")
  xmlAttrs(optas_j2k_src[[7]][[i_var[1] + j_var[2] - 1]])[["value"]] <- paste(par_names[under_selection], collapse = ";")
  xmlAttrs(optas_j2k_src[[7]][[i_var[1] + j_var[3] - 1]])[["value"]] <- paste(par_bounds[under_selection], collapse = ";")
  
  xmlAttrs(optas_j2k_src[[7]][[i_var[1] + j_var[4] - 1]])[["value"]] <- "optas.sampler.FileListSampler"
  parametrization <- xmlAttrs(optas_j2k_src[[7]][[i_var[1] + j_var[5] - 1]])[["value"]] 
  xmlAttrs(optas_j2k_src[[7]][[i_var[1] + j_var[5] - 1]])[["value"]] <- paste0(parametrization, "fileName=optimizer.dat;")
  
  saveXML(optas_j2k_src, file = fp_out,
          prefix="<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n")
  invisible()
}

# given an optas model file path, get the names of
# the efficiency value/argument/parameter
j2kGetOptasEffValue <- function(fp) {
  optas_j2k_src <- xmlRoot(xmlTreeParse(fp))
  optas_j2k <- xmlChildren(optas_j2k_src[["contextcomponent"]])
  optas_j2k <- optas_j2k[names(optas_j2k) == "var"]
  optas_j2k_names <- vapply(optas_j2k, function(e) xmlAttrs(e)[["name"]], character(1L))
  names(optas_j2k) <- optas_j2k_names
  strsplit(xmlAttrs(optas_j2k$effValue)[["attribute"]], ";", fixed = TRUE)[[1]]
}

# end user function to
# 1/ fetch the optas template, parameter names, etc.
# 2/ correct the optas model
j2kOptasTemplate <- function(model_fp, opimizer_fp, model_out_fp = NULL) {
  eff_value <- j2kGetOptasEffValue(model_fp)
  optas     <- j2kGetOptimizerTemplate(fp = opimizer_fp, effValue = eff_value)
  
  optas_optimizer_col <- optas
  optas_onlyparameter <- optas[optas$type == "parameter", ]
  optas_onlyuniquepar <- optas[optas$type == "parameter" & !duplicated(optas$name), ]
  
  # correct optas model parametrization
  if (!is.null(model_out_fp)) {
    j2kOptasModelCorrection(fp_in  = model_fp, fp_out = model_out_fp,
                            optas_parameters = optas_onlyparameter$optas)
  }
  
  optas_onlyparameter <- optas_onlyparameter[, -3]
  optas_onlyuniquepar <- optas_onlyuniquepar[, -3]
  list(all = optas_onlyparameter, unique = optas_onlyuniquepar)
  list(all = optas_optimizer_col, unique = optas_onlyuniquepar)
}

# get HRU parameters from parameter files
j2kGetHRUparameters <- function(landusepar, soilpar, hgeopar, hrupar,
                                hru_id = "ID", landuse_id = "landuseID",
                                soil_id = "soilID", hgeo_id = "hgeoID",
                                LID = "LID", SID = "SID", GID = "GID") {
  l_match <- match(hrupar[, landuse_id], landusepar[, LID])
  s_match <- match(hrupar[, soil_id], soilpar[, SID])
  g_match <- match(hrupar[, hgeo_id], hgeopar[, GID])
  as.data.frame(cbind(hrupar, landusepar[l_match, ],
                      soilpar[s_match, ], hgeopar[g_match, ]))
}

# get distributed parameters from a jams model
j2kGetDistributedParameters <- function(j2kproj, bystation=TRUE) {
  para_fp <- j2kproj$parameterfiles
  main_fp <- paste0(j2kproj$directory, "/")
  
  landusepar <- j2kReadPar(paste0(main_fp, para_fp["landufile"]))
  soilpar    <- j2kReadPar(paste0(main_fp, para_fp["soilfile"]))
  hgeopar    <- j2kReadPar(paste0(main_fp, para_fp["groundwfile"]))
  hrupar     <- j2kReadPar(paste0(main_fp, para_fp["hrufile"]))
  reachpar   <- j2kReadPar(paste0(main_fp, para_fp["reachfile"]))
  newhrupar  <- j2kGetHRUparameters(landusepar = landusepar, soilpar = soilpar, 
                                    hgeopar = hgeopar, hrupar = hrupar,
                                    hru_id = "ID", landuse_id = "landuseID", 
                                    soil_id = "soilID", hgeo_id = "hgeoID",
                                    LID = "LID", SID = "SID", GID = "GID")
  newreachpar <- reachpar
  if (bystation) {
    sc <- j2kproj$subcatchments
    st_reach_ids <- sc[, "reach"]
    st_id <- sc[, "id"]
    upstream <- j2kGetUpstreamHRUandReach(reachpar = reachpar, hrupa = hrupar,
                                          st_reach_ids = st_reach_ids)
    newpar <- lapply(upstream, function(e, newreachpar, reach_id, newhrupar, hru_id) {
      nrp <- newreachpar[which(newreachpar[, reach_id]%in%e$reach), , drop=FALSE]
      nhp <- newhrupar[which(newhrupar[, hru_id]%in%e$hru), , drop=FALSE]
      list(hru = nhp, reach = nrp)
    }, newhrupar = newhrupar, hru_id = "ID", newreachpar = newreachpar, reach_id = "ID")
    names(newpar) <- st_id
    newpar
  }else {
    list(hru = newhrupar, reach = newreachpar)
  }
}

# compute maxMPS as done in J2K
j2kComputePar_maxMPS <- function(hrupar, rootdepth = "rootDepth", fc_suffix = "fc_") {
  apply(hrupar, 1, function(e) {
    ne <- names(e)
    rd <- as.numeric(e[rootdepth])
    sum(e[paste0(fc_suffix, 1:rd)])
  })
}
# compute maxLPS, only a copy of aircap, as in J2K
j2kComputePar_maxLPS <- function(hrupar, aircap = "aircap") hrupar[aircap]


# aggregate parameters according to the type of parameters
# using information provided in the parameterInfo.csv file
j2kAggregateParameters <- function(hru_par, reach_par, info_par) {

  reach_lengths_w   <- reach_par[, "length"] / sum(reach_par[, "length"])
  hru_area  <- sum(hru_par[, "area"])
  hru_areas <- hru_par[, "area"]
  hru_areas_w <- hru_areas / hru_area
  
  # aggregate parameters found in info_par
  n <- nrow(info_par)
  aggregated_par <- rep(NA, n)
  for (k in 1:n) {
    # is it a lumped parameters
    if (info_par[k, "lumped"]) {
      aggregated_par[k] <- info_par[k, "default"]
    } else {
      # how it should be aggrgated depends on the type of unit (misc, mm, L or reach)
      if (info_par[k, "unit_type"] == "misc" || info_par[k, "unit_type"] == "mm") {
        i <- which(colnames(hru_par) == info_par[k, "name_parfile"])
        if (length(i) == 1) aggregated_par[k] <- sum(hru_par[, i] * hru_areas_w)
      } else if (info_par[k, "unit_type"] == "L") {
        i <- which(colnames(hru_par) == info_par[k, "name_parfile"])
        if (length(i) == 1) aggregated_par[k] <- sum(hru_par[, i]) / hru_area
      } else if (info_par[k, "unit_type"] == "reach") {
        i <- which(colnames(reach_par) == info_par[k, "name_parfile"])
        if (length(i) == 1) aggregated_par[k] <- sum(reach_par[, i] * reach_lengths_w)
      } else {
        stop("unknown 'unit_type'")
      }
      if (length(i) != 1) warning("Cannot find parameter '", info_par[k, "name_parfile"], "'")
    }
  }
  names(aggregated_par) <- info_par[, "name"]
  aggregated_par
}

# write multiple optimizer.dat files
j2kOptasWriteOptimizer <- function(exp, headers, folder, filename = "optim", n_files = 1, verbose = TRUE) {
  stopifnot(ncol(exp) == length(headers))
  n <- nrow(exp)
  m <- ncol(exp)
  exp_types <- c("@types", paste(c("JAMSInteger", rep("JAMSDouble", m), ""), collapse = "\t"), "@data", "@start")
  exp_headers <- paste(c("ID", headers, ""), collapse = "\t")
  exp_values <- cbind(seq(0, n - 1, 1), "", exp, "")
  if (n_files == 1) {
    if (verbose) {message("Writting file '", paste0(folder, filename, ".dat"), "'"); flush.console()}
    .j2kOptasWriteOptim(exp_values, exp_types = exp_types, exp_headers = exp_headers, filepath = paste0(folder, filename, ".dat"))
  } else {
    starts <- seq(1, n, floor(n / n_files))[1:n_files]
    chunck <- data.frame(start = starts, end = c(starts[-1L] - 1, n))
    chunck$length <- chunck$end - chunck$start + 1
    chunck$fn <- paste0(filename, "_", 1:nrow(chunck), ".dat")
    if (verbose) {message("Writting ", nrow(chunck), " files in folder '", folder, "'"); flush.console()}
    for (k in 1:nrow(chunck)) {
      if (verbose) {message("Writting file '", chunck$fn[k], "' [", chunck$length[k], " rows: ", chunck$start[k], " --> ", chunck$end[k], "]"); flush.console()}
      .j2kOptasWriteOptim(exp_values[chunck$start[k]:chunck$end[k], ], exp_types = exp_types, exp_headers = exp_headers, filepath = paste0(folder, chunck$fn[k]))
    }
  } 
}

# write a single optimizer.dat file (not for end users)
.j2kOptasWriteOptim <- function(exp, exp_types, exp_headers, filepath) {
  write(x = c("@context", "optas.optimizer.management.SimpleOptimizationController	optimizer	2", 
              "@ancestors", "@filters", "@attributes"),
        file = filepath)
  write(x = paste(exp_headers, collapse = "\t"), file = filepath, append = TRUE)
  write(x = exp_types,  file = filepath, append = TRUE)
  write.table(x = exp, file = filepath, quote = FALSE, dec = ".", na = "ERROR",
              row.names = FALSE, col.names = FALSE, sep = "\t", append = TRUE)
}



j2kOptasReadResults <- function(fp, variables = NULL, verbose = TRUE) {
  .formatInfo <- function(info) {
    names(info) <- c("n_variable", "n_timestep", "n_simulation", "variables_names", "time", "simulations_ids", "first_simulation")
    info[[4]] <- info[[4]][-length(info[[4]])]
    info[[5]] <- as.Date(info[[5]])
    colnames(info[[7]]) <- info[[4]][-1]
    info
  }
  if (is.null(variables)) {
    if (verbose) message("Getting first simulation data and general information on file ...")
    if (length(fp) > 1) warning("Only first file used.")
    fp <- fp[1];
    if (is_optas_file(fp)) {
      info <- get_optas_template(fp)
      info <- .formatInfo(info)
      if (verbose) {
        message(info[[1]], " variables were found:") 
        cat(info[[4]], sep = "   ")
        cat("\n")
        message(info[[2]], " timesteps were found:")
        print(summary(info[[5]]))
        message(info[[3]], " simulations were found:")
        cat(head(info[[6]]))
        cat(" ... ")
        cat(tail(info[[6]]))
        cat("\n")
      }
      time_vector <- info[[5]]
      common_data <- info[[7]]
      info <- info[-c(5, 7)]
      list(info = info, time = time_vector, data = common_data)
    } else {
      stop("Error while trying to read the file!")
    }
  } else {
    optas <- list()
    n <- length(fp)
    for (k in 1:n) {
      if (verbose) message("Processing file ", k, "/", n, ": ", fp[k])
      optas[[k]] <- get_optas(fp[k], col_ids = variables - 1, verbose = verbose)
    }
    info <- .formatInfo(optas[[1]][[1]])
    time_vector <- info[[5]]
    common_data <- info[[7]]
    info <- info[-c(5, 7)]
    common_data <- as.data.frame(common_data)
    common_data <- common_data[, - (variables - 1)]
    out <- list()
    for (k in 1:length(variables)) {
      out[[k]] <- do.call(cbind, lapply(optas, function(e) e[[k+1]]))
    }
    names(out) <- info[[4]][variables]
    c(list(info = info, time = time_vector, data = common_data), out)
  }
}



#####################################################################
#
# Plot Functions Section
# 
#####################################################################

j2kOptasPlot_parglimpse <- function(p, p_values, 
                                    p_types_col = c(snow = "lightblue", landuse = "darkgreen", soil = "brown", 
                                                    geology = "darkblue", reach = "blue", hru = "orange")) {
  stopifnot(
    "type"%in%colnames(p),
    "label"%in%colnames(p),
    "unit"%in%colnames(p),
    require(latex2exp)
  )
  n <- nrow(p)
  lbls <- list()
  for (k in 1:n) lbls[[k]] <- TeX(paste0(par_sel$label[k], " \\[", par_sel$unit[k], "\\] = ", signif(p_values[k], 3)))
  max_width <- max(vapply(lbls, strwidth, numeric(1L), unit = "inch", USE.NAMES = FALSE))
  cols <- p_types_col[as.character(p[["type"]])]
  plot(c(0, 1), c(0, n), type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty ="n")
  for (k in 1:n)
    text(0.5, nrow(par_sel) - k + 1, labels = lbls[[k]], col = cols[k])
  
}

saPlotEffect <- function(E1, ET=NULL, title = "", labels = NULL,
                         cols = c("red", "grey"), horiz = FALSE) {
  .sa.addbarplot <- function(Z, col, horiz, xlim, ylim, las, width, space) {
    barplot(Z[, 1], ylim = ylim, xlim = xlim,  add = TRUE,
            col = col, border = NA, horiz = horiz, las = las, 
            width = width, space = space)
    os <- 1:n * (width + width * space) - width / 2
    if (horiz) {
      arrows(Z[, 2], os, Z[, 3], os, length = 0.05, code = 3, angle = 90, lwd = 2, col = "black", lend = 3)
      arrows(Z[, 1], os, Z[, 3], os, length = 0.05, code = 2, angle = 90, lwd = 2, col = col, lend = 3)
    } else {
      arrows(os, Z[, 2], os, Z[, 3], length = 0.05, code = 3, angle = 90, lwd = 2, col = "black", lend = 3)
      arrows(os, Z[, 1], os, Z[, 3], length = 0.05, code = 2, angle = 90, lwd = 2, col = col, lend = 3)
    }
  }
  
  E1[E1 < 0] <- 0
  if (is.null(ET)) ET <- matrix(NA, nrow(E1), ncol(E1))
  ET[ET < 0] <- 0
  if (is.null(labels)) labels <- rownames(E1)
  
  n <- nrow(E1)
  width <- 0.8
  space <- 0.1
  xlim <- c(0, (width + width * space) * n)
  las <- 2
  ylab <- "Effect [-]"
  if (horiz) {
    las <- 1
    ylim <- xlim
    xlim <- c(0, 1)
    E1 <- E1[n:1, ]
    ET <- ET[n:1, ]
    labels <- rev(labels)
    xlab <- ylab
    ylab <- ""
  } else {
    xlab <- ""
    ylim <- c(0, 1)
  }
  
  barplot(E1[, 1], ylim = ylim, xlim = xlim, names.arg = labels, 
          col = "transparent", border = NA, horiz = horiz, las = las, 
          width = width, space = space, xlab = xlab, ylab = ylab)
  grid()
  mtext(title, side = 3, line = space * width)
  .sa.addbarplot(ET, cols[2], horiz, xlim, ylim, las, width, space)
  .sa.addbarplot(E1, cols[1], horiz, xlim, ylim, las, width, space)
  
}


saEffectSummary <- function(E1, E2, par_lab, sig_lab, osy =  c(0.05, 0.05)) {
  .rect <- function(nx, ny, z, z_max = 1, osx = c(0.1, 0.1), osy = c(0.01, 0.01), ...) {
    val_max <- 1 - sum(osy)
    val <- z / z_max * val_max
    rect(nx + osx[1], ny + osy[1], nx + 1 - osx[2], ny + osy[2] + val, ...)
  }
  
  .axis <- function(x, y, size_tick, at, labels, cex = 0.3, las = 1, lwd = 0.5) {
    stopifnot(length(at)==length(labels))
    segments(x, y[1], x, y[2], lwd = lwd)
    n <- length(at)
    ratio <- diff(y) / 1
    for (k in 1:n) {
      # segments(x, y[1] + at[k] * ratio, x + size_tick, y[1] + at[k] * ratio, lwd = lwd)
      # text(x + size_tick, y[1] + at[k] * ratio, labels = labels[k], pos = 4, cex = cex)
      segments(x, y[1] + at[k] * ratio, x - size_tick, y[1] + at[k] * ratio, lwd = lwd, offset = 0.1)
      text(x - size_tick, y[1] + at[k] * ratio, labels = labels[k], pos = 2, cex = cex, offset = 0.1)
    }
  }
  
  nx <- length(par_lab)
  ny <- length(sig_lab)
  par("mai" = c(0.8, 0.8, 0.1, 0.1))
  plot(c(0, nx), c(0, ny), type = "n", xaxt = "n", xlab = "", yaxt = "n", ylab = "")
  abline(h = 0:ny, v = 0:nx, col = "lightgrey", lty = 1, lwd = 0.5)
  
  for (i in 1:nx) {
    for (j in 1:ny) {
      .rect(i - 1, j - 1, E1[i, j], osx = c(0.1, 0.5), osy = osy, col = "red", border = NA)
      .rect(i - 1, j - 1, E2[i, j], osx = c(0.5, 0.1), osy = osy, col = "grey", border = NA)
    }
  }
  for (k in 1:nx) {
    mtext(text = par_lab[[k]], side = 1, line = 0.25, at = k - 0.5, las = 2)
  }
  for (k in 1:ny) {
    mtext(text = sig_lab[[k]], side = 2, line = 0.25, at = k - 0.5, las = 1)
    # .axis(par("usr")[1], c(k - 1 + osy[1], k - osy[1]), nx / 100, at = c(0, 0.5, 1), labels = c(0, 0.5, 1), las = 1, lwd = 0.5)
    .axis(0, c(k - 1 + osy[1], k - osy[1]), nx / 200, at = c(0, 0.5, 1), labels = c(0, 0.5, 1), las = 1, lwd = 0.5)
  }
  redrawPlotEdges()
}


saSimpleBoxPlots <- function(val, cols, titles, units, filename = NULL, osu = 0.3, os = 0.1, ylim_add = NULL, ylim_global = FALSE) {
  
  n <- ncol(val[[1]])
  m <- length(val)
  
  plotdim <- c(2, n)
  matmai <- matrix(1:n, 1, n)
  
  ylim_g <- range(ylim_add, val, na.rm = TRUE)
  
  if (!is.null(filename)) png(filename = filename, width = plotdim[2], height = plotdim[1], res = 300, units = "in")
  
  maifun <- ihLayout2(matmai, plotdim, mai = maimai, lmai = rep(0.3, n))
  
  for (j in 1:n) {
    
    xlim <- c(0, m)
    ylim <- range(ylim_add[j], lapply(val, function(e, index) e[, index], index = j), na.rm = TRUE)
    if (ylim_global)  ylim <- ylim_g
    
    maifun(j)
    plot(xlim, ylim, type = "n", xaxt = "n", xlab = "", yaxt = "n", ylab = "")
    grid()
    
    old_par <- par(tck = -0.05, cex.axis = 0.7, mgp = c(3, 0.25, 0))
    axis(side = 2, las = 1)
    mtext(TeX(titles[j]), side = 3, line = 1.2, cex = 0.7)
    mtext(TeX(paste0(" $\\[$", units[j], "$\\]$")), side = 3, line = 0.2, cex = 0.4)
    par(old_par)
    
    for (k in 1:m) {
      if (nrow(val[[k]]) == 3) {
        rect(k - 1 + osu , val[[k]][2, j], k - osu, val[[k]][3, j], col = cols[[k]][2], border = cols[[k]][1], lwd = 1)
      }
      segments(k - 1 + os, val[[k]][1, j], k - os, val[[k]][1, j], col = cols[[k]][1], lend = 3, lwd = 2, lty = 1)
    }
    
    redrawPlotEdges()
  }
  
  layout(matrix(1, 1, 1))
  
  if (!is.null(filename)) dev.off()
  
}
