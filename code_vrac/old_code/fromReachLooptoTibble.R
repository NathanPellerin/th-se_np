#*******************************************************************************
#* Created 16/11/2022
#* Creator J.B
#* ReachLoop to tibble
#*******************************************************************************

#define function
fromReachLooptoTibble <- function(block){
  
  blockout = block
  blockout = unlist(blockout)
  blockout = strsplit(blockout, split ='\t')
  blockout = blockout[blockout != c("@start", "@end")]
  
  time = unlist(blockout[1])
  time = strsplit(time, split ='\t')
  time = time[2]

  blockout = blockout[-c(1,2)] %>% 
    unlist() %>% 
    matrix(ncol = 3, byrow =TRUE)
  
  blockout  = as_tibble(blockout)
  blockout= rename(blockout, ReachID = V1, 
                   Storage = V2,
                   Runoff = V3) 
  blockout = mutate(blockout, Storage = as.numeric(Storage)/1e3, #in m3
                    Runoff = as.numeric(Runoff)/(1e3*24*3600), #in m3/s
                    time = as.Date(unlist(time), format="%Y-%m-%d %H:%M"))
  
  return(blockout)
}

j2kReadReachLoop <- function(fp, verbose=TRUE){
  # get column names
  if (verbose){message("Fetch variable names ..."); flush.console()}
  colnames <- readLines(con = fp, n = 8, ok = TRUE, warn = TRUE,
                        encoding = "unknown", skipNul = FALSE)[8]
  colnames <- strsplit(colnames, "\t")[[1]]
  
  # get data
  if (verbose){message("Fetch ReachLoop data ..."); flush.console()}
  reachloop <- read.table(fp,skip=11,fill = NA,col.names=colnames)
  colnames(reachloop) <- colnames
  if (verbose){message("Converting time ..."); flush.console()}
  date <- as.Date(as.character(reachloop[reachloop[,1] == "TimeLoop",2]), format="%Y-%m-%d")
  
  if (verbose){message("Creating final data.frame ..."); flush.console()}
  reachloop <- reachloop[!(reachloop[,1] %in% c("TimeLoop","@start","@end","@data")),]
  reachloop = tibble(reachloop)
  for(i in 1:dim(reachloop)[2]){
    reachloop[[i]] = as.numeric(as.character(reachloop[[i]]))
  }
  reachloop = mutate(reachloop, Date = rep(date, each=dim(unique(reachloop[,1]))[1]))
  return(reachloop)
}


