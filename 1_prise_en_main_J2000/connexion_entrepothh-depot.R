# Script to import and visualise BNPE withdrawal data over the Rhone basin
# Author : LC
# Last update : 2021-05-25


# LIBRARIES -----------

library(rgdal)
library(maptools)
library(sf)
library(data.table)
library(tidyverse)

# FUNCTION --------

GetServer <- function() {
  
  hhly = "ly-unites/Riverly/Hhly"
  path1=file.path(hhly)
  path2=file.path('//10.69.192.6',hhly)
  path3=file.path('//10.69.192.7',hhly)
  path4=file.path('//10.69.192.8',hhly)
  path5=file.path('//10.69.192.9',hhly)
  path6=file.path('//ly-data.irstea.priv',hhly)
  if(file.exists(path1)) {
    path=path1
  } else if(file.exists(path2)) {
    path=path2
  } else if(file.exists(path3)) {
    path=path3
  } else if(file.exists(path4)) {
    path=path4
  } else if(file.exists(path5)) {
    path=path5
  } else if(file.exists(path6)) {
    path=path6
  }
  return(path)
}
addTrans  <- function(color,trans) {
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}



# DIRECTORIES --------

bnpe_fold  = '/Entrepothh_depot/databaseBNPE/Export BNPE 2020'
mdr_fold   = '/Entrepothh_depot/MDR/SIG/'
out_fold   = 'C:/Data/Data/bnpe/rhone'



# IMPORT ALL NECESSARY DATA --------------

# BNPE shapefile
bnpe_shp  = st_read(file.path(GetServer(),bnpe_fold,'Shapefiles','OuvragePrel_FRA.shp'))

# BNPE prelevements par anne
prelev = list()
for(yr in 2012:2018) {
  file = paste(yr,'.csv',sep='')
  prelev[[as.character(yr)]] = read.csv(file.path(GetServer(),bnpe_fold,'Quantitatif par annes',file),sep=";")
}

# Codes usages BNPE
bnpe_usages = fread(file.path(GetServer(),bnpe_fold,'Codes Usages','NSA_481_20210217_SANDRE.csv'),data.table=F,skip=1,header=T)

# Contour Rhone
rhone_shp  = st_read(file.path(GetServer(),mdr_fold,'Gographie','contour_bv_rhone.shp'))
rhone_shp  = st_read(file.path(GetServer(),mdr_fold,'J2000','watersheds.shp'))
st_crs(rhone_shp) <- st_crs(bnpe_shp)



# EXTRACT RHONE --------------------

# Points de prelevements localiss dans le bassin du Rhone
tmp = st_contains(rhone_shp,bnpe_shp,sparse=F)
i_rhone = which(sapply(1:ncol(tmp), function(i) any(tmp[,i])))
bnpe_rhone = bnpe_shp[i_rhone,]
rm(tmp)
cat(nrow(bnpe_rhone)*100/nrow(bnpe_shp),'% (',nrow(bnpe_rhone),'/',nrow(bnpe_shp),') points dans le bassin du Rhne')
if(!file.exists(file.path(out_fold,'OuvragePrel_RHONE.shp'))) {
  st_write(obj = bnpe_rhone, file.path(out_fold,'OuvragePrel_RHONE.shp'))
}

# Volumes correspondant  des points de prlvements localiss dans le Rhne
prelev_rhone = list()
prelev_rhone_long = NULL
for(yr in 2012:2018) {
  i_rhone = which(prelev[[as.character(yr)]]$Code %in% bnpe_rhone$CdOuvrageP)
  tmp = prelev[[as.character(yr)]][i_rhone,]
  if(any(tmp$Volume==0)) tmp = tmp[-which(tmp$Volume==0),]
  prelev_rhone[[as.character(yr)]] = tmp
  prelev_rhone_long = rbind(prelev_rhone_long,tmp)
  cat('Year',yr,':',length(i_rhone),'/',nrow(prelev[[as.character(yr)]]),'\n')
  rm(tmp)
}

# Ajout de la source (CONT/SOUT) dans le shp
type <- prelev_rhone_long %>%
  as_tibble() %>%
  select(Code,Type_Eau) %>%
  distinct()
bnpe_rhone_source <- bnpe_rhone %>% 
  full_join(type, by=c("CdOuvrageP"="Code")) %>%
  mutate(Usage_Type = paste(UsagePrinE,Type_Eau,sep='_'))
st_write(obj = bnpe_rhone_source, file.path(out_fold,'OuvragePrel_RHONE_source2.shp'))


# Ajout de la source (CONT/SOUT), de tous les usages par point et du volume mdian dans le shp - export HRU-delin
type <- prelev_rhone_long %>%
  as_tibble() %>%
  group_by(Code,Type_Eau,Code_Usage) %>% 
  summarise(Median_Volume = median(Volume))
bnpe_rhone_source_vol <- bnpe_rhone %>% 
  full_join(type, by=c("CdOuvrageP"="Code"))
st_write(obj = bnpe_rhone_source_vol, file.path(out_fold,'OuvragePrel_RHONE_source_vol.shp'))


# Table rorganise
tmp1 <- prelev_rhone_long %>%
  as_tibble() %>%
  mutate(Code_Usage_2 = ifelse(Code_Usage=='5A','5',Code_Usage)) %>%                             #     and month
  pivot_wider(id_cols = c(Code, Type_Eau,Code_Usage_2),        # Transform into wider table format
              names_from = Annee,                             #     and select names column from 'phase' factor levels
              values_from = Volume)
tmp2 <- prelev_rhone_long %>%
  as_tibble() %>%
  mutate(Code_Usage_2 = ifelse(Code_Usage=='5A','5',Code_Usage)) %>%                             #     and month
  pivot_wider(id_cols = c(Code, Type_Eau, Annee),        # Transform into wider table format
              names_from = Code_Usage_2,                             #     and select names column from 'phase' factor levels
              values_from = Volume)

# Points de prlvements avec duplicats pour diffrents usages
duplicates <- tmp1 %>%
  filter(Code %in% Code[which(duplicated(tmp1$Code))]) %>%
  # filter(grepl(pattern = '4', Code_Usage_2)) %>%
  arrange(Code)
write.csv(duplicates,file = file.path(out_fold,'duplicates.csv'),quote = F,row.names = F,sep=';',na = '-')



# COMPARISON VOLUME/POINTS INFO ------------

# Primary uses located
usage_primary = list()
for(i in unique(bnpe_rhone$UsagePrinE)) {
  usage_primary[[i]] = bnpe_rhone$CdOuvrageP[which(bnpe_rhone$UsagePrinE==i)]
}

# All uses associated with a volume anytime between 2008:2018
usage_all = list()
names = NULL
for(yr in 2008:2018) names = c(names,unique(prelev_rhone[[as.character(yr)]]$Code_Usage))
for(i in unique(names)) {
  codes = NULL
  for(yr in 2008:2018) {
    idx   = which(prelev_rhone[[as.character(yr)]]$Code_Usage == i)
    codes = c(codes,prelev_rhone[[as.character(yr)]]$Code[idx])
  }
  usage_all[[i]] = unique(codes)
}

# Non-primary uses -  what is the primary use of these stations?
usage_secondary = names(usage_all)[which(! names(usage_all) %in% names(usage_primary))]
for(i in usage_secondary) {
  cat(i,'-',length(usage_all[[i]]),'\n')
  for(j in names(usage_primary)) {
    if(any(usage_all[[i]] %in% usage_primary[[j]])) {
      cat('...',j,'-',length(which(usage_all[[i]] %in% usage_primary[[j]])),'\n')
    }
  }
}



# EXPORT RHONE DATA (POINTS+VOLUMES) --------------------

Prelevement_BNPE_Rhone = list()

Prelevement_BNPE_Rhone[['points']][['rhone']] <- bnpe_rhone

# Volume prlev par anne
for(ouvrage in bnpe_rhone$CdOuvrageP) {
  tmp <- NULL
  for(yr in 2012:2018) {
    yr_char = as.character(yr)
    idx = match(ouvrage,prelev_rhone[[yr_char]]$Code)
    if(!is.na(idx)) {
      tmp = rbind(tmp,prelev_rhone[[yr_char]][idx,])
    }
  }
  Prelevement_BNPE_Rhone[['volumes']][[ouvrage]] <- tmp
}

# Points de prelevements du Rhone par type d'usage
for(i in unique(bnpe_rhone$UsagePrinE)) {
  
  # extraction des points
  ix = which(bnpe_rhone$UsagePrinE == i)
  Prelevement_BNPE_Rhone[['points']][[i]][['points']] = bnpe_rhone[ix,]
  
  # extraction des infos sandre sur l'usage
  ix = which(bnpe_usages[,5] == i)
  Prelevement_BNPE_Rhone[['points']][[i]][['info']] = bnpe_usages[ix,]
  
}

setwd(out_fold)
save(Prelevement_BNPE_Rhone, file='Prelevement_BNPE_Rhone.RDATA')
load('Prelevement_BNPE_Rhone.RDATA')



# STATS d'USAGES -----------------

# Nb of extraction points per use type
len = length(Prelevement_BNPE_Rhone$points$rhone$UsagePrinE)
table(Prelevement_BNPE_Rhone$points$rhone$UsagePrinE)

# Nb of extraction points per source type (e.g. 2018)
len = length(prelev_rhone$'2018'$Type_Eau)
table(prelev_rhone$'2018'$Type_Eau)*100/len
table(prelev_rhone$'2018'$Type_Eau)

# Volume extracted per use type (e.g. 2018)
aggregate(prelev_rhone$'2018'$Volume, 
          by=list(prelev_rhone$'2018'$Code_Usage),
          FUN=sum)

# Volume extracted per source type (e.g. 2018)
aggregate(prelev_rhone$'2018'$Volume, 
          by=list(prelev_rhone$'2018'$Type_Eau),
          FUN=sum)

# Volume extracted per use type (e.g. 2018)
vol_year <- c(sum(prelev_rhone$'2012'$Volume),
              sum(prelev_rhone$'2013'$Volume),
              sum(prelev_rhone$'2014'$Volume),
              sum(prelev_rhone$'2015'$Volume),
              sum(prelev_rhone$'2016'$Volume),
              sum(prelev_rhone$'2017'$Volume),
              sum(prelev_rhone$'2018'$Volume))*10**-9

# Source per use type (e.g. 2018)
aggregate(prelev_rhone$'2018'$Type_Eau, 
          by=list(prelev_rhone$'2018'$Code_Usage),
          FUN=unique)

## Summary
# number of points
count = aggregate(prelev_rhone$'2018'$Type_Eau, 
                  by=list(prelev_rhone$'2018'$Code_Usage),
                  FUN=function(x) c(length(which(x=='CONT')),
                                    length(which(x=='SOUT'))))
# volume
count[,3]=count[,2]
for(i in count$Group.1) {
  for(j in c('CONT','LIT','SOUT')) {
    ix = which(prelev_rhone$'2018'$Code_Usage==i)
    jx = which(prelev_rhone$'2018'$Type_Eau==j)
    count$V3[which(count$Group.1==i),which(c('CONT','SOUT')==j)] = round(sum(prelev_rhone$'2018'$Volume[intersect(ix,jx)])*10^(-6),2)
  }
}

summary = count
colnames(summary) <- c('usage','points','volume')
colnames(summary$points) <- c('CONT','SOUT')
colnames(summary$volume) <- c('CONT','SOUT')
summary[match(c("0","2","2B","4","4D","5","6","6D","7","13a"),summary$usage),]
summary = summary[-9,]

# PLOT --------------------

setwd(file.path(out_fold,'Prelevement_BNPE_Rhone'))

png('barplot_prelev_par-usage-source_rhone.png',width=10,height=5,units='in',res=500)
col    = c('royalblue','darkblue')
source = c('CONT','SOUT')
par(mar=c(5,8,4,2),las=1,mfrow=c(1,2))
names = bnpe_usages[match(summary$usage[order(rowSums(summary$points))],bnpe_usages[,5]),6]
barplot(t(summary$points[order(rowSums(summary$points)),]),names.arg = names,horiz = T,col=col)
mtext(side=1,text = 'Number of extraction points', line = 2.5, las=0)
legend('bottomright',legend = source,fill=col)
names = bnpe_usages[match(summary$usage[order(rowSums(summary$volume))],bnpe_usages[,5]),6]
barplot(t(summary$volume[order(rowSums(summary$volume)),]),names.arg = names,horiz = T,col=col)
mtext(side=1,text = 'Volume extracted (Mm3 over 2018)', line = 2.5, las=0)
legend('bottomright',legend = source,fill=col)
dev.off()



png('barplot_prelev_par-annee_rhone.png',width=6,height=3.5,units='in',res=500)
par(mar=c(5,8,4,2),las=1,mfrow=c(1,1))
barplot(vol_year,names.arg = 2012:2018,horiz = F,ylab='Volume extracted (Mm3)')
dev.off()


# MAPS --------------------

setwd(file.path(out_fold,'Prelevement_BNPE_Rhone'))

pdf('points-prelev_par-usage_rhone.pdf',width=5,height=10)
png('points-prelev_par-usage_rhone.png',width=5.5,height=7,units='in',res=500)
col = c(addTrans('black',50),'forestgreen','green','green','gold2','gold2','royalblue','darkblue','darkblue','orange','red','grey')
par(mfrow=c(3,4),mar=c(1,1,2,1))
names = c("rhone","2","2B","2E","4","4D","5","6","6D","7","13a","0")
for(i in names) {
  
  cat(i,' ')
  
  icount = which(names == i)
  
  # bassin du rhone
  plot(rhone_shp$geometry, border='lightgrey', lwd=0.5, col=NA)
  
  # points
  if(i == 'rhone') {
    plot(Prelevement_BNPE_Rhone$points[[i]]$geometry,pch=16,col=col[icount],cex=0.3,add=T)
  } else {
    plot(Prelevement_BNPE_Rhone$points[[i]]$points$geometry,pch=16,col=col[icount],cex=0.3,add=T)
  }
  
  # type d'usage (title)
  if(i == 'rhone') {
    main = 'All points'
  } else {
    main = Prelevement_BNPE_Rhone$points[[i]]$info[6]
  }
  mtext(side = 3, text = paste(i,tolower(main),sep = ' - '), col = col[icount])
  
}
dev.off()

