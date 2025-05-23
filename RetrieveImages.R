library(utils)
library(stringr)
library(ridigbio)
library(tidyverse)
#install.packages("rgbif")
library("rgbif")
#install.packages("gatoRs")
library("gatoRs")
library(TrenchR)

### Retrieve specimen info 

#ridigbio
#https://cran.r-project.org/web/packages/ridigbio/vignettes/MediaAPIDemo.html
# https://idigbio.github.io/ridigbio/articles/BasicUsage.html

# Load core libraries; install these packages if you have not already
#install.packages("ridigbio")


#Find specimens
po <- idig_search_media(rq=list(scientificname="Pontia occidentalis"))

#-----
#GBIF
#https://docs.ropensci.org/rgbif/articles/getting_occurrence_data.html

#https://docs.ropensci.org/rgbif/articles/gbif_credentials.html
#install.packages("usethis")
#usethis::edit_r_environ()

#GBIF_USER="lbuckley"
#GBIF_PWD="c.....3.."
#GBIF_EMAIL="l.b.buckley@gmail.com"

name_backbone(name="Pontia occidentalis") #5137791
name_backbone(name="Pieris rapae") #1920496

#---
occ_download(pred("taxonKey", 5137791),format = "DWCA")
d <- occ_download_get('0000616-250117142028555') %>%
  occ_download_import()
#https://doi.org/10.15468/dl.yk2m25

po1<- read.delim("po/multimedia.txt", header = TRUE, sep = "\t")
  
po2<- po1[po1$publisher=="Museum of Comparative Zoology, Harvard University",]
po2<- po1[po1$publisher=="Smithsonian Institution, NMNH, Entomology",]

#---
occ_download(pred("taxonKey", 1920496),format = "DWCA")
occ_download_wait('0000619-250117142028555')
pr <- occ_download_get('0000618-250117142028555') %>%
  occ_download_import()

pr1<- read.delim("pr/multimedia.txt", header = TRUE, sep = "\t")
pr2<- pr1[pr1$publisher=="Museum of Comparative Zoology, Harvard University",]

#Mothra NHM iCollections
#https://data.nhm.ac.uk/dataset/

#---
#gatoRs
df <- gators_download(synonyms.list = c("Pontia occidentalis"), write.file = TRUE, filename = "data/idigbio_gbif_Poccidentalis.csv")

#=====================================
### Download images by URL

#set download location
#toggle between desktop (y) and laptop (n)

desktop<- "n"

if(desktop=="y") location <- "/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/WingColoration/images/"
if(desktop=="n") location <- "/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/WingColoration/images/"

#setwd to access image data
if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/WingColoration/")
if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/WingColoration/")

#Pontia occidentalis
#SCAN
imgs <- read.csv("data/SCAN_Poccidentalis/images.csv")

#imgs<- imgs[imgs$Owner=="Yale University",]
#imgs<- imgs[imgs$Owner=="Rights for individual observations belong to the individual observers. In jurisdictions where collections of data are are considered intellectual property, the rights holder of this collection is the California Academy of Sciences.",]

#find and remove problematic images
imgs$valid<-"yes"
#imgs$valid[grep("default", imgs$accessURI)]<-"no"
imgs$valid[grep("original", imgs$accessURI)]<-"no"
imgs$valid[grep("inaturalist", imgs$accessURI)]<-"no"
#inds<- which(imgs$valid=="yes")
imgs<- imgs[which(imgs$valid=="yes"),]

#make download id
imgs$downloadID<- paste("scan_po_img", 1:nrow(imgs), ".jpg", sep="")

#write out
write.csv(imgs, "data/ImageList/scan_po.csv")

#  try(download.file(imgs$accessURI[i], paste(location,"scan_po/",str_extract(imgs$thumbnailAccessURI[i], "([^/]+$)"),".jpg",sep=""),
#                    cacheOK = FALSE, mode = "wb"))

for(i in 1:nrow(imgs)){
  try(download.file(imgs$accessURI[i], paste(location,"specimens/",imgs$downloadID[i],sep=""),
                cacheOK = FALSE, mode = "wb"))
}

#-----
#gbif
imgs_gb<- read.delim("data/gbif/Poccidentalis/multimedia.txt", header = TRUE, sep = "\t")

#find and remove problematic images
imgs_gb$valid<-"yes"
#imgs_gb$valid[grep("default", imgs_gb$identifier)]<-"no"
imgs_gb$valid[grep("original", imgs_gb$identifier)]<-"no"
imgs_gb$valid[grep("inaturalist", imgs_gb$identifier)]<-"no"
imgs_gb$valid[grep("e-butterfly", imgs_gb$identifier)]<-"no"
imgs_gb$valid[grep("bugguide.net", imgs_gb$identifier)]<-"no"
#inds<- which(imgs_gb$valid=="yes")
imgs<- imgs_gb[which(imgs_gb$valid=="yes"),]

#make download id
imgs$downloadID<- paste("gbif_po_img", 1:nrow(imgs), ".jpg", sep="")

#write out
write.csv(imgs, "data/ImageList/gbif_po.csv")

grep("Preview", imgs_gb$identifier) #** fix overwrite

#check duplicate institutions
match(unique(imgs$Owner), unique(imgs_gb$publisher))

for(i in 1:nrow(imgs)){
  try(download.file(imgs$identifier[i], paste(location,"specimens/",imgs$downloadID[i],sep=""),
                    cacheOK = FALSE, mode = "wb"))
}

#-----
#idigbio
imgs_db <- read.csv("data/idigbio_Poccidentalis/multimedia.csv")

#find and remove problematic images
imgs_db$valid<-"yes"
imgs_db$valid[grep("default", imgs_db$ac.accessURI)]<-"no"

#check duplicate institutions
occ <- read.csv("data/idigbio_Poccidentalis/occurrence_raw.csv")

match1<- match(unique(imgs$Owner), unique(occ$dcterms.rightsHolder))
dups<- unique(imgs$Owner)[!is.na(match1)]
match1<- match(unique(imgs_gb$publisher), unique(occ$dcterms.rightsHolder))
dups1<- unique(imgs_gb$publisher)[!is.na(match1)]

dup.ids<- occ$coreid[which(occ$dcterms.rightsHolder %in% dups)]
imgs_db$valid[which(imgs_db$coreid %in% dup.ids)]<-"duplicate"

#inds<- which(imgs_db$valid=="yes")
imgs<- imgs_db[which(imgs_db$valid=="yes"),]

#make download id
imgs$downloadID<- paste("idigbio_po_img", 1:nrow(imgs), ".jpg", sep="")

#write out
write.csv(imgs, "data/ImageList/idigbio_po.csv")

for(i in 1:nrow(imgs)){
  try(download.file(imgs$ac.accessURI[i], paste(location,"specimens/",imgs$downloadID[i],sep=""),
                    cacheOK = FALSE, mode = "wb"))
}
#** Some download errors

#-----------------------
#Pieris rapae
#SCAN
imgs <- read.csv("data/SCAN_Prapae/images.csv")

#find and remove problematic images
imgs$valid<-"yes"
imgs$valid[grep("default", imgs$accessURI)]<-"no"
imgs$valid[grep("original", imgs$accessURI)]<-"no"
imgs$valid[grep("inaturalist", imgs$accessURI)]<-"no"
inds<- which(imgs$valid=="yes")
imgs<- imgs[which(imgs$valid=="yes"),]

#make download id
imgs$downloadID<- paste("scan_pr_img", 1:nrow(imgs), ".jpg", sep="")

#write out
write.csv(imgs, "data/ImageList/scan_pr.csv")

for(i in 1:nrow(imgs)){
  try(download.file(imgs$accessURI[i], paste(location,"specimens/",imgs$downloadID[i],sep=""),
                    cacheOK = FALSE, mode = "wb"))
}

#check duplicates
scan_pr<- read.csv("data/ImageList/scan_pr.csv")
#find duplicated links
inds<- which(duplicated(scan_pr$accessURI))
#files to delete
df<- scan_pr[inds,"downloadID"]

setwd(paste(location,"specimens/",sep="") )
for(i in 1:length(df)){
fn<- df[i]
#Check its existence
if (file.exists(fn)) {
  #Delete file if it exists
  file.remove(fn)
}
}

#-----
#idigbio
imgs_db <- read.csv("data/idigbio_Prapae/multimedia.csv")

#find and remove problematic images
imgs_db$valid<-"yes"
imgs_db$valid[grep("default", imgs_db$ac.accessURI)]<-"no"

#check duplicate institutions
occ <- read.csv("data/idigbio_Prapae/occurrence_raw.csv")

match1<- match(unique(imgs$Owner), unique(occ$dcterms.rightsHolder))
dups<- unique(imgs$Owner)[!is.na(match1)]

dup.ids<- occ$coreid[which(occ$dcterms.rightsHolder %in% dups)]
imgs_db$valid[which(imgs_db$coreid %in% dup.ids)]<-"duplicate"

imgs<- imgs_db[which(imgs_db$valid=="yes"),]

#make download id
imgs$downloadID<- paste("idigbio_pr_img", 1:nrow(imgs), ".jpg", sep="")

#write out
write.csv(imgs, "data/ImageList/idigbio_pr.csv")

for(i in 1:nrow(imgs)){
  try(download.file(imgs$ac.accessURI[i], paste(location,"specimens/",imgs$downloadID[i],sep=""),
                    cacheOK = FALSE, mode = "wb"))
}
#-----

#gbif
imgs_gb<- read.delim("data/gbif/Prapae/multimedia.txt", header = TRUE, sep = "\t")

#find and remove problematic images
imgs_gb$valid<-"yes"
imgs_gb$valid[grep("default", imgs_gb$identifier)]<-"no"
imgs_gb$valid[grep("original", imgs_gb$identifier)]<-"no"
imgs_gb$valid[grep("inaturalist", imgs_gb$identifier)]<-"no"
imgs_gb$valid[grep("e-butterfly", imgs_gb$identifier)]<-"no"
imgs_gb$valid[grep("bugguide.net", imgs_gb$identifier)]<-"no"
imgs_gb$valid[grep("images.ala.org.au", imgs_gb$identifier)]<-"no"
imgs_gb$valid[grep("www.artsobservasjoner.no", imgs_gb$identifier)]<-"no"
imgs_gb$valid[grep("observation.org", imgs_gb$identifier)]<-"no"
imgs_gb$valid[grep("nhm.ac.uk", imgs_gb$identifier)]<-"down" #** server currently down https://data.nhm.ac.uk/

#check duplicate institutions
match(unique(imgs$Owner), unique(imgs_gb$publisher))
match1<- match(unique(imgs_gb$publisher), unique(occ$dcterms.rightsHolder))
dups1<- unique(imgs_gb$publisher)[!is.na(match1)]

imgs<- imgs_gb[which(imgs_gb$valid=="yes"),]

#make download id
imgs$downloadID<- paste("gbif_pr_img", 1:nrow(imgs), ".jpg", sep="")

#write out
write.csv(imgs, "data/ImageList/gbif_pr.csv")

grep("Preview", imgs_gb$identifier) #** fix overwrite

#check duplicate institutions
match(unique(imgs$Owner), unique(imgs_gb$publisher))

for(i in 1:nrow(imgs)){
  try(download.file(imgs$identifier[i], paste(location,"specimens/",imgs$downloadID[i],sep=""),
                    cacheOK = FALSE, mode = "wb"))
}

#----------------
#NOT DOWNLOADED
#Download from Yale

#idigbio
imgs_db <- read.csv("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/WingColoration/analysis/traininsets/Yale_Prapae.csv")

#find and remove problematic images
imgs_db$valid<-"yes"
imgs_db$valid[grep("deliver.odai.yale.edu", imgs_db$image_link)]<-"no"

inds<- which(imgs_db$valid=="yes")

for(i in inds){
  try(download.file(imgs_db$image_link[i], paste(location,"yale/",str_extract(imgs_db$image_link[i], "([^/]+$)"),".jpg",sep=""),
                    cacheOK = FALSE, mode = "wb"))
}

for(i in inds){
  try(download.file(imgs_db$image_link[i], paste(location,"yale/",i,".jpg",sep=""),
                    cacheOK = FALSE, mode = "wb"))
}

#=========================================
#Combine Data

#read records with ids
#P. occidentalis
d.scan.po <-read.csv("data/ImageList/scan_po.csv")
d.gbif.po <-read.csv("data/ImageList/gbif_po.csv")
d.idig.po <-read.csv("data/ImageList/idigbio_po.csv")
d.scan.pr <-read.csv("data/ImageList/scan_pr.csv")
d.gbif.pr <-read.csv("data/ImageList/gbif_pr.csv")
d.idig.pr <-read.csv("data/ImageList/idigbio_pr.csv")

if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/WingColoration/")
if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/WingColoration/")

#gbif
#po
dat.po<- read.delim("data/gbif/Poccidentalis/occurrence.txt", header = TRUE, sep = "\t")
dat.po<- dat.po[,c("gbifID","eventDate","decimalLatitude","decimalLongitude")]   

match1<- match(dat.po$gbifID, d.gbif.po$gbifID) 
dat.po$link<- NA
dat.po$link<- d.gbif.po$identifier[match1]
dat.po$downloadID<- d.gbif.po$downloadID[match1]
dat.po<- dat.po[which(!is.na(dat.po$downloadID)),]

#pr
dat.pr<- read.delim("data/gbif/Prapae/occurrence.txt", header = TRUE, sep = "\t")
dat.pr<- dat.pr[,c("gbifID","eventDate","decimalLatitude","decimalLongitude")]   

match1<- match(dat.pr$gbifID, d.gbif.pr$gbifID) 
dat.pr$link<- NA
dat.pr$link<- d.gbif.pr$identifier[match1]
dat.pr$downloadID<- d.gbif.pr$downloadID[match1]
dat.pr<- dat.pr[which(!is.na(dat.pr$downloadID)),]

names(dat.po)<- c("id","date","lat","lon","link","downloadID")
names(dat.pr)<- c("id","date","lat","lon","link","downloadID")

#format dates
#drop 1800 dates
dat.po$date <- gsub("1800-01-01/", "", dat.po$date)
dat.pr$date <- gsub("1800-01-01/", "", dat.pr$date)
#keep only first date 
dat.po$date <- gsub("/.*", "", dat.po$date)
dat.pr$date <- gsub("/.*", "", dat.pr$date)
#ditch time
dat.po$date <- gsub("T.*", "", dat.po$date)
dat.pr$date <- gsub("T.*", "", dat.pr$date)

#add columns
dat.po$doy<- day_of_year(dat.po$date, format = "%Y-%m-%d")
dat.po$year<- format(as.Date(dat.po$date, format="%Y-%m-%d"),"%Y")
dat.po$month<- format(as.Date(dat.po$date, format="%Y-%m-%d"),"%m")
dat.po$day<- format(as.Date(dat.po$date, format="%Y-%m-%d"),"%d")

dat.pr$doy<- day_of_year(dat.pr$date, format = "%Y-%m-%d")
dat.pr$year<- format(as.Date(dat.pr$date, format="%Y-%m-%d"),"%Y")
dat.pr$month<- format(as.Date(dat.pr$date, format="%Y-%m-%d"),"%m")
dat.pr$day<- format(as.Date(dat.pr$date, format="%Y-%m-%d"),"%d")

dat.po$provider<-"gbif"
dat.pr$provider<-"gbif"

#---
dat.po.all<- dat.po
dat.pr.all<- dat.pr

#----------------------
#idigbio

dat.po<- read.csv("data/idigbio_Poccidentalis/occurrence.csv")
dat.po<- dat.po[,c("coreid",'idigbio.eventDate', 'idigbio.geoPoint')]  
#split lat, lon
inds<- which(dat.po$idigbio.geoPoint!="")
latlon<- lapply(dat.po$idigbio.geoPoint, FUN= function(x) strsplit(gsub("[{}\"]", "",x),",") )
latlon<- matrix(unlist(latlon), ncol=2, byrow=TRUE)

coord<- matrix(NA, nrow=nrow(dat.po), ncol=2)
coord[inds,1] <- as.numeric(sub("lat: ", "", latlon[,1]))
coord[inds,2] <- as.numeric(sub(" lon: ", "", latlon[,2]))
dat.po= as.data.frame(cbind(dat.po[,1:2],coord))

match1<- match(dat.po$coreid, d.idig.po$coreid) 
dat.po$link<-NA
dat.po$link[which(!is.na(match1))]<- d.idig.po$ac.accessURI[na.omit(match1)]
dat.po$downloadID<-NA
dat.po$downloadID[which(!is.na(match1))]<- d.idig.po$downloadID[na.omit(match1)]
dat.po<- dat.po[which(!is.na(dat.po$downloadID)),]

#--
dat.pr<- read.csv("data/idigbio_Prapae/occurrence.csv")
dat.pr<- dat.pr[,c("coreid",'idigbio.eventDate', 'idigbio.geoPoint')]  
#split lat, lon
inds<- which(dat.pr$idigbio.geoPoint!="")
latlon<- lapply(dat.pr$idigbio.geoPoint, FUN= function(x) strsplit(gsub("[{}\"]", "",x),",") )
latlon<- matrix(unlist(latlon), ncol=2, byrow=TRUE)

coord<- matrix(NA, nrow=nrow(dat.pr), ncol=2)
coord[inds,1] <- as.numeric(sub("lat: ", "", latlon[,1]))
coord[inds,2] <- as.numeric(sub(" lon: ", "", latlon[,2]))
dat.pr= cbind(dat.pr[,1:2],coord)

match1<- match(dat.pr$coreid, d.idig.pr$coreid) 
dat.pr$link<-NA
dat.pr$link[which(!is.na(match1))]<- d.idig.pr$ac.accessURI[na.omit(match1)]
dat.pr$downloadID<-NA
dat.pr$downloadID[which(!is.na(match1))]<- d.idig.pr$downloadID[na.omit(match1)]
dat.pr<- dat.pr[which(!is.na(dat.pr$downloadID)),]

names(dat.po)<- c("id","date","lat","lon","link","downloadID")
names(dat.pr)<- c("id","date","lat","lon","link","downloadID")

#format dates
#ditch time
dat.po$date <- gsub("T.*", "", dat.po$date)
dat.pr$date <- gsub("T.*", "", dat.pr$date)

#add columns
dat.po$doy<- day_of_year(dat.po$date, format = "%Y-%m-%d")
dat.po$year<- format(as.Date(dat.po$date, format="%Y-%m-%d"),"%Y")
dat.po$month<- format(as.Date(dat.po$date, format="%Y-%m-%d"),"%m")
dat.po$day<- format(as.Date(dat.po$date, format="%Y-%m-%d"),"%d")

dat.pr$doy<- day_of_year(dat.pr$date, format = "%Y-%m-%d")
dat.pr$year<- format(as.Date(dat.pr$date, format="%Y-%m-%d"),"%Y")
dat.pr$month<- format(as.Date(dat.pr$date, format="%Y-%m-%d"),"%m")
dat.pr$day<- format(as.Date(dat.pr$date, format="%Y-%m-%d"),"%d")

dat.po$provider<-"idigbio"
dat.pr$provider<-"idigbio"

dat.po.all<- rbind(dat.po.all, dat.po)
dat.pr.all<- rbind(dat.pr.all, dat.pr)

#----
#SCAN
dat.po <- read.csv("data/SCAN_Poccidentalis/occurrences.csv")
dat.po<- dat.po[,c("id","eventDate","startDayOfYear",'year','month','day','decimalLatitude','decimalLongitude')]  

match1<- match(dat.po$id, d.scan.po$coreid) 
dat.po$link<-NA
dat.po$link[which(!is.na(match1))]<- d.scan.po$accessURI[na.omit(match1)]
dat.po$downloadID<-NA
dat.po$downloadID[which(!is.na(match1))]<- d.scan.po$downloadID[na.omit(match1)]
dat.po<- dat.po[which(!is.na(dat.po$downloadID)),]

dat.pr <- read.csv("data/SCAN_Prapae/occurrences.csv")
dat.pr<- dat.pr[,c("id","eventDate","startDayOfYear",'year','month','day','decimalLatitude','decimalLongitude')]  

match1<- match(dat.pr$id, d.scan.pr$coreid) 
dat.pr$link<-NA
dat.pr$link[which(!is.na(match1))]<- d.scan.pr$accessURI[na.omit(match1)]
dat.pr$downloadID<-NA
dat.pr$downloadID[which(!is.na(match1))]<- d.scan.pr$downloadID[na.omit(match1)]
dat.pr<- dat.pr[which(!is.na(dat.pr$downloadID)),]

dat.po<- dat.po[,c("id","eventDate","decimalLatitude","decimalLongitude","link","downloadID","startDayOfYear","year","month","day")]
colnames(dat.po)<-c("id","date","lat","lon","link","downloadID","doy","year","month","day" )

dat.pr<- dat.pr[,c("id","eventDate","decimalLatitude","decimalLongitude","link","downloadID","startDayOfYear","year","month","day")]
colnames(dat.pr)<-c("id","date","lat","lon","link","downloadID","doy","year","month","day" )

dat.po$provider<-"scan"
dat.pr$provider<-"scan"

dat.po.all<- rbind( dat.po.all, dat.po)
dat.pr.all<- rbind( dat.pr.all, dat.pr)

#----
#yale
#figure out renumbering
dat.pr <- read.csv("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/WingColoration/data/Yale_Prapae.csv")

#find and remove problematic images
dat.pr$valid<-"yes"
dat.pr$valid[grep("deliver.odai.yale.edu", dat.pr$image_link)]<-"no"
dat.pr<- dat.pr[which(dat.pr$valid=="yes"),]

#dat.pr <- dat.pr[,c("id","occurrence_id","collecting_date","latitude","longitude")]   
#dat.pr.all<- rbind( dat.pr.all, dat.pr)
#-----------------------
#write out

dat.po.all$species<- "P. occidentalis"
dat.pr.all$species<- "P. rapae"

dat.all<- rbind(dat.po.all, dat.pr.all)
#write out
"/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/WingColoration/"

##image name
#dat.po.all$image<- paste(str_extract(dat.po.all$link, "([^/]+$)"),".jpg",sep="")
#dat.pr.all$image<- paste(str_extract(dat.pr.all$link, "([^/]+$)"),".jpg",sep="")

#write image list
write.csv(dat.all, "out/WhiteButterfliesRenamed_list.csv")

#-------------------------



