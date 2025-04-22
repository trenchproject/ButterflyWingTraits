library(ggplot2)
library(reshape2)
library(sjPlot)
library(viridis)

desktop<- "y"

if(desktop=="y") setwd("/Users/laurenbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/WingColoration/out/")
if(desktop=="n") setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/WingColoration/out/")

#load specimen info
bdat<- read.csv("WhiteButterfliesRenamed_list.csv")

#------------
#load analysis
gray<- read.csv("bfdata_wid.csv")
colnames(gray)[2:9]<- c('gray_rfw', 'gray_lfw', 'gray_rhw', 'gray_lhw','length_rfw', 'length_lfw', 'length_rhw', 'length_lhw')

#change zero values to NA
gray[gray==0]<- NA

#extract dorsal ventral
gray$side<- NA
gray$side[grep("0_dorsal", gray$image)] <-"dorsal"
gray$side[grep("1_ventral", gray$image)] <-"ventral"

#account for split images
gray$butterfly.n<- NA
gray$butterfly.n[grep("0_butterfly", gray$image)] <-0
gray$butterfly.n[grep("1_butterfly", gray$image)] <-1
gray$butterfly.n[grep("2_butterfly", gray$image)] <-2

#isolate image names
gray$downloadID.m<- sub("--.*", "", gray$image)
bdat$downloadID.m<- gsub(".jpg", "", bdat$downloadID)
  
#merge data
bdatm<- merge(x=bdat, y=gray, by = "downloadID.m", all=TRUE)

#lots of images not downloaded?
bdat<- bdatm[which(!is.na(bdatm$image)),]      

#-------------
#PLOT
bdat$species<- as.factor(bdat$species)

#average left and right
bdat$gray_fw<- rowMeans(bdat[,c("gray_rfw","gray_lfw")])
bdat$gray_hw<- rowMeans(bdat[,c("gray_rhw","gray_lhw")])
bdat$length_fw<- rowMeans(bdat[,c("length_rfw","length_lfw")])
bdat$length_hw<- rowMeans(bdat[,c("length_rhw","length_lhw")])

#to long format
bdat.l <- melt(bdat[,c("downloadID.m","date","lat","lon","doy","year","species","side",'length_rfw', 'length_lfw', 'length_rhw', 'length_lhw')], 
               id.vars = c("downloadID.m","date","lat","lon","doy","year","species","side"), variable.name = "label", value.name="length")
bdat.g <- melt(bdat[,c("downloadID.m","date","lat","lon","doy","year","species","side",'gray_rfw', 'gray_lfw', 'gray_rhw', 'gray_lhw')], 
               id.vars = c("downloadID.m","date","lat","lon","doy","year","species","side"), variable.name = "label", value.name="gray")
#code wings
bdat.l$wing<- "fw"
bdat.l$wing[grep("hw", bdat.l$label)]<- "hw"
bdat.g$wing<- "fw"
bdat.g$wing[grep("hw", bdat.l$label)]<- "hw"

bdat.l$lr<- "left"
bdat.l$lr[grep("_r", bdat.l$label)]<- "right"
bdat.g$lr<- "left"
bdat.g$lr[grep("_r", bdat.g$label)]<- "right"

#convert grayscale 0-255 to percent
bdat.g$grayscale= (255-bdat.g$gray)/255

#drop NA side
bdat.g <- bdat.g[-is.na(bdat.g$side),]
bdat.l <- bdat.l[-is.na(bdat.l$side),]

bdat.g$wing<- as.factor(bdat.g$wing)
bdat.l$wing<- as.factor(bdat.l$wing)
bdat.g$lr<- as.factor(bdat.g$lr)
bdat.l$lr<- as.factor(bdat.l$lr)
bdat.g$side<- factor(bdat.g$side, levels=c("dorsal","ventral"))
bdat.l$side<- factor(bdat.l$side, levels=c("dorsal","ventral"))

#fix negative latitudes?
bdat.g$lat<- abs(bdat.g$lat)
bdat.l$lat<- abs(bdat.l$lat)

#plot
#gray #bdat.g[which(bdat.g$side=="dorsal"),]
colors= viridis(5, option="mako")[c(2:4)]

gdat<- bdat.g[which(bdat.g$species== c("P. occidentalis","P. rapae")[2] & bdat.g$side=="dorsal"),]
gdat<- gdat[, c("grayscale","doy","lat","wing","side","year","lr")]
gdat<- na.omit(gdat)

ggplot(bdat.g[which(bdat.g$species== c("P. occidentalis","P. rapae")[2]),], aes(x=doy, y=grayscale, color=lr)) + 
  facet_grid(wing~side)+
  geom_point()+geom_smooth(method="lm")+theme_bw()+scale_colour_manual(values=colors)

mod= lm(grayscale ~ doy*lat*wing*year+lr, data= gdat) 
anova(mod)

plot_model(mod, type = "pred", terms = c("doy","year"), show.data=TRUE)
plot_model(mod, type = "pred", terms = c("doy","lat"), show.data=TRUE)
plot_model(mod, type = "pred", terms = c("lat","year"), show.data=TRUE)

#length
ggplot(bdat.l[which(bdat.l$species== c("P. occidentalis","P. rapae")[2]),], aes(x=doy, y=length, color=lr)) + 
  facet_grid(wing~side)+
  geom_point()+geom_smooth(method="lm")+theme_bw()+scale_colour_manual(values=colors)

mod= lm(length ~ doy*lat*wing*year+lr, data= bdat.l[which(bdat.l$species== c("P. occidentalis","P. rapae")[2] & bdat.g$side=="dorsal"),]) 
anova(mod)

plot_model(mod, type = "pred", terms = c("doy", "lat", "wing"), show.data=TRUE)

plot_model(mod, type = "pred", terms = c("doy","year"), show.data=TRUE)
plot_model(mod, type = "pred", terms = c("doy","lat"), show.data=TRUE)
plot_model(mod, type = "pred", terms = c("lat","year"), show.data=TRUE)
plot_model(mod, type = "pred", terms = c("wing","year"), show.data=TRUE)
