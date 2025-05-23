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

match1<- match(gray$downloadID.m, bdat$downloadID.m)
gray$downloadID.m[is.na(match1)]
#some final digits dropped when downloading images. Mechanism unclear.

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
bdat.l$wing<- "forewing"
bdat.l$wing[grep("hw", bdat.l$label)]<- "hindwing"
bdat.g$wing<- "forewing"
bdat.g$wing[grep("hw", bdat.l$label)]<- "hindwing"

bdat.l$lr<- "left"
bdat.l$lr[grep("_r", bdat.l$label)]<- "right"
bdat.g$lr<- "left"
bdat.g$lr[grep("_r", bdat.g$label)]<- "right"

#convert grayscale 0-255 to percent
bdat.g$grayscale= (255-bdat.g$gray)/255

#drop NA side
bdat.g$side<- factor(bdat.g$side, levels=c("dorsal","ventral"))
bdat.l$side<- factor(bdat.l$side, levels=c("dorsal","ventral"))
bdat.g <- bdat.g[!is.na(bdat.g$side),]
bdat.l <- bdat.l[!is.na(bdat.l$side),]

bdat.g$wing<- as.factor(bdat.g$wing)
bdat.l$wing<- as.factor(bdat.l$wing)
bdat.g$lr<- as.factor(bdat.g$lr)
bdat.l$lr<- as.factor(bdat.l$lr)

#fix negative latitudes?
bdat.g$lat<- abs(bdat.g$lat)
bdat.l$lat<- abs(bdat.l$lat)

#set doy of 1 or 365 to zero
bdat.g$doy[bdat.g$doy==1]<-NA
bdat.g$doy[bdat.g$doy==365]<-NA
bdat.l$doy[bdat.l$doy==1]<-NA
bdat.l$doy[bdat.l$doy==365]<-NA

#plot
#gray #bdat.g[which(bdat.g$side=="dorsal"),]
colors= viridis(5, option="mako")[c(2:4)]

gdat<- bdat.g[which(bdat.g$species== c("P. occidentalis","P. rapae")[2] & bdat.g$side=="dorsal"),]
gdat<- gdat[, c("grayscale","doy","lat","wing","side","year","lr")]
gdat<- na.omit(gdat)

plot1<- ggplot(bdat.g[which(bdat.g$species== c("P. occidentalis","P. rapae")[2]),], aes(x=doy, y=grayscale, color=lr)) + 
  facet_grid(wing~side)+ ylab("grayscale (%)")+ xlab("day of year")+
  geom_point()+geom_smooth(method="lm")+theme_bw(base_size = 16)+scale_colour_manual(values=colors)

pdf("Prape_gray.pdf",height = 8, width = 8)
plot1
dev.off()

mod= lm(grayscale ~ doy*lat*wing*year+lr, data= gdat) 
anova(mod)

pm.lat<- plot_model(mod, type = "pred", terms = c("doy","lat"), show.data=FALSE, title="")
pm.yr<- plot_model(mod, type = "pred", terms = c("doy","year"), show.data=FALSE, title="")

pm.plot<- pm.yr+ theme_bw(base_size=18)+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  xlab("day of year")

pdf("Prapae_graymod.pdf",height = 8, width = 8)
pm.plot
dev.off()

#length
plot2<- ggplot(bdat.l[which(bdat.l$species== c("P. occidentalis","P. rapae")[2]),], aes(x=year, y=length, color=lr)) + 
  facet_grid(wing~side)+ylab("length (pixels)")+
  geom_point()+geom_smooth(method="lm")+theme_bw()+scale_colour_manual(values=colors)

pdf("Prape_length.pdf",height = 8, width = 8)
plot2
dev.off()

mod= lm(length ~ doy*lat*wing*year+lr, data= bdat.l[which(bdat.l$species== c("P. occidentalis","P. rapae")[2] & bdat.g$side=="dorsal"),]) 
anova(mod)

mod= lm(length ~ doy*lat*year+lr, data= bdat.l[which(bdat.l$species== c("P. occidentalis","P. rapae")[2] & bdat.g$side=="dorsal" & bdat.g$wing=="hindwing"),]) 
anova(mod)

pm.plot<- plot_model(mod, type = "pred", terms = c("year", "lat", "wing"), show.data=TRUE)

pm.plot<- pm.plot+ theme_bw(base_size=18)+ 
  scale_color_viridis_c()+
  scale_fill_viridis_c()

pdf("Prapae_lengthmod.pdf",height = 5, width = 8)
pm.plot
dev.off()

plot_model(mod, type = "pred", terms = c("year","lat"), show.data=TRUE)
plot_model(mod, type = "pred", terms = c("wing","year"), show.data=TRUE)
