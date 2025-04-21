library(ggplot2)
library(reshape2)
library(sjPlot)

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
#gray$side<- as.factor(gray$side)

#account for split images
gray$butterfly.n<- NA
gray$butterfly.n[grep("0_butterfly", gray$image)] <-0
gray$butterfly.n[grep("1_butterfly", gray$image)] <-1
gray$butterfly.n[grep("2_butterfly", gray$image)] <-2

#isolate image names
gray$downloadID.m<- sub("--.*", "", gray$image)
bdat$downloadID.m<- gsub(".jpg", "", bdat$downloadID)
  
#match
match1<- match(bdat$downloadID.m, gray$downloadID.m)
bdat$gray_rfw <-NA
bdat$gray_lfw <-NA
bdat$gray_rhw <-NA
bdat$gray_lhw <-NA
bdat$length_rfw <-NA
bdat$length_lfw <-NA
bdat$length_rhw <-NA
bdat$length_lhw<-NA
#add data
bdat$side[which(!is.na(match1))] <- gray$side[na.omit(match1)]
bdat$gray_rfw[which(!is.na(match1))] <- gray$gray_rfw[na.omit(match1)]
bdat$gray_lfw[which(!is.na(match1))] <- gray$gray_lfw[na.omit(match1)]
bdat$gray_rhw[which(!is.na(match1))] <- gray$gray_rhw[na.omit(match1)]
bdat$gray_lhw[which(!is.na(match1))] <- gray$gray_lhw[na.omit(match1)]
bdat$length_rfw[which(!is.na(match1))] <- gray$length_rfw[na.omit(match1)]
bdat$length_lfw[which(!is.na(match1))] <- gray$length_lfw[na.omit(match1)]
bdat$length_rhw[which(!is.na(match1))] <- gray$length_rhw[na.omit(match1)]
bdat$length_lhw[which(!is.na(match1))] <- gray$length_lhw[na.omit(match1)]

#-------------
#PLOT
bdat$species<- as.factor(bdat$species)

#average left and right
bdat$gray_fw<- rowMeans(bdat[,c("gray_rfw","gray_lfw")])
bdat$gray_hw<- rowMeans(bdat[,c("gray_rhw","gray_lhw")])
bdat$length_fw<- rowMeans(bdat[,c("length_rfw","length_lfw")])
bdat$length_hw<- rowMeans(bdat[,c("length_rhw","length_lhw")])

#to long format
bdat.l <- melt(bdat[,c("downloadID.m","date","lat","lon","doy","year","species","side","length_fw","length_hw")], 
               id.vars = c("downloadID.m","date","lat","lon","doy","year","species","side"), variable.name = "wing", value.name="length")
bdat.g <- melt(bdat[,c("downloadID.m","date","lat","lon","doy","year","species","side","gray_fw","gray_hw")], 
               id.vars = c("downloadID.m","date","lat","lon","doy","year","species","side"), variable.name = "wing", value.name="gray")

#plot
ggplot(bdat.g, aes(x=gray, color=wing)) + 
  facet_wrap(.~species)+
  geom_density()

#gray
ggplot(bdat.g[which(bdat.g$side=="dorsal"),], aes(x=doy, y=gray, color=wing)) + 
  facet_grid(.~species)+
  geom_point()+geom_smooth(method="lm")

mod= lm(gray ~ doy*lat*wing, data= bdat.g[which(bdat.g$species=="P. occidentalis"),]) 
mod= lm(gray ~ doy*lat*wing, data= bdat.g[which(bdat.g$species=="P. rapae"),]) 
anova(mod)

plot_model(mod, type = "pred", terms = c("doy", "lat", "wing"), show.data=TRUE)

#length
ggplot(bdat.l[which(bdat.g$side=="dorsal"),], aes(x=doy, y=length, color=wing)) + 
  facet_grid(.~species)+
  geom_point()+geom_smooth(method="lm")

mod= lm(length ~ doy*lat*wing, data= bdat.l[which(bdat.g$species=="P. occidentalis"),]) 
mod= lm(length ~ doy*lat*wing, data= bdat.l[which(bdat.g$species=="P. rapae"),]) 
anova(mod)

plot_model(mod, type = "pred", terms = c("doy", "lat", "wing"), show.data=TRUE)


