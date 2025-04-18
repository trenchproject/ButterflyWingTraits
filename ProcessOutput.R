library(ggplot2)
library(reshape2)

#load specimen info
setwd("/Users/lbuckley/Google Drive/Shared drives/TrEnCh/Projects/WARP/Projects/WingColoration/out/")
po.dat<- read.csv("po_image_data.csv")
pr.dat<- read.csv("po_image_data.csv")

po.dat$species<- "po"
pr.dat$species<- "pr"

bdat<- rbind(po.dat, pr.dat)

#----
#load ouput
output_folder = '/Users/lbuckley/presplit_out_ventral'
#output_folder = '/Users/lbuckley/presplit_out_dorsal'

setwd(output_folder)
gray<- read.csv("bfdata_wid.csv")

#change zero values to NA
gray[gray==0]<- NA

#----
colnames(gray)[2:9]<- c('gray_rfw', 'gray_lfw', 'gray_rhw', 'gray_lhw','length_rfw', 'length_lfw', 'length_rhw', 'length_lhw')

#match
gmatch<- gsub("_crop.jpg", "", gray$image)
gmatch<- gsub("_crop_2.jpg", "", gmatch)
bmatch<- gsub(".jpg", "", bdat$image)
bmatch<- gsub("?", "", bmatch, fixed = TRUE)
  
match(gmatch, bmatch)

#plot distributions
gray.l <- melt(gray[,c(1,6:9)], id.vars = c("image"), variable.name = "wing", value.name="length")
gray.g <- melt(gray[,1:5], id.vars = c("image"), variable.name = "wing", value.name="gray")

ggplot(gray.l, aes(x=length, color=wing)) + 
  geom_density()
ggplot(gray.g, aes(x=gray, color=wing)) + 
  geom_density()


