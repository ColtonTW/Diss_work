library("ggplot2")
library("patchwork")
library("rmarkdown")
library("report")
library("tidyverse")
library("marmap")
library("raster")
library("vegan")
library("readxl")
library("dplyr")
library("MASS")
library("mblm")
library("Rfit")
library("pscl")
library("boot")
library("plotrix")

weather <- read_xlsx("Bi_Bu_data.xlsx",sheet="general")

###################   for both years 
## Simple  for weather vars

cor.test(weather$`Mean_Wind_Speed_(m/s)`,weather$Mean_Temperature,method="spearman") 
plot(weather$`Mean_Wind_Speed_(m/s)`,weather$Mean_Temperature,xlab="Mean wind speed (m/s)",ylab="Mean temperature (celsius)",pch=4, col="darkgreen")

cor.test(weather$Mean_Visibility,weather$Mean_Temperature,method = "spearman") ## signif
plot(weather$Mean_Visibility,weather$Mean_Temperature,xlab="Mean visibility",ylab="Mean temperature (celsius)",pch=4, col="green")

cor.test(weather$Mean_Cloud_cover,weather$Mean_Temperature,method = "spearman") ## signif
plot(weather$Mean_Cloud_cover,weather$Mean_Temperature,xlab="Mean cloud cover",ylab="Mean temperature (celsius)",pch=4, col="green3")

cor.test(weather$Mean_Sun_percentage,weather$Mean_Temperature,method = "spearman") ## signif
plot(weather$Mean_Sun_percentage,weather$Mean_Temperature,xlab="Mean sun intensity (%)",ylab="Mean temperature (celsius)",pch=4, col="green4")


#### 

cor.test(weather$Mean_Visibility,weather$'Mean_Wind_Speed_(m/s)',method = "spearman") ## not signif
plot(weather$Mean_Visibility,weather$`Mean_Wind_Speed_(m/s)`,xlab="Mean visibilty",ylab="Mean wind speed (m/s)",pch=4, col="darkorange")

cor.test(weather$Mean_Cloud_cover,weather$'Mean_Wind_Speed_(m/s)',method = "spearman") ## not signif
plot(weather$Mean_Cloud_cover,weather$`Mean_Wind_Speed_(m/s)`,xlab="Mean cloud cover",ylab="Mean wind speed (m/s)",pch=4, col="darkorange3")

cor.test(weather$Mean_Sun_percentage,weather$'Mean_Wind_Speed_(m/s)',method = "spearman") ## signif
plot(weather$Mean_Sun_percentage,weather$`Mean_Wind_Speed_(m/s)`,xlab="Mean sun intensity (%)",ylab="Mean wind speed (m/s)",pch=4, col="darkorange4")

##
cor.test(weather$Mean_Cloud_cover,weather$Mean_Visibility,method = "spearman") ## VERY SIGNIF 
plot(weather$Mean_Cloud_cover,weather$Mean_Visibility,xlab="Mean cloud cover",ylab="Mean visibility",pch=4, col="purple")

cor.test(weather$Mean_Sun_percentage,weather$Mean_Visibility,method = "spearman") ## VERY SIGNIF 
plot(weather$Mean_Sun_percentage,weather$Mean_Visibility,xlab="Mean sun intensity (%)",ylab="Mean visibility",pch=4, col="purple4")

##
cor.test(weather$Mean_Sun_percentage,weather$Mean_Cloud_cover,method = "spearman") ## VERY SIGNIF 
plot(weather$Mean_Sun_percentage,weather$Mean_Cloud_cover,xlab="Mean sun intensity (%)",ylab="Mean cloud cover",pch=4, col="gold4")




##### 2020
weather_20 <- subset.data.frame(weather[c(1:46),])

## Simple  for weather vars

cor.test(weather_20$`Mean_Wind_Speed_(m/s)`,weather_20$Mean_Temperature,method = "spearman") ## signif   
plot(weather_20$`Mean_Wind_Speed_(m/s)`,weather_20$Mean_Temperature,xlab="Mean wind speed (m/s)",ylab="Mean temperature (celsius)",pch=4, col="darkgreen")

cor.test(weather_20$Mean_Visibility,weather_20$Mean_Temperature,method = "spearman") ## signif
plot(weather_20$Mean_Visibility,weather_20$Mean_Temperature,xlab="Mean visibility",ylab="Mean temperature (celsius)",pch=4, col="green")

cor.test(weather_20$Mean_Cloud_cover,weather_20$Mean_Temperature,method = "spearman") ## not signif
plot(weather_20$Mean_Cloud_cover,weather_20$Mean_Temperature,xlab="Mean cloud cover",ylab="Mean temperature (celsius)",pch=4, col="green3")

cor.test(weather_20$Mean_Sun_percentage,weather_20$Mean_Temperature,method = "spearman") ## signif
plot(weather_20$Mean_Sun_percentage,weather_20$Mean_Temperature,xlab="Mean sun intensity (%)",ylab="Mean temperature (celsius)",pch=4, col="green4")


#### 

cor.test(weather_20$Mean_Visibility,weather_20$'Mean_Wind_Speed_(m/s)',method = "spearman") ## not signif
plot(weather_20$Mean_Visibility,weather_20$`Mean_Wind_Speed_(m/s)`,xlab="Mean visibilty",ylab="Mean wind speed (m/s)",pch=4, col="darkorange")

cor.test(weather_20$Mean_Cloud_cover,weather_20$'Mean_Wind_Speed_(m/s)',method = "spearman") ## not signif
plot(weather_20$Mean_Cloud_cover,weather_20$`Mean_Wind_Speed_(m/s)`,xlab="Mean cloud cover",ylab="Mean wind speed (m/s)",pch=4, col="darkorange3")

cor.test(weather_20$Mean_Sun_percentage,weather_20$'Mean_Wind_Speed_(m/s)',method = "spearman") ## signif
plot(weather_20$Mean_Sun_percentage,weather_20$`Mean_Wind_Speed_(m/s)`,xlab="Mean sun intensity (%)",ylab="Mean wind speed (m/s)",pch=4, col="darkorange4")

##
cor.test(weather_20$Mean_Cloud_cover,weather_20$Mean_Visibility,method = "spearman") ## VERY SIGNIF 
plot(weather_20$Mean_Cloud_cover,weather_20$Mean_Visibility,xlab="Mean cloud cover",ylab="Mean visibility",pch=4, col="purple")

cor.test(weather_20$Mean_Sun_percentage,weather_20$Mean_Visibility,method = "spearman") ## VERY SIGNIF 
plot(weather_20$Mean_Sun_percentage,weather_20$Mean_Visibility,xlab="Mean sun intensity (%)",ylab="Mean visibility",pch=4, col="purple4")

##
cor.test(weather_20$Mean_Sun_percentage,weather_20$Mean_Cloud_cover,method = "spearman") ## VERY SIGNIF 
plot(weather_20$Mean_Sun_percentage,weather_20$Mean_Cloud_cover,xlab="Mean sun intensity (%)",ylab="Mean cloud cover",pch=4, col="gold4")




#######2021
weather_21 <- subset.data.frame(weather[c(47:95),])


cor.test(weather_21$`Mean_Wind_Speed_(m/s)`,weather_21$Mean_Temperature,method = "spearman") ## signif   
plot(weather_21$`Mean_Wind_Speed_(m/s)`,weather_21$Mean_Temperature,xlab="Mean wind speed (m/s)",ylab="Mean temperature (celsius)",pch=4, col="darkgreen")

cor.test(weather_21$Mean_Visibility,weather_21$Mean_Temperature,method = "spearman") ## signif
plot(weather_21$Mean_Visibility,weather_21$Mean_Temperature,xlab="Mean visibility",ylab="Mean temperature (celsius)",pch=4, col="green")

cor.test(weather_21$Mean_Cloud_cover,weather_21$Mean_Temperature,method = "spearman") ## not signif
plot(weather_21$Mean_Cloud_cover,weather_21$Mean_Temperature,xlab="Mean cloud cover",ylab="Mean temperature (celsius)",pch=4, col="green3")

cor.test(weather_21$Mean_Sun_percentage,weather_21$Mean_Temperature,method = "spearman") ## signif
plot(weather_21$Mean_Sun_percentage,weather_21$Mean_Temperature,xlab="Mean sun intensity (%)",ylab="Mean temperature (celsius)",pch=4, col="green4")

#### 
cor.test(weather_21$Mean_Visibility,weather_21$'Mean_Wind_Speed_(m/s)',method = "spearman") ## not signif
plot(weather_21$Mean_Visibility,weather_21$`Mean_Wind_Speed_(m/s)`,xlab="Mean visibilty",ylab="Mean wind speed (m/s)",pch=4, col="darkorange")

cor.test(weather_21$Mean_Cloud_cover,weather_21$'Mean_Wind_Speed_(m/s)',method = "spearman") ## not signif
plot(weather_21$Mean_Cloud_cover,weather_21$`Mean_Wind_Speed_(m/s)`,xlab="Mean cloud cover",ylab="Mean wind speed (m/s)",pch=4, col="darkorange3")

cor.test(weather_21$Mean_Sun_percentage,weather_21$'Mean_Wind_Speed_(m/s)',method = "spearman") ## signif
plot(weather_21$Mean_Sun_percentage,weather_21$`Mean_Wind_Speed_(m/s)`,xlab="Mean sun intensity (%)",ylab="Mean wind speed (m/s)",pch=4, col="darkorange4")

##
cor.test(weather_21$Mean_Cloud_cover,weather_21$Mean_Visibility,method = "spearman") ## VERY SIGNIF 
plot(weather_21$Mean_Cloud_cover,weather_21$Mean_Visibility,xlab="Mean cloud cover",ylab="Mean visibility",pch=4, col="purple")

cor.test(weather_21$Mean_Sun_percentage,weather_21$Mean_Visibility,method = "spearman") ## VERY SIGNIF 
plot(weather_21$Mean_Sun_percentage,weather_21$Mean_Visibility,xlab="Mean sun intensity (%)",ylab="Mean visibility",pch=4, col="purple4")

##
cor.test(weather_21$Mean_Sun_percentage,weather_21$Mean_Cloud_cover,method = "spearman") ## VERY SIGNIF 
plot(weather_21$Mean_Sun_percentage,weather_21$Mean_Cloud_cover,xlab="Mean sun intensity (%)",ylab="Mean cloud cover",pch=4, col="gold4")


### between years