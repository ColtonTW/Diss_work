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


birds <- read_xlsx("Bi_Bu_data.xlsx",sheet="general")
birds




###### Testing normality!! -- Per year can be seen in Butterfly coding and further down

shapiro.test(sqrt(birds$`Mean_Wind_Speed_(m/s)`))  ## p<0.05 so NOT NORMAL

#normal data

# cannot be normalised data
shapiro.test(birds$Mean_Temperature)
shapiro.test(birds$`Mean_Wind_Speed_(m/s)`)
shapiro.test(birds$Mean_Visibility)
shapiro.test(birds$Mean_Cloud_cover)
shapiro.test(birds$Mean_Sun_percentage)
shapiro.test(birds$Daily_bi_species_count) 
shapiro.test(birds$Daily_bird_count)
shapiro.test(birds$Daily_bi_species_count) 


#need mblm package for this ---- DO NOT USE THIS DATA 
#different regressions used further along that is better
# just keeping this coding here as a remind of the work I have done
'par(mfrow=c(1,1))
B_wind_s <- rfit(birds2$Daily_bi_species_count~birds2$`Mean_Wind_Speed_(m/s)`)
plot(B_wind_s)
summary(B_wind_s) ## not signif 
plot(birds2$Daily_bi_species_count~birds2$`Mean_Wind_Speed_(m/s)`,xlab="Mean wind speed (m/s)",ylab="Daily species count",pch=4,col="darkred") 
abline(B_wind_s)

B_temp_s <- rfit(birds2$Daily_bi_species_count~birds2$Mean_Temperature)
plot(B_temp_s)
summary(B_temp_s) ## MOST SIGNIF 
plot(birds2$Daily_bi_species_count~(birds2$Mean_Temperature),xlab="Mean Temperature (celsius)",ylab="Daily species count",pch=4,col="darkred")
abline(B_temp_s)

B_vis_s <- rfit(birds2$Daily_bi_species_count~birds2$Mean_Visibility)
plot(B_vis_s)
summary(B_vis_s) ## EXTREMELY SIGNIF 
plot(birds2$Daily_bi_species_count~(birds2$Mean_Visibility),xlab="Mean Visibility",ylab="Daily species count",pch=4,col="darkred")
abline(B_vis_s)


B_sun_s <- rfit(birds2$Daily_bi_species_count~birds2$Mean_Sun_percentage)
plot(B_sun_s)
summary(B_sun_s) ## SIGNIF
plot(birds2$Daily_bi_species_count~(birds2$Mean_Sun_percentage),xlab="Mean Sun intensity (%)",ylab="Daily species count",pch=4,col="darkred")
abline(B_sun_s)

B_cloud_s <- rfit(birds2$Daily_bi_species_count~birds2$Mean_Cloud_cover)
plot(B_cloud_s)
summary(B_cloud_s) ## SIGNIF 
plot(birds2$Daily_bi_species_count~(birds2$Mean_Cloud_cover),xlab="Mean cloud coverage",ylab="Daily species count",pch=4,col="darkred")
abline(B_cloud_s)

#birds now   

B_wind_b <- rfit(birds2$Daily_bird_count~(log(birds2$`Mean_Wind_Speed_(m/s)`)))
plot(B_wind_b)
summary(B_wind_b) ##  not signif
plot(birds2$Daily_bird_count~(log(birds2$`Mean_Wind_Speed_(m/s)`)),xlab="Mean wind speed (m/s)", ylab="Daily bird count",pch=4,col="darkblue")
abline(B_wind_b)

B_temp_b <- rfit(birds2$Daily_bird_count~birds2$Mean_Temperature)
plot(B_temp_b)
summary(B_temp_b) # signif
plot(birds2$Daily_bird_count~(birds2$Mean_Temperature),xlab="Mean temperature (celsius)", ylab="Daily bird count",pch=4,col="darkblue")
abline(B_temp_b)


B_vis_b <- rfit(birds2$Daily_bird_count~birds2$Mean_Visibility)
plot(B_vis_b)
summary(B_vis_b) # not sig
plot(birds2$Daily_bird_count~(birds2$Mean_Visibility),xlab="Mean visibility", ylab="Daily bird count",pch=4,col="darkblue")
abline(B_vis_b)

B_sun_b <- rfit(birds2$Daily_bird_count~birds2$Mean_Sun_percentage)
plot(B_sun_b)
summary(B_sun_b) # not signif
plot(birds2$Daily_bird_count~(birds2$Mean_Sun_percentage),xlab="Mean sun intensity (%)", ylab="Daily bird count",pch=4,col="darkblue")
abline(B_sun_b)


B_cloud_b <- rfit(birds2$Daily_bird_count~birds2$Mean_Cloud_cover)
plot(B_cloud_b)
summary(B_cloud_b) # not signif 
plot(birds2$Daily_bird_count~(birds2$Mean_Cloud_cover),xlab="Mean cloud cover", ylab="Daily bird count",pch=4,col="darkblue")
abline(B_cloud_b)'


### zinbr coding ---- USE THIS REGRESSION DATA NEED TO ADD NEGBIN TO ALL 
m1 <- zeroinfl(birds$Daily_bi_species_count~birds$`Mean_Wind_Speed_(m/s)`,dist = "negbin")
summary(m1) ## not  
plot(birds$Daily_bi_species_count~birds$`Mean_Wind_Speed_(m/s)`,xlab="Mean wind speed (m/s)",ylab="Daily species count",pch=4,col="darkred") 
abline(m1)


m2 <- zeroinfl(birds$Daily_bi_species_count~birds$Mean_Temperature)
summary(m2) ## signify signif 
plot(birds$Daily_bi_species_count~(birds$Mean_Temperature),xlab="Mean Temperature (celsius)",ylab="Daily species count",pch=4,col="darkred")
abline(m2)

m3 <- zeroinfl(birds$Daily_bi_species_count~birds$Mean_Visibility)
summary(m3) ## not SIGNIF 
plot(birds$Daily_bi_species_count~(birds$Mean_Visibility),xlab="Mean Visibility",ylab="Daily species count",pch=4,col="darkred")
abline(m3)


m4 <- zeroinfl(birds$Daily_bi_species_count~birds$Mean_Sun_percentage)
summary(m4) ##signif 
plot(birds$Daily_bi_species_count~(birds$Mean_Sun_percentage),xlab="Mean Sun intensity (%)",ylab="Daily species count",pch=4,col="darkred")
abline(m4)


m5 <- zeroinfl(birds$Daily_bi_species_count~birds$Mean_Cloud_cover)
summary(m5) ## SIGNIF 
plot(birds$Daily_bi_species_count~birds$Mean_Cloud_cover,xlab="Mean cloud coverage",ylab="Daily species count",pch=4,col="darkred")
abline(m5)


# bird count now 
m6 <- zeroinfl(birds$Daily_bird_count~birds$`Mean_Wind_Speed_(m/s)`)
summary(m6) ## signif
plot((log(birds$Daily_bird_count))~birds$`Mean_Wind_Speed_(m/s)`,xlab="Mean wind speed (m/s)", ylab="log Daily bird count",pch=4,col="darkblue")
abline(m6)

m7 <- zeroinfl(birds$Daily_bird_count~birds$Mean_Temperature)
summary(m7) ## not signif
plot((log(birds$Daily_bird_count))~birds$Mean_Temperature,xlab="Mean temperature (celsius)", ylab="log Daily bird count",pch=4,col="darkblue")
abline(m7)

m8 <- zeroinfl(birds$Daily_bird_count~birds$Mean_Visibility)
summary(m8) ### not signif
plot((log(birds$Daily_bird_count))~birds$Mean_Visibility,xlab="Mean visibility", ylab="log Daily bird count",pch=4,col="darkblue")
abline(m8)

m9 <- zeroinfl(birds$Daily_bird_count~birds$Mean_Sun_percentage)
summary(m9) # signif
plot((log(birds$Daily_bird_count))~birds$Mean_Sun_percentage,xlab="Mean sun intensity (%)", ylab="log Daily bird count",pch=4,col="darkblue")
abline(m9)

m10 <- zeroinfl(birds$Daily_bird_count~birds$Mean_Cloud_cover)
summary(m10) ## signif
plot((log(birds$Daily_bird_count))~birds$Mean_Cloud_cover,xlab="Mean cloud cover", ylab="log Daily bird count",pch=4,col="darkblue")
abline(m10)
#p=0.0004


### Bar plots pf Head vs tail wind
birds2 <- subset.data.frame(birds[birds$Mean_Wind_Heading==c("T","H"),])
birds2$Mean_Wind_Heading
birds2
?geom_boxplot
dev.off()
ggplot(birds2,mapping = aes(Mean_Wind_Heading,Daily_bi_species_count)) + geom_boxplot(fill=c("red","blue")) + labs(x="Wind direction",y="Daily species counts",caption="H - Head wind, T - Tail wind") 
t.test(birds2$Daily_bi_species_count[birds2$Mean_Wind_Heading=="T"],birds2$Daily_bi_species_count[birds2$Mean_Wind_Heading=="H"],paired=FALSE)

ggplot(birds2,mapping = aes(Mean_Wind_Heading,Daily_bird_count)) + geom_boxplot(fill=c("red","blue")) + labs(x="Wind direction", y="Daily bird count", caption="H - Head wind, T - Tail wind")
t.test(birds2$Daily_bi_species_count[birds2$Mean_Wind_Heading=="T"],birds2$Daily_bird_count[birds2$Mean_Wind_Heading=="H"],paired=FALSE)
# removing the outliers from birds 
ggplot(birds2,mapping = aes(Mean_Wind_Heading,Daily_bird_count)) + geom_boxplot(fill=c("red","blue")) + labs(x="Wind direction", y="Daily bird count", caption="H - Head wind, T - Tail wind") + ylim(0,300)


#### per year diffs -- none are normal, using zero inflated again
#2020
birds_20 <- subset.data.frame(birds[c(1:46),])
birds_20

m11 <- zeroinfl(birds_20$Daily_bi_species_count~birds_20$`Mean_Wind_Speed_(m/s)`,dist = "negbin")
summary(m11) 
plot(birds_20$Daily_bi_species_count~birds_20$`Mean_Wind_Speed_(m/s)`,xlab="Mean wind speed (m/s)",ylab="Daily species count",pch=4,col="darkred") 
abline(m11)


m12 <- zeroinfl(birds_20$Daily_bi_species_count~birds_20$Mean_Temperature)
summary(m12) 
plot(birds_20$Daily_bi_species_count~(birds_20$Mean_Temperature),xlab="Mean Temperature (celsius)",ylab="Daily species count",pch=4,col="darkred")
abline(m12)

m13 <- zeroinfl(birds_20$Daily_bi_species_count~birds_20$Mean_Visibility)
summary(m13) 
plot(birds_20$Daily_bi_species_count~(birds_20$Mean_Visibility),xlab="Mean Visibility",ylab="Daily species count",pch=4,col="darkred")
abline(m13)


m14 <- zeroinfl(birds_20$Daily_bi_species_count~birds_20$Mean_Sun_percentage)
summary(m14) 
plot(birds_20$Daily_bi_species_count~(birds_20$Mean_Sun_percentage),xlab="Mean Sun intensity (%)",ylab="Daily species count",pch=4,col="darkred")
abline(m14)


m15 <- zeroinfl(birds_20$Daily_bi_species_count~birds_20$Mean_Cloud_cover)
summary(m15)
plot(birds_20$Daily_bi_species_count~birds_20$Mean_Cloud_cover,xlab="Mean cloud coverage",ylab="Daily species count",pch=4,col="darkred")
abline(m15)


# bird count now 
m16 <- zeroinfl(birds_20$Daily_bird_count~birds_20$`Mean_Wind_Speed_(m/s)`)
summary(m16) 
plot(birds_20$Daily_bird_count~birds_20$`Mean_Wind_Speed_(m/s)`,xlab="Mean wind speed (m/s)", ylab="Daily bird count",pch=4,col="darkblue")
abline(m16)

m17 <- zeroinfl(birds_20$Daily_bird_count~birds_20$Mean_Temperature)
summary(m17)
plot(birds_20$Daily_bird_count~(birds_20$Mean_Temperature),xlab="Mean temperature (celsius)", ylab="Daily bird count",pch=4,col="darkblue")
abline(m17)

m18 <- zeroinfl(birds_20$Daily_bird_count~birds_20$Mean_Visibility)
summary(m18) 
plot(birds_20$Daily_bird_count~(birds_20$Mean_Visibility),xlab="Mean visibility", ylab="Daily bird count",pch=4,col="darkblue")
abline(m18)

m19 <- zeroinfl(birds_20$Daily_bird_count~birds_20$Mean_Sun_percentage)
summary(m19) 
plot(birds_20$Daily_bird_count~(birds_20$Mean_Sun_percentage),xlab="Mean sun intensity (%)", ylab="Daily bird count",pch=4,col="darkblue")
abline(m19)

m20 <- zeroinfl(birds_20$Daily_bird_count~birds_20$Mean_Cloud_cover)
summary(m20) 
plot(birds_20$Daily_bird_count~(birds_20$Mean_Cloud_cover),xlab="Mean cloud cover", ylab="Daily bird count",pch=4,col="darkblue")
abline(m20)


## 2021
birds_21 <- subset.data.frame(birds[c(47:95),])

m21 <- zeroinfl(birds_21$Daily_bi_species_count~birds_21$`Mean_Wind_Speed_(m/s)`,dist = "negbin")
summary(m21) 
plot(birds_21$Daily_bi_species_count~birds_21$`Mean_Wind_Speed_(m/s)`,xlab="Mean wind speed (m/s)",ylab="Daily species count",pch=4,col="darkred") 
abline(m21)


m22 <- zeroinfl(birds_21$Daily_bi_species_count~birds_21$Mean_Temperature)
summary(m22) 
plot(birds_21$Daily_bi_species_count~(birds_21$Mean_Temperature),xlab="Mean Temperature (celsius)",ylab="Daily species count",pch=4,col="darkred")
abline(m22)

m23 <- zeroinfl(birds_21$Daily_bi_species_count~birds_21$Mean_Visibility)
summary(m23) 
plot(birds_21$Daily_bi_species_count~(birds_21$Mean_Visibility),xlab="Mean Visibility",ylab="Daily species count",pch=4,col="darkred")
abline(m23)


m24 <- zeroinfl(birds_21$Daily_bi_species_count~birds_21$Mean_Sun_percentage)
summary(m24) 
plot(birds_21$Daily_bi_species_count~(birds_21$Mean_Sun_percentage),xlab="Mean Sun intensity (%)",ylab="Daily species count",pch=4,col="darkred")
abline(m24)


m25 <- zeroinfl(birds_21$Daily_bi_species_count~birds_21$Mean_Cloud_cover)
summary(m25) 
plot(birds_21$Daily_bi_species_count~birds_21$Mean_Cloud_cover,xlab="Mean cloud coverage",ylab="Daily species count",pch=4,col="darkred")
abline(m25)


# bird count now 
m26 <- zeroinfl(birds_21$Daily_bird_count~birds_21$`Mean_Wind_Speed_(m/s)`)
summary(m26) 
plot(birds_21$Daily_bird_count~birds_21$`Mean_Wind_Speed_(m/s)`,xlab="Mean wind speed (m/s)", ylab="Daily bird count",pch=4,col="darkblue")
abline(m26)

m27 <- zeroinfl(birds_21$Daily_bird_count~birds_21$Mean_Temperature)
summary(m27) 
plot(birds_21$Daily_bird_count~(birds_21$Mean_Temperature),xlab="Mean temperature (celsius)", ylab="Daily bird count",pch=4,col="darkblue")
abline(m27)

m28 <- zeroinfl(birds_21$Daily_bird_count~birds_21$Mean_Visibility)
summary(m28) 
plot(birds_21$Daily_bird_count~(birds_21$Mean_Visibility),xlab="Mean visibility", ylab="Daily bird count",pch=4,col="darkblue")
abline(m28)

m29 <- zeroinfl(birds_21$Daily_bird_count~birds_21$Mean_Sun_percentage)
summary(m29) 
plot(birds_21$Daily_bird_count~(birds_21$Mean_Sun_percentage),xlab="Mean sun intensity (%)", ylab="Daily bird count",pch=4,col="darkblue")
abline(m29)

m30 <- zeroinfl(birds_21$Daily_bird_count~birds_21$Mean_Cloud_cover)
summary(m30) 
plot(birds_21$Daily_bird_count~(birds_21$Mean_Cloud_cover),xlab="Mean cloud cover", ylab="Daily bird count",pch=4,col="darkblue")
abline(m30)

## time to compare years 
anova(m11,m21)
anova(m12,m22)
anova(m13,m23)
anova(m14,m24)
anova(m15,m25)
anova(m16,m26)
anova(m17,m27)
anova(m18,m28)
anova(m19,m29)
anova(m20,m30)

######## species counts

## 2020 birds ------ Further insights are given to those w/ more than 50 observations
### these were discovered through manipulation of an excel spreadsheet and totaling bird sightings 

## Meadow pipits 
## linnet 
## swallow 
### chaffinch
##Northern wheatear
#### finish list at gold finches 
plot(birds_20$Swallow ~birds_20$Date ,type="line")
plot(birds_20$Linnet ~birds_20$Date ,type="line")
plot(birds_20$`Meadow pipit` ~birds_20$Date ,type="line")
plot(birds_20$Chaffinch ~birds_20$Date ,type="line")
plot(birds_20$`Northern Wheatear` ~birds_20$Date ,type="line")
p1 <- ggplot(birds_20 ,mapping=aes(Date,Daily_bi_species_count)) + geom_point() + geom_smooth()  + labs(x="Date", y="Daily species count",title = "2020 bird species richness") + ylim(0,10)
p1

## looking at the 2021 birds ------ Further insights are given to those w/ more than 50 observations
# need to look at swallows as very common 
# chaffinch 
# meadow pipit 
# snow finches
plot(birds_21$Swallow ~birds_21$Date )
plot(birds_21$Chaffinch ~birds_21$Date )
plot(birds_21$`Meadow pipit` ~birds_21$Date )
plot(birds_21$`Snow Finches` ~birds_21$Date )
p2 <- ggplot(birds_21,mapping=aes(Date ,Daily_bi_species_count )) + geom_point() + geom_smooth() + labs(x="Date", y="Daily species count",title = "2021 bird species richness") + ylim(0,10) 
p2
summary(birds)

p1+p2
ggsave(filename = "Species richness over time.jpg", plot = last_plot(),limitsize = FALSE,height=4, width = 10)

# signif diff? 
t.test(birds_20$Daily_bi_species_count,birds_21$Daily_bi_species_count,paired=FALSE)


### abundance counts for most populous spp ### REDO WITHOUT THE CHOUGHS 
par(mfrow=c(1,1))
plot(birds_20$Date ,birds_20$Swallow ,type="line",col="blue",xlab="Date in 2020",ylab="Frequency of observation") + lines(birds_20$Date ,birds_20$`Meadow pipit` ,col="brown") +
  lines(birds_20$Date ,birds_20$Linnet ,col="red") + 
  lines(birds_20$Date ,birds_20$Chaffinch ,col="lightgreen") + axis(2,at=c(0,50,100,500,1000,1500)) +
  legend(x="topleft",legend=c("Swallow","Meadow pipit","Linnet","Chaffinch"),lty=1,col = c("blue","brown","red","lightgreen"))

# focused in without the swallows 
plot(birds_20$Date ,birds_20$`Meadow pipit` ,col="brown",type="line",xlab="Date in 2020",ylab="Frequency of observation") +
  lines(birds_20$Date ,birds_20$Linnet ,col="red") + 
  lines(birds_20$Date ,birds_20$Chaffinch ,col="lightgreen") +
  legend(x="topright",legend=c("Meadow pipit","Linnet","Chaffinch"),lty=1,col = c("brown","red","lightgreen"))

#2021
plot(birds_21$Date ,birds_21$Swallow ,type="line",col="blue",xlab="Date in 2021",ylab="Frequency of observation") + lines(birds_21$Date ,birds_21$Chaffinch ,col="lightgreen") + lines(birds_21$Date ,birds_21$`Meadow pipit` ,col="brown") +
  lines(birds_21$Date ,birds_21$Chaffinch ,col="lightgreen") +
  lines(birds_21$Date ,birds_21$`Grey Wagtail` ,col="purple") + axis(2,at=c(0,50,100,500,1000,1500)) +
  legend(x="topleft",legend=c("Swallow","Chaffinch","Meadow pipit","Grey wagtail"),lty=1,col = c("blue","lightgreen","brown","purple"))

plot(birds_21$Date ,birds_21$Chaffinch ,type="line",col="lightgreen",xlab="Date in 2021",ylab="Frequency of observation") +
  lines(birds_21$Date ,birds_21$`Meadow pipit` ,col="brown") +
  lines(birds_21$Date ,birds_21$`Grey Wagtail` ,col="purple") +
  legend(x="topleft",legend=c("Chaffinch","Meadow pipit","Grey wagtail"),lty=1,col = c("lightgreen","brown","purple"))

plot(birds_21$Date ,birds_21$`Meadow pipit` ,type="line",col="brown",xlab="Date in 2021",ylab="Frequency of observation") +
  lines(birds_21$Date ,birds_21$`Grey Wagtail` ,col="purple") +
  legend(x="topleft",legend=c("Meadow pipit","Grey wagtail"),lty=1,col = c("brown","purple"))




## focus on swallows and env conditions:
s <- lm(birds_20$Swallow~birds_20$Date)
sw <- lm(birds_21$Swallow~birds_21$Date)
plot(birds_20$Swallow~birds_20$Date,ylim=c(0,3500),type="line" ,xlab="Date in 2020", ylab="Swallow sightings",col="blue")
plot(birds_21$Swallow~birds_21$Date,ylim=c(0,3500),type="line" ,xlab="Date in 2021", ylab="Swallow sightings",col="blue")

anova(s,sw)

# in env conditions
s1 <- zeroinfl(birds$Swallow~birds$`Mean_Wind_Speed_(m/s)`,dist = "negbin")
summary(s1)   
plot((log(birds$Swallow))~birds$`Mean_Wind_Speed_(m/s)`,xlab="Mean wind speed (m/s)",ylab="Log swallow count",pch=4,col="darkred") 
abline(s1)


s2 <- zeroinfl(birds$Swallow~birds$Mean_Temperature)
summary(s2) 
plot((log(birds$Swallow))~(birds$Mean_Temperature),xlab="Mean Temperature (celsius)",ylab="Log swallow count",pch=4,col="darkred")
abline(s2)

s3 <- zeroinfl(birds$Swallow~birds$Mean_Visibility)
summary(s3)  
plot((log(birds$Swallow))~birds$Mean_Visibility,xlab="Mean Visibility",ylab="Log swallow count",pch=4,col="darkred")
abline(s3)


s4 <- zeroinfl(birds$Swallow~birds$Mean_Sun_percentage)
summary(s4) 
plot((log(birds$Swallow))~birds$Mean_Sun_percentage,xlab="Mean Sun intensity (%)",ylab="Log swallow count",pch=4,col="darkred")
abline(s4)


s5 <- zeroinfl(birds$Swallow~birds$Mean_Cloud_cover)
summary(s5)  
plot((log(birds$Swallow))~birds$Mean_Cloud_cover,xlab="Mean cloud coverage",ylab="Log swallow count",pch=4,col="darkred")
abline(s5)

## wind direction
ggplot(birds2,mapping = aes(Mean_Wind_Heading,Swallow)) + geom_boxplot(fill=c("red","blue")) + labs(x="Wind direction",y="Daily swallow counts",caption="H - Head wind, T - Tail wind")
t.test(birds2$Swallow[birds2$Mean_Wind_Heading=="T"],birds2$Swallow[birds2$Mean_Wind_Heading=="H"],paired=FALSE)

                                                                                                    