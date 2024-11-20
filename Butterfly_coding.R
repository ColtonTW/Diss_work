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

Bflies <- read_xlsx("Bi_Bu_data.xlsx",sheet="general")

## look at richness and counts first 
## are vars normal -- overall from both years can be seen in the butterfly_coding

Bflies$Daily_butterfly_count <- as.numeric(Bflies$Daily_butterfly_count)

#2020
#normal 

#not normal
shapiro.test(Bflies$`Mean_Wind_Speed_(m/s)`[Bflies$Year=="2020"])
shapiro.test(Bflies$Mean_Visibility[Bflies$Year=="2020"])
shapiro.test(Bflies$Mean_Cloud_cover[Bflies$Year=="2020"])
shapiro.test(Bflies$Mean_Sun_percentage[Bflies$Year=="2020"])
shapiro.test(Bflies$Mean_Temperature[Bflies$Year=="2020"])
shapiro.test(Bflies$Daily_bu_species_countss[Bflies$Year=="2020"])
shapiro.test(Bflies$Daily_butterfly_count[Bflies$Year=="2020"])

#2021
#normal 
shapiro.test(Bflies$Mean_Temperature[Bflies$Year=="2021"])

#not normal
shapiro.test(Bflies$`Mean_Wind_Speed_(m/s)`[Bflies$Year=="2021"])
shapiro.test(Bflies$Mean_Visibility[Bflies$Year=="2021"])
shapiro.test(Bflies$Mean_Cloud_cover[Bflies$Year=="2021"])
shapiro.test(Bflies$Mean_Sun_percentage[Bflies$Year=="2021"])
shapiro.test(Bflies$Daily_bu_species_countss[Bflies$Year=="2021"])
shapiro.test(Bflies$Daily_butterfly_count[Bflies$Year=="2021"])

## weather vars
Bflies
### zinbr coding ---- USE THIS REGRESSION DATA   NEED TO ADD NEGBIN TO ALL 
n1 <- zeroinfl(Bflies$Daily_bu_species_counts~Bflies$`Mean_Wind_Speed_(m/s)`,dist = "negbin")
summary(n1)  
plot(Bflies$Daily_bu_species_counts~Bflies$`Mean_Wind_Speed_(m/s)`,xlab="Mean wind speed (m/s)",ylab="Daily species count",pch=4,col="darkred") 
abline(n1)


n2 <- zeroinfl(Bflies$Daily_bu_species_counts~Bflies$Mean_Temperature)
summary(n2)  
plot(Bflies$Daily_bu_species_counts~(Bflies$Mean_Temperature),xlab="Mean Temperature (celsius)",ylab="Daily species count",pch=4,col="darkred")
abline(n2)

n3 <- zeroinfl(Bflies$Daily_bu_species_counts~Bflies$Mean_Visibility)
summary(n3)  
plot(Bflies$Daily_bu_species_counts~(Bflies$Mean_Visibility),xlab="Mean Visibility",ylab="Daily species count",pch=4,col="darkred")
abline(n3)


n4 <- zeroinfl(Bflies$Daily_bu_species_counts~Bflies$Mean_Sun_percentage)
summary(n4) 
plot(Bflies$Daily_bu_species_counts~(Bflies$Mean_Sun_percentage),xlab="Mean Sun intensity (%)",ylab="Daily species count",pch=4,col="darkred")
abline(n4)


n5 <- zeroinfl(Bflies$Daily_bu_species_counts~Bflies$Mean_Cloud_cover)
summary(n5) 
plot(Bflies$Daily_bu_species_counts~Bflies$Mean_Cloud_cover,xlab="Mean cloud coverage",ylab="Daily species count",pch=4,col="darkred")
abline(n5)


# butterfly count now 
n6 <- zeroinfl(Bflies$Daily_butterfly_count~Bflies$`Mean_Wind_Speed_(m/s)`)
summary(n6) 
plot((log(Bflies$Daily_butterfly_count))~Bflies$`Mean_Wind_Speed_(m/s)`,xlab="Mean wind speed (m/s)", ylab="log Daily butterfly count",pch=4,col="darkblue")
abline(n6)

n7 <- zeroinfl(Bflies$Daily_butterfly_count~Bflies$Mean_Temperature)
summary(n7) 
plot((log(Bflies$Daily_butterfly_count))~Bflies$Mean_Temperature,xlab="Mean temperature (celsius)", ylab="log Daily butterfly count",pch=4,col="darkblue")
abline(n7)

n8 <- zeroinfl(Bflies$Daily_butterfly_count~Bflies$Mean_Visibility)
summary(n8) 
plot((log(Bflies$Daily_butterfly_count))~Bflies$Mean_Visibility,xlab="Mean Visibility", ylab="log Daily butterfly count",pch=4,col="darkblue")
abline(n8)

n9 <- zeroinfl(Bflies$Daily_butterfly_count~Bflies$Mean_Sun_percentage)
summary(n9) 
plot((log(Bflies$Daily_butterfly_count))~Bflies$Mean_Sun_percentage,xlab="Mean sun intensity (%)", ylab="log Daily butterfly count",pch=4,col="darkblue")
abline(n9)

n10 <- zeroinfl(Bflies$Daily_butterfly_count~Bflies$Mean_Cloud_cover)
summary(n10) 
plot((log(Bflies$Daily_butterfly_count))~Bflies$Mean_Cloud_cover,xlab="Mean cloud cover", ylab="log Daily butterfly count",pch=4,col="darkblue")
abline(n10)


#### per year diffs -- none are normal, using zero inflated again
#2020
bflies_20 <- subset.data.frame(Bflies[c(1:46),])

m11 <- zeroinfl(bflies_20$Daily_bu_species_counts~bflies_20$`Mean_Wind_Speed_(m/s)`,dist = "negbin")
summary(m11) 
plot(bflies_20$Daily_bu_species_counts~bflies_20$`Mean_Wind_Speed_(m/s)`,xlab="Mean wind speed (m/s)",ylab="Daily species count",pch=4,col="darkred") 
abline(m11)


m12 <- zeroinfl(bflies_20$Daily_bu_species_counts~bflies_20$Mean_Temperature)
summary(m12) 
plot(bflies_20$Daily_bu_species_counts~(bflies_20$Mean_Temperature),xlab="Mean Temperature (celsius)",ylab="Daily species count",pch=4,col="darkred")
abline(m12)

m13 <- zeroinfl(bflies_20$Daily_bu_species_counts~bflies_20$Mean_Visibility)
summary(m13) 
plot(bflies_20$Daily_bu_species_counts~(bflies_20$Mean_Visibility),xlab="Mean Visibility",ylab="Daily species count",pch=4,col="darkred")
abline(m13)


m14 <- zeroinfl(bflies_20$Daily_bu_species_counts~bflies_20$Mean_Sun_percentage)
summary(m14) 
plot(bflies_20$Daily_bu_species_counts~(bflies_20$Mean_Sun_percentage),xlab="Mean Sun intensity (%)",ylab="Daily species count",pch=4,col="darkred")
abline(m14)


m15 <- zeroinfl(bflies_20$Daily_bu_species_counts~bflies_20$Mean_Cloud_cover)
summary(m15)
plot(bflies_20$Daily_bu_species_counts~bflies_20$Mean_Cloud_cover,xlab="Mean cloud coverage",ylab="Daily species count",pch=4,col="darkred")
abline(m15)


# butterfly count now 
m16 <- zeroinfl(bflies_20$Daily_butterfly_count~bflies_20$`Mean_Wind_Speed_(m/s)`)
summary(m16) 
plot(bflies_20$Daily_butterfly_count~bflies_20$`Mean_Wind_Speed_(m/s)`,xlab="Mean wind speed (m/s)", ylab="Daily butterfly count",pch=4,col="darkblue")
abline(m16)

m17 <- zeroinfl(bflies_20$Daily_butterfly_count~bflies_20$Mean_Temperature)
summary(m17)
plot(bflies_20$Daily_butterfly_count~(bflies_20$Mean_Temperature),xlab="Mean temperature (celsius)", ylab="Daily butterfly count",pch=4,col="darkblue")
abline(m17)

m18 <- zeroinfl(bflies_20$Daily_butterfly_count~bflies_20$Mean_Visibility)
summary(m18) 
plot(bflies_20$Daily_butterfly_count~(bflies_20$Mean_Visibility),xlab="Mean visibility", ylab="Daily butterfly count",pch=4,col="darkblue")
abline(m18)

m19 <- zeroinfl(bflies_20$Daily_butterfly_count~bflies_20$Mean_Sun_percentage)
summary(m19) 
plot(bflies_20$Daily_butterfly_count~(bflies_20$Mean_Sun_percentage),xlab="Mean sun intensity (%)", ylab="Daily butterfly count",pch=4,col="darkblue")
abline(m19)

m20 <- zeroinfl(bflies_20$Daily_butterfly_count~bflies_20$Mean_Cloud_cover)
summary(m20) 
plot(bflies_20$Daily_butterfly_count~(bflies_20$Mean_Cloud_cover),xlab="Mean cloud cover", ylab="Daily butterfly count",pch=4,col="darkblue")
abline(m20)


## 2021
bflies_21 <- subset.data.frame(Bflies[c(47:95),])

m21 <- zeroinfl(bflies_21$Daily_bu_species_counts~bflies_21$`Mean_Wind_Speed_(m/s)`,dist = "negbin")
summary(m21) 
plot(bflies_21$Daily_bu_species_counts~bflies_21$`Mean_Wind_Speed_(m/s)`,xlab="Mean wind speed (m/s)",ylab="Daily species count",pch=4,col="darkred") 
abline(m21)


m22 <- zeroinfl(bflies_21$Daily_bu_species_counts~bflies_21$Mean_Temperature)
summary(m22) 
plot(bflies_21$Daily_bu_species_counts~(bflies_21$Mean_Temperature),xlab="Mean Temperature (celsius)",ylab="Daily species count",pch=4,col="darkred")
abline(m22)

m23 <- zeroinfl(bflies_21$Daily_bu_species_counts~bflies_21$Mean_Visibility)
summary(m23) 
plot(bflies_21$Daily_bu_species_counts~(bflies_21$Mean_Visibility),xlab="Mean Visibility",ylab="Daily species count",pch=4,col="darkred")
abline(m23)


m24 <- zeroinfl(bflies_21$Daily_bu_species_counts~bflies_21$Mean_Sun_percentage)
summary(m24) 
plot(bflies_21$Daily_bu_species_counts~(bflies_21$Mean_Sun_percentage),xlab="Mean Sun intensity (%)",ylab="Daily species count",pch=4,col="darkred")
abline(m24)


m25 <- zeroinfl(bflies_21$Daily_bu_species_counts~bflies_21$Mean_Cloud_cover)
summary(m25) 
plot(bflies_21$Daily_bu_species_counts~bflies_21$Mean_Cloud_cover,xlab="Mean cloud coverage",ylab="Daily species count",pch=4,col="darkred")
abline(m25)


# butterfly count now 
m26 <- zeroinfl(bflies_21$Daily_butterfly_count~bflies_21$`Mean_Wind_Speed_(m/s)`)
summary(m26) 
plot(bflies_21$Daily_butterfly_count~bflies_21$`Mean_Wind_Speed_(m/s)`,xlab="Mean wind speed (m/s)", ylab="Daily butterfly count",pch=4,col="darkblue")
abline(m26)

m27 <- zeroinfl(bflies_21$Daily_butterfly_count~bflies_21$Mean_Temperature)
summary(m27) 
plot(bflies_21$Daily_butterfly_count~(bflies_21$Mean_Temperature),xlab="Mean temperature (celsius)", ylab="Daily butterfly count",pch=4,col="darkblue")
abline(m27)

m28 <- zeroinfl(bflies_21$Daily_butterfly_count~bflies_21$Mean_Visibility)
summary(m28) 
plot(bflies_21$Daily_butterfly_count~(bflies_21$Mean_Visibility),xlab="Mean visibility", ylab="Daily butterfly count",pch=4,col="darkblue")
abline(m28)

m29 <- zeroinfl(bflies_21$Daily_butterfly_count~bflies_21$Mean_Sun_percentage)
summary(m29) 
plot(bflies_21$Daily_butterfly_count~(bflies_21$Mean_Sun_percentage),xlab="Mean sun intensity (%)", ylab="Daily butterfly count",pch=4,col="darkblue")
abline(m29)

m30 <- zeroinfl(bflies_21$Daily_butterfly_count~bflies_21$Mean_Cloud_cover)
summary(m30) 
plot(bflies_21$Daily_butterfly_count~(bflies_21$Mean_Cloud_cover),xlab="Mean cloud cover", ylab="Daily butterfly count",pch=4,col="darkblue")
abline(m30)


### Bar plots pf Head vs tail wind
Bflies2 <- subset.data.frame(Bflies[Bflies$Mean_Wind_Heading==c("T","H"),])
Bflies2$Mean_Wind_Heading
Bflies2
?geom_boxplot
dev.off()
ggplot(Bflies2,mapping = aes(Mean_Wind_Heading,Daily_bu_species_counts)) + geom_boxplot(fill=c("red","blue")) + labs(x="Wind direction",y="Daily species counts",caption="H - Head wind, T - Tail wind")
  t.test(Bflies2$Daily_bu_species_counts[Bflies2$Mean_Wind_Heading=="T"],Bflies2$Daily_bu_species_counts[Bflies2$Mean_Wind_Heading=="H"],paired=FALSE)

ggplot(Bflies2,mapping = aes(Mean_Wind_Heading,Daily_butterfly_count)) + geom_boxplot(fill=c("red","blue")) + labs(x="Wind direction", y="Daily butterfly count", caption="H - Head wind, T - Tail wind")
t.test(Bflies2$Daily_bu_species_counts[Bflies2$Mean_Wind_Heading=="T"],Bflies2$Daily_butterfly_count[Bflies2$Mean_Wind_Heading=="H"],paired=FALSE)

ggplot(Bflies2,mapping = aes(Mean_Wind_Heading,Daily_butterfly_count)) + geom_boxplot(fill=c("red","blue")) + labs(x="Wind direction", y="Daily butterfly count", caption="H - Head wind, T - Tail wind") + ylim(0,220)
ggplot(Bflies2,mapping = aes(Mean_Wind_Heading,Daily_butterfly_count)) + geom_boxplot(fill=c("red","blue")) + labs(x="Wind direction", y="Daily butterfly count", caption="H - Head wind, T - Tail wind") + ylim(0,100)


#looking at species richness
p1 <- ggplot(bflies_20 ,mapping=aes(Date,Daily_bu_species_counts)) + geom_point() + geom_smooth()  + labs(x="Date", y="Daily species count",title = "2020 butterfly species richness") + ylim(0,10)
p2 <- ggplot(bflies_21,mapping=aes(Date,Daily_bu_species_counts)) + geom_point() + geom_smooth() + labs(x="Date", y="Daily species count",title = "2021 butterfly species richness") + ylim(0,10) 
p1+p2
ggsave(filename = "Butterfly species richness over time.jpg", plot = last_plot(),limitsize = FALSE, height=4, width = 10)

t.test(bflies_20$Daily_bu_species_counts,bflies_21$Daily_bu_species_counts,paired=FALSE)

## wanna do some comparing with the bird stuff 
t.test(bflies_20$Daily_bi_species_count,bflies_20$Daily_bu_species_counts,paired=TRUE)
t.test(bflies_21$Daily_bi_species_count,bflies_21$Daily_bu_species_counts,paired=TRUE)

##2020
# clouded yellow
# long tailed blue 
# red admiral
#small tortoiseshell
plot(bflies_20$Date ,bflies_20$`Clouded Yellow` ,type="line",col="navy",xlab="Date in 2020",ylab="Frequency of observation") + lines(bflies_20$Date ,bflies_20$`Long tailed blue` ,col="orange") +
        lines(bflies_20$Date ,bflies_20$`Red admiral` ,col="darkgreen") + 
        lines(bflies_20$Date ,bflies_20$`Small tortoiseshell` ,col="lightblue3") + axis(2,at=c(0,50,100,500,1000,1500)) +
       legend(x="topright",legend=c("Clouded yellow","Long tailed blue","Red admiral","Small tortoiseshell"),lty=1,col = c("navy","orange","darkgreen","lightblue3"))

plot(bflies_20$Date ,bflies_20$`Long tailed blue` ,type="line",col="orange",xlab="Date in 2020",ylab="Frequency of observation") +
  lines(bflies_20$Date ,bflies_20$`Red admiral` ,col="darkgreen") + 
  lines(bflies_20$Date ,bflies_20$`Small tortoiseshell` ,col="lightblue3") + axis(2,at=c(0,50,100,500,1000,1500)) +
  legend(x="topright",legend=c("Long tailed blue","Red admiral","Small tortoiseshell"),lty=1,col = c("orange","darkgreen","lightblue3"))

plot(bflies_20$Date ,bflies_20$`Red admiral` ,type="line",col="darkgreen",xlab="Date in 2020",ylab="Frequency of observation") +
  lines(bflies_20$Date ,bflies_20$`Small tortoiseshell` ,col="lightblue3") + axis(2,at=c(0,50,100,500,1000,1500)) +
  legend(x="topleft",legend=c("Red admiral","Small tortoiseshell"),lty=1,col = c("darkgreen","lightblue3"))

##2021
# clouded yellow
# red admiral 
# small white
# hummingbird hawkmoth 

plot(bflies_21$Date ,bflies_21$`Clouded Yellow` ,type="line",col="navy",xlab="Date in 2021",ylab="Frequency of observation") + lines(bflies_21$Date ,bflies_21$`Red admiral` ,col="darkgreen") + 
  lines(bflies_21$Date ,bflies_21$`Small white` ,col="red") +
  lines(bflies_21$Date ,bflies_21$`Hummingbird Hawkmoth` ,col="blue") + axis(2,at=c(0,50,100,500,1000,1500)) +
  legend(x="topright",legend=c("Clouded yellow","Red admiral","Small white", "Hummingbird hawkmoth"),lty=1,col = c("navy","darkgreen","red","blue"))



## focus on clouded yellows 
c <- lm(bflies_20$`Clouded Yellow`~bflies_20$Date)
cy <- lm(bflies_21$`Clouded Yellow`~bflies_21$Date)
plot(bflies_20$`Clouded Yellow`~bflies_20$Date,ylim=c(0,750),type="line" ,xlab="Date in 2020", ylab="Clouded yellow sightings",col="blue")
plot(bflies_21$`Clouded Yellow`~bflies_21$Date,ylim=c(0,750),type="line" ,xlab="Date in 2021", ylab="Clouded yellow sightings",col="blue")

anova(c,cy)

# in env conditions
c1 <- zeroinfl(Bflies$'Clouded Yellow'~Bflies$`Mean_Wind_Speed_(m/s)`,dist = "negbin")
summary(c1)   
plot((log(Bflies$'Clouded Yellow'))~Bflies$`Mean_Wind_Speed_(m/s)`,xlab="Mean wind speed (m/s)",ylab="Log Clouded Yellow count",pch=4,col="darkred") 
abline(c1)


c2 <- zeroinfl(Bflies$'Clouded Yellow'~Bflies$Mean_Temperature)
summary(c2) 
plot((log(Bflies$'Clouded Yellow'))~(Bflies$Mean_Temperature),xlab="Mean Temperature (celsius)",ylab="Log Clouded Yellow count",pch=4,col="darkred")
abline(c2)

c3 <- zeroinfl(Bflies$'Clouded Yellow'~Bflies$Mean_Visibility)
summary(c3)  
plot((log(Bflies$'Clouded Yellow'))~Bflies$Mean_Visibility,xlab="Mean Visibility",ylab="Log Clouded Yellow count",pch=4,col="darkred")
abline(c3)


c4 <- zeroinfl(Bflies$'Clouded Yellow'~Bflies$Mean_Sun_percentage)
summary(c4) 
plot((log(Bflies$'Clouded Yellow'))~Bflies$Mean_Sun_percentage,xlab="Mean Sun intensity (%)",ylab="Log Clouded Yellow count",pch=4,col="darkred")
abline(c4)


c5 <- zeroinfl(Bflies$'Clouded Yellow'~Bflies$Mean_Cloud_cover)
summary(c5)  
plot((log(Bflies$'Clouded Yellow'))~Bflies$Mean_Cloud_cover,xlab="Mean cloud coverage",ylab="Log Clouded Yellow count",pch=4,col="darkred")
abline(c5)

## wind direction
ggplot(Bflies2,mapping = aes(Bflies2$Mean_Wind_Heading,Bflies2$`Clouded Yellow`)) + geom_boxplot(fill=c("red","blue")) + labs(x="Wind direction",y="Daily Clouded yellow butterfly counts",caption="H - Head wind, T - Tail wind")
t.test(Bflies2$`Clouded Yellow`[Bflies2$Mean_Wind_Heading=="T"],Bflies2$`Clouded Yellow`[Bflies2$Mean_Wind_Heading=="H"],paired=FALSE)



