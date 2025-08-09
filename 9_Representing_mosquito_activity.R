################################################################################

#Representing mosquito activity

################################################################################

#Load libraries

library(lubridate)
library(magrittr) 
library(dplyr) 
library(activity)
library(tidyr)
library(readr)
library(ggplot2)
library(patchwork) 

#Set up working directory

setwd("/yourdirectory")

#Load data

#Raw
df_aspb <- read.csv("~/yourpath/df_aspb_2021_2024.csv")

#Corrected by sampling effort and modeled
df_model_allTraps <- read_csv("~/yourpath/df_model_allTraps_female.csv")

################################################################################

#Part 1: Representing raw data from smart-traps

################################################################################

df_aspb_act <- df_aspb

#Filter data from May to October according to S1_Supplementary information

df_aspb_act <- df_aspb_act[df_aspb_act$month %in% c("May", "Jun", "Jul", "Aug", "Sep", "Oct"), ]

#Filter only females

df_aspb_act <- df_aspb_act[df_aspb_act$sex == "Female", ]

                           
#Activity package
#Obtain solar time and clock time in radians

library(activity)
tmp <- solartime ( df_aspb_act$datetime_local,   
                   df_aspb_act$latitude,  
                   df_aspb_act$longitude, 
                   tz=1,      
                   format="%Y-%m-%d %H:%M:%S")

df_aspb_act$clock<-tmp$clock #Add clock time and solar time to the original dataset
df_aspb_act$solar<-tmp$solar 

#Obtain approximate hours for sunrise, sunset and day length

snt<-get_suntimes(df_aspb_act$datetime_local,
                  df_aspb_act$latitude,  
                  df_aspb_act$longitude,
                  offset= 1) #Equal to tz

df_aspb_act$sunrise<-snt$sunrise #Add avrg hour of sunset, sunrise and day length to the df
df_aspb_act$sunset<-snt$sunset
df_aspb_act$daylength<-snt$daylength

# Fit an activity model for Aedes and Culex for each trap
# Deal with bias: absences in some cases are because traps were OFF

#Aedes

#Solar time (average anchoring)

t1_Ae <- fitact(df_aspb_act$solar[df_aspb_act$genus=="Aedes" & df_aspb_act$trap==1], sample="data")
saveRDS(t1_Ae, file = "t1_Ae.rds") #Save object in the current working directory

t2_Ae <- fitact(df_aspb_act$solar[df_aspb_act$genus=="Aedes" & df_aspb_act$trap==2], sample="data")
saveRDS(t2_Ae, file = "t2_Ae.rds") 

t3_Ae <- fitact(df_aspb_act$solar[df_aspb_act$genus=="Aedes" & df_aspb_act$trap==3], sample="data")
saveRDS(t3_Ae, file = "t3_Ae.rds") 

t4_Ae <- fitact(df_aspb_act$solar[df_aspb_act$genus=="Aedes" & df_aspb_act$trap==4], sample="data")
saveRDS(t4_Ae, file = "t4_Ae.rds") 

#Load .rds files

t1_Ae <- readRDS("~/yourpath/t1_Ae.rds")
t2_Ae <- readRDS("~/yourpath/t2_Ae.rds")
t3_Ae <- readRDS("~/yourpath/Fitact/t3_Ae.rds")
t4_Ae <- readRDS("~/yourpath/Fitact/t4_Ae.rds")

#Plot

par(xaxs = "i", yaxs = "i", mar=c(4,4,4,12)) 

plot(t1_Ae, yunit="density", data="none", las=1, lwd=2,
     tline=list(col="lightskyblue",lwd=2), 
     cline=list(lty=0),
     ylim=c(0,0.3)) 

plot(t2_Ae, yunit="density", data="none", add=TRUE, 
     tline=list(col="deepskyblue3", lwd=2),
     cline=list(lty=0))

plot(t3_Ae, yunit="density", data="none", add=TRUE,
     tline=list(col="turquoise",lwd=2), 
     cline=list(lty=0))

plot(t4_Ae, yunit="density", data="none", add=TRUE, 
     tline=list(col="royalblue1", lwd=2),
     cline=list(lty=0))

# Calculate maximum and minimun sunrise and sunset times

mx.sr<-max(df_aspb_act$sunrise)
mn.sr<-min(df_aspb_act$sunrise)
mx.ss<-max(df_aspb_act$sunset)
mn.ss<-min(df_aspb_act$sunset)
av.sr<-mean(df_aspb_act$sunrise)
av.ss<-mean(df_aspb_act$sunset)

# Add polygons representing night length and sunrise/sunset times

polygon(x=c(0,mx.sr, mx.sr,0), y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
polygon(x=c(0,mn.sr, mn.sr,0), y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
polygon(x=c(mn.ss, 24,24, mn.ss),y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
polygon(x=c(mx.ss, 24,24, mx.ss),y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
abline(v=av.sr,lty = 2, lwd = 1)
abline(v=av.ss, lty = 2, lwd = 1)

# Add legend

legend("right", inset = c(-0.35, 0), 
       legend = c("Trap 1 (Sant Andreu)", "Trap 2 (Horta)", "Trap 3 (Pedralbes)", "Trap 4 (Zoo)"),
       col= c("lightskyblue", "deepskyblue3", "turquoise", "royalblue1"),
       lty = 1, lwd = 2, cex = 0.8, box.lty = 0,
       xpd = TRUE,  
       y.intersp = 1.5)  

# Add title

title(main = "Daily activity of Aedes albopictus")

#Culex

#Solar time (average anchoring)

t1_Cx <- fitact(df_aspb_act$solar[df_aspb_act$genus=="Culex" & df_aspb_act$trap==1], sample="data")
saveRDS(t1_Cx, file = "t1_Cx.rds") #Save object in the current working directory

t2_Cx <- fitact(df_aspb_act$solar[df_aspb_act$genus=="Culex" & df_aspb_act$trap==2], sample="data")
saveRDS(t2_Cx, file = "t2_Cx.rds") 

t3_Cx <- fitact(df_aspb_act$solar[df_aspb_act$genus=="Culex" & df_aspb_act$trap==3], sample="data")
saveRDS(t3_Cx, file = "t3_Cx.rds") 

t4_Cx <- fitact(df_aspb_act$solar[df_aspb_act$genus=="Culex" & df_aspb_act$trap==4], sample="data")
saveRDS(t4_Cx, file = "t4_Cx.rds") 

#Load .rds files

t1_Cx <- readRDS("~/yourpath/t1_Cx.rds")
t2_Cx <- readRDS("~/yourpath/t2_Cx.rds")
t3_Cx <- readRDS("~/yourpath/t3_Cx.rds")
t4_Cx <- readRDS("~/yourpath/t4_Cx.rds")

#Plot

par(xaxs = "i", yaxs = "i", mar=c(4,4,4,12)) 

plot(t1_Cx, yunit="density", data="none", las=1, lwd=2,
     tline=list(col="lightskyblue",lwd=2), 
     cline=list(lty=0),
     ylim=c(0,0.3)) 

plot(t2_Cx, yunit="density", data="none", add=TRUE, 
     tline=list(col="deepskyblue3", lwd=2),
     cline=list(lty=0))

plot(t3_Cx, yunit="density", data="none", add=TRUE,
     tline=list(col="turquoise",lwd=2), 
     cline=list(lty=0))

plot(t4_Cx, yunit="density", data="none", add=TRUE, 
     tline=list(col="royalblue1", lwd=2),
     cline=list(lty=0))

# Calculate maximum and minimun sunrise and sunset times

mx.sr<-max(df_aspb_act$sunrise)
mn.sr<-min(df_aspb_act$sunrise)
mx.ss<-max(df_aspb_act$sunset)
mn.ss<-min(df_aspb_act$sunset)
av.sr<-mean(df_aspb_act$sunrise)
av.ss<-mean(df_aspb_act$sunset)

# Add polygons representing night length and sunrise/sunset times

polygon(x=c(0,mx.sr, mx.sr,0), y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
polygon(x=c(0,mn.sr, mn.sr,0), y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
polygon(x=c(mn.ss, 24,24, mn.ss),y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
polygon(x=c(mx.ss, 24,24, mx.ss),y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
abline(v=av.sr,lty = 2, lwd = 1)
abline(v=av.ss, lty = 2, lwd = 1)

# Add legend

legend("right", inset = c(-0.35, 0), 
       legend = c("Trap 1 (Sant Andreu)", "Trap 2 (Horta)", "Trap 3 (Pedralbes)", "Trap 4 (Zoo)"),
       col= c("lightskyblue", "deepskyblue3", "turquoise", "royalblue1"),
       lty = 1, lwd = 2, cex = 0.8, box.lty = 0,
       xpd = TRUE,  
       y.intersp = 1.5)  

# Add title

title(main = "Daily activity of Culex pipiens")

################################################################################

#Fit an activity model for each genus and month

################################################################################

#Aedes

may_Ae <- fitact(df_aspb_act$solar[df_aspb_act$genus=="Aedes" & df_aspb_act$month=="May"], sample="data")
saveRDS(may_Ae, file = "may_Ae.rds") #Save object in the current working directory

jun_Ae <- fitact(df_aspb_act$solar[df_aspb_act$genus=="Aedes" & df_aspb_act$month=="Jun"], sample="data")
saveRDS(jun_Ae, file = "jun_Ae.rds") 

jul_Ae <- fitact(df_aspb_act$solar[df_aspb_act$genus=="Aedes" & df_aspb_act$month=="Jul"], sample="data")
saveRDS(jul_Ae, file = "jul_Ae.rds") 

aug_Ae <- fitact(df_aspb_act$solar[df_aspb_act$genus=="Aedes" & df_aspb_act$month=="Aug"], sample="data")
saveRDS(aug_Ae, file = "aug_Ae.rds") 

sep_Ae <- fitact(df_aspb_act$solar[df_aspb_act$genus=="Aedes" & df_aspb_act$month=="Sep"], sample="data")
saveRDS(sep_Ae, file = "sep_Ae.rds") 

oct_Ae <- fitact(df_aspb_act$solar[df_aspb_act$genus=="Aedes" & df_aspb_act$month=="Oct"], sample="data")
saveRDS(oct_Ae, file = "oct_Ae.rds") 

#Load .rds files

may_Ae <- readRDS("~/yourpath/may_Ae.rds")
jun_Ae <- readRDS("~/yourpath/jun_Ae.rds")
jul_Ae <- readRDS("~/yourpath/jul_Ae.rds")
aug_Ae <- readRDS("~/yourpath/aug_Ae.rds")
sep_Ae <- readRDS("~/yourpath/sep_Ae.rds")
oct_Ae <- readRDS("~/yourpath/oct_Ae.rds")

#Plot

par(xaxs = "i", yaxs = "i", mar=c(4,4,4,12)) 

plot(may_Ae, yunit="density", data="none", las=1, lwd=2,
     tline=list(col="springgreen3",lwd=2), 
     cline=list(lty=0),
     ylim=c(0,0.3)) 

plot(jun_Ae, yunit="density", data="none", add=TRUE, 
     tline=list(col="gold1", lwd=2),
     cline=list(lty=0))

plot(jul_Ae, yunit="density", data="none", add=TRUE,
     tline=list(col="darkorange1",lwd=2), 
     cline=list(lty=0))

plot(aug_Ae, yunit="density", data="none", add=TRUE, 
     tline=list(col="red1", lwd=2),
     cline=list(lty=0))

plot(sep_Ae, yunit="density", data="none", add=TRUE, 
     tline=list(col="royalblue1", lwd=2),
     cline=list(lty=0))

plot(oct_Ae, yunit="density", data="none", add=TRUE, 
     tline=list(col="mediumorchid", lwd=2),
     cline=list(lty=0))

# Calculate maximum and minimun sunrise and sunset times

mx.sr<-max(df_aspb_act$sunrise)
mn.sr<-min(df_aspb_act$sunrise)
mx.ss<-max(df_aspb_act$sunset)
mn.ss<-min(df_aspb_act$sunset)
av.sr<-mean(df_aspb_act$sunrise)
av.ss<-mean(df_aspb_act$sunset)

# Add polygons representing night length and sunrise/sunset times

polygon(x=c(0,mx.sr, mx.sr,0), y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
polygon(x=c(0,mn.sr, mn.sr,0), y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
polygon(x=c(mn.ss, 24,24, mn.ss),y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
polygon(x=c(mx.ss, 24,24, mx.ss),y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
abline(v=av.sr,lty = 2, lwd = 1)
abline(v=av.ss, lty = 2, lwd = 1)

# Add legend

legend("right", inset = c(-0.35, 0), 
       legend = c("May", "June", "July", "August", "September", "October"),
       col= c("springgreen3", "gold1", "darkorange1","red1","royalblue1", "mediumorchid"),
       lty = 1, lwd = 2, cex = 0.8, box.lty = 0,
       xpd = TRUE,  
       y.intersp = 1.5)  

# Add title

title(main = "Daily activity of Aedes albopictus thorought the year")

#Culex

may_Cx <- fitact(df_aspb_act$solar[df_aspb_act$genus=="Culex" & df_aspb_act$month=="May"], sample="data")
saveRDS(may_Cx, file = "may_Cx.rds") #Save object in the current working directory

jun_Cx <- fitact(df_aspb_act$solar[df_aspb_act$genus=="Culex" & df_aspb_act$month=="Jun"], sample="data")
saveRDS(jun_Cx, file = "jun_Cx.rds") 

jul_Cx <- fitact(df_aspb_act$solar[df_aspb_act$genus=="Culex" & df_aspb_act$month=="Jul"], sample="data")
saveRDS(jul_Cx, file = "jul_Cx.rds") 

aug_Cx <- fitact(df_aspb_act$solar[df_aspb_act$genus=="Culex" & df_aspb_act$month=="Aug"], sample="data")
saveRDS(aug_Cx, file = "aug_Cx.rds") 

sep_Cx <- fitact(df_aspb_act$solar[df_aspb_act$genus=="Culex" & df_aspb_act$month=="Sep"], sample="data")
saveRDS(sep_Cx, file = "sep_Cx.rds") 

oct_Cx <- fitact(df_aspb_act$solar[df_aspb_act$genus=="Culex" & df_aspb_act$month=="Oct"], sample="data")
saveRDS(oct_Cx, file = "oct_Cx.rds") 

#Load .rds files

may_Cx <- readRDS("~/yourpath/may_Cx.rds")
jun_Cx <- readRDS("~/yourpath/jun_Cx.rds")
jul_Cx <- readRDS("~/yourpath/jul_Cx.rds")
aug_Cx <- readRDS("~/yourpath/aug_Cx.rds")
sep_Cx <- readRDS("~/yourpath/sep_Cx.rds")
oct_Cx <- readRDS("~/yourpath/oct_Cx.rds")

#Plot

par(xaxs = "i", yaxs = "i", mar=c(4,4,4,12)) 

plot(may_Cx, yunit="density", data="none", las=1, lwd=2,
     tline=list(col="springgreen3",lwd=2), 
     cline=list(lty=0),
     ylim=c(0,0.3)) 

plot(jun_Cx, yunit="density", data="none", add=TRUE, 
     tline=list(col="gold1", lwd=2),
     cline=list(lty=0))

plot(jul_Cx, yunit="density", data="none", add=TRUE,
     tline=list(col="darkorange1",lwd=2), 
     cline=list(lty=0))

plot(aug_Cx, yunit="density", data="none", add=TRUE, 
     tline=list(col="red1", lwd=2),
     cline=list(lty=0))

plot(sep_Cx, yunit="density", data="none", add=TRUE, 
     tline=list(col="royalblue1", lwd=2),
     cline=list(lty=0))

plot(oct_Cx, yunit="density", data="none", add=TRUE, 
     tline=list(col="mediumorchid", lwd=2),
     cline=list(lty=0))

# Calculate maximum and minimun sunrise and sunset times

mx.sr<-max(df_aspb_act$sunrise)
mn.sr<-min(df_aspb_act$sunrise)
mx.ss<-max(df_aspb_act$sunset)
mn.ss<-min(df_aspb_act$sunset)
av.sr<-mean(df_aspb_act$sunrise)
av.ss<-mean(df_aspb_act$sunset)

# Add polygons representing night length and sunrise/sunset times

polygon(x=c(0,mx.sr, mx.sr,0), y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
polygon(x=c(0,mn.sr, mn.sr,0), y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
polygon(x=c(mn.ss, 24,24, mn.ss),y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
polygon(x=c(mx.ss, 24,24, mx.ss),y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
abline(v=av.sr,lty = 2, lwd = 1)
abline(v=av.ss, lty = 2, lwd = 1)

# Add legend

legend("right", inset = c(-0.35, 0), 
       legend = c("May", "June", "July", "August", "September", "October"),
       col= c("springgreen3", "gold1", "darkorange1","red1","royalblue1", "mediumorchid"),
       lty = 1, lwd = 2, cex = 0.8, box.lty = 0,
       xpd = TRUE,  
       y.intersp = 1.5)  

# Add title

title(main = "Daily activity of Culex pipiens thorought the year")

################################################################################

#Fit an activity model for males and females

################################################################################

df_aspb_act2 <- df_aspb

#Filter data from May to October according to S1_Supplementary information

df_aspb_act2 <- df_aspb_act2[df_aspb_act2$month %in% c("May", "Jun", "Jul", "Aug", "Sep", "Oct"), ]

#Obtain solar time and clock time in radians

tmp <- solartime ( df_aspb_act2$datetime_local,   
                   df_aspb_act2$latitude,  
                   df_aspb_act2$longitude, 
                   tz=1,      
                   format="%Y-%m-%d %H:%M:%S")

df_aspb_act2$clock<-tmp$clock #Add clock time and solar time to the original dataset
df_aspb_act2$solar<-tmp$solar 

#Obtain approximate hours for sunrise, sunset and day length

snt<-get_suntimes(df_aspb_act2$datetime_local,
                  df_aspb_act2$latitude,  
                  df_aspb_act2$longitude,
                  offset= 1) #Equal to tz

df_aspb_act2$sunrise<-snt$sunrise #Add avrg hour of sunset, sunrise and day length to the df
df_aspb_act2$sunset<-snt$sunset
df_aspb_act2$daylength<-snt$daylength

#Sex comparison. Are there any differences in the daily activity by sex?
#Should we only consider females? (epidemiological crisis)

Ae_fem <- fitact(df_aspb_act2$solar[df_aspb_act2$class=="Aedes female"], sample="data")
saveRDS(Ae_fem, file = "Ae_fem.rds")

Ae_mal<- fitact(df_aspb_act2$solar[df_aspb_act2$class=="Aedes male"], sample="data")
saveRDS(Ae_mal, file = "Ae_mal.rds")

Cx_fem <- fitact(df_aspb_act2$solar[df_aspb_act2$class=="Culex female"], sample="data")
saveRDS(Cx_fem, file = "Cx_fem.rds")

Cx_mal <- fitact(df_aspb_act2$solar[df_aspb_act2$class=="Culex male"], sample="data")
saveRDS(Cx_mal, file = "Cx_mal.rds")

#Cargar ahivos .rds
Ae_fem <- readRDS("~/yourpath/Ae_fem.rds")
Ae_mal <- readRDS("~/Desktop/yourpath/Fitact/Ae_mal.rds")
Cx_fem <- readRDS("~/Desktop/yourpath/Cx_fem.rds")
Cx_mal <- readRDS("~/Desktop/yourpath/Cx_mal.rds")

#Plot to compare activity from different genus and sex

par(xaxs = "i", yaxs = "i", mar=c(4,6,4,12)) 

plot(Ae_fem, yunit="density", data="none", las=1, lwd=2,
     tline=list(col="darkorange",lwd=2), 
     cline=list(lty=0),
     ylim=c(0,0.3)) 

plot(Ae_mal, yunit="density", data="none", add=TRUE, 
     tline=list(col="darkorange", lwd=2, lty=2),
     cline=list(lty=0))

plot(Cx_fem, yunit="density", data="none", add=TRUE, 
     tline=list(col="mediumorchid", lwd=2),
     cline=list(lty=0))

plot(Cx_mal, yunit="density", data="none", add=TRUE, 
     tline=list(col="mediumorchid", lwd=2, lty=2),
     cline=list(lty=0))

# Add polygons representing night length and sunrise/sunset times
polygon(x=c(0,mx.sr, mx.sr,0), y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
polygon(x=c(0,mn.sr, mn.sr,0), y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
polygon(x=c(mn.ss, 24,24, mn.ss),y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
polygon(x=c(mx.ss, 24,24, mx.ss),y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
abline(v=av.sr,lty = 2, lwd = 1)
abline(v=av.ss, lty = 2, lwd = 1)

legend("right", inset = c(-0.4, 0),
       legend = c(bquote(italic("Ae. albopictus") ~ female),
                  bquote(italic("Ae. albopictus") ~ male),
                  bquote(italic("Cx. pipiens") ~ female),
                  bquote(italic("Cx. pipiens") ~ male)),
       col = c("darkorange", "darkorange", "mediumorchid", "mediumorchid"),
       lty = c(1, 2, 1, 2), lwd = 2, cex = 0.8, box.lty = 0,
       xpd = TRUE,
       y.intersp = 1.5)

# Añadir título al gráfico
title(main = "Daily activity rythms of females and males from Ae. albopictus and Cx. pipiens")

################################################################################

#Represent each month separately to see if the activity peaks match sunrise and sunset events

################################################################################

# Calculate maximum and minimun sunrise and sunset times for each month

#May

mx.sr_May<-max(df_aspb_act[df_aspb_act$month == "May", ]$sunrise)
mn.sr_May<-min(df_aspb_act[df_aspb_act$month == "May", ]$sunrise)
mx.ss_May<-max(df_aspb_act[df_aspb_act$month == "May", ]$sunset)
mn.ss_May<-min(df_aspb_act[df_aspb_act$month == "May", ]$sunset)
av.sr_May<-mean(df_aspb_act[df_aspb_act$month == "May", ]$sunrise)
av.ss_May<-mean(df_aspb_act[df_aspb_act$month == "May", ]$sunset)

#June

mx.sr_Jun<-max(df_aspb_act[df_aspb_act$month == "Jun", ]$sunrise)
mn.sr_Jun<-min(df_aspb_act[df_aspb_act$month == "Jun", ]$sunrise)
mx.ss_Jun<-max(df_aspb_act[df_aspb_act$month == "Jun", ]$sunset)
mn.ss_Jun<-min(df_aspb_act[df_aspb_act$month == "Jun", ]$sunset)
av.sr_Jun<-mean(df_aspb_act[df_aspb_act$month == "Jun", ]$sunrise)
av.ss_Jun<-mean(df_aspb_act[df_aspb_act$month == "Jun", ]$sunset)

#July

mx.sr_Jul<-max(df_aspb_act[df_aspb_act$month == "Jul", ]$sunrise)
mn.sr_Jul<-min(df_aspb_act[df_aspb_act$month == "Jul", ]$sunrise)
mx.ss_Jul<-max(df_aspb_act[df_aspb_act$month == "Jul", ]$sunset)
mn.ss_Jul<-min(df_aspb_act[df_aspb_act$month == "Jul", ]$sunset)
av.sr_Jul<-mean(df_aspb_act[df_aspb_act$month == "Jul", ]$sunrise)
av.ss_Jul<-mean(df_aspb_act[df_aspb_act$month == "Jul", ]$sunset)

#August

mx.sr_Aug<-max(df_aspb_act[df_aspb_act$month == "Aug", ]$sunrise)
mn.sr_Aug<-min(df_aspb_act[df_aspb_act$month == "Aug", ]$sunrise)
mx.ss_Aug<-max(df_aspb_act[df_aspb_act$month == "Aug", ]$sunset)
mn.ss_Aug<-min(df_aspb_act[df_aspb_act$month == "Aug", ]$sunset)
av.sr_Aug<-mean(df_aspb_act[df_aspb_act$month == "Aug", ]$sunrise)
av.ss_Aug<-mean(df_aspb_act[df_aspb_act$month == "Aug", ]$sunset)

#September

mx.sr_Sep<-max(df_aspb_act[df_aspb_act$month == "Sep", ]$sunrise)
mn.sr_Sep<-min(df_aspb_act[df_aspb_act$month == "Sep", ]$sunrise)
mx.ss_Sep<-max(df_aspb_act[df_aspb_act$month == "Sep", ]$sunset)
mn.ss_Sep<-min(df_aspb_act[df_aspb_act$month == "Sep", ]$sunset)
av.sr_Sep<-mean(df_aspb_act[df_aspb_act$month == "Sep", ]$sunrise)
av.ss_Sep<-mean(df_aspb_act[df_aspb_act$month == "Sep", ]$sunset)

#October

mx.sr_Oct<-max(df_aspb_act[df_aspb_act$month == "Oct", ]$sunrise)
mn.sr_Oct<-min(df_aspb_act[df_aspb_act$month == "Oct", ]$sunrise)
mx.ss_Oct<-max(df_aspb_act[df_aspb_act$month == "Oct", ]$sunset)
mn.ss_Oct<-min(df_aspb_act[df_aspb_act$month == "Oct", ]$sunset)
av.sr_Oct<-mean(df_aspb_act[df_aspb_act$month == "Oct", ]$sunrise)
av.ss_Oct<-mean(df_aspb_act[df_aspb_act$month == "Oct", ]$sunset)

#Graphical representation

# May

par(xaxs = "i", yaxs = "i", mar=c(4,4,4,12)) # Adjust axis limits 

plot(may_Ae, yunit="density", data="none", las=1, lwd=2,
     tline=list(col="springgreen3",lwd=2), 
     cline=list(lty=0),
     ylim=c(0,0.3)) 

plot(may_Cx, yunit="density", data="none", add=TRUE, 
     tline=list(col="springgreen", lwd=2),
     cline=list(lty=0))

# Add polygons representing night length and sunrise/sunset times
polygon(x=c(0,mx.sr_May, mx.sr_May,0), y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
polygon(x=c(0,mn.sr_May, mn.sr_May,0), y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
polygon(x=c(mn.ss_May, 24,24, mn.ss_May),y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
polygon(x=c(mx.ss_May, 24,24, mx.ss_May),y=c(0,0,10,10), col=rgb(0.2, 0.2, 0.2, 0.2), border=NA)
abline(v=av.sr_May,lty = 2, lwd = 1)
abline(v=av.ss_May, lty = 2, lwd = 1)

# Legend
legend("right", inset = c(-0.3, 0),
       legend = c("Ae. albopictus", "Cx.pipiens"),
       col= c("springgreen3", "springgreen"),
       lty = 1, lwd = 2, cex = 0.8, box.lty = 0,
       xpd = TRUE,
       y.intersp = 1.5)  

# Title
title(main = "Daily activity of Ae. albopictus and Cx.pipiens in May")

# June
par(xaxs = "i", yaxs = "i", mar = c(4,4,4,12))

plot(jun_Ae, yunit="density", data="none", las=1, lwd=2,
     tline=list(col="gold1", lwd=2), 
     cline=list(lty=0),
     ylim=c(0,0.3)) 

plot(jun_Cx, yunit="density", data="none", add=TRUE, 
     tline=list(col="yellow", lwd=2),
     cline=list(lty=0))

polygon(x=c(0, mx.sr_Jun, mx.sr_Jun, 0), y=c(0,0,10,10), col=rgb(0.2,0.2,0.2,0.2), border=NA)
polygon(x=c(0, mn.sr_Jun, mn.sr_Jun, 0), y=c(0,0,10,10), col=rgb(0.2,0.2,0.2,0.2), border=NA)
polygon(x=c(mn.ss_Jun, 24,24, mn.ss_Jun), y=c(0,0,10,10), col=rgb(0.2,0.2,0.2,0.2), border=NA)
polygon(x=c(mx.ss_Jun, 24,24, mx.ss_Jun), y=c(0,0,10,10), col=rgb(0.2,0.2,0.2,0.2), border=NA)
abline(v=av.sr_Jun, lty=2, lwd=1)
abline(v=av.ss_Jun, lty=2, lwd=1)

legend("right", inset=c(-0.3, 0),
       legend=c("Ae. albopictus", "Cx.pipiens"),
       col=c("gold1", "yellow"),
       lty=1, lwd=2, cex=0.8, box.lty=0,
       xpd=TRUE,
       y.intersp=1.5)

title(main="Daily activity of Ae. albopictus and Cx.pipiens in June")

# July

par(xaxs = "i", yaxs = "i", mar = c(4,4,4,12))

plot(jul_Ae, yunit="density", data="none", las=1, lwd=2,
     tline=list(col="darkorange3", lwd=2), 
     cline=list(lty=0),
     ylim=c(0,0.3)) 

plot(jul_Cx, yunit="density", data="none", add=TRUE, 
     tline=list(col="orange", lwd=2),
     cline=list(lty=0))

polygon(x=c(0, mx.sr_Jul, mx.sr_Jul, 0), y=c(0,0,10,10), col=rgb(0.2,0.2,0.2,0.2), border=NA)
polygon(x=c(0, mn.sr_Jul, mn.sr_Jul, 0), y=c(0,0,10,10), col=rgb(0.2,0.2,0.2,0.2), border=NA)
polygon(x=c(mn.ss_Jul, 24,24, mn.ss_Jul), y=c(0,0,10,10), col=rgb(0.2,0.2,0.2,0.2), border=NA)
polygon(x=c(mx.ss_Jul, 24,24, mx.ss_Jul), y=c(0,0,10,10), col=rgb(0.2,0.2,0.2,0.2), border=NA)
abline(v=av.sr_Jul, lty=2, lwd=1)
abline(v=av.ss_Jul, lty=2, lwd=1)

legend("right", inset=c(-0.3, 0),
       legend=c("Ae. albopictus", "Cx.pipiens"),
       col=c("darkorange3", "orange"),
       lty=1, lwd=2, cex=0.8, box.lty=0,
       xpd=TRUE,
       y.intersp=1.5)

title(main="Daily activity of Ae. albopictus and Cx.pipiens in July")

# August

par(xaxs = "i", yaxs = "i", mar = c(4,4,4,12))

plot(aug_Ae, yunit="density", data="none", las=1, lwd=2,
     tline=list(col="brown", lwd=2), 
     cline=list(lty=0),
     ylim=c(0,0.3)) 

plot(aug_Cx, yunit="density", data="none", add=TRUE, 
     tline=list(col="red1", lwd=2),
     cline=list(lty=0))

polygon(x=c(0, mx.sr_Aug, mx.sr_Aug, 0), y=c(0,0,10,10), col=rgb(0.2,0.2,0.2,0.2), border=NA)
polygon(x=c(0, mn.sr_Aug, mn.sr_Aug, 0), y=c(0,0,10,10), col=rgb(0.2,0.2,0.2,0.2), border=NA)
polygon(x=c(mn.ss_Aug, 24,24, mn.ss_Aug), y=c(0,0,10,10), col=rgb(0.2,0.2,0.2,0.2), border=NA)
polygon(x=c(mx.ss_Aug, 24,24, mx.ss_Aug), y=c(0,0,10,10), col=rgb(0.2,0.2,0.2,0.2), border=NA)
abline(v=av.sr_Aug, lty=2, lwd=1)
abline(v=av.ss_Aug, lty=2, lwd=1)

legend("right", inset=c(-0.3, 0),
       legend=c("Ae. albopictus", "Cx.pipiens"),
       col=c("brown", "red1"),
       lty=1, lwd=2, cex=0.8, box.lty=0,
       xpd=TRUE,
       y.intersp=1.5)

title(main="Daily activity of Ae. albopictus and Cx.pipiens in August")

# September
par(xaxs = "i", yaxs = "i", mar = c(4,4,4,12))

plot(sep_Ae, yunit="density", data="none", las=1, lwd=2,
     tline=list(col="royalblue1", lwd=2), 
     cline=list(lty=0),
     ylim=c(0,0.3)) 

plot(sep_Cx, yunit="density", data="none", add=TRUE, 
     tline=list(col="cyan3", lwd=2),
     cline=list(lty=0))

polygon(x=c(0, mx.sr_Sep, mx.sr_Sep, 0), y=c(0,0,10,10), col=rgb(0.2,0.2,0.2,0.2), border=NA)
polygon(x=c(0, mn.sr_Sep, mn.sr_Sep, 0), y=c(0,0,10,10), col=rgb(0.2,0.2,0.2,0.2), border=NA)
polygon(x=c(mn.ss_Sep, 24,24, mn.ss_Sep), y=c(0,0,10,10), col=rgb(0.2,0.2,0.2,0.2), border=NA)
polygon(x=c(mx.ss_Sep, 24,24, mx.ss_Sep), y=c(0,0,10,10), col=rgb(0.2,0.2,0.2,0.2), border=NA)
abline(v=av.sr_Sep, lty=2, lwd=1)
abline(v=av.ss_Sep, lty=2, lwd=1)

legend("right", inset=c(-0.3, 0),
       legend=c("Ae. albopictus", "Cx.pipiens"),
       col=c("royalblue1", "cyan3"),
       lty=1, lwd=2, cex=0.8, box.lty=0,
       xpd=TRUE,
       y.intersp=1.5)

title(main="Daily activity of Ae. albopictus and Cx.pipiens in September")

# October
par(xaxs = "i", yaxs = "i", mar = c(4,4,4,12))

plot(oct_Ae, yunit="density", data="none", las=1, lwd=2,
     tline=list(col="mediumorchid", lwd=2), 
     cline=list(lty=0),
     ylim=c(0,0.3)) 

plot(oct_Cx, yunit="density", data="none", add=TRUE, 
     tline=list(col="plum1", lwd=2),
     cline=list(lty=0))

polygon(x=c(0, mx.sr_Oct, mx.sr_Oct, 0), y=c(0,0,10,10), col=rgb(0.2,0.2,0.2,0.2), border=NA)
polygon(x=c(0, mn.sr_Oct, mn.sr_Oct, 0), y=c(0,0,10,10), col=rgb(0.2,0.2,0.2,0.2), border=NA)
polygon(x=c(mn.ss_Oct, 24,24, mn.ss_Oct), y=c(0,0,10,10), col=rgb(0.2,0.2,0.2,0.2), border=NA)
polygon(x=c(mx.ss_Oct, 24,24, mx.ss_Oct), y=c(0,0,10,10), col=rgb(0.2,0.2,0.2,0.2), border=NA)
abline(v=av.sr_Oct, lty=2, lwd=1)
abline(v=av.ss_Oct, lty=2, lwd=1)

legend("right", inset=c(-0.3, 0),
       legend=c("Ae. albopictus", "Cx.pipiens"),
       col=c("mediumorchid", "plum1"),
       lty=1, lwd=2, cex=0.8, box.lty=0,
       xpd=TRUE,
       y.intersp=1.5)

title(main="Daily activity of Ae. albopictus and Cx.pipiens in October")
