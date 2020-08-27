#### Climate Data Project: Principal Component Analysis Script ####
## Author: Wes Slaughter
## Org: Sierra Streams Institute
## Last Modified: 8/13/2020

# Hello! Welcome to the Princiapl Component Analysis (PCA) script. This script's
# primary purpose is to prepare the data for PCA and run PCA

# Load in necessary libraries
library(plyr)
library(tidyverse)
library(vegan)
library(zoo)

# Set working directory
setwd("C:/Users/Wes/github/ClimateDataProject")

# Bring in the raw datasets:

# Water Quality (WQ), Climate (CL), Bugs (BMI) dataframe
# Subset to only 200-2016, as these are the years for which we 
# have climate data

DF.i <- read.csv("R/Principal Component Analysis/FinalData.csv")[,-1] %>% 
                    subset(WY < 2017)

#Clean data to remove outliers.
#Outliers here deteremined after internal discussion of "plausible"
#versus "impossible" outliers. 

#For now, primary outliers/changes are DO values >14, and 
#changing one high conductivity value that had a misplaced decimal
# WQ.CL.BMI.raw <- WQ.CL.BMI.raw[WQ.CL.BMI.raw$O2 < 14,]
# WQ.CL.BMI.raw[WQ.CL.BMI.raw$Cond > 859, colnum] <- 86.6 #Note: change colnum to your Cond column.
#Alternatively, just manually change the value of 860.667 to 86.67


# Now, I become bad, because I'm going the distance, 
# and Im going for speed, and Jeff's all alone in his time of need

# Sorry, future me, this is your mess to clean up

#### Overall PCA ####


master.i <- apply(DF.i[,-c(22,27,28,119)],2,as.numeric,header=TRUE)
master.pc <- prcomp(master.i,center = TRUE, scale = TRUE)

 sink("Overall_i_PC.txt")
  summary(master.pc)
 sink()

# Plot

plot(master.pc$x[,1],master.pc$x[,2], 
     xlab="PC1 (13.19%)", ylab = "PC2 (08.35%)", 
     main = "All Vars PCA \nPC1 / PC2")
plot(wq.vec.lag1,col="pink")
plot(wq.vec,col="blue")
plot(cl.vec,col="red")

### No Bugs PCA


wq.i <- apply(DF.i[,-c(22,27,28,73:176)],2,as.numeric,header=TRUE)
wq.pc <- prcomp(wq.i,center = TRUE, scale = TRUE)

 sink("Overall_nobmi_i_PC.txt")
 summary(wq.pc)
 sink()

# Plot

plot(wq.pc$x[,1],wq.pc$x[,2], 
     xlab="PC1 (21.23%)", ylab = "PC2 (14.73%)", 
     main = "All Non-BMI Vars PCA \nPC1 / PC2")


## Vectors All Vars


# lag0 (oiginal)
wq.vec <- envfit(master.pc,master.i[,c(5:14)])
cl.vec <- envfit(master.pc,master.i[,c(15:18)])

#Lag1
cl.lag1.vec.bmi <- envfit(master.pc,bmi.lag[,c(151,154,157,160)])
wq.vec.lag1 <- envfit(master.pc,master.i[,c(40:47)])

#lag two
cl.lag2.vec.bmi <- envfit(bmi.only.lag.pc,bmi.lag[,c(152,155,158,161)])
wq.lag2.vec.bmi <- envfit(bmi.only.lag.pc,bmi.lag[,c(125,128,131,134,137,140,143,146,149)]) 

# Lag Three
cl.lag3.vec.bmi <- envfit(bmi.only.lag.pc,bmi.lag[,c(153,156,159,162)])
wq.lag3.vec.bmi <- envfit(bmi.only.lag.pc,bmi.lag[,c(126,129,132,135,138,141,144,147,150)]) 

# LULC

lc.vec <- envfit(master.pc,master.i[,c(19:21,24:39)])
plot(lc.vec,col="darkgrey")

#info
info.vec.bmi <- envfit(master.pc,master.i[,c(155:172)])

plot(info.vec.bmi,col="#c51b8a")
plot(wq.lag1.vec.bmi, col="#00AFBB")


#### WQ/CL/LC Vectors ####

wq.vec <- envfit(master.pc,master.i[,c(4:13)])
cl.vec <- envfit(master.pc,master.i[,c(14:17)])
lc.vec <- envfit(master.pc,master.i[,c(18:20)])


##### Vectors AllVars noBMI

# lag0 (oiginal)
wq.vec.noBMI <- envfit(wq.pc,wq.i[,c(5:14)])
cl.vec.noBMI <- envfit(wq.pc,wq.i[,c(15:18)])

plot(wq.vec.noBMI)
plot(cl.vec.noBMI,col="red")

# LULC

lc.vec <- envfit(wq.pc,wq.i[,c(19:21,24:39)])
plot(lc.vec,col="darkgrey")

#info
# info.vec.bmi <- envfit(wq.pc,wq.i[,c(155:172)])

#Lag1
cl.lag1.vec.bmi <- envfit(wq.pc,wq.i[,c(151,154,157,160)])
wq.vec.lag1 <- envfit(wq.pc,wq.i[,c(40:47)])

# lag3
cl.lag3.vec <- envfit(wq.pc,wq.i[,c(65:68)])
plot(cl.lag3.vec,col="pink")
