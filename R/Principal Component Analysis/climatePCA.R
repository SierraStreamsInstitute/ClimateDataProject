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

DF.i <- read.csv("R/AllVars.csv")[,-1] %>% 
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





################## APPENDIX : Z-Scores Plots #########################

library(Rmisc)

#Calculate z scores
master.z <- as.data.frame(apply(master.i[,c(4:21,24:38)], 2, scale)) %>%
  cbind(master.i[,c(1:3)])

# No 2020, because data incomplete- maybe later.

master.z <- subset(master.z,
                 WY < 2020)

DF.i.m <- ddply(master.z, c("WY"), summarize,
                # Water Quality
                # mPO4 = mean(PO4, na.rm = TRUE), 
                # uPO4 = CI(PO4)[1],              
                # lPO4 = CI(PO4)[3],
                # mNO3 = mean(NO3, na.rm = TRUE),
                # uNO3 = CI(NO3)[1],
                # lNO3 = CI(NO3)[3],
                # mAir = mean(Air, na.rm = TRUE),
                # uAir = CI(Air)[1],
                # lAir = CI(Air)[3],
                mpH = mean(pH, na.rm = TRUE),
                upH = CI(pH)[1],
                lpH = CI(pH)[3],
                mCond = mean(Cond, na.rm = TRUE),
                uCond = CI(Cond)[1],
                lCond = CI(Cond)[3],
                # mTur = mean(Tur, na.rm = TRUE),
                # uTur = CI(Tur)[1],
                # lTur = CI(Tur)[3],
                mO2 = mean(O2, na.rm = TRUE),
                uO2 = CI(O2)[1],
                lO2 = CI(O2)[3],
                mH2Otemp = mean(H2Otemp, na.rm = TRUE),
                uH2Otemp = CI(H2Otemp)[1],
                lH2Otemp = CI(H2Otemp)[3],
                # mTotalColiform = mean(TotalColiform, na.rm = TRUE),
                # uTotalColiform = CI(TotalColiform)[1],
                # lTotalColiform = CI(TotalColiform)[3],
                # mEColi = mean(EColi, na.rm = TRUE),
                # uEColi = CI(EColi)[1],
                # lEColi = CI(EColi)[3],
                
                # Climate
                # mtmn = mean(tmn, na.rm = TRUE),
                # utmn = CI(tmn)[1],
                # ltmn = CI(tmn)[3],
                mppt = mean(ppt, na.rm = TRUE),
                uppt= CI(ppt)[1],
                lppt = CI(ppt)[3],
                mcwd = mean(cwd, na.rm = TRUE),
                ucwd = CI(cwd)[1],
                lcwd = CI(cwd)[3],
                mtmx = mean(tmx, na.rm = TRUE),
                utmx = CI(tmx)[1],
                ltmx = CI(tmx)[3],
                
                # Land Cover
                mNDVI = mean(NDVI, na.rm = TRUE),
                uNDVI = CI(NDVI)[1],
                lNDVI = CI(NDVI)[3],
                # mPctUrb = mean(PctUrb, na.rm = TRUE),
                # uPctUrb = CI(PctUrb)[1],
                # lPctUrb = CI(PctUrb)[3],
                mPctImp = mean(PctImp, na.rm = TRUE),
                uPctImp = CI(PctImp)[1],
                lPctImp = CI(PctImp)[3],
                mFC = mean(ForestChange, na.rm = TRUE),
                uFC = CI(ForestChange)[1],
                lFC = CI(ForestChange)[3],
                
                
)

# Nitrate
pFram <- as.data.frame(master.i)
# Basic Plot
NO3det.effplot <- ggplot(DF.i.m, aes(x = WY, y = mH2Otemp)) #mNO3 = your target resposne variablesof choice
NO3det.effplot 

# Layer on mean value lines, with CI ribbons
NO3eff <- NO3det.effplot + 
  geom_line(aes(y = mH2Otemp), col="red", size = 1.1) + 
  geom_ribbon(aes(ymin = 0, ymax = 0, col="NO3"), alpha = 0.2)+
  
  geom_line(aes(y = (mppt)), col="blue4", size = 1.1) +
  geom_ribbon(aes(ymin = (0), ymax = (0), col="PPT"), alpha = 0.2) +
  
  geom_line(aes(y = mNDVI), col="#DC7633", size = 1.1,) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="NDVI"), fill = "#DC7633", alpha = 0.2) +
  
  geom_line(aes(y = mtmx),col="#C39BD3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="TMX"), fill = "#C39BD3", alpha = 0.2) +
  
  geom_line(aes(y = mFC),col="goldenrod3", size = 1.1) +
  geom_ribbon(aes(ymin = 0, ymax = 0, col="FC"), fill = "goldenrod3", alpha = 0.2) +
  
  geom_line(aes(y = (mcwd)),col="slateblue2", size = 1.1) +
  geom_ribbon(aes(ymin = (0), ymax = (0),col="CWD"), fill = "slateblue2", alpha = 0.2) +
  
  # geom_line(aes(y = -(mPctUrb)),col="black", size = 1.1) +
  # geom_ribbon(aes(ymin = -(0), ymax = -(0), col="PctUrb"), fill = "black", alpha = 0.2) +
  
  
  ylab("Nitrate and Climate Metric z-scores")+
  scale_color_manual(values=c("red","blue4","#DC7633","#C39BD3","goldenrod3",
                              "slateblue2"),
                     labels=c("NO3","PPT","NDVI","TMX","FC","CWD"),
                     breaks=c("NO3","PPT","NDVI","TMX","FC","CWD"),
                     guide = guide_legend(),
                     name="Variable")+
  scale_x_continuous(breaks = round(seq(min(DF.i.m$WY), max(DF.i.m$WY), by = 1),1))+
  
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 16, colour = "black"),
    
    panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "white", size = 0.5),
        panel.grid.minor = element_line(color = "gray70", size = 0.25))

NO3eff

