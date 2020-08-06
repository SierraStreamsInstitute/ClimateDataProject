####Climate Change impacts on Deer Creeek BMI communities and Water Quality
###Authors: Jeff Lauder
###Date: 06/03/2020
###Edited: 08/06/2020

####Log####
#Things to add (08/06/2020):
#Hypothesis tests: so far we have only compared SEMs based on model structure (i.e. lag or no lag, different varaibles) but have
#not explicitly tested any hypotheses.
#Consider: H0: background change is random, SEM with only WQ and BMI? Separate SEM for each?
#Ha1: change driven by land use: Land use only + WQ
#Ha2: change driven by climate: climate only + WQ
#Ha3: change in BMI driven by Land use -> WQ: same as Ha1 + BMI
#Ha4: change in BMI driven by climate -> WQ: same as Ha2 + BMI
#Ha5: collective model (land use + climate -> WQ -> BMI)
#Run each, sort by fit, then run predictions and compare to raw WQ (and eventually BMI) data

###########################################################################
####Load packages####
#install.packages('lavaan')
#install.packages('vegan)
#install.packages('semTools')
#install.packages('sem')
#install.packages('semPlot')

library(lavaan)
library(vegan)
library(semTools)
library(semPlot)
library(data.table)

####Load in data####
setwd("~")
AllVars <- read.csv("AllVars_i_full.csv")

####View Data####
View(AllVars)

#Check missingness
nrow(AllVars) #3369
nrow(AllVars[complete.cases(AllVars),]) #196

AllVars2 <- na.omit(AllVars)

###########################################################################
####Build SEMs####
#x ~ y = regressions
#x =~ = Latent variables
#x ~~ = variance covariance

###Variables
#Latent: "Climate", "Land Use","Water Quality", "Benthic Community"


####Model 1: WQ only, no BMI, all vars
#Regressions: WQ ~ Climate + Land Use
#WQ ~ Climate
#WQ ~ Land Use

#Trim dataset to limit dropping of missing values
AllVarsWQ <- AllVars2[,1:22]
AllVars <- AllVars2[AllVars2$WY < 2017,]

#Calculate lags
names(AllVars)
#column numbers of measured vars that can have monthly lags: 4,5,6,7,8,9,10,11,12,13,14,15,16,17
#Annual lags: 20 (NDVI)
AllVarslag <- as.data.frame(matrix(nrow = nrow(AllVars), ncol = 29))
colnames(AllVarslag) <- c("Air1","PO41","NO31","pH1","Cond1","Tur1","O21","H2Otemp1","TotalColiform1","EColi1","ppt1","cwd1","tmx1","tmn1","NDVI1",
                          "Air3","PO43","NO33","pH3","Cond3","Tur3","O23","H2Otemp3","TotalColiform3","EColi3","ppt3","cwd3","tmx3","tmn3")

AllVars <- cbind(AllVars, AllVarslag)

#Create lagged values based on column names/numbers
for(i in 1:nrow(AllVars)){
  for(j in 23:36){
    AllVars[i+1,j] = AllVars[i,j-18]
  }
}

for(i in 1:nrow(AllVars)){
  AllVars[i+1,37] = AllVars[i,21]
}

for(i in 1:nrow(AllVars)){
  for(j in 38:51){
    AllVars[i+2,j] = mean(AllVars[c(i,i+1,i+2),j-33])
  }
}

#Load in BMI
macro <- read.csv("bmicounts.csv")
AllVarsM <- merge(AllVars, macro, by = c("Site","WY","Month"))

#Model 1: Complete model. All direct and indirect paths. No lag effects
model1 <- '
#Latent Variables
Climate =~ cwd + ppt
LU =~ PctImp + NDVI
WQ =~ NO3 + Tur + O2 + H2Otemp + EColi

#Regressions
WQ ~ a * Climate + b * LU

IndLU:=b * a
DirLU:= b
DirCl:= a

#Covariances
NO3 ~~ NO3
Tur ~~ Tur
O2 ~~ O2
H2Otemp ~~ H2Otemp
EColi ~~EColi
H2Otemp ~~ cwd
'

fit1 <- sem(model1, data = AllVars)
summary(fit1, standardized = TRUE)

#Variance inflation, divide by 100:
#cwd, cwd1, cwd3, cond, cond1, cond3
#Divide by 1000:
#ppt, ppt1, ppt3
#Multiply by 10:
#NDVI, NDVI1
#Divide by 10:
#tmx, tmx1, tmx3, tmn, pcturb, pctimp, air, tur, h2otemp, air1, air3, tur1, tur3, h2otemp2, h20temp3

AllVars[,c(9,16,27,34,42,49)] <- AllVars[,c(9,16,27,34,42,49)]/100

AllVars[,c(15,33,48)] <- AllVars[,c(15,33,48)]/1000

AllVars[,c(5,10,12,17,18,19,20,23,28,30,35,36,38,43,45,50,51)] <- AllVars[,c(5,10,12,17,18,19,20,23,28,30,35,36,38,43,45,50,51)]/10




#Model 2: Complete model. All direct and indirect paths. Lag effects by 1 month
model2 <- '
#Latent Variables
Climate =~ cwd1 + ppt1
LU =~ PctImp + NDVI1
WQ =~ NO31 +  Tur1 + O21 + H2Otemp1 + EColi1

#Regressions
WQ ~ a * Climate + b * LU

IndLU:=b * a
DirLU:= b
DirCl:= a

#Covariances
NO31 ~~ NO31
Tur1 ~~ Tur1
O21 ~~ O21
H2Otemp1 ~~ H2Otemp1
EColi1 ~~ EColi1
H2Otemp1 ~~ cwd1
Tur1 ~~ ppt1
O2 ~~ O21
'

fit2 <- sem(model2, data = AllVars)
summary(fit2, standardized = TRUE)

#Model 3: Same as above but seasonal lags
model3 <- '
#Latent Variables
Climate =~ cwd3 + tmx3 + tmn3 + ppt3
LU =~ PctUrb + PctImp + NDVI1
WQ =~ NO33 + Tur3 + O23 + H2Otemp3 + EColi3

#Regressions
WQ ~ a * Climate + b * LU

IndLU:=b * a
DirLU:= b
DirCl:= a

#Covariances
NO33 ~~ NO33
PO43 ~~ PO43
Air3 ~~ Air3
pH3 ~~ pH3
Cond3 ~~ Cond3
Tur3 ~~ Tur3
O23 ~~ O23
H2Otemp3 ~~ H2Otemp3
EColi3 ~~ EColi3
Tur3 ~~ ppt3
'

fit3 <- sem(model3, data = AllVars)
summary(fit3, standardized = TRUE)


#Model 4: Complete model with macros. All direct and indirect paths. No lag
model4 <- '
#Latent Variables
Climate =~ cwd + ppt
LU =~ PctImp + NDVI
WQ =~ NO3 + Tur + O2 + H2Otemp + EColi
Bio =~ ept.p

#Regressions
WQ ~ a * Climate + b * LU
Bio ~ c * WQ

#Paths
IndLU:=c * b
IndCl:=c * a
DirLU:= b
DirCl:= a

#Covariances
NO3 ~~ NO3
Tur ~~ Tur
O2 ~~ O2
H2Otemp ~~ H2Otemp
EColi ~~ EColi
H2Otemp ~~ cwd
Tur ~~ ppt
ept.p ~ ept.p
'

fit4 <- sem(model4, data = AllVarsM)
summary(fit4)

#High variance in:
#cwd, ppt, cwd3, ppt3, cond, cond3
#Divide each by 100? Then re-run mode 3

AllVarsM[,c(16,15,9,42,48,49)] <- AllVarsM[,c(16,15,9,42,48,49)]/100
AllVarsM[,143] <- AllVarsM[,143]/10
AllVarsM[,c(38,147,156)] <- AllVarsM[,c(38,147,156)]/10

AllVarsM[,c(19,20)] <- apply(AllVarsM[,c(19,20)], 1, as.numeric)

#Model 5: Complete model with macros. All direct and indirect paths. No lag
model5 <- '
#Latent Variables
Climate =~ cwd + ppt
LU =~ PctImp + NDVI
WQ =~ NO3 + Tur + O2 + H2Otemp + EColi
Bio =~ ibi.score + tol.p + intol.p

#Regressions
WQ ~ a * Climate + b * LU
Bio ~ c * WQ

#Paths
IndLU:=c * b
IndCl:=c * a
DirLU:= b
DirCl:= a

#Covariances
NO3 ~~ NO3
Tur ~~ Tur
O2 ~~ O2
H2Otemp ~~ H2Otemp
EColi ~~EColi
H2Otemp ~~ cwd
Tur ~~ ppt
tol.p ~~ tol.p
intol.p ~~ intol.p
ibi.score ~~ ibi.score
'

fit5 <- sem(model5, data = AllVarsM)
summary(fit5)

#Model 6: Complete model with macros. All direct and indirect paths. 3 month lag
model6 <- '
#Latent Variables
Climate =~ cwd3 + ppt3
LU =~ PctImp + NDVI
WQ =~ NO33 + Tur3 + O23 + H2Otemp3 + EColi3
Bio =~ ibi.score + tol.p + intol.p

#Regressions
WQ ~ a * Climate + b * LU
Bio ~ c * WQ + d * LU

#Paths
IndLU:=c * b
IndCl:=c * a
DirLU:= d

#Covariances
NO33 ~~ NO33
Tur3 ~~ Tur3
O23 ~~ O23
H2Otemp3 ~~ H2Otemp3
EColi3 ~~EColi3
H2Otemp3 ~~ cwd3
Tur3 ~~ ppt3
tol.p ~~ tol.p
intol.p ~~ intol.p
ibi.score ~~ ibi.score
'

fit6 <- sem(model6, data = AllVarsM)
summary(fit6, standardized = TRUE)

#Divide intolerance by 10
AllVarsM[,145] <- AllVarsM[,145]/10

semPaths(fit1, 'std')

####Path diagram creation####
#Note: this code is incomplete. Ended up just manually creating path diagrams for presentation purposes. Here these can be cleaned up more but needs some work. 
#Clean up path diagrams
#Define labels
lbls<-c("NO3","Turbidity","DO","Water\nTemp","E.Coli","IBI","Tol","Intol","CWD",
        "PPT","% Imp.","Canopy\nCover","Climate","WQ","Bio","LandUse")
#define the groups
grps<-list(WQ = c("NO33","Tur3","O23","H2Otemp3","EColi","WQ"),
           Bio = c("ibi.score","tol.p","intol.p","Bio"),
           Climate=c("cwd3","ppt3","Climate"),
           LandUse=c("PctImp","NDVI", "LU"))

#define the layout
ly<-matrix(c(-0.8,-0.5,-0.55,-0.5,-0.35,-0.5,-0.15,-0.5,0.05,-0.5,0.4,-0.5,
             0.6,-0.5,0.8,-0.5,-0.8,0.8,-0.5,0.8,0.3,0.8,0.6,0.8,-0.6,0.5,
             -0.6,0,0.5,0,0.5,0.5),ncol=2,byrow=TRUE)
#new plot
#new plot
semPaths(fit6, what="std",layout = ly, residuals=FALSE, nCharNodes=0,
         groups=grps, color = c("Blue","Green","Brown","Khaki"),
         nodeLabels=lbls,sizeMan=8, curvature = 2,
         posCol=c("blue","red"),edge.label.cex=1.4,legend=FALSE)



####Predictions####
#Predictions template. Edit 08/07/2020
# fit <- sem(YOUR MODEL AND DATA)
# 
# i <- INDEX OF PREDICTED VARIABLE
# j <- INDICES OF PREDICTOR VARIABLES
# 
# cv <- fitted(fit)$cov
# coef <- solve(cv[j,j],cv[j,i])
# 
# predictions <- YOUR_DATA[,j]%*% coef

####Bayesian network analysis here####
#In progress in collaboration with Dan Wang of California Department of Pesticide Regulation
#Will be much better able to handle non-linear relationships

####Support/Supplemental Figures####
#Line plots of CWD and IBI over time
library(reshape2)
library(plyr)
library(ggplot2)

AllVarsM$Day <- 1
AllVarsM$Date1 <- paste0(AllVarsM$Month,"/",AllVarsM$Day,"/",AllVarsM$WY)
AllVarsM$Date <- as.Date(AllVarsM$Date1, '%m/%d/%Y')

cwd.plot <- ggplot(AllVarsM, aes(x = Date))
cwd.plot + geom_line(aes(y = scale(cwd*100), colour = factor(Site))) +
  geom_point(aes(y = scale(ibi.score), colour = factor(Site))) +
  facet_wrap(~Site)

no3.plot <- ggplot(AllVarsM, aes(x = Date))
no3.plot + geom_line(aes(y = scale(NO3), colour = factor(Site))) +
  geom_point(aes(y = scale(ibi.score), colour = factor(Site))) +
  facet_wrap(~Site)


cwd.plot <- ggplot(AllVarsM, aes(x = cwd, y = ibi.score))
cwd.plot + geom_smooth(method = "loess") +
  geom_point() +
  facet_wrap(~Site) +
  theme_bw() +
  scale_colour_viridis_d()