############# Multilevel Modeling Analyses -- Daily Line Bisection Data ############

library(haven) #Allows spss files to be read
library(lmerTest) #Multilevel modeling package
library(sjPlot) #to create plots


#Changes the number of digits after decimal
options(digits = 4) 

#set working directory
setwd("~/Desktop/Rthings/Rpaper") 

#Load dataset
LineBisectionStudy <- read_sav("Data.MLMinR.JPNpsy.sav")
View(LineBisectionStudy)

#Compute ICC -- run null/unconditional model and use intercept and residual variances to solve for ICC (i.e., Intercept Variance / (Intercept Variance + Residual Variance)) 
Model.Null <- lmer(VerbPhysAggSum ~ 1 + (1 | ID), data = LineBisectionStudy)
summary(Model.Null)

ICC <- (18.69/(18.69+7.76))
print(ICC)

#Cluster means for level-1 predictor
LineBisectionStudy$GRPMnbias <- ave(LineBisectionStudy$bias, LineBisectionStudy$ID)

#Group mean-center (person-center) Level-1 predictors -- subtract cluster mean for bias from each bias score
LineBisectionStudy$GRCbias <- (LineBisectionStudy$bias - LineBisectionStudy$GRPMnbias)

#MLM with Level 1 predictor person-centered with person mean added to Level 2
MLMLevel1model <- lmer(VerbPhysAggSum ~ 1 + GRCbias + GRPMnbias + (1 + GRCbias | ID), data = LineBisectionStudy)
summary(MLMLevel1model)

#Grand mean-center Level-2 predictor
LineBisectionStudy$GMCagreeable <- (LineBisectionStudy$BFIAgree - mean(LineBisectionStudy$BFIAgree))

#Level 2 predictors and cross-level interactions
MLMmodelCross <- lmer(VerbPhysAggSum ~ 1 + GRCbias + GRPMnbias +  GMCagreeable + Male + GRCbias:GMCagreeable + GRCbias:Male + (1 + GRCbias | ID), data = LineBisectionStudy)
summary(MLMmodelCross)

vcov(MLMmodelCross)
sd(LineBisectionStudy$GRCbias)

############################Plot of Interaction##########################################

plot_model(MLMmodelCross, type = "int", mdrt.values = "minmax", title = "", axis.title = c("Visual Field Bias", "Aggressive Tendencies"))


###############################--Reliability Analyses--###################################
#Load Aggression Reliability dataset
AggReliabilityData <- read_sav("ReliabilityData.DailyAgg.sav")
View(AggReliabilityData)

#Reliability analyses for daily aggressive tendencies
RelAgg <- lmer(Resp ~ 1 + (1 | ID) + (1 | LogNumber), data = AggReliabilityData, REML = FALSE)
summary(RelAgg)

###Reliability use formula 6 of Bonito et al., 2012###

