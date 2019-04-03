# --------------------------------------------------
# title: ENVIRON 790 | Time Series Analysis: Final Project Script
# author: Alexander Yoshizumi, Songyun Lee, Asger Hansen 
# date: 26 March 2019
# --------------------------------------------------

# Clear environmental variables
rm(list=ls())

# Set working directory
setwd("~/01_CourseWork/2019_Spring/ENVIRON_790/01_Assignments/FinalProject")

# Load packages
library(forecast)
library(tseries)
library(Kendall)
library(dplyr)
library(sarima)
library(corrplot)
library(PerformanceAnalytics)

# Read in data to table
data=read.table(file="01_Data/02_ProcessedData/CoalConsumption_ModelData.csv",header=TRUE,sep=',',dec='.')

# Remove rows in which some data columns have no observations
data=subset(data,select=-c(CL_LCOE_GLB))
data=data[-c(1,2,3,4,26),]

# Set columns and rows
ncols=ncol(data)
nrows=nrow(data)

# Assess correlation matrix
layout(matrix(c(1),nrow=1,ncol=,byrow=TRUE))
corr_mtrx=cor(data[,2:6],method='pearson')
corrplot(corr_mtrx,type='upper',order='hclust',tl.col='black',tl.srt=45)
chart.Correlation(corr_mtrx, histogram=TRUE, pch=19)

# Model with all independent variables
model = lm(CL_CONS_EPS ~ SO2_ALWCLRSPT + ELCT_GNRT_TTL + HH_NGSPTPRC + CL_NPCPCT, data=data)
summary(model)

# Predicted values model fit
layout(matrix(c(1),nrow=1,ncol=,byrow=TRUE))
model_fit=fitted(model)
plot(data$CL_CONS_EPS,type='l',col='blue')
lines(model_fit,col='orange')

# Model diagnostics
layout(matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=TRUE))
plot(model)

# Model without Electrcity Generation in US
model = lm(CL_CONS_EPS ~ SO2_ALWCLRSPT + HH_NGSPTPRC + CL_NPCPCT, data=data)
summary(model)

# Predicted values model fit
layout(matrix(c(1),nrow=1,ncol=,byrow=TRUE))
model_fit=fitted(model)
plot(data$CL_CONS_EPS,type='l',col='blue')
lines(model_fit,col='orange')

# Model diagnostics
layout(matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=TRUE))
plot(model)

# Model without Operational Coal Capacity in US
model = lm(CL_CONS_EPS ~ SO2_ALWCLRSPT + ELCT_GNRT_TTL + HH_NGSPTPRC, data=data)
summary(model)

# Predicted values model fit
layout(matrix(c(1),nrow=1,ncol=,byrow=TRUE))
model_fit=fitted(model)
plot(data$CL_CONS_EPS,type='l',col='blue')
lines(model_fit,col='orange')

# Model diagnostics
layout(matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=TRUE))
plot(model)

# Model without SO2 Allowance Spot Market Price
model = lm(CL_CONS_EPS ~ ELCT_GNRT_TTL + HH_NGSPTPRC + CL_NPCPCT, data=data)
summary(model)

# Predicted values model fit
layout(matrix(c(1),nrow=1,ncol=,byrow=TRUE))
model_fit=fitted(model)
plot(data$CL_CONS_EPS,type='l',col='blue')
lines(model_fit,col='orange')

# Model diagnostics
layout(matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=TRUE))
plot(model)