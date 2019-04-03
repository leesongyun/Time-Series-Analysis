# --------------------------------------------------
# title: ENVIRON 790 | Time Series Analysis: Final Project Script
# author: Alexander Yoshizumi, Songyun Lee, Asger Hansen 
# date: 26 March 2019
# --------------------------------------------------

# Clear environmental variables
rm(list=ls())

# Set working directory
setwd("~/R/Time-Series-Analysis")

# Load packages
#library(forecast)
#library(tseries)
#library(Kendall)
#library(dplyr)
#library(sarima)
#library(tidyverse)
library(corrplot)
library(PerformanceAnalytics)

# Read in data to table
data=read.table(file="CoalConsumption_ModelData.csv",header=TRUE,sep=',',dec='.')

# Remove rows in which some data columns have no observations
data=subset(data,select=-c(YEAR))
data<-data[-c(1)]
data=data[-c(1,2,3,4,26),]

# Set columns and rows
ncols=ncol(data)
nrows=nrow(data)

# Assess correlation matrix
layout(matrix(c(1),nrow=1,ncol=,byrow=TRUE))
corr_mtrx=cor(data,method='pearson')
corrplot(corr_mtrx,type='upper',order='hclust',tl.col='black',tl.srt=45)
chart.Correlation(corr_mtrx, histogram=TRUE, pch=19)

# Model with productivity, consumption, exports, and GDP per capita growth
model <- lm(CL_EMPL_APL ~ CL_PRDT_APL + CL_PRDT_INT + CL_PRDT_WST + CL_CONS_TTL + CL_EXPT_TTL + EW_GDPC_TTL, data=data)
summary(model)

# Predicted values model fit
layout(matrix(c(1),nrow=1,ncol=,byrow=TRUE))
model_fit=fitted(model)
plot(data$CL_EMPL_APL,type='l',col='blue',x=data$Year)
lines(model_fit,col='orange',x=data$Year)

# Model diagnostics
layout(matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=TRUE))
plot(model)

# Model with productivity, consumption, and GDP per capita growth
model <- lm(CL_EMPL_APL ~ CL_PRDT_APL + CL_PRDT_INT + CL_PRDT_WST + CL_CONS_TTL + EW_GDPC_TTL, data=data)
summary(model)

# Predicted values model fit
layout(matrix(c(1),nrow=1,ncol=,byrow=TRUE))
model_fit=fitted(model)
plot(data$CL_EMPL_APL,type='l',col='blue',x=data$Year)
lines(model_fit,col='orange',x=data$Year)

# Model diagnostics
layout(matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=TRUE))
plot(model)