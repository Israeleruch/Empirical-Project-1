# Setting the random seed
set.seed(1234)

# Importing Packages
library(foreign)
library(data.table)
library(ggplot2)
library(MASS)
library(Hotelling)
library(randomForest)
library(Hmisc)

# Reading in the data files and converting into data.table objects
setwd("C:/Users/eruch/Dropbox/Academic/Brown University/Second Year/Autumn Semester/Applied Econometrics/Empirical Project 1/Code")

nsw <- read.dta("../Data/nsw.dta")
control_obs <- read.dta("../Data/cps_controls.dta")

nsw <- as.data.table(nsw)
control_obs <- as.data.table(control_obs)

## Sourcing all question files in order
source("Question_1.r")
source("Question_2.r")
source("Question_3.r")
source("Question_4.r")
source("Question_5.r")
source("Question_6.r")
source("Question_7.r")
source("Question_8.r")
source("Question_9.r")
source("Question_10.r")
source("Question_11.r")
source("Question_12.r")
source("Question_13.r")