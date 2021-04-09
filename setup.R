#--------------------------------------
# This script sets out to load all the
# things necessary to run the project
#--------------------------------------

#--------------------------------------
# Author: Trent Henderson, 9 April 2021
#--------------------------------------

library(data.table)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(scales)
library(catch22) # devtools::install_github("hendersontrent/catch22")
library(theft) # devtools::install_github("hendersontrent/theft")
library(Cairo)
library(caret)

# Create important folders if none exist:

if(!dir.exists('webscraping')) dir.create('webscraping')
if(!dir.exists('analysis')) dir.create('analysis')
if(!dir.exists('output')) dir.create('output')
if(!dir.exists('data')) dir.create('data')
