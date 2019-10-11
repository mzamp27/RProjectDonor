# load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggthemes)




# import data
Leadership_Donor_Analysis <- read.delim("~/R/SampleDonor/Leadership_Donor_Analysis.txt", row.names=NULL)

#create dataframe with just prior 5 years $ value
donor <- select(Leadership_Donor_Analysis, FY19 = Total.Giving.FY19, FY18 =Total.Giving.FY18,FY17 =Total.Giving.FY17,FY16 =Total.Giving.FY16, FY15 =Total.Giving.FY15)

#clean currency into numeric
donor$FY19 <- as.numeric(gsub('[$,]','', donor$FY19))
donor$FY18 <- as.numeric(gsub('[$,]','', donor$FY18))
donor$FY17 <- as.numeric(gsub('[$,]','', donor$FY17))
donor$FY16 <- as.numeric(gsub('[$,]','', donor$FY16))
donor$FY15 <- as.numeric(gsub('[$,]','', donor$FY15))



#which() will return index numbers for identification of specific donors who donated more money this year than last
increaseddonor2yr <- which(donor$FY18 < donor$FY19)
length(Leadership_Donor_Analysis$Contact.Preferred.Name[increaseddonor2yr])



#can also check donors who gave less
shrunkendonor2yr <- which(donor$FY18 > donor$FY19)
length(Leadership_Donor_Analysis$Contact.Preferred.Name[shrunkendonor2yr])


#filter() will create a new dataframe for analysis of numeric quantities
consistentdonor5yr <- filter(donor, FY19 != 0 & FY18 !=0 & FY17 != 0 & FY16 !=0 & FY15 != 0)
consistentdonor3yr <- filter(donor, FY19 != 0 & FY18 !=0 & FY17 != 0)

#largedonor analysis
largedonor19 <- which(donor$FY19 >= 100000)
largedonor18 <- which(donor$FY18 >= 100000)
largedonor17 <- which(donor$FY17 >= 100000)

#check which large donors made a donation in 18 but not in 19
missinglargedonor <- largedonor18[!(largedonor18 %in% largedonor19)]


