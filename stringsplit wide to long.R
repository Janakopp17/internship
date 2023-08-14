
#Transforms a wide format dataframe into a long format dataframe
# 19.07.2023 (Jana Kopp)

#load packages, if missing, install

library(RHRV)
library(parsedate)
library(tcltk)
library(haven)
library(foreign)
library(WaveletComp)
library(ggplot2)
library(plyr)
library(plotrix)
library(yarrr)
library(tidyr)
library(dplyr)
library(reshape2)
library(Hmisc)
library(stringr)

#if neccessary uncomment and install
#install.packages("RHRV")
#install.packages("parsedate")
#install.packages("tcltk")
#install.packages("haven")
#install.packages("foreign")
#install.packages("WaveletComp")
#install.packages("tcltk")
#install.packages("ggplot2")
#install.packages("plyr")
#install.packages("plotrix")
#install.packages("yarrr")
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("Hmisc")
#install.packages("stringr")


#read in the wide format data, adjust path for file location on computer
widedata = read.csv2("/home/jana/Documents/datasheet.csv", sep = ",", dec = ".", header = T)

#remove columns which are not needed (the first column in this case)
widedata = widedata[,-1]

#melt everything. Columns specified in id will stay as they are (write the column names that are not supposed to be melted here)
alll = melt(widedata, id = c("PB_ID", "CTQ_ea", "CTQ_en", "CTQ_pa", "CTQ_pn", "CTQ_sa", "CTQ_denial", "CTQ_sum"), value.name = "hr_bl")

#the values in the melted column now contain multiple pieces of information (seconds, pic nr and emotion)
#create new columns for the desired information to be extracted from the melted column. 
alll$seconds = NA
alll$pic_nr = NA
alll$emotion = NA

#in each value the pieces of information (seconds, emotion, pic nr) are divided by underscores. Split each value/string at each underscore
#to extract the pieces of information. Some emotions have an extra underscore in the emotion name (e.g. p_angst),
#therefore the if clause makes sure that the first p is not accidentally dropped for these cases
#Iterate through all rows and write the required piece of information from the melted column (here: column 9) in the desired columns (here columns 11-13)
#the last parameter in str_split_i() determines which part of the split string should be returned. -x indicates the xth element from the right

for(i in 1:length(alll$variable)){
  
b = str_split(alll[i,9], "_")

if(lengths(b)<4){

  alll[i,11] = str_split_i(alll[i,9], "_", -1)
  alll[i,12] = str_split_i(alll[i,9], "_", -2)
  alll[i,13] = str_split_i(alll[i,9], "_", -3)
}
else{
  alll[i,11] = str_split_i(alll[i,9], "_", -1)
  alll[i,12] = str_split_i(alll[i,9], "_", -2)
  alll[i,13] = paste(str_split_i(alll[i,9], "_", -4), str_split_i(alll[i,9], "_", -3))
  }
}
#class(alll$emotion) # to check if the emotion column is in the required format "character"
#recode values which are not named uniformly
alll[alll == "angst."] = "angst"
alll[alll == "p angst"] = "p_angst"
alll[alll == "p neut"] = "p_neutral"
alll[alll == "p trauer"] = "p_trauer"

#Some unneccessary lines are at the bottom of the file, delete them
alll = alll[-(40501:40545),]

#delete the melted column as it is not needed anymore
alll = alll[,-9]

#write the resulting dataframe in a csv file, adjust path for desired file location
write.csv(alll, "/home/jana/Documents/datasheet_new.csv", row.names=FALSE)
