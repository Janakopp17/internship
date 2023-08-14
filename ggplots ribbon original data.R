### Script for plot with error bars on original data

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

#if missing uncomment and install
#install.packages("RHRV")
#install.packages("parsedate")
#install.packages("tcltk")
#install.packages("haven")
#install.packages("foreign")
# install.packages("WaveletComp")
#install.packages("ggplot2")
#install.packages('plyr')
#install.packages('plotrix')
#install.packages('yarrr')
#install.packages("dplyr")
#install.packages('tidyr')
#install.packages("Hmisc")
#install.packages("reshape2")


#Read in data, adjust path for file location on computer
BL_data_org = read.csv2("/home/jana/Documents/datasheet.csv", sep = ",", dec = ".", header = T)

#Filter the dataframe to only include the first 15 seconds
BL_data_org = BL_data_org %>%
  filter(sek < 16) %>%
  as.data.frame()

#find median of BDI, create new column and recode the BDI as to groups with median split
median(BL_data_org$Summe_BDI)
BL_data_org$BDI_group <- NA
BL_data_org$BDI_group[BL_data_org$Summe_BDI < 6] <- 0
BL_data_org$BDI_group[BL_data_org$Summe_BDI > 5] <- 1

#ggplot with confidence interval as ribbon
  ggplot(BL_data_org, aes(x = sek, y = hr_bl, fill = bild, colour = bild)) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun.data = mean_cl_boot, geom = "ribbon", alpha = 0.2, linetype = 0) + labs(x = "Sekunde", y = "hr", colour = "bild")  + theme_bw()

#ggplot with facet wrap with confidence interval as ribbon
  ggplot(BL_data_org, aes(x = sek, y = hr_bl, fill = bild, colour = bild)) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun.data = mean_cl_boot, geom = "ribbon", alpha = 0.2, linetype = 0) + labs(x = "Sekunde", y = "hr", colour = "bild")  + theme_bw() + facet_wrap(~BDI_group, labeller = labeller(BDI_group = c("0" = "BDI <= 5","1" = "BDI > 5")))

#ggplot with standard error as ribbon
  ggplot(BL_data_org, aes(x = sek, y = hr_bl, fill = bild, colour = bild)) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, linetype = 0) + labs(x = "Sekunde", y = "hr", colour = "bild")  + theme_bw()
  
#ggplot with facet wrap with standard error as ribbon
  ggplot(BL_data_org, aes(x = sek, y = hr_bl, fill = bild, colour = bild)) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, linetype = 0) + labs(x = "Sekunde", y = "hr", colour = "bild")  + theme_bw() + facet_wrap(~BDI_group, labeller = labeller(BDI_group = c("0" = "BDI <= 5","1" = "BDI > 5")))

#ggplot with errorbars
#ggplot(BL_data_org, aes(x = sek, y = hr_bl, colour = bild)) +
#  geom_point() +
#  stat_summary(fun = mean, geom = "line") +
#  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.0001)
