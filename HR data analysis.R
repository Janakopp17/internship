
#Hr data analysis
#14.07.23 (Jana Kopp)

#loading packages
library(tcltk) # this package is required when working in linux
library(DescTools)
library(ez)
library(gdata)
library(reshape2)
library(lme4)
library(stats)
library(rstatix)
library(ggplot2)
library(psych)
library(ggpubr)
library(tidyr)
library(dplyr)
library(jtools)
library(nlme)

#if neccessary uncomment and install packages first
#install.packages("DescTools")
#install.packages("ez")
#install.packages("gdata")
#install.packages("rstatix")
#install.packages("psych")
#install.packages("ggpubr")
#install.packages("jtools")
#install.packages("nlme")
#install.packages("....")

#read the data in a dataframe (adjust path for file location on computer)
HR_BL_data = read.csv2("/home/jana/Documents/datasheet.csv", sep = ",", dec = ".", header = T)

# remove missing data 
HR_BL_data_fin <- HR_BL_data[complete.cases(HR_BL_data),]
# -> no missing data, not really neccessary

#choose amount of seconds to be analyzed
HR_BL_first15 = HR_BL_data_fin %>%
  filter(sek < 16) %>%
  as.data.frame()

# aov function from stats package for all 27 seconds
results.aov <- aov(hr_bl ~ bild*sek + Error(VP.Nummer/sek), data=HR_BL_data_fin)
summary(results.aov)

# aov function from stats package 15 sec
results.aov <- aov(hr_bl ~ bild*sek + Error(VP.Nummer/sek), data=HR_BL_first15)
summary(results.aov)


#psych describe function to see some values of the variables
describeBy(HR_BL_first15, group = list(HR_BL_first15$VP.Nummer , HR_BL_first15$bild), mat = FALSE, type = 3, digits=3)

describeBy(HR_BL_first15, group = HR_BL_first15$bild, mat = FALSE, type = 3, digits=3)

#recode SUMME BDI (15 sec dataframe) to divide the participants in two groups, high and low BDI (median split)
median(HR_BL_first15$Summe_BDI)
HR_BL_first15$BDI_group <- NA

HR_BL_first15$BDI_group[HR_BL_first15$Summe_BDI < 6] <- 0
HR_BL_first15$BDI_group[HR_BL_first15$Summe_BDI > 5] <- 1

#Recode Summe BDI (27 sec dataframe) to divide the participants in two groups, high and low BDI (median split)
median(HR_BL_data_fin$Summe_BDI)
HR_BL_data_fin$BDI_group <- NA

HR_BL_data_fin$BDI_group[HR_BL_data_fin$Summe_BDI < 6] <- 0
HR_BL_data_fin$BDI_group[HR_BL_data_fin$Summe_BDI > 5] <- 1


#anova with BDI (15 sec)
result.aov <- aov(hr_bl ~ bild*sek*BDI_group + Error(VP.Nummer/sek), data=HR_BL_first15)
summary(result.aov)

#anova with BDI (27 sec)
result.aov <- aov(hr_bl ~ bild*sek*BDI_group + Error(VP.Nummer/sek), data=HR_BL_data_fin)
summary(result.aov)

#linear mixed effects models (works for unbalanced data)
#lme bild, sekunde (15 sec)
tutorial<-lme(hr_bl ~ bild * sek, random = ~ 1 | VP.Nummer, data=HR_BL_first15)
summary(tutorial)

#lme bild sekunde (27 sec)
tutorial<-lme(hr_bl ~ bild * sek, random = ~ 1 | VP.Nummer, data=HR_BL_data_fin)
summary(tutorial)

#lme bild, Sekunde, BDI (15 sec)
tutorial<-lme(hr_bl ~ bild * sek *BDI_group, random = ~ 1|VP.Nummer, data=HR_BL_first15)
summary(tutorial)

#lme Bild Sekunde BDI (27 sec)
tutorial<-lme(hr_bl ~ bild * sek *BDI_group, random = ~ 1|VP.Nummer, data=HR_BL_data_fin)
summary(tutorial)

#more anovas from other packages (does not work for unbalanced data!)
#anova from rstatix
#anova_result <- anova_test(data = HR_BL_first15, dv = hr_bl, wid = VP.Nummer, between = c(bild, BDI_group), within = sek, type = 'II')
#summary(anova_result)
#get_anova_table(anova_result)
#anova from ez
#anova_res = ezANOVA(data = HR_BL_first15, dv = hr_bl, wid = VP.Nummer, within = sek, between = .(bild, BDI_group))


#more mixed models
#Basic Model predicted by intercept only
Basic <- gls(hr_bl ~ 1, data=HR_BL_first15, method="ML"); summary(Basic)

#Model predicted by varying intercepts across Person
randomInterc <- lme(hr_bl ~ 1, data=HR_BL_first15, random= ~1|VP.Nummer, method = "ML", na.action=na.exclude); summary(randomInterc)
anova(Basic, randomInterc)

#Adding fixed slopes & covariable
#nur Sekunde:
Fit_fixed1 <- lme(hr_bl ~ sek, data=HR_BL_first15, random= ~1|VP.Nummer, method = "ML", na.action=na.exclude); summary(Fit_fixed1)


Fit_fixed2 <- lme(hr_bl ~ sek + bild, data=HR_BL_first15, random= ~1|VP.Nummer, method = "ML", na.action=na.exclude); summary(Fit_fixed2)
anova(randomInterc, Fit_fixed1, Fit_fixed2)

#Adding random slopes
Fit_random <- lme(hr_bl ~ sek + bild, data=HR_BL_first15, random= ~sek|VP.Nummer, method = "ML", na.action=na.exclude); summary(Fit_random)
anova(Fit_fixed2, Fit_random)
#interpretation: relationship between x and y significantly varies across different contexts.
#check correlation between intercepts and slopes (random effects Corr)

#Adding interaction
Fit_main <- lme(hr_bl ~ sek + BDI_group + bild, data=HR_BL_first15, random= ~sek|VP.Nummer, method = "ML", na.action=na.exclude); summary(Fit_main)
Fit_interaction <- lme(hr_bl ~ nr * sek, data=HR_BL_first15, random= ~sek|VP.Nummer, method = "ML", na.action=na.exclude); summary(Fit_interaction)
anova(tutorial, Fit_interaction)

#Bild und Sekunde mit nur VP Nummer im random
tutorial<-lme(hr_bl ~ bild * sek, random = ~ 1 | VP.Nummer, data=HR_BL_first15, method = "ML")
summary(tutorial)

#bild und Sekunde mit Sekunde und VP Nummer in random
tutorial2<-lme(hr_bl ~ bild * sek, random = ~ sek | VP.Nummer, data=HR_BL_first15, method = "ML")
summary(tutorial2)
