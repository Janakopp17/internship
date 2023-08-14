##Script to plot means with errorbars/ribbons for whole group and for BDI_groups.
#creates new dataframes and ggplots

## 14.07.2023 (Jana Kopp)

#load packages
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

#if packages are not installed yet, uncomment and install
#install.packages("RHRV")
#install.packages("parsedate")
#install.packages("tcltk")
#install.packages("haven")
#install.packages("foreign")
#install.packages("waveletComp")
#install.packages('plyr')
#install.packages('plotrix')
#install.packages('yarrr')
#install.packages('tidyr')
#install.packages('ggplot2')
#install.packages('dplyr')
#install.packages('reshape2')


#Read in the original data (adjust path for the location on the computer)
BL_data_org = read.csv2("/home/jana/Documents/datasheet.csv", sep = ",", dec = ".", header = T)

#cut dataframe to the first 15 seconds of measures (comment out or adjust for different range)
BL_data_org = BL_data_org %>%
  select(sek, hr_bl, bild, Summe_BDI) %>%
  filter(sek < 16) %>%
  as.data.frame()
#set length of dataframe (seconds to be included, adjust for desired range)
datalength = 15

#recode BDI sum into BDI group (median split)
median(BL_data_org$Summe_BDI)
BL_data_org$BDI_group <- NA

BL_data_org$BDI_group[BL_data_org$Summe_BDI < 6] <- 0
BL_data_org$BDI_group[BL_data_org$Summe_BDI > 5] <- 1

#dataframe for BDI group 0 and BDI group 1

BDInull = BL_data_org %>%
  select(sek, hr_bl, bild, BDI_group) %>%
  filter(BDI_group == 0) %>%
  as.data.frame()

BDIeins = BL_data_org %>%
  select(sek, hr_bl, bild, BDI_group) %>%
  filter(BDI_group == 1) %>%
  as.data.frame()


#calculate mean, standard deviation and standard error for each 
#second in each condition and enter in new dataframe for BDI group 0
#create dataframe, create columns, read in first row of values for BDI group 0
ibb0 = BDInull %>%
  select(sek, hr_bl, bild) %>%
  filter(bild == 'positiv' & sek == 1) %>%
  summarise(meanpositiv = mean(hr_bl), sdpositiv = sd(hr_bl), sepositiv = std.error(hr_bl)) %>%
  as.data.frame()
#loop through all other rows and columns to read in all other values
for (j in 2:datalength) {
  
  ibb0[j,1] = BDInull %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'positiv' & sek == j) %>%
    summarise(meanpositiv = mean(hr_bl)) %>%
    as.data.frame()
  ibb0[j,2] = BDInull %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'positiv' & sek == j) %>%
    summarise(sdpositiv = sd(hr_bl)) %>%
    as.data.frame()
  ibb0[j,3] = BDInull %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'positiv' & sek == j) %>%
    summarise(sepositiv = std.error(hr_bl)) %>%
    as.data.frame()
  
}
for (k in 1:datalength) {
  
  ibb0[k,4] = BDInull %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'negativ' & sek == k) %>%
    summarise(meannegativ = mean(hr_bl)) %>%
    as.data.frame()
  ibb0[k,5] = BDInull %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'negativ' & sek == k) %>%
    summarise(sdnegativ = sd(hr_bl)) %>%
    as.data.frame()
  ibb0[k,6] = BDInull %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'negativ' & sek == k) %>%
    summarise(senegativ = std.error(hr_bl)) %>%
    as.data.frame()
}
for (l in 1:datalength) {
  
  ibb0[l,7] = BDInull %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'neutral' & sek == l) %>%
    summarise(meanneutral = mean(hr_bl)) %>%
    as.data.frame()
  ibb0[l,8] = BDInull %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'neutral' & sek == l) %>%
    summarise(sdneutral = sd(hr_bl)) %>%
    as.data.frame()
  ibb0[l,9] = BDInull %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'neutral' & sek == l) %>%
    summarise(seneutral = std.error(hr_bl)) %>%
    as.data.frame()
}

#add seconds column to BDI group 0 dataframe
ibb0$sek = 1:datalength


#do the same for BDI group 1 (create dataframe, extract mean, se, sd for the three groups)

ibb1 = BDIeins %>%
  select(sek, hr_bl, bild) %>%
  filter(bild == 'positiv' & sek == 1) %>%
  summarise(meanpositiv = mean(hr_bl), sdpositiv = sd(hr_bl), sepositiv = std.error(hr_bl)) %>%
  as.data.frame()
#loop through all other rows and columns to read in all other values
for (m in 2:datalength) {
  
  ibb1[m,1] = BDIeins %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'positiv' & sek == m) %>%
    summarise(meanpositiv = mean(hr_bl)) %>%
    as.data.frame()
  ibb1[m,2] = BDIeins %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'positiv' & sek == m) %>%
    summarise(sdpositiv = sd(hr_bl)) %>%
    as.data.frame()
  ibb1[m,3] = BDIeins %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'positiv' & sek == m) %>%
    summarise(sepositiv = std.error(hr_bl)) %>%
    as.data.frame()
  
}
for (n in 1:datalength) {
  
  ibb1[n,4] = BDIeins %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'negativ' & sek == n) %>%
    summarise(meannegativ = mean(hr_bl)) %>%
    as.data.frame()
  ibb1[n,5] = BDIeins %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'negativ' & sek == n) %>%
    summarise(sdnegativ = sd(hr_bl)) %>%
    as.data.frame()
  ibb1[n,6] = BDIeins %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'negativ' & sek == n) %>%
    summarise(senegativ = std.error(hr_bl)) %>%
    as.data.frame()
}
for (o in 1:datalength) {
  
  ibb1[o,7] = BDIeins %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'neutral' & sek == o) %>%
    summarise(meanneutral = mean(hr_bl)) %>%
    as.data.frame()
  ibb1[o,8] = BDIeins %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'neutral' & sek == o) %>%
    summarise(sdneutral = sd(hr_bl)) %>%
    as.data.frame()
  ibb1[o,9] = BDIeins %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'neutral' & sek == o) %>%
    summarise(seneutral = std.error(hr_bl)) %>%
    as.data.frame()
}

#add seconds column to BDI group 1 dataframe
ibb1$sek = 1:datalength



#make wide data long again: make a dataframe each for mean, SD, SE for BDI group 0
#mean dataframe
ibb_long_mean0 <- melt(ibb0,
                       # ID variables - all the variables to keep but not split apart on
                       id.vars=c("sek"),
                       # The source columns
                       measure.vars= c("meanpositiv", "meannegativ", "meanneutral"),
                       # Name of the destination column that will identify the original
                       # column that the measurement came from
                       variable.name="condition",
                       value.name= "hrmean")

#sd dataframe
ibb_long_sd0 <- melt(ibb0,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("sek"),
                     # The source columns
                     measure.vars=c("sdpositiv", "sdnegativ", "sdneutral" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="condition",
                     value.name="hrsd"
)
#se dataframe
ibb_long_se0 <- melt(ibb0,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("sek"),
                     # The source columns
                     measure.vars=c("sepositiv", "senegativ", "seneutral" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="condition",
                     value.name="hrse")

#condition column has differently labeled values in all dataframes,
#make content of condition column compatible to be able to merge dataframes
#column needs to be converted to character type to change the value names (for BDI group 0)

#mean dataframe
ibb_long_mean0$condition = as.character(ibb_long_mean0$condition)
class(ibb_long_mean0$condition)
ibb_long_mean0[ibb_long_mean0 == "meanpositiv"] <- "positiv"
ibb_long_mean0[ibb_long_mean0 == "meannegativ"] <- "negativ"
ibb_long_mean0[ibb_long_mean0 == "meanneutral"] <- "neutral"

#sd dataframe
ibb_long_sd0$condition = as.character(ibb_long_sd0$condition)
class(ibb_long_sd0$condition)
ibb_long_sd0[ibb_long_sd0 == "sdpositiv"] <- "positiv"
ibb_long_sd0[ibb_long_sd0 == "sdnegativ"] <- "negativ"
ibb_long_sd0[ibb_long_sd0 == "sdneutral"] <- "neutral"

#se dataframe
ibb_long_se0$condition = as.character(ibb_long_se0$condition)
class(ibb_long_se0$condition)
ibb_long_se0[ibb_long_se0 == "sepositiv"] <- "positiv"
ibb_long_se0[ibb_long_se0 == "senegativ"] <- "negativ"
ibb_long_se0[ibb_long_se0 == "seneutral"] <- "neutral"

#convert condition columns back to factor to be able to continue
ibb_long_mean0$condition= as.factor(ibb_long_mean0$condition)
class(ibb_long_mean0$condition)

ibb_long_sd0$condition= as.factor(ibb_long_sd0$condition)
class(ibb_long_sd0$condition)

ibb_long_se0$condition= as.factor(ibb_long_se0$condition)
class(ibb_long_se0$condition)

#merge the 3 dataframes to one dataframe in long format for group0
ibb_long0 = merge(ibb_long_mean0, ibb_long_sd0)
ibb_long0 = merge(ibb_long0, ibb_long_se0)


#Do the same for BDI group 1 (make wide data long, recode condition column, merge to one dataframe again)

#make wide data long again: make a dataframe each for mean, SD, SE for BDI group 1
#mean dataframe
ibb_long_mean1 <- melt(ibb1,
                       # ID variables - all the variables to keep but not split apart on
                       id.vars=c("sek"),
                       # The source columns
                       measure.vars= c("meanpositiv", "meannegativ", "meanneutral"),
                       # Name of the destination column that will identify the original
                       # column that the measurement came from
                       variable.name="condition",
                       value.name= "hrmean")

#sd dataframe
ibb_long_sd1 <- melt(ibb1,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("sek"),
                     # The source columns
                     measure.vars=c("sdpositiv", "sdnegativ", "sdneutral" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="condition",
                     value.name="hrsd"
)
#se dataframe
ibb_long_se1 <- melt(ibb1,
                     # ID variables - all the variables to keep but not split apart on
                     id.vars=c("sek"),
                     # The source columns
                     measure.vars=c("sepositiv", "senegativ", "seneutral" ),
                     # Name of the destination column that will identify the original
                     # column that the measurement came from
                     variable.name="condition",
                     value.name="hrse")

#condition column has differently labeled values in all dataframes,
#make content of condition column compatible to be able to merge dataframes
#column needs to be converted to character type to change the value names (for BDI group 1)

#mean dataframe
ibb_long_mean1$condition = as.character(ibb_long_mean1$condition)
class(ibb_long_mean1$condition)
ibb_long_mean1[ibb_long_mean1 == "meanpositiv"] <- "positiv"
ibb_long_mean1[ibb_long_mean1 == "meannegativ"] <- "negativ"
ibb_long_mean1[ibb_long_mean1 == "meanneutral"] <- "neutral"

#sd dataframe
ibb_long_sd1$condition = as.character(ibb_long_sd1$condition)
class(ibb_long_sd1$condition)
ibb_long_sd1[ibb_long_sd1 == "sdpositiv"] <- "positiv"
ibb_long_sd1[ibb_long_sd1 == "sdnegativ"] <- "negativ"
ibb_long_sd1[ibb_long_sd1 == "sdneutral"] <- "neutral"

#se dataframe
ibb_long_se1$condition = as.character(ibb_long_se1$condition)
class(ibb_long_se1$condition)
ibb_long_se1[ibb_long_se1 == "sepositiv"] <- "positiv"
ibb_long_se1[ibb_long_se1 == "senegativ"] <- "negativ"
ibb_long_se1[ibb_long_se1 == "seneutral"] <- "neutral"

#convert condition columns back to factor to be able to continue
ibb_long_mean1$condition= as.factor(ibb_long_mean1$condition)
class(ibb_long_mean1$condition)

ibb_long_sd1$condition= as.factor(ibb_long_sd1$condition)
class(ibb_long_sd1$condition)

ibb_long_se1$condition= as.factor(ibb_long_se1$condition)
class(ibb_long_se1$condition)

#merge the 3 dataframes to one dataframe in long format group 1
ibb_long1 = merge(ibb_long_mean1, ibb_long_sd1)
ibb_long1 = merge(ibb_long1, ibb_long_se1)


#create BDI group column in both dataframes

ibb_long0$BDI_group = 0
ibb_long1$BDI_group = 1


#merge both BDI group dataframes to the final dataframe for plotting

ibb_long_BDI = merge(ibb_long0, ibb_long1, all = TRUE)



#create dataframe for plotting the whole group (not split by BDI groups)

#calculate mean, standard deviation and standard error for each 
#second in each condition (positive, negative, neutral) and enter in new dataframe
#create dataframe, create columns, read in first row of values
ibbwhole = BL_data_org %>%
  select(sek, hr_bl, bild) %>%
  filter(bild == 'positiv' & sek == 1) %>%
  summarise(meanpositiv = mean(hr_bl), sdpositiv = sd(hr_bl), sepositiv = std.error(hr_bl)) %>%
  as.data.frame()
#loop through all other rows and columns to read in all other values
for (p in 2:datalength) {
  
  ibbwhole[p,1] = BL_data_org %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'positiv' & sek == p) %>%
    summarise(meanpositiv = mean(hr_bl)) %>%
    as.data.frame()
  ibbwhole[p,2] = BL_data_org %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'positiv' & sek == p) %>%
    summarise(sdpositiv = sd(hr_bl)) %>%
    as.data.frame()
  ibbwhole[p,3] = BL_data_org %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'positiv' & sek == p) %>%
    summarise(sepositiv = std.error(hr_bl)) %>%
    as.data.frame()
  
}
for (q in 1:datalength) {
  
  ibbwhole[q,4] = BL_data_org %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'negativ' & sek == q) %>%
    summarise(meannegativ = mean(hr_bl)) %>%
    as.data.frame()
  ibbwhole[q,5] = BL_data_org %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'negativ' & sek == q) %>%
    summarise(sdnegativ = sd(hr_bl)) %>%
    as.data.frame()
  ibbwhole[q,6] = BL_data_org %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'negativ' & sek == q) %>%
    summarise(senegativ = std.error(hr_bl)) %>%
    as.data.frame()
}
for (r in 1:datalength) {
  
  ibbwhole[r,7] = BL_data_org %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'neutral' & sek == r) %>%
    summarise(meanneutral = mean(hr_bl)) %>%
    as.data.frame()
  ibbwhole[r,8] = BL_data_org %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'neutral' & sek == r) %>%
    summarise(sdneutral = sd(hr_bl)) %>%
    as.data.frame()
  ibbwhole[r,9] = BL_data_org %>%
    select(sek, hr_bl, bild) %>%
    filter(bild == 'neutral' & sek == r) %>%
    summarise(seneutral = std.error(hr_bl)) %>%
    as.data.frame()
}

#add seconds column to dataframe
ibbwhole$sek = 1:datalength

#make wide data long again: make a dataframe each for mean, SD, SE
#mean dataframe
ibb_long_whole_mean <- melt(ibbwhole,
                      # ID variables - all the variables to keep but not split apart on
                      id.vars=c("sek"),
                      # The source columns
                      measure.vars= c("meanpositiv", "meannegativ", "meanneutral"),
                      # Name of the destination column that will identify the original
                      # column that the measurement came from
                      variable.name="condition",
                      value.name= "hrmean")

#sd dataframe
ibb_long_whole_sd <- melt(ibbwhole,
                    # ID variables - all the variables to keep but not split apart on
                    id.vars=c("sek"),
                    # The source columns
                    measure.vars=c("sdpositiv", "sdnegativ", "sdneutral" ),
                    # Name of the destination column that will identify the original
                    # column that the measurement came from
                    variable.name="condition",
                    value.name="hrsd"
)
#se dataframe
ibb_long_whole_se <- melt(ibbwhole,
                    # ID variables - all the variables to keep but not split apart on
                    id.vars=c("sek"),
                    # The source columns
                    measure.vars=c("sepositiv", "senegativ", "seneutral" ),
                    # Name of the destination column that will identify the original
                    # column that the measurement came from
                    variable.name="condition",
                    value.name="hrse")

#condition column has differently labeled values in all dataframes,
#make content of condition column compatible to be able to merge dataframes
#column needs to be converted to character type to change the value names

#mean dataframe
ibb_long_whole_mean$condition = as.character(ibb_long_whole_mean$condition)
class(ibb_long_whole_mean$condition)
ibb_long_whole_mean[ibb_long_whole_mean == "meanpositiv"] <- "positiv"
ibb_long_whole_mean[ibb_long_whole_mean == "meannegativ"] <- "negativ"
ibb_long_whole_mean[ibb_long_whole_mean == "meanneutral"] <- "neutral"

#sd dataframe
ibb_long_whole_sd$condition = as.character(ibb_long_whole_sd$condition)
class(ibb_long_whole_sd$condition)
ibb_long_whole_sd[ibb_long_whole_sd == "sdpositiv"] <- "positiv"
ibb_long_whole_sd[ibb_long_whole_sd == "sdnegativ"] <- "negativ"
ibb_long_whole_sd[ibb_long_whole_sd == "sdneutral"] <- "neutral"

#se dataframe
ibb_long_whole_se$condition = as.character(ibb_long_whole_se$condition)
class(ibb_long_whole_se$condition)
ibb_long_whole_se[ibb_long_whole_se == "sepositiv"] <- "positiv"
ibb_long_whole_se[ibb_long_whole_se == "senegativ"] <- "negativ"
ibb_long_whole_se[ibb_long_whole_se == "seneutral"] <- "neutral"

#convert condition columns back to factor to be able to continue
ibb_long_whole_mean$condition= as.factor(ibb_long_whole_mean$condition)
class(ibb_long_whole_mean$condition)

ibb_long_whole_sd$condition= as.factor(ibb_long_whole_sd$condition)
class(ibb_long_whole_sd$condition)

ibb_long_whole_se$condition= as.factor(ibb_long_whole_se$condition)
class(ibb_long_whole_se$condition)

#merge the 3 dataframes to one dataframe in long format
ibb_long_whole = merge(ibb_long_whole_mean, ibb_long_whole_sd)
ibb_long_whole = merge(ibb_long_whole, ibb_long_whole_se)



#PLOTS:
#plots with facet wrap for BDI groups

#ggplot lineplot
ggplot(ibb_long_BDI, aes(x = sek, y = hrmean, colour = condition, group = condition)) +
  geom_line() + facet_wrap(~BDI_group, labeller = labeller(BDI_group = c("0" = "BDI <= 5","1" = "BDI > 5")))

#ggplot with errorbars denoting SD
ggplot(ibb_long_BDI, aes(x = sek, y = hrmean, colour = condition, group = condition)) +
  geom_line() + geom_errorbar(aes(ymin=hrmean-hrsd, ymax=hrmean+hrsd), width=.2,
                              position=position_dodge(.3)) + facet_wrap(~BDI_group, labeller = labeller(BDI_group = c("0" = "BDI <= 5","1" = "BDI > 5")))

#ggplot with errorbars denoting SE
ggplot(ibb_long_BDI, aes(x = sek, y = hrmean, colour = condition, group = condition)) +
  geom_line() + geom_errorbar(aes(ymin=hrmean-hrse, ymax=hrmean+hrse), width=.4,
                              position=position_dodge(.3)) + facet_wrap(~BDI_group, labeller = labeller(BDI_group = c("0" = "BDI <= 5","1" = "BDI > 5")))

#ggplot with shaded SE

ggplot(data = ibb_long_BDI, aes(x = sek, group = condition)) + 
  geom_line(aes(y = hrmean, color = condition), linewidth = 0.7) + 
  geom_ribbon(aes(y = hrmean, ymin = hrmean - hrse, ymax = hrmean + hrse, fill = condition), alpha = .2) +
  xlab("sek") + 
  theme_bw() +  
  theme(legend.key = element_blank()) +
  theme(legend.title = element_blank()) +
  facet_wrap(~BDI_group, labeller = labeller(BDI_group = c("0" = "BDI <= 5","1" = "BDI > 5")))


#Plots for whole group (no BDI grouping)

#ggplot lineplot
ggplot(ibb_long_whole, aes(x = sek, y = hrmean, colour = condition, group = condition)) +
  geom_line()

#ggplot with errorbars denoting SD
ggplot(ibb_long_whole, aes(x = sek, y = hrmean, colour = condition, group = condition)) +
  geom_line() + geom_errorbar(aes(ymin=hrmean-hrsd, ymax=hrmean+hrsd), width=.2,
                              position=position_dodge(.3))

#ggplot with errorbars denoting SE
ggplot(ibb_long_whole, aes(x = sek, y = hrmean, colour = condition, group = condition)) +
  geom_line() + geom_errorbar(aes(ymin=hrmean-hrse, ymax=hrmean+hrse), width=.4,
                              position=position_dodge(.3))

#ggplot with shaded SE

ggplot(data = ibb_long_whole, aes(x = sek, group = condition)) + 
  geom_line(aes(y = hrmean, color = condition), linewidth = 0.7) + 
  geom_ribbon(aes(y = hrmean, ymin = hrmean - hrse, ymax = hrmean + hrse, fill = condition), alpha = .2) +
  xlab("sek") + 
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(legend.title = element_blank())

