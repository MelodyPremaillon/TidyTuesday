## EMPLOYED STATUS

#The data this week comes from the BLS (https://www.bls.gov/cps/tables.htm#charemp_m)
#This BLS article (https://www.bls.gov/careeroutlook/2018/article/blacks-in-the-labor-force.htm) talks about Employed Status in greater detail.

#Credit: [Mélody Prémaillon](Twitter handle or other social media profile)



#Packages
library(tidytuesdayR)
library(ggplot2)
library(tidyverse)


##Loading datas
tuesdata <- tidytuesdayR::tt_load(2021, week = 9)
employed <- tuesdata$employed
earn <- tuesdata$earn
