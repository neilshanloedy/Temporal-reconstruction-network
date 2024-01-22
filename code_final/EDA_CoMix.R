rm(list = ls())
library(rstudioapi)

dir <- dirname(getActiveDocumentContext()$path)
setwd(dirname(getActiveDocumentContext()$path))
source("helper/functions.R")
prop.age.partdata <- read.csv("../data/BE_2008/prop_agedata.csv")[,-1]

# CoMix physical
#-----------------
data_merge_comix <- read.csv("../data/CoMix_Fatigue/redistribute_physical.csv")

for(wave in seq(9, 43)){
  print(sprintf("doing %s", wave))
  source('helper/simulate_weekly_temporal.R')
  dataset <- data_merge_comix[data_merge_comix$wave %in% wave, ]
  dataset$frequency_multi <- factor(dataset$frequency_multi,
                                    levels=c('daily','weekly','monthly',
                                             'a few times a year','first time'))
  dataset$dayofweek <- factor(dataset$dayofweek,
                              levels=c('Mon','Tue','Wed','Thu',
                                       'Fri','Sat','Sun'))
  number.contact.input <- input_binomial(dataset)
  prop.week <- proportion.week.prep(dataset)
  prop.week[sapply(prop.week, is.character)] <- lapply(prop.week[sapply(prop.week, is.character)], as.factor)
  
  if(!file.exists(sprintf("rds/CoMix/data_temp_CoMix_physical_5000pop_%04d.rds", wave))){
    data_temp <- simulate_weekly_contact_gp(n_sample = 5000,
                                                         number.contacts = number.contact.input,
                                                         proportion.week = prop.week,
                                                         proportion.age = prop.age.partdata)
    saveRDS(data_temp, sprintf("rds/CoMix/data_temp_CoMix_physical_5000pop_%04d.rds", wave))
  } 
}

# CoMix all contacts
#-----------------
data_merge_comix_allcontacts <- read.csv("../data/CoMix_Fatigue/redistribute_allcontacts.csv")

for(wave in seq(9, 43)){
  print(sprintf("doing %s", wave))
  source('helper/simulate_weekly_temporal.R')
  dataset <- data_merge_comix_allcontacts[data_merge_comix_allcontacts$wave %in% wave, ]
  dataset$frequency_multi <- factor(dataset$frequency_multi,
                                    levels=c('daily','weekly','monthly',
                                             'a few times a year','first time'))
  dataset$dayofweek <- factor(dataset$dayofweek,
                              levels=c('Mon','Tue','Wed','Thu',
                                       'Fri','Sat','Sun'))
  number.contact.input <- input_binomial(dataset)
  prop.week <- proportion.week.prep(dataset)
  prop.week[sapply(prop.week, is.character)] <- lapply(prop.week[sapply(prop.week, is.character)], as.factor)
  
  if(!file.exists(sprintf("rds/CoMix/data_temp_CoMix_allcontacts_5000pop_%04d.rds", wave))){
    data_temp <- simulate_weekly_contact_gp(n_sample = 5000,
                                            number.contacts = number.contact.input,
                                            proportion.week = prop.week,
                                            proportion.age = prop.age.partdata)
    saveRDS(data_temp, sprintf("rds/CoMix/data_temp_CoMix_allcontacts_5000pop_%04d.rds", wave))
  } 
}

# CoMix outhome
#-----------------
data_merge_comix_outhome <- read.csv("../data/CoMix_Fatigue/redistribute_outhome.csv")

for(wave in seq(9, 43)){
  print(sprintf("doing %s", wave))
  source('helper/simulate_weekly_temporal.R')
  dataset <- data_merge_comix_outhome[data_merge_comix_outhome$wave %in% wave, ]
  dataset$frequency_multi <- factor(dataset$frequency_multi,
                                    levels=c('daily','weekly','monthly',
                                             'a few times a year','first time'))
  dataset$dayofweek <- factor(dataset$dayofweek,
                              levels=c('Mon','Tue','Wed','Thu',
                                       'Fri','Sat','Sun'))
  number.contact.input <- input_binomial(dataset)
  prop.week <- proportion.week.prep(dataset)
  prop.week[sapply(prop.week, is.character)] <- lapply(prop.week[sapply(prop.week, is.character)], as.factor)
  
  if(!file.exists(sprintf("rds/CoMix/data_temp_CoMix_outhome_5000pop_%04d.rds", wave))){
    data_temp <- simulate_weekly_contact_gp(n_sample = 5000,
                                            number.contacts = number.contact.input,
                                            proportion.week = prop.week,
                                            proportion.age = prop.age.partdata)
    saveRDS(data_temp, sprintf("rds/CoMix/data_temp_CoMix_outhome_5000pop_%04d.rds", wave))
  } 
}

# CoMix inshome
#-----------------
data_merge_comix_inshome <- read.csv("../data/CoMix_Fatigue/redistribute_inshome.csv")

for(wave in seq(9, 43)){
  print(sprintf("doing %s", wave))
  source('helper/simulate_weekly_temporal.R')
  dataset <- data_merge_comix_inshome[data_merge_comix_inshome$wave %in% wave, ]
  dataset$frequency_multi <- factor(dataset$frequency_multi,
                                    levels=c('daily','weekly','monthly',
                                             'a few times a year','first time'))
  dataset$dayofweek <- factor(dataset$dayofweek,
                              levels=c('Mon','Tue','Wed','Thu',
                                       'Fri','Sat','Sun'))
  number.contact.input <- input_binomial(dataset)
  prop.week <- proportion.week.prep(dataset)
  prop.week[sapply(prop.week, is.character)] <- lapply(prop.week[sapply(prop.week, is.character)], as.factor)
  
  if(!file.exists(sprintf("rds/CoMix/data_temp_CoMix_inshome_5000pop_%04d.rds", wave))){
    data_temp <- simulate_weekly_contact_gp(n_sample = 5000,
                                            number.contacts = number.contact.input,
                                            proportion.week = prop.week,
                                            proportion.age = prop.age.partdata)
    saveRDS(data_temp, sprintf("rds/CoMix/data_temp_CoMix_inshome_5000pop_%04d.rds", wave))
  } 
}
