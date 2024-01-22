rm(list = ls())
library(rstudioapi)

dir <- dirname(getActiveDocumentContext()$path)
setwd(dirname(getActiveDocumentContext()$path))
source("functions.R")
part_common <- read.csv("../../data/BE_2008/2008_Mossong_POLYMOD_participant_common.csv")

country <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")

for(i in country){
  prop.age.partdata <- read.csv("../../data/population_data_proportion.csv")[,-1] %>% filter(geo %in% i)
  # cross-sectional survey
  dataset <- input_data(country = i)
  dataset <- data_cleaning(dataset)
  dataset <- data_to_agecat(dataset)
  dataset <- dataset[dataset$phys_contact == "1",]
  
  dataset$frequency_multi <- factor(dataset$frequency_multi,
                                 levels=c('1','2','3','4','5'),
                                 labels=c('daily','weekly','monthly',
                                          'a few times a year','first time'))
  
  dataset$dayofweek <- factor(dataset$dayofweek,
                           levels=c('2','3','4','5','6','0','1'),
                           labels=c('Mon','Tue','Wed','Thu',
                                    'Fri','Sat','Sun'))
  
  dataset <- dataset[!is.na(dataset$frequency_multi),]
  # differentiate between weekend and weekday contacts for rpois.
  temp_dat <- data_prepare(dataset)
  table(unique(cbind(temp_dat$part_id, temp_dat$part_age_cat))[,2])/sum(table(unique(cbind(temp_dat$part_id, temp_dat$part_age_cat))[,2]))
  
  # prepare input dataset
  prop <- table(dataset$part_age_cat, dataset$frequency_multi)/rowSums(table(dataset$part_age_cat, dataset$frequency_multi))
  
  number.contact.input <- input_binomial(temp_dat)
  prop.week <- proportion.week.prep(temp_dat)
  prop.week.no.weekend <- prop.week %>% 
    group_by(part_age_cat) %>% 
    mutate(sum = sum(n), pctg = n/sum)
  print(xtable(prop.week.no.weekend))
  print(xtable(prop.week))
  
  weekend <- c('Sat', 'Sun')
  weekday <- c('Mon','Tue','Wed','Thu',
               'Fri')
  week <- c('Sun','Mon','Tue','Wed','Thu',
            'Fri','Sat')
  age_cat <- c("Children", "Teens", "Adult", "Elderly")
  freq_label <- c('daily','weekly','monthly',
                  'a few times a year','first time')
  temp_dat$part_age_cat <- factor(temp_dat$part_age_cat,
                                  levels = c("Children", "Teens", "Adult", "Elderly"))
  prop.age.partdata$part_age_cat <- factor(prop.age.partdata$part_age_cat,
                                           levels = c("Children", "Teens", "Adult", "Elderly"))
  
  prop.age.partdata <- arrange(prop.age.partdata, part_age_cat)
  
  contact <- temp_dat %>% 
    group_by(part_id) %>% 
    summarise(n = sum(n))
  
  fivenum(contact$n)
  mean(contact$n)
  
  contact_weekend <- temp_dat %>% 
    group_by(contact_day, part_id) %>% 
    summarise(n = sum(n)) 
  
  contact_weekend %>% 
    group_by(contact_day, part_id) %>% 
    summarise(n = n_distinct(part_id)) %>% 
    group_by(contact_day) %>% 
    summarise(n = n())
  
  table_contacts <- temp_dat %>% 
    group_by(part_id, part_age_cat) %>% 
    summarise(sum = sum(n)) %>% 
    group_by(part_age_cat) %>% 
    summarise(mean_data = mean(sum)) %>% 
    mutate(daily_times = mean_data*7)
  
  contact_age <- temp_dat %>% 
    group_by(part_id, part_age_cat) %>% 
    summarise(sum = sum(n)) %>% 
    group_by(part_age_cat) %>% 
    summarise(mean_data = mean(sum),
              sd = sd(sum))
  
  contact_age$part_age_cat <- factor(contact_age$part_age_cat,
                                     levels = c("Children", "Teens", "Adult", "Elderly"))
  
  filename <- sprintf('../../data/data_input/POLYMOD/input_polymod_physical_%s.RData', i)
  save.image(file= filename)
}
