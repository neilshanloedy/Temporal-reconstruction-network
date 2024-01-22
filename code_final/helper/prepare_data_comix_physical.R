rm(list = ls())
library(rstudioapi)

dir <- dirname(getActiveDocumentContext()$path)
setwd(dirname(getActiveDocumentContext()$path))

source("functions.R")
prop.age.partdata <- read.csv("../../data/BE_2008/prop_agedata.csv")[,-1]
data_merge_comix <- read.csv("../../data/CoMix_fatigue/comix_dataset_physical.csv")[,-1]

# cross-sectional survey
for(wave in seq(9,43)){
  
  print(sprintf("doing %s", wave))
  
  dataset <- data_merge_comix[data_merge_comix$wave %in% wave, ]
  dataset <- data_cleaning_comix(dataset)

  dataset <- dataset[!is.na(dataset$frequency_multi),]
  
  # differentiate between weekend and weekday contacts for rpois.
  temp_dat <- data_prepare_comix(dataset)
  number.contact.input <- input_binomial(temp_dat)
  
  contact_age <- temp_dat %>% 
    group_by(part_id, part_age_cat) %>% 
    summarise(sum = sum(n)) %>% 
    group_by(part_age_cat) %>% 
    summarise(mean_data = mean(sum),
              sd = sd(sum))
  
  contact_age$part_age_cat <- factor(contact_age$part_age_cat,
                                     levels = c("Children", "Teens", "Adult", "Elderly"))
  
  save.image(file = sprintf('../../data/data_input/CoMix_fatigue/input_CoMix_physical_%04d.RData', wave))
}
