# This script is used to create data with fatigue correction
# using the comix data 
#
# Created by : Neilshan Loedy
# Date : 30/12/2023
#------------------------
rm(list = ls())

# run first code.R
library(rstudioapi)
library(readr)
dir <- dirname(getActiveDocumentContext()$path)
setwd(dirname(getActiveDocumentContext()$path))
source("../helper/functions.R")
source("../helper/functions_fatigue.R")

# prepare data on wave 9-11
#----------------------------
contact_common_be_911 <- read.csv("../../data/CoMix/BE_Waves_9_11/CoMix_BE_contact_common.csv")
participant_common_be_911 <- read.csv("../../data/CoMix/BE_Waves_9_11/CoMix_BE_participant_common.csv")
sday_be_911 <- read.csv("../../data/CoMix/BE_Waves_9_11/CoMix_BE_sday.csv")
hh_common_be_911 <- read.csv("../../data/CoMix/BE_Waves_9_11/CoMix_BE_hh_common.csv")
participant_extra_be_911 <- read.csv("../../data/CoMix/BE_Waves_9_11/CoMix_BE_participant_extra.csv")

data_be_911 <- combine_data(participant_common_be_911,
                            sday_be_911, hh_common_be_911, participant_extra_be_911)

data_be_911 <- data_be_911 %>% 
  dplyr::select(part_id, panel_id, panel,
                sday_id, wave, country, hh_size, dayofweek, panel, part_face_mask,
                part_age_group, part_elevated_risk, n_cnt_all, area_3_name) %>% 
  dplyr::mutate(part_vacc = "No") %>% 
  mutate(part_uid = paste0("be_", panel_id),
         part_face_mask = tolower(part_face_mask),
         part_elevated_risk = tolower(part_elevated_risk)) %>% 
  dplyr::select(-panel_id)

data_be_911 <- data_be_911[is.na(data_be_911$part_age_group) == F,]

# Contacts during the Armistice day in Belgium (2020-11-11)
data_be_911 <- data_be_911 %>% 
  mutate(holiday = ifelse(sday_id == "2020.11.11", "1", "0"))
data_be_911$sday_id <- as.Date(data_be_911$sday_id, format = "%Y.%m.%d")

# part_elevated_risk --> NA for Children
data_be_911 <- data_be_911 %>% 
  mutate(part_elevated_risk = ifelse(part_age_group %in% c("[0,1)", "[1,5)", "[5,7)",
                                                           "[7,12)", "[12,16)", "[16,17)"), "no", part_elevated_risk))

data_be_911 <- data_be_911 %>%
  separate(area_3_name, into = c('drop', 'area_3_name'),
           sep = '\\s*/\\s*') %>%
  dplyr::select(-drop)

rm(contact_common_be_911, participant_common_be_911, sday_be_911, hh_common_be_911,
   participant_extra_be_911)

# prepare data on wave 12-43
#----------------------------
contact_common_be_12p <- read.csv("../../data/CoMix/CoMix_BE_contact_common.csv")
participant_common_be_12p <- read.csv("../../data/CoMix/CoMix_BE_participant_common.csv")
sday_be_12p <- read.csv("../../data/CoMix/CoMix_BE_sday.csv")
hh_common_be_12p <- read.csv("../../data/CoMix/CoMix_BE_hh_common.csv")
participant_extra_be_12p <- read.csv("../../data/CoMix/CoMix_BE_participant_extra.csv")

data_be_12p <- combine_data(participant_common_be_12p,
                            sday_be_12p, hh_common_be_12p, participant_extra_be_12p)

data_be_12p <- data_be_12p %>% 
  dplyr::select(part_id, part_uid, panel, part_vacc,
                sday_id, wave, country, hh_size, dayofweek, panel, part_face_mask,
                part_age_group, part_elevated_risk, n_cnt_all, area_3_name) %>% 
  mutate(part_face_mask = tolower(part_face_mask),
         part_elevated_risk = tolower(part_elevated_risk))

# Assuming your data is in a data frame called data_be_12p
enc_guess <- guess_encoding(data_be_12p$area_3_name)

# Use the guessed encoding in iconv
data_be_12p$area_3_name <- iconv(data_be_12p$area_3_name, to = "UTF-8", from = enc_guess$encoding[1])

data_be_12p <- data_be_12p %>%
  separate(area_3_name, into = c('drop', 'area_3_name'),
           sep = '\\s*/\\s*') %>%
  dplyr::select(-drop)

data_be_12p <- data_be_12p %>%
  arrange(part_uid) %>%
  group_by(part_uid) %>%
  fill(part_vacc, .direction = "down")

data_be_12p <- data_be_12p %>%
  arrange(part_uid) %>%
  group_by(part_uid) %>%
  fill(part_elevated_risk, .direction = "down")

data_be_12p <- merge(data_be_12p, sday_be_12p) %>% 
  dplyr::select(-c(day, month, year))

data_be_12p$sday_id <- as.Date(data_be_12p$sday_id, format = "%Y.%m.%d")

# part_elevated_risk --> NA for Children
data_be_12p <- data_be_12p %>% 
  mutate(part_elevated_risk = ifelse(part_age_group %in% c("Under 1", "1-4", "5-11", "12-15",
                                                           "16-17"), "no", part_elevated_risk))

data_be_12p <- data_be_12p %>% 
  mutate(part_vacc = ifelse(part_age_group %in% c("Under 1", "1-4", "5-11", "12-15",
                                                  "16-17"), "No answer", part_vacc))

rm(contact_common_be_12p, participant_common_be_12p, sday_be_12p, hh_common_be_12p,
   participant_extra_be_12p, enc_guess)

#------------
data_be_total <- rbind(data_be_911, data_be_12p) %>% 
  dplyr::select(-panel)

data_be_total <- data_be_total %>% 
  dplyr::select(part_uid, sday_id, wave) %>% 
  distinct() %>% 
  dplyr::arrange(part_uid, sday_id, wave) %>%
  dplyr::group_by(part_uid) %>% 
  # create wave_bin to mark the n-th wave for each individual participation
  dplyr::mutate(wave_bin = as.factor(ifelse(row_number() == 1, "1",
                                            ifelse(row_number() == 2, "2",
                                                   ifelse(row_number() == 3, "3",
                                                          ifelse(row_number() == 4, "4",
                                                                 ifelse(row_number() == 5, "5",
                                                                        ifelse(row_number() == 6, "6",
                                                                               ifelse(row_number() == 7, "7", "8p"))))))))) %>% 
  inner_join(data_be_total, by= c("part_uid", "sday_id", "wave"),
             multiple = "all")

data_be_total <- data_be_total %>% 
  mutate(hh_size = ifelse(hh_size == 1, "1",
                          ifelse(hh_size == 2, "2",
                                 ifelse(hh_size == 3, "3",
                                        "4p"))))

rm(contact_common_be_12p, participant_common_be_911, data_be_12p, data_be_911)

#------------
part_common <- read.csv("../../data/CoMix_be_zenodo/CoMix_BE_participant_common.csv")
prop.age.partdata <- read.csv("../../data/BE_2008/prop_agedata.csv")[,-1]
data_comix <- readRDS("../../data/CoMix/survey_belgium2020_comix_up_to_wave_43.rds")
data_merge_comix <- merge(data_comix$participants, data_comix$contacts, by = "part_id")

dataset <- data_merge_comix[data_merge_comix$wave %in% seq(9, 43), ]
dataset <- data_cleaning(dataset)
dataset <- data_to_agecat(dataset)

dataset$frequency_multi <- factor(dataset$frequency_multi,
                                  levels=c('1','2','3','4','5'),
                                  labels=c('daily','weekly','monthly',
                                           'a few times a year','first time'))

dataset$dayofweek <- factor(dataset$dayofweek,
                            levels=c('2','3','4','5','6','0','1'),
                            labels=c('Mon','Tue','Wed','Thu',
                                     'Fri','Sat','Sun'))

# create a dataset for all contacts
dataset <- dataset[!is.na(dataset$frequency_multi),]
temp_dat <- data_prepare(dataset)

# create a dataset for only physical contacts
dataset_physical <- dataset[dataset$phys_contact == "1",]
temp_dat_physical <- data_prepare(dataset_physical)

# create a dataset for contacts outside home
dataset_out_home <- dataset[dataset$cnt_home == "1",]
temp_dat_outhome <- data_prepare(dataset_out_home)

# create a dataset for all contacts inside home
dataset_ins_home <- dataset[dataset$cnt_home == "0",]
temp_dat_inshome <- data_prepare(dataset_ins_home)

#---- merge the dataset
merged_dataset <- data_be_total %>% 
  dplyr::select(part_id, part_uid, sday_id, wave, wave_bin, hh_size, part_face_mask,
         part_elevated_risk, area_3_name, part_vacc, holiday) %>% 
  mutate(part_id = as.factor(part_id)) %>% 
  inner_join(temp_dat, by = c("part_id"), multiple = "all")

merged_dataset <- merged_dataset %>%
  group_by(part_uid) %>%
  mutate(ref_date = min(sday_id))

merged_dataset$day_number <- as.numeric(difftime(merged_dataset$sday_id,
                                                 merged_dataset$ref_date))/(86400*365)

#---- merge the physical dataset
merged_dataset_physical <- data_be_total %>% 
  dplyr::select(part_id, part_uid, sday_id, wave, wave_bin, hh_size, part_face_mask,
                part_elevated_risk, area_3_name, part_vacc, holiday) %>% 
  mutate(part_id = as.factor(part_id)) %>% 
  inner_join(temp_dat_physical, by = c("part_id"), multiple = "all")

merged_dataset_physical <- merged_dataset_physical %>%
  group_by(part_uid) %>%
  mutate(ref_date = min(sday_id))

merged_dataset_physical$day_number <- as.numeric(difftime(merged_dataset_physical$sday_id,
                                                          merged_dataset_physical$ref_date))/(86400*365)

#---- merge the outside home dataset
merged_dataset_outhome <- data_be_total %>% 
  dplyr::select(part_id, part_uid, sday_id, wave, wave_bin, hh_size, part_face_mask,
                part_elevated_risk, area_3_name, part_vacc, holiday) %>% 
  mutate(part_id = as.factor(part_id)) %>% 
  inner_join(temp_dat_outhome, by = c("part_id"), multiple = "all")

merged_dataset_outhome <- merged_dataset_outhome %>%
  group_by(part_uid) %>%
  mutate(ref_date = min(sday_id))

merged_dataset_outhome$day_number <- as.numeric(difftime(merged_dataset_outhome$sday_id,
                                                         merged_dataset_outhome$ref_date))/(86400*365)

#---- merge the inside home dataset
merged_dataset_inshome <- data_be_total %>% 
  dplyr::select(part_id, part_uid, sday_id, wave, wave_bin, hh_size, part_face_mask,
                part_elevated_risk, area_3_name, part_vacc, holiday) %>% 
  mutate(part_id = as.factor(part_id)) %>% 
  inner_join(temp_dat_inshome, by = c("part_id"), multiple = "all")

merged_dataset_inshome <- merged_dataset_inshome %>%
  group_by(part_uid) %>%
  mutate(ref_date = min(sday_id))

merged_dataset_inshome$day_number <- as.numeric(difftime(merged_dataset_inshome$sday_id,
                                                         merged_dataset_inshome$ref_date))/(86400*365)

write.csv(merged_dataset, "../../data/CoMix_fatigue/comix_dataset_allcontacts.csv")
write.csv(merged_dataset_physical, "../../data/CoMix_fatigue/comix_dataset_physical.csv")
write.csv(merged_dataset_outhome, "../../data/CoMix_fatigue/comix_dataset_outhome.csv")
write.csv(merged_dataset_inshome, "../../data/CoMix_fatigue/comix_dataset_inshome.csv")

#---- create dataset without frequency

merged_dataset_sum <- merged_dataset %>% 
  group_by(part_id, part_uid, sday_id, wave, wave_bin, hh_size, part_face_mask,
           part_elevated_risk, area_3_name, part_vacc, holiday, part_age_cat, contact_day, dayofweek, ref_date, day_number) %>% 
  summarise(n = sum(n))

merged_dataset_physical_sum <- merged_dataset_physical %>% 
  group_by(part_id, part_uid, sday_id, wave, wave_bin, hh_size, part_face_mask,
           part_elevated_risk, area_3_name, part_vacc, holiday, part_age_cat, contact_day, dayofweek, ref_date, day_number) %>% 
  summarise(n = sum(n))

merged_dataset_outhome_sum <- merged_dataset_outhome %>% 
  group_by(part_id, part_uid, sday_id, wave, wave_bin, hh_size, part_face_mask,
           part_elevated_risk, area_3_name, part_vacc, holiday, part_age_cat, contact_day, dayofweek, ref_date, day_number) %>% 
  summarise(n = sum(n))

merged_dataset_inshome_sum <- merged_dataset_inshome %>% 
  group_by(part_id, part_uid, sday_id, wave, wave_bin, hh_size, part_face_mask,
           part_elevated_risk, area_3_name, part_vacc, holiday, part_age_cat, contact_day, dayofweek, ref_date, day_number) %>% 
  summarise(n = sum(n))

write.csv(merged_dataset_sum, "../../data/CoMix_fatigue/comix_dataset_allcontacts_sum.csv")
write.csv(merged_dataset_physical_sum, "../../data/CoMix_fatigue/comix_dataset_physical_sum.csv")
write.csv(merged_dataset_outhome_sum, "../../data/CoMix_fatigue/comix_dataset_outhome_sum.csv")
write.csv(merged_dataset_inshome_sum, "../../data/CoMix_fatigue/comix_dataset_inshome_sum.csv")
