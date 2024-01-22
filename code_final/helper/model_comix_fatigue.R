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
library(gamlss)
library(gamlss.cens)
dir <- dirname(getActiveDocumentContext()$path)
setwd(dirname(getActiveDocumentContext()$path))
source("../helper/functions.R")
source("../helper/functions_fatigue.R")
# model building with LRT
gen.cens(GPO, type = "right")

# load data
#-------------
merged_dataset <- read.csv("../../data/CoMix_fatigue/comix_dataset_allcontacts.csv")[,-1]
merged_dataset_physical <- read.csv("../../data/CoMix_fatigue/comix_dataset_physical.csv")[,-1]
merged_dataset_outhome <- read.csv("../../data/CoMix_fatigue/comix_dataset_outhome.csv")[,-1]
merged_dataset_inshome <- read.csv("../../data/CoMix_fatigue/comix_dataset_inshome.csv")[,-1]

merged_dataset <- prepare_data_tomodel(merged_dataset)
merged_dataset_physical <- prepare_data_tomodel(merged_dataset_physical)
merged_dataset_outhome <- prepare_data_tomodel(merged_dataset_outhome)
merged_dataset_inshome <- prepare_data_tomodel(merged_dataset_inshome)

# create a model
var_cnt = c("part_age_cat",
            "hh_size",
            "area_3_name",
            "wave_bin", 
            "part_face_mask",
            "holiday",
            "frequency_multi",
            "frequency_multi*part_age_cat",
            "hh_size*part_age_cat",
            "hh_size*area_3_name",
            "holiday*area_3_name",
            "cs(day_number, by = part_vacc)",
            "re(random = ~ 1 |part_uid)")

variance_cnt = c("part_age_cat",
                 "hh_size",
                 "area_3_name",
                 "wave_bin", 
                 "part_face_mask",
                 "holiday",
                 "frequency_multi",
                 "frequency_multi*part_age_cat",
                 "hh_size*part_age_cat",
                 "hh_size*area_3_name",
                 "holiday*area_3_name")

model_allcontacts <- gamlss(as.formula(paste("n ~", paste(var_cnt, collapse = "+"))),
                         sigma.fo = paste(" ~ ", paste(variance_cnt, collapse = "+")),
                         family = "GPO",
                         method = RS(2000),
                         data = na.omit(merged_dataset),
                         mu.start = mean(merged_dataset$n),
                         control = gamlss.control(c.crit = 0.015,
                                                  maxIter = 10000,
                                                  msMaxEval = 10000))

# Here, we use NBI instead of GPO, since GPO is not converging
model_physical <- gamlss(as.formula(paste("n ~", paste(var_model, collapse = "+"))),
                        sigma.fo = paste(" ~ ", paste(variance_cnt, collapse = "+")),
                        family = "GPO",
                        method = RS(2000),
                        data = na.omit(merged_dataset_physical),
                        mu.start = mean(merged_dataset_physical$n),
                        control = gamlss.control(c.crit = 0.015,
                                                 maxIter = 10000,
                                                 msMaxEval = 10000))

model_outhome <- gamlss(as.formula(paste("n ~", paste(var_cnt, collapse = "+"))),
                            sigma.fo = paste(" ~ ", paste(variance_cnt, collapse = "+")),
                            family = "GPO",
                            method = RS(2000),
                            data = na.omit(merged_dataset_outhome),
                            mu.start = mean(merged_dataset_outhome$n),
                            control = gamlss.control(c.crit = 0.015,
                                                     maxIter = 10000,
                                                     msMaxEval = 10000))

model_inshome <- gamlss(as.formula(paste("n ~", paste(var_cnt, collapse = "+"))),
                            sigma.fo = paste(" ~ ", paste(variance_cnt, collapse = "+")),
                            family = "GPO",
                            method = RS(2000),
                            data = na.omit(merged_dataset_inshome),
                            mu.start = mean(merged_dataset_inshome$n),
                            control = gamlss.control(c.crit = 0.015,
                                                     maxIter = 10000,
                                                     msMaxEval = 10000))

saveRDS(model_allcontacts, "../rds/CoMix_fatigue_gamlss/Gamlss_fatigue_allcontacts.rds")
saveRDS(model_physical, "../rds/CoMix_fatigue_gamlss/Gamlss_fatigue_physical.rds")
saveRDS(model_outhome, "../rds/CoMix_fatigue_gamlss/Gamlss_fatigue_outhomecontacts.rds")
saveRDS(model_inshome, "../rds/CoMix_fatigue_gamlss/Gamlss_fatigue_inhomecontacts.rds")

#--------------
# create the model without frequency 
#--------------

merged_dataset_sum <- read.csv("../../data/CoMix_fatigue/comix_dataset_allcontacts_sum.csv")[,-1]
merged_dataset_physical_sum <- read.csv("../../data/CoMix_fatigue/comix_dataset_physical_sum.csv")[,-1]
merged_dataset_outhome_sum <- read.csv("../../data/CoMix_fatigue/comix_dataset_outhome_sum.csv")[,-1]
merged_dataset_inshome_sum <- read.csv("../../data/CoMix_fatigue/comix_dataset_inshome_sum.csv")[,-1]

merged_dataset_sum <- prepare_sumdata_tomodel(merged_dataset_sum)
merged_dataset_physical_sum <- prepare_sumdata_tomodel(merged_dataset_physical_sum)
merged_dataset_outhome_sum <- prepare_sumdata_tomodel(merged_dataset_outhome_sum)
merged_dataset_inshome_sum <- prepare_sumdata_tomodel(merged_dataset_inshome_sum)

# create a model
var_cnt = c("part_age_cat",
            "hh_size",
            "area_3_name",
            "wave_bin", 
            "holiday",
            "wave_bin*part_age_cat",
            "hh_size*part_age_cat",
            "hh_size*area_3_name",
            "holiday*area_3_name",
            "cs(day_number, by = part_vacc)",
            "re(random = ~ 1 |part_uid)")

variance_cnt = c("part_age_cat",
                 "hh_size",
                 "area_3_name",
                 "wave_bin", 
                 "part_face_mask",
                 "holiday",
                 "hh_size*part_age_cat",
                 "hh_size*area_3_name",
                 "holiday*area_3_name")

model_allcontacts_sum <- gamlss(as.formula(paste("n ~", paste(var_cnt, collapse = "+"))),
                            sigma.fo = paste(" ~ ", paste(variance_cnt, collapse = "+")),
                            family = "NBI",
                            method = RS(2000),
                            data = na.omit(merged_dataset_sum),
                            mu.start = mean(merged_dataset_sum$n),
                            control = gamlss.control(c.crit = 0.025,
                                                     maxIter = 10000,
                                                     msMaxEval = 10000))

model_physical_sum <- gamlss(as.formula(paste("n ~", paste(var_cnt, collapse = "+"))),
                         sigma.fo = paste(" ~ ", paste(variance_cnt, collapse = "+")),
                         family = "NBI",
                         method = RS(2000),
                         data = na.omit(merged_dataset_physical_sum),
                         mu.start = mean(merged_dataset_physical_sum$n),
                         control = gamlss.control(c.crit = 0.025,
                                                  maxIter = 10000,
                                                  msMaxEval = 10000))

model_outhome_sum <- gamlss(as.formula(paste("n ~", paste(var_cnt, collapse = "+"))),
                        sigma.fo = paste(" ~ ", paste(variance_cnt, collapse = "+")),
                        family = "NBI",
                        method = RS(2000),
                        data = na.omit(merged_dataset_outhome_sum),
                        mu.start = mean(merged_dataset_outhome_sum$n),
                        control = gamlss.control(c.crit = 0.025,
                                                 maxIter = 10000,
                                                 msMaxEval = 10000))

model_inshome_sum <- gamlss(as.formula(paste("n ~", paste(var_cnt, collapse = "+"))),
                        sigma.fo = paste(" ~ ", paste(variance_cnt, collapse = "+")),
                        family = "NBI",
                        method = RS(2000),
                        data = na.omit(merged_dataset_inshome_sum),
                        mu.start = mean(merged_dataset_inshome_sum$n),
                        control = gamlss.control(c.crit = 0.025,
                                                 maxIter = 10000,
                                                 msMaxEval = 10000))

saveRDS(model_allcontacts_sum, "../rds/CoMix_fatigue_gamlss/Gamlss_fatigue_physical_sum.rds")
saveRDS(model_physical_sum, "../rds/CoMix_fatigue_gamlss/Gamlss_fatigue_physical_sum.rds")
saveRDS(model_outhome_sum, "../rds/CoMix_fatigue_gamlss/Gamlss_fatigue_physical_sum.rds")
saveRDS(model_inshome_sum, "../rds/CoMix_fatigue_gamlss/Gamlss_fatigue_physical_sum.rds")