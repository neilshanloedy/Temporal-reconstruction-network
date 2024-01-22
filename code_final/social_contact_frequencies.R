##################################################################################
## This script provides contact matrices per frequency based on the
## Belgium POLYMOD study
## The contact matrices were weighted by age and weekend
## 
## In socialcontactdata.org, the inputs are:
#' Country = Belgium 2006 (Mossong 2008)
#' Age breaks = 0, 13, 19, 66
#' All --> type of contacts, contact duration, contact intensity, gender
#' check --> weigh by age, week/weekend, include all locations.
#' then you will get approximately the ((cm$all matrix))
## Author: Neilshan Loedy
## last update: 31/07/2023
##################################################################################
rm(list = ls())
library(rstudioapi)
library(socialmixr)

dir <- dirname(getActiveDocumentContext()$path)
setwd(dirname(getActiveDocumentContext()$path))

country <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")

for(i in country){
  cm <- list()
  cm_capita <- list()
  frequency.label <- c(1:5)
  frequency.name <- c("daily", "weekly", "monthly", "few", "first")
  names <- c("Children", "Teens", "Adult", "Elderly")
  
  for(freq in frequency.label){
    cm[[frequency.name[freq]]] <- contact_matrix(polymod,
                                                 countries = i,
                                                 age.limits = c(0, 13, 19, 66),
                                                 weigh.dayofweek = T,
                                                 weigh.age = T,
                                                 filter = list(frequency_multi=freq))$matrix
    
    cm_capita[[frequency.name[freq]]] <- contact_matrix(polymod,
                                                 countries = i,
                                                 age.limits = c(0, 13, 19, 66),
                                                 weigh.dayofweek = T,
                                                 weigh.age = T,
                                                 per.capita = T,
                                                 symmetric = T,
                                                 filter = list(frequency_multi=freq))$matrix.per.capita
    
  }
  
  cm[["all"]] <- Reduce('+', cm)
  cm_capita[["all"]] <- Reduce('+', cm_capita)
  
  # Function to change colnames and rownames for a matrix
  change_names <- function(mat) {
    colnames(mat) <- paste0(names)
    rownames(mat) <- paste0(names)
    return(mat)
  }
  
  cm <- lapply(cm, change_names)
  cm_capita <- lapply(cm_capita, change_names)
  
  save(list = ls(), file = sprintf("../../data/data_input/POLYMOD/social_matrices_%s.RData", i))
}

#------ Physical #

for(i in country){
  cm <- list()
  cm_capita <- list()
  frequency.label <- c(1:5)
  frequency.name <- c("daily", "weekly", "monthly", "few", "first")
  names <- c("Children", "Teens", "Adult", "Elderly")
  
  for(freq in frequency.label){
    cm[[frequency.name[freq]]] <- contact_matrix(polymod,
                                                 countries = i,
                                                 age.limits = c(0, 13, 19, 66),
                                                 weigh.dayofweek = T,
                                                 weigh.age = T,
                                                 filter = list(frequency_multi=freq,
                                                               phys_contact=1))$matrix
    
    cm_capita[[frequency.name[freq]]] <- contact_matrix(polymod,
                                                        countries = i,
                                                        age.limits = c(0, 13, 19, 66),
                                                        weigh.dayofweek = T,
                                                        weigh.age = T,
                                                        per.capita = T,
                                                        symmetric = T,
                                                        filter = list(frequency_multi=freq,
                                                                      phys_contact=1))$matrix.per.capita
    
  }
  
  cm[["all"]] <- Reduce('+', cm)
  cm_capita[["all"]] <- Reduce('+', cm_capita)
  
  # Function to change colnames and rownames for a matrix
  change_names <- function(mat) {
    colnames(mat) <- paste0(names)
    rownames(mat) <- paste0(names)
    return(mat)
  }
  
  cm <- lapply(cm, change_names)
  cm_capita <- lapply(cm_capita, change_names)
  
  save(list = ls(), file = sprintf("../../data/data_input/POLYMOD/social_matrices_physical_%s.RData", i))
}

#------ CoMix Physical #

comix_data <- read.csv("../data/CoMix_Fatigue/redistribute_physical.csv")
wave = seq(9, 43)

for(i in wave){
  cm <- list()
  cm_capita <- list()
  frequency.label <- c(1:5)
  frequency.name <- c("daily", "weekly", "monthly", "few", "first")
  names <- c("Children", "Teens", "Adult", "Elderly")
  
  for(freq in frequency.label){
    cm[[frequency.name[freq]]] <- contact_matrix(comix_data,
                                                 countries = "BE",
                                                 age.limits = c(0, 13, 19, 66),
                                                 weigh.dayofweek = T,
                                                 weigh.age = T,
                                                 filter = list(frequency_multi=freq,
                                                               wave = i,
                                                               phys_contact=1))$matrix
    
    cm_capita[[frequency.name[freq]]] <- contact_matrix(comix_data,
                                                        countries = "BE",
                                                        age.limits = c(0, 13, 19, 66),
                                                        weigh.dayofweek = T,
                                                        weigh.age = T,
                                                        per.capita = T,
                                                        symmetric = T,
                                                        filter = list(frequency_multi=freq,
                                                                      wave = i,
                                                                      phys_contact=1))$matrix.per.capita
    
  }
  
  cm[["all"]] <- Reduce('+', cm)
  cm_capita[["all"]] <- Reduce('+', cm_capita)
  
  # Function to change colnames and rownames for a matrix
  change_names <- function(mat) {
    colnames(mat) <- paste0(names)
    rownames(mat) <- paste0(names)
    return(mat)
  }
  
  cm <- lapply(cm, change_names)
  cm_capita <- lapply(cm_capita, change_names)
  
  save(list = ls(), file = sprintf("../../data/data_input/CoMix_Fatigue/social_matrices_physical_%04d.RData", i))
}

