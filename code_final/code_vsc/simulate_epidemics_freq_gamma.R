rm(list = ls())
vsc = 1

if(vsc == 0){
  dir <- dirname(getActiveDocumentContext()$path)
  setwd(dirname(getActiveDocumentContext()$path))
} else {
  setwd("/vsc-hard-mounts/leuven-user/348/vsc34872/weekly_temporal_final")
}

args = commandArgs(trailingOnly = T)
id = as.numeric(args[1])
foi = as.numeric(args[2])
R0 = as.numeric(args[3])
gamma_num = as.numeric(args[4])
gamma = as.numeric(args[5])

country <- 1
country_list <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB")
input <- country_list[country]

N = 5000

data_temp <- readRDS(sprintf("rds/POLYMOD/data_temp_%04d_pop_physical_%s.rds", N, input))

load(sprintf('/vsc-hard-mounts/leuven-data/348/vsc34872/weekly_temporal_final/data_input/POLYMOD/input_polymod_physical_%s.RData', input))
load(sprintf('/vsc-hard-mounts/leuven-data/348/vsc34872/weekly_temporal_final/data_input/POLYMOD/social_matrices_physical_%s.RData', input))
source('helper/simulate_epidemic_CM_fullepidemics.R')
source('helper/functions.R')

data.epi = data_temp
initial.case = 1
starting_day = "Mon"
set.seed(123 + id)

output.simulate.vsc <- simulate.epidemic.CM(data.epi = data.epi, foi = foi, initial.case = initial.case, gamma = gamma)

filename_save <- sprintf("/vsc-hard-mounts/leuven-data/348/vsc34872/weekly_temporal_final/output_physical_%s_gamma_%03d/full_epidemic_simulation_CM_frequency_%03d_%05d", input, gamma_num, R0, id)
save(list = c("output.simulate.vsc"), file = sprintf("%s.RData", filename_save))
