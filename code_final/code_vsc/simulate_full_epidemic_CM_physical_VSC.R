rm(list = ls())
vsc = 1

if(vsc == 0){
  dir <- dirname(getActiveDocumentContext()$path)
  setwd(paste0(dirname(getActiveDocumentContext()$path)))
} else {
  setwd("/vsc-hard-mounts/leuven-user/348/vsc34872/weekly_temporal_final")
}

args = commandArgs(trailingOnly = T)
id = as.numeric(args[1])
foi = as.numeric(args[2])
country = as.numeric(args[3])

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
gamma = 1/7
set.seed(123 + id)

output.simulate.vsc <- simulate.epidemic.CM(data.epi = data.epi, foi = foi, initial.case = initial.case, gamma = gamma)
output.simulate.baseline.vsc <- simulate.baseline.CM(data.epi = data.epi, foi = foi, initial.case = initial.case, gamma = gamma)

foi_name = foi*10000
filename_save <- sprintf("/vsc-hard-mounts/leuven-data/348/vsc34872/weekly_temporal_final/output_physical_%s/full_epidemic_simulation_CM_%s_%05d", input, foi_name, id)
save(list = c("output.simulate.vsc", "output.simulate.baseline.vsc"), file = sprintf("%s.RData", filename_save))
