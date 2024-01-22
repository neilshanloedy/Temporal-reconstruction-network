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
wave_list = as.numeric(args[3])

N = 5000

# this is using the data corrected for the fatigue effect
data_temp <- readRDS(sprintf("rds/CoMix/data_temp_CoMix_physical_5000pop_%04d.rds", wave_list))

load(sprintf('/vsc-hard-mounts/leuven-data/348/vsc34872/weekly_temporal_final/data_input/CoMix/input_CoMix_physical_%04d.RData', wave_list))
load(sprintf('/vsc-hard-mounts/leuven-data/348/vsc34872/weekly_temporal_final/data_input/CoMix/social_matrices_physical_%04d.RData', wave_list))

source('helper/simulate_epidemic_CM_fullepidemics.R')
source('helper/functions.R')

data.epi = data_temp
initial.case = 1
starting_day = "Mon"
gamma = 1/7
set.seed(123 + id)

cm <- lapply(cm, function(x) x + 0.00001)

output.simulate.vsc <- simulate.epidemic.CM(data.epi = data.epi, foi = foi, initial.case = initial.case, gamma = gamma)
output.simulate.baseline.vsc <- simulate.baseline.CM.poisson(data.epi = data.epi, foi = foi, initial.case = initial.case, gamma = gamma)

foi_name = foi*10000
filename_save <- sprintf("/vsc-hard-mounts/leuven-data/348/vsc34872/weekly_temporal_final/output_CoMix_wave_physical_%04d/full_epidemic_simulation_CM_%05d_%05d", as.integer(wave_list), as.integer(foi_name), as.integer(id))
save(list = c("output.simulate.vsc", "output.simulate.baseline.vsc"), file = sprintf("%s.RData", filename_save))