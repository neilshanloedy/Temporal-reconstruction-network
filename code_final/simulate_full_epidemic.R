rm(list = ls())
dir <- dirname(getActiveDocumentContext()$path)
setwd(paste0(dirname(getActiveDocumentContext()$path)))

# input functions
#-------------------
source('helper/simulate_epidemic_CM_fullepidemics.R')
source('helper/functions.R')

#-------------------
N = 5000 # population size
iteration = 3000*2 # number of iteration
settings <- c("pre-pandemic", "pandemic")
country_list <- c("BE", "DE", "FI", "IT", "LU", "NL", "PL", "GB") # country list for the pre-pandemic setting
wave_list <- c(19, 27, 43) # wave list for the pandemic setting
initial.case = 1
starting_day = "Mon"
gamma = 1/7

settings <- "pandemic"

for(setting in settings){
  
  if(setting %in% "pre-pandemic") {
      for (i in 1:length(country_list)) {
        input <- country_list[i]
        # load data input for the simulation (country, and foi per country)
        data_input <- read.csv(sprintf("../data/vsc_polymod_input/input_forVSC_physical_%s.csv", input))
        for (iter in 1:iteration) {
          id = data_input[iter, "id"]
          set.seed(123 + id)
          foi = data_input[iter, "foi"]
          
          # input the frequency-based reconstructed contact
          data_temp <- readRDS(sprintf("rds/POLYMOD/data_temp_%04d_pop_physical_%s.rds", N, input))
          
          # input contact data from polymod
          load(sprintf('../data/data_input/POLYMOD/input_polymod_physical_%s.RData', input))
          load(sprintf('../data/data_input/POLYMOD/social_matrices_physical_%s.RData', input))
          
          data.epi = data_temp
        
          #simulate the epidemics based on the frequency-based reconstructed contacts
          output.simulate.vsc <- simulate.epidemic.CM(data.epi = data.epi, foi = foi, initial.case = initial.case, gamma = gamma)
          
          # simulate (and reconstruct naive approach contacts) the epidemics
          output.simulate.baseline.vsc <- simulate.baseline.CM(data.epi = data.epi, foi = foi, initial.case = initial.case, gamma = gamma)
          foi_name = foi*10000
          
          filename_save <- sprintf("rds/POLYMOD/output_physical_%s/full_epidemic_simulation_CM_%s_%05d", input, foi_name, id)
          save(list = c("output.simulate.vsc", "output.simulate.baseline.vsc"), file = sprintf("%s.RData", filename_save))
          
        }
        
      }
    
  }  
  
  if(setting %in% "pandemic") {
    for (i in 1:length(wave_list)) {
      input <- wave_list[i]
      
      # load data input for the simulation (wave, and foi per wave)
      data_input <- read.csv(sprintf("../data/vsc_comix_input/input_forVSC_physical_%04d.csv", input))
      for (iter in 1:iteration) {
        id = data_input[iter, "id"]
        set.seed(123 + id)
        foi = data_input[iter, "foi"]
        
        # input the frequency-based reconstructed contact
        data_temp <- readRDS(sprintf("rds/CoMix/data_temp_CoMix_physical_5000pop_%04d.rds", input))
        
        # input contact data from polymod
        load(sprintf('../data/data_input/CoMix_fatigue/input_CoMix_physical_%04d.RData', input))
        load(sprintf('../data/data_input/CoMix_fatigue/social_matrices_physical_%04d.RData', input))
        
        data.epi = data_temp
        
        # sometimes no contacts are reported in some frequency, hence we add very small rates
        cm <- lapply(cm, function(x) x + 0.00001)
        
        #simulate the epidemics based on the frequency-based reconstructed contacts
        output.simulate.vsc <- simulate.epidemic.CM(data.epi = data.epi, foi = foi, initial.case = initial.case, gamma = gamma)
        
        # simulate (and reconstruct naive approach contacts) the epidemics
        output.simulate.baseline.vsc <- simulate.baseline.CM(data.epi = data.epi, foi = foi, initial.case = initial.case, gamma = gamma)
        foi_name = foi*10000
        
        filename_save <- sprintf("rds/CoMix/output_CoMix_wave_physical_%04d/full_epidemic_simulation_CM_%05d_%05d", input, foi_name, id)
        save(list = c("output.simulate.vsc", "output.simulate.baseline.vsc"), file = sprintf("%s.RData", filename_save))
        
      }
    }
    
  }
  
}
