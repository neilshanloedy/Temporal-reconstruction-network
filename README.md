# Repetition in Social Contacts: Implications in Modelling the Transmission of Respiratory Infectious Diseases in Pre-pandemic & Pandemic Settings ##

## Summary of the study
In our study, we explored how human interactions impact the spread of respiratory viral infections. We utilised social contact data (POLYMOD and Belgian CoMix studies) to uncover the recurrent nature of contacts, which is often overlooked in traditional analyses. To bridge this gap, we undertake the task of describing individuals’ contact patterns over time, by characterising the interactions made with distinct individuals during a week. By reconstructing individual contact patterns over time, we found fewer distinct new contacts compared to a naive approach that ignores contact repetition. This highlights the importance of considering contact frequency to avoid underestimating the probability of transmission. Using sophisticated modelling techniques, we show how this temporal reconstruction affects disease transmission, using data from pre-pandemic and pandemic scenarios. Our findings underscore the important role of longitudinal contact analyses in shaping effective public health strategies, offering valuable insights for disease control efforts.

## Code version
Manual version number 1.0

## Overview of folders and files
This Git repository is divided into two major folders, code_final and data folders.
In the code_final folder, you will find all the necessary codes to reproduce the results in the manuscript.
In the data folder, you will find all the data utilised to run the analyses.

### 1. data
#### 1.1 BE_2008 Folder
This folder contains all the data from the POLYMOD study for all 8 countries (Mossong et al., 2008).

#### 1.2 CoMix
This folder contains all the data from the CoMix study for waves 9-43. The data in this folder were obtained from socialcontactdata.org.

#### 1.3 CoMix_be_zonodo
This folder also contains data from the CoMix study for waves 9-43. The data here were obtained from Zenodo. It is identical to the data obtained from socialcontactdata.org (1.2).

#### 1.4 CoMix_fatigue
This folder contains the under-reported corrected version of the CoMix data (Loedy et al., 2023). The data were obtained from the "helper/marginal_comix_fatigue.R" file.

#### 1.5 data_input
This folder contains all the inputs needed for epidemic simulations for pre-pandemic, pandemic, and pandemic (fatigue correction). It includes inputs such as the average number of contacts, variance, etc., obtained from helper/prepare_data_*.R, as well as the social contact matrices for each setting.

#### 1.6 population_data_proportion.csv and population_eurostat_2019.csv
The "population_data_proportion.csv" file contains the proportion of the population of each country in the POLYMOD study (and CoMix), which can be used when reconstructing the population.

#### 1.7 vsc_comix_input and vsc_polymod_input
These folders contain inputs (e.g., file name, country (POLYMOD), waves (CoMix), and FOI) for each epidemic simulation conducted.

### 2. code_final
The workflow of the code to generate all outputs are the following;
EDA_*.R (to generate population with frequency-based contacts) --> simulate_full_epidemic.R --> plot_output_paper.R.

The main codes used to generate results are *simulate_full_epidemic.R* and *plot_output_paper.R*. 

In the *simulate_full_epidemic.R* we simulate the epidemics using two different approaches (naive and frequency-based). All output on the paper is generated using *plot_output_paper.R*.

Note: In *plot_output_paper.R* there is a function to parse the results of each epidemic simulation so that each simulation does not need to be saved (instead, each analysed outbreak characteristic is kept to reduce computer memory).

#### 2.1 code_vsc
This is the code to run the simulations when one is using a Super Computer (VSC)

#### 2.2 helper
This folder contains all files that can be utilised to support the main script.
#### 2.2.a Appendix_contacts_proportions.R
R file to produce all figures in the Supplementary Materials (Section 4).

#### 2.2.a functions_fatigue.R
R file including all functions to help correct under-reporting due to fatigue in the Belgian CoMix data

#### 2.2.b functions.R
R file including all main functions to help run all the analysis

#### 2.2.c gamma_comparison_flu.R
R file to disentangle attack rates when one compares attack rates for influenza-like and covid-19-like illnesses for different gamma (infectious days)

#### 2.2.d input_vsc.R
R file to generate epidemic simulation inputs.

#### 2.2.e model_comix_fatigue.R
This R file contains codes to develop 'GAMLSS' model to correct under-reporting due to fatigue

#### 2.2.f prepare_data_*.R
These R files can be utilised to prepare all the inputs necessary for simulating epidemics (output exported into data_input)

#### 2.2.g prepare_dataset_for_comix_fatigue.R
This script is used to prepare data used to correct under-reporting due to fatigue using the CoMix data (data preparation/clearing stage) 

#### 2.2.h simulate_epidemic_CM_fullepidemics.R
R file including epidemic simulations (naive and frequency-based approaches)

#### 2.2.i simulate_weekly_temporal.R and simulate_weekly_temporal_poisson.R
These R files include functions to simulate the frequency-based contact patterns using GPO and PO distribution, respectively.

#### 2.3 EDA_*.R
These files can be used to generate all frequency-based contacts in each setting.

#### 2.4 marginal_comix_fatigue.R
This code generates CSV files of the CoMix data corrected for under-reported using the GAMLSS model using the cleared data.

#### 2.5 social_contact_frequencies.R
This code is used to generate contact matrices for all settings.
