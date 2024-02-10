# Repetition in Social Contacts: Implications in Modelling the Transmission of Respiratory Infectious Diseases in Pre-pandemic & Pandemic Settings ##

## Short summary of the study
In our study, we explored how human interactions impact the spread of respiratory viral infections. We utilised social contact data (POLYMOD and Belgian CoMix studies) to uncover the recurrent nature of contacts, which is often overlooked in traditional analyses. To bridge this gap, we undertake the task of describing individuals’ contact patterns over time, by characterising the interactions made with distinct individuals during a week. By reconstructing individual contact patterns over time, we found fewer distinct new contacts compared to a naive approach that ignores contact repetition. This highlights the importance of considering contact frequency to avoid underestimating the probability of transmission. Using sophisticated modelling techniques, we show how this temporal reconstruction affects disease transmission, using data from pre-pandemic and pandemic scenarios. Our findings underscore the important role of longitudinal contact analyses in shaping effective public health strategies, offering valuable insights for disease control efforts.

## Code version
Manual version number 1.0

## Overview of folders and files
This Git repository is divided into two major folders, code_final and data folders.
In the code_final folder, you will find all the necessary codes to reproduce the results in the manuscript.
In the data folder, you will find all the data utilised to run the analyses.

### 1. data
#### 1.1 BE_2008 Folder
This folder contains all the data from the POLYMOD study for all 8 countries (Mossong et al, 2008)

#### 1.2 CoMix
This folder contains all the data from the CoMix study for waves 9-43.
The data from this folder was obtained from the socialcontactdata.org

#### 1.3 CoMix_be_zonodo
This folder contains all the data from the CoMix study for waves 9-43.
The data from this folder was obtained from the zenodo.
The data obtained from this folder are exactly the same as the one
obtained from socialcontactdata.org (1.2)

#### 1.4 CoMix_fatigue
This folder is the under-reported corrected (Loedy et al, 2023) version of the CoMix data,
obtained from the "helper/marginal_comix_fatigue.R" file.

#### 1.5 data_input
This folder contains all the input needed to do epidemic simulations for pre-pandemic,
pandemic, and pandemic (fatigue correction). 

The data_input contains inputs such as the average number of contacts, variance, etc utilised
for epidemic simulations (obtained from helper/prepare_data_*.R),
as well as the social contact matrices for each setting.

#### 1.6 population_data_proportion.csv and population_eurostat_2019.csv
The population_data_proportion.csv contains the proportion of the population of each country in 
the POLYMOD study (and CoMix) that can be used when reconstructing the population.

#### 1.7 vsc_comix_input and vsc_polymod_input
These folders contain inputs (e.g., file name, country (POLYMOD), waves (CoMix), and foi)
for each epidemic simulation done.

### 2. code_final



Title of the study
Short summary of the study
Code version (e.g., Git fingerprint, manual version number)
Overview of folders/files and their contents
Instructions for users to run the software (e.g. explain the project workflow and any configuration parameters of your software)
Links to protocols.io or equivalent methods repositories, where applicable
Once a paper is accepted, please include author names, contact details, links to preprint and the publication.
