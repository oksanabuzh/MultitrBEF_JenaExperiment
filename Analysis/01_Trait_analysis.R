
# Aboveground plant community and species-specific vegetation cover

library(tidyverse)

# read data from PANGAEA -----
# Citation: De Luca, Enrica; Weigelt, Alexandra; Schmid, Bernhard; Fischer, Markus (2016): Aboveground plant community and species-specific vegetation cover from the Jena Experiment (Main Experiment, year 2010) [dataset]. PANGAEA, https://doi.org/10.1594/PANGAEA.865273
# install.packages("pangaear")

library(pangaear)

Commun <- pg_data(doi = '10.1594/PANGAEA.865273')
Commun
str(Commun)

# write_csv(Commun[[1]]$data, "Data/Plant_Community_data.csv")

ComComp <- Commun[[1]]$data

# for each species take mean from two sampling dates "Date/time start" 

