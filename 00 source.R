# Packages
if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
pacman::p_load(
  tidyverse
)

# Datasets
load("00 envr/Compulsory/ass_copy.R")
