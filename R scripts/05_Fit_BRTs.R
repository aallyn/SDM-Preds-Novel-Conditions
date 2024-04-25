#####
## Overview 
#####

# This script will fit a boosted regression tree model to a training dataset. 

## Load libraries and source functions
library(tidyverse)
library(dismo)
library(gbm)

source(here::here("R functions/fit_brt.R"))

## Run main_fit_brt function over different datasets (n = 4 for this study, with two LMEs and two species archetypes in each LME)
region_vec <- c("cc", "ne")
sp_vec <- c("res", "seas")
region_sp <- expand.grid("Region" = region_vec, "Species" = sp_vec)

for (i in 1:nrow(region_sp)) {
    main_fit_brt(training_testing_list_path = here::here(paste0("data/train_test_lme_", region_sp$Species[i], "/", region_sp$Region[i], "/Base_1985-01-01_to_2004-01-01.rds")), predictors_vec = c("depth,oisst_daily"), response = "Real", family = "bernoulli", tree_complexity = 3, learning_rate = 0.01, bag_fraction = 0.6, out_file = here::here(paste0("mods/brt_fits_lme", region_sp$Species[i], "/", region_sp$Region[i], "/", "Base_1985-01-01_to_2004-01-01_fit.rds")))
}
