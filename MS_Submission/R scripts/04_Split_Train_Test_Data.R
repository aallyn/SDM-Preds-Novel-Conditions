#####
## Overview
#####
# This script will gather training and testing datasets by converting habitat suitability surfaces into presence/absences, gathering sample data, enhancing sample data with covariates, and then splitting it into training and testing data.

## Load libraries and source functions
library(virtualspecies)
library(tidyverse)
library(here)

sf::sf_use_s2(FALSE)

source(here::here("R functions/convert_vs_hab_suit.R"))
source(here::here("R functions/sample_vs_pa.R"))
source(here::here("R functions/enhance_sample_vs_pa.R"))
source(here::here("R functions/enhance_r_funcs.R"))
source(here::here("R functions/split_train_test.R"))


## Convert continuous surfaces to presences/absences 
region_vec <- c("cc", "ne")
sp_vec <- c("res", "seas")
region_sp <- expand.grid("Region" = region_vec, "Species" = sp_vec)

for (i in 1:nrow(region_sp)) {
    main_convert_vs_hab_suit(vs_hab_suit_path = here::here(paste0("data/vs_hab_suit_lme_", region_sp$Species[i], "/", region_sp$Region[i], "_vs_suit.rds")), convert_vs_hab_suit_params_path = here::here("data/convert_vs_hab_suit_params/convert_vs_hab_suit_params.csv"), out_file = here::here(paste0("data/vs_hab_suit_lme_", region_sp$Species[i], "/", region_sp$Region[i], "_vs_pa.rds")))
}

## Sample presence/absences (for this study, we used a census)
for (i in 1:nrow(region_sp)) {
    main_sample_vs_pa(vs_pa_path = here::here(paste0("data/vs_hab_suit_lme_", region_sp$Species[i], "/", region_sp$Region[i], "_vs_pa.rds")), region_sf_path = here::here(paste0("data/region_shapefiles_lme/", region_sp$Region[i], ".geojson")), sample_vs_pa_params_path = here::here(paste0("data/sample_vs_pa_params/", region_sp$Region[i], "_sample_vs_pa_params.csv")), out_file = here::here(paste0("data/vs_hab_suit_lme_", region_sp$Species[i], "/", region_sp$Region[i], "_vs_samp.rds")))
}

## Enhance presence/absences with covariates (depth/sst)
for (i in 1:nrow(region_sp)) {
    main_enhance_sample_vs_pa(sample_vs_pa_path = here::here(paste0("data/vs_hab_suit_lme_", region_sp$Species[i], "/", region_sp$Region[i], "_vs_samp.rds")), habitat_covs_dir = here::here("data/habitat_covs/"), out_file = here::here(paste0("data/vs_hab_suit_lme_", region_sp$Species[i], "/", region_sp$Region[i], "_vs_enhanced.rds")))
}

## Split into training and testing subsets
for (i in 1:nrow(region_sp)) {
    main_split_train_test(all_data_path = here::here(paste0("data/vs_hab_suit_lme_", region_sp$Species[i], "/", region_sp$Region[i], "_vs_enhanced.rds")), train_date_start = "1985-01-01", train_date_end = "2004-01-01", out_file = here::here(paste0("data/train_test_lme", region_sp$Species[i], "/", region_sp$Region[i], "/Base_1985-01-01_to_2004-01-01.rds")))
}