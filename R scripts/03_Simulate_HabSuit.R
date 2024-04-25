#####
## Overview
#####
# This script will generate virtual species habitat suitability given habitat covariate raster layers/stacks (found in ./data/habitat_covs) and the species response to these habitat covariates depending on species-response curve parameters (found in ./data/SppEnvCurveParams.csv) and user supplied habitat formula. 

## Load libraries and source functions
library(raster)
library(virtualspecies)
library(tidyverse)
library(here)

sf::sf_use_s2(FALSE)

source(here::here("R functions/spp_env_funcs.R"))
source(here::here("R functions/gen_vs_hab_suit.R"))

## Run main_sim_vs_hab_suit function, looping over the two regions. This function is mostly just a `sim_vs_hab_suit` function (found in ./R functions/gen_vs_hab_suit.R), which leverages the `virtualspecies::generateSpFromFun` function to generate virtual species habitat suitabilities.
region_vec <- c("cc", "ne")
sp_vec <- c("res", "seas")
region_sp<- expand.grid("Region" = region_vec, "Species" = sp_vec)

for(i in 1:nrow(region_sp)){
    main_sim_vs_hab_suit(habitat_covs_dir = here::here("data/habitat_covs/"), spp_env_params_path = here::here(paste0("data/spp_env_params_lme_", region_sp$Species[i], "/", region_sp$Region[i], "_spp_env_params.csv")), habitat_formula = "0.1*depth + 1*sst", region_path = here::here(paste0("data/region_shapefiles_lme/", region_sp$Region[i], ".geojson")), out_file = here::here(paste0("data/vs_hab_suit_lme_", region_sp$Species[i], "/", region_sp$Region[i], "_vs_suit.rds")))
}
