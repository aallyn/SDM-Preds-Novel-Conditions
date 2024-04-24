#####
## Searching for mean/sd parameters to define normal species-response curves
#####

## What is the objective?
# We want to find the the mean and sd for a normal species-response curve to temperature in each of the large marine ecosystems, which gets closest to an average 0.5 prevalance across months/years during the baseline period (1985-2004) and one mean/sd parameter set that minimizes the month to month variability to produce a "resident" species archetype and another that maximizes the month to month variability to produce a "seasonally migrating" species archetype. 

## How can we do this -- smartly?
# There are an infinite number of possible combinations, but we have limited comp power and time. So, trying to place some bounds on the parameter space to explore.
# Means: We have already plotted the annual cycle for each of the years during the baseline period and had originally been using the mean/sd of those values for our parameterization. This got us close. Also, it's unlikely for a species to ever be in the system if its *average* preferred temperature falls at either the extreme warm or cold temperatures experienced in a region. To start then, we might think of proposing a sequence of 10 values ranging from the 0.2 to the 0.8 percentile of these temperatures. 
# SDs: I always struggle a bit with SD. Just to remind myself, assuming a normal curve: ~68% of obs are within 1 SD, 95% within 2SD and 99% within 3SD. One place to start on the minimum side might be using the "range rule" to estimate an SD. We could propose that across the systems, the most "specialized" a species can be is that it is observed within a range of 4 degrees. Going to the range rule, this would give us a minimum SD value of 1. Now, how to bound the maximum SD. Cap at SD of the system? 

## Libraries
library(tidyverse)
library(sf)
library(raster)
library(zoo)
library(lubridate)
library(gbm)
library(forecast)
library(MLmetrics)
library(PresenceAbsence)
library(viridis)
# library(facetscales)
library(plotly)
library(biscale)
library(ggforce)
library(gmRi)
library(scales)
library(geomtextpath)
library(gganimate)
library(geojsonio)
library(dsmextra)
library(distrEx)
library(patchwork)
library(binr)
library(SDMTools)
source("~/GitHub/ForecastingChallenge/scripts/SDM_PredValidation_Functions.R")
source(here::here("pipelines/sim_spp/R/gen_vs_hab_suit.R"))
source(here::here("pipelines/sim_spp/R/convert_vs_hab_suit.R"))
res_data_path <- "/Users/aallyn/Library/CloudStorage/Box-Box/RES_Data/"

## Set up stuff
norm_func <- function(x, mean, sd) {
    out <- dnorm(x, mean = mean, sd = sd)
    return(out)
}

suit_to_pa<- function(prob.raster, ...)
{
  calc(prob.raster, fun = function (x, ...)
  {
    sapply(x, FUN = function(y, ...)
    {
      if(is.na(y))
      { NA } else
      {
        rbinom(n = 1, size = 1, prob = y, ...)
      }
    }
    )
  })
}


#####
## Regions
#####
land <- st_read(here::here("data/sim_spp/ne_50m_land.shp"))

lme_files <- list.files(here::here("data/sim_spp/region_shapefiles_lme"), full.names = TRUE)
region_shapes <- data.frame("Region" = c("cc", "goa", "ne", "seaus", "wcentaus"), "Region_Long" = c("California_Current", "Gulf_of_Alaska", "Northeast_US_Shelf", "Southeast_Australia", "West_Central_Australia"), "File_Path" = unlist(lme_files)) %>%
    as_tibble() %>%
    mutate(., "Shapefile" = map(File_Path, st_read)) %>%
    dplyr::select(., -File_Path)
region_shapes <- region_shapes[c(1, 3),]

# lme_files <- list.files(here::here("data/sim_spp/region_shapefiles_lme"), full.names = TRUE)[[3]]
# region_shapes <- data.frame("Region" = c("ne"), "Region_Long" = c("Northeast_US_Shelf"), "File_Path" = unlist(lme_files)) %>%
#     as_tibble() %>%
#     mutate(., "Shapefile" = map(File_Path, st_read)) %>%
#     dplyr::select(., -File_Path)

#####
## Covariates
#####
sst_rast <- raster::stack(here::here("data/sim_spp/habitat_covs/oisst/sst.grd"))
depth_rast <- raster::stack(here::here("data/sim_spp/habitat_covs/depth/depth.grd"))

#####
## Loop time??
#####

# Set up stuff
base_start <- as.Date("1985-01-01")
base_end <- as.Date("2003-12-31")
# mean_fixed <- 15.283
# mean_range <- NULL
# mean_samps <- 10
# sd_fixed <- 4.785
# sd_samps <- 10
# sd_range<- NULL

mean_fixed<- NULL
mean_range <- c(0.1, 0.9)
mean_samps <- 20
sd_fixed<- NULL
sd_samps<- 20
sd_range<- 3
hab_formula_use<- "depth + sst"

# Convert PA params -- will use rbinom
convert_pa_params_use<- data.frame("PA.method" = "custom", "beta" = "NULL", "alpha" = "NULL", "species.prevalence" = "NULL", "plot" = "NULL")

res_out_all<- vector("list", length(lme_files))

for(i in seq_along(region_shapes$Region)){

    # Going to need to get the mean and sd info for the region based on its shape and the baseline period...
    # Subset global/full time series
    mask_use <- region_shapes$Shapefile[region_shapes$Region == region_shapes$Region[i]][[1]]
    sst_ind_keep <- which(format(as.Date(names(sst_rast), format = "X%Y.%m.%d")) >= format(as.Date(paste0("X", base_start), format = "X%Y-%m-%d")) & format(as.Date(names(sst_rast), format = "X%Y.%m.%d")) < format(as.Date(paste0("X", base_end), format = "X%Y-%m-%d")))
    sst_temp <- raster::mask(sst_rast[[sst_ind_keep]], mask_use)
    sst_temp <- crop(sst_temp, st_bbox(mask_use))

    if (is.null(mean_fixed)) {
        sst_quants <- as.numeric(quantile(getValues(sst_temp), na.rm = TRUE), probs = mean_range)
        sst_means <- seq(from = min(sst_quants), to = max(sst_quants), length.out = mean_samps)
    } else {
        sst_means <- seq(from = (mean_fixed - 2.5), to = (mean_fixed + 2.5), length.out = mean_samps)
    }
    
    if (is.null(sd_fixed)) {
        sst_sds <- seq(from = 0.1, to = sd(getValues(sst_temp), na.rm = TRUE) + sd_range, length.out = sd_samps)
    } else {
        sst_sds <- seq(from = (sd_fixed - 1.5), to = sd_fixed + 1.5, length.out = sd_samps)
    }
    
    sst_param_grid <- expand.grid("Mean" = sst_means, "SD" = sst_sds)

    depth_rast_temp <- raster::mask(depth_rast, mask = mask_use)
    depth_rast_temp<- crop(depth_rast_temp, st_bbox(mask_use))
    depth_mean <- as.numeric(mean(getValues(depth_rast_temp), na.rm = TRUE))
    depth_sd <- sd(getValues(depth_rast_temp), na.rm = TRUE) / 3
    
    # Set up results object
    res_out <- data.frame(sst_param_grid, "Prev" = rep(NA, nrow(sst_param_grid)), "Var" = rep(NA, nrow(sst_param_grid)))

    # Set up habitat list stack
    hab_list <- list("depth" = depth_rast_temp, "sst" = sst_temp)

    # Run through each...
    for(j in 1:nrow(res_out)){

        # Format vs functions based on proposed mean/sd
        vs_params <- format_vs_funcs(data.frame("Region" = rep(region_shapes$Region[i], 2), "variable" = c("depth", "sst"), "function_name" = rep("norm_func", 2), "mean" = c(depth_mean, res_out$Mean[j]), "sd" = c(depth_sd, res_out$SD[j])))
        # vs_params <- format_vs_funcs(data.frame("Region" = rep(region_shapes$Region[i], 1), "variable" = c("sst"), "function_name" = rep("norm_func", 1), "mean" = c(res_out$Mean[j]), "sd" = c(res_out$SD[j])))

        # Habitat suitability
        hab_suit <- sim_vs_hab_suit(habitat_list = hab_list, vs_params = vs_params, habitat_formula = hab_formula_use, vs_rescale = FALSE, vs_species.type = NULL, vs_rescale.each.response = FALSE, vs_plot = FALSE)

        # To a stack
        suitab_rasts <- rast(lapply(hab_suit, "[[", c("suitab.raster"))[1:228])

        res_temp <- data.frame("Name" = names(suitab_rasts), "Prev" = rep(NA, nlyr(suitab_rasts)))
        res_temp$Date <- as.Date(res_temp$Name, format = "X%Y.%m.%d")
        res_temp$Month<- format(res_temp$Date, "%m")

        # Plot
        if (FALSE) {
            suit_rasts_plot <- suitab_rasts[[(nlayers(suitab_rasts) - 12):nlayers(suitab_rasts)]]

            suit_df <- as.data.frame(suit_rasts_plot, xy = TRUE) %>%
                pivot_longer(., -c(x, y), names_to = "raster.layer", values_to = "value") %>%
                mutate(., "raster.layer" = gsub("X", "", raster.layer)) %>%
                separate(., col = raster.layer, into = c("Year", "Month", "Day"), sep = "[.]") %>%
                mutate(., "Date" = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
                arrange(., Date)

            bbox <- st_bbox(mask_use)

            plot_suit_temp <- ggplot() +
                geom_raster(data = suit_df, aes(x = x, y = y, fill = value), na.rm = TRUE) +
                scale_fill_viridis(name = "Habitat suitability", na.value = "transparent") +
                geom_sf(data = land, color = "#d9d9d9") +
                coord_sf(xlim = bbox[c(1, 3)], ylim = bbox[c(2, 4)]) +
                xlab("Longitude") +
                ylab("Latitude") +
                theme_bw() +
                facet_wrap(~Month) +
                theme(
                    strip.background = element_rect(colour = NA, fill = NA),
                    strip.text = element_text(size = 16, face = "bold")
                )
        }
        
        rbinom_fun <- function(i) {
            rbinom(prob = i, n = 1, size = 1)
            #rbinom(i, n = 1, size = 1)
        }

        for(k in 1:nlyr(suitab_rasts)){
            pa_out_temp <- raster::calc(as(suitab_rasts[[k]], "Raster"), rbinom_fun)
            # pa_out_temp <- terra::app(suitab_rasts[[k]], rbinom_fun)
            res_temp1s <- length(pa_out_temp[pa_out_temp == 1, ])
            res_temp0s <- length(pa_out_temp[pa_out_temp == 0, ])
            res_temp$Prev[k] <- res_temp1s / (res_temp1s + res_temp0s)
        }

        # Calculate average prevalence -- at each time step, get the proportion of sites occupied (# pres/total cells), then get average
        month_pa <- res_temp %>%
            group_by(., Month) %>%
            summarize(., "Mean" = mean(Prev), "SD" = sd(Prev))
        
        res_out$Prev[j] <- mean(month_pa$Mean)
        res_out$Var[j]<- sd(month_pa$Mean)
    }
    res_out_all[[i]]<- res_out
}

## What do we want to keep?
# Find value closest to 0.5 PREV with the LOWEST VAR = Resident
# Find value closest to 0.5 PREV with the HIGHEST VAR and HIGHEST Mean = Leading seasonal
# Find value closest to 0.5 PREV with the HIGHEST VAR and LOWEST Mean = Trailing seasonal

names(res_out_all)<- region_shapes$Region
# Get to a nested dataframe...
res_dat <- dplyr::bind_rows(res_out_all, .id = "Region") 

# Going to want one column for prevalence closest to 0.5. Then add some rank columns and sum em up to get "best". Filter, keep ones only from 0.4 to 0.6
res_dat_temp <- res_dat %>%
    mutate(., "Prev_Diff" = abs(res_dat$Prev - 0.5)) %>%
    filter(., Prev_Diff <= 0.1)

# Rank the Prev_Diff
rank_diff_func <- function(data) {
    rank <- rank(data$Prev_Diff)
    return(rank)
}

res_dat_grp <- res_dat %>%
    group_by(Region) %>%
    nest() %>%
    mutate(., "Prev_Rank" = map(data, rank_diff_func))

rank_varmin_func <- function(data) {
    rank <- rank(data$Var)
    return(rank)
}

rank_varmax_func<- function(data) {
    rank <- rank(-data$Var)
    return(rank)
}

rank_meanmax_func<- function(data) {
    rank <- rank(-data$Mean)
    return(rank)
}

rank_meanmin_func <- function(data) {
    rank <- rank(data$Mean)
    return(rank)
}

res_dat_grp <- res_dat_grp %>%
    mutate(.,
        "VarMin_Rank" = map(data, rank_varmin_func),
        "VarMax_Rank" = map(data, rank_varmax_func),
        "MeanMax_Rank" = map(data, rank_meanmax_func),
        "MeanMin_Rank" = map(data, rank_meanmin_func)
    )

# Overall ranks...
resi_rank<- function(prev_rank, varmin_rank){
    resi_rank <- prev_rank + varmin_rank
    return(resi_rank)
}

seas_warm_rank<- function(prev_rank, varmax_rank, meanmax_rank){
    seas_warm_rank <- prev_rank + varmax_rank + meanmax_rank
    return(seas_warm_rank)
}

seas_cool_rank<- function(prev_rank, varmax_rank, meanmin_rank){
    seas_cool_rank <- prev_rank + varmax_rank + meanmin_rank
    return(seas_cool_rank)
}

res_dat_grp <- res_dat_grp %>%
    mutate(.,
        "Res_Ranks" = pmap(list(Prev_Rank, VarMin_Rank), resi_rank),
        "SeasWarm_Rank" = pmap(list(Prev_Rank, VarMax_Rank, MeanMax_Rank), seas_warm_rank),
        "SeasCold_Rank" = pmap(list(Prev_Rank, VarMax_Rank, MeanMin_Rank), seas_cool_rank),
    )

# Get minimums....
res_dat_grp <- res_dat_grp %>%
    mutate(.,
        "ResID" = map_dbl(Res_Ranks, which.min),
        "SeasWarmID" = map_dbl(SeasWarm_Rank, which.min),
        "SeasColdID" = map_dbl(SeasCold_Rank, which.min)
    )

for(i in seq_along(res_dat_grp$Region)){
    row_use <- res_dat_grp$ResID[i]
    scens_out_temp <- data.frame("Region" = res_dat_grp$Region[[i]], "Scenario" = "Res", "Prevalence" = round(res_dat_grp$data[[i]]$Prev[row_use], 3), "Prevalence_Var" = round(res_dat_grp$data[[i]]$Var[row_use], 3), "Mean" = round(res_dat_grp$data[[i]]$Mean[row_use], 3), "SD" = round(res_dat_grp$data[[i]]$SD[row_use], 3))
    if(i == 1){
        scens_out<- scens_out_temp
    } else {
        scens_out<- bind_rows(scens_out, scens_out_temp)
    }
}   

for(i in seq_along(res_dat_grp$Region)){
    row_use <- res_dat_grp$SeasWarmID[i]
    scens_out_temp <- data.frame("Region" = res_dat_grp$Region[[i]], "Scenario" = "SeasWarm", "Prevalence" = round(res_dat_grp$data[[i]]$Prev[row_use], 3), "Prevalence_Var" = round(res_dat_grp$data[[i]]$Var[row_use], 3), "Mean" = round(res_dat_grp$data[[i]]$Mean[row_use], 3), "SD" = round(res_dat_grp$data[[i]]$SD[row_use], 3))
    scens_out<- bind_rows(scens_out, scens_out_temp)
}   

for(i in seq_along(res_dat_grp$Region)){
    row_use <- res_dat_grp$SeasColdID[i]
    scens_out_temp <- data.frame("Region" = res_dat_grp$Region[[i]], "Scenario" = "SeasCold", "Prevalence" = round(res_dat_grp$data[[i]]$Prev[row_use], 3), "Prevalence_Var" = round(res_dat_grp$data[[i]]$Var[row_use], 3), "Mean" = round(res_dat_grp$data[[i]]$Mean[row_use], 3), "SD" = round(res_dat_grp$data[[i]]$SD[row_use], 3))
    scens_out<- bind_rows(scens_out, scens_out_temp)
}  


write.csv(scens_out, here::here("data/sim_spp/SppEnvCurveParams.csv"))
