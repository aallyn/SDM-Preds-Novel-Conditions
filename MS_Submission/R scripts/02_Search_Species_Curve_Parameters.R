#####
## Overview
#####

# This script searches for mean/sd parameters to define normal species-response curves to SST in each large marine ecosystem to facilitate simulating the distribution of a resident-mobile and seasonally-migrating warm water species archetype. To do this, we search a combination of possible mean/sd values and then keep the ones that gets closest to an average 0.5 prevalance across months/years during the baseline period (1985-2004) and then the mean/sd parameter set that also minimizes the month to month variability to produce a "resident-mobile" species archetype and another that also maximizes the month to month variability to produce a "seasonally-migrating" species archetype. 

## Load libraries and source functions
## Libraries
library(raster)
library(virtualspecies)
library(tidyverse)
library(here)

source(here::here("R functions/gen_vs_hab_suit.R"))
source(here::here("R functions/convert_vs_hab_suit.R"))

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
land <- st_read(here::here("data/ne_50m_land.shp"))

lme_files <- list.files(here::here("data/region_shapefiles_lme"), full.names = TRUE)
region_shapes <- data.frame("Region" = c("cc", "ne"), "Region_Long" = c("California_Current", "Northeast_US_Shelf"), "File_Path" = unlist(lme_files)) %>%
    as_tibble() %>%
    mutate(., "Shapefile" = map(File_Path, st_read)) %>%
    dplyr::select(., -File_Path)

#####
## Covariates
#####
sst_rast <- raster::stack(here::here("data/habitat_covs/oisst/sst.grd"))
depth_rast <- raster::stack(here::here("data/habitat_covs/depth/depth.grd"))

#####
## Loop to get average annual prevalence and variablility given each mean/SD combination
#####

# Set up stuff
# Time period to look over
base_start <- as.Date("1985-01-01")
base_end <- as.Date("2003-12-31")

# Mean/SD ranges to search over
mean_fixed<- NULL
mean_range <- NULL
mean_samps <- 20
sd_fixed<- NULL
sd_samps<- 20
sd_range<- 3
hab_formula_use<- "0.1*depth + 1*sst"

# Parameters for convert continuous habitat suitability to presence/absence -- will use rbinom
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

#####
## Processing the results to keep what we want prevalence closest to 0.5 and then min/max variability
#####
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


write.csv(scens_out, here::here("data/SppEnvCurveParams.csv"))
