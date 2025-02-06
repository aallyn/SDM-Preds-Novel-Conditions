#####
## Overview
#####

# This script makes predictions from a fitted BRT, calculates a variety of prediciton skill metrics and summarizes the environmental novelty for each prediction time step.

#####
## Libraries
#####
devtools::install_github("statsbomb/SDMTools")
library(SDMTools)
library(MLmetrics)
library(CalibratR)
library(distrEx)
library(tidyverse)
library(dismo)
library(gbm)

source(here::here("R functions/fit_brt.R"))
source(here::here("R functions/plot.gbm.aja.R"))
source("~/GitHub/ForecastingChallenge/scripts/SDM_PredValidation_Functions.R")

#####
## Model prediction validation functions
#####
cog_diff_function <- function(x) {
    cog_true <- COGravity(x = x$x, y = x$y, wt = x$Real)
    cog_true_sf <- data.frame(x = cog_true[1], y = cog_true[3]) %>%
        st_as_sf(., coords = c("x", "y"), crs = 4326)
    cog_pred <- COGravity(x = x$x, y = x$y, wt = x$Pred)
    cog_pred_sf <- data.frame(x = cog_pred[1], y = cog_pred[3]) %>%
        st_as_sf(., coords = c("x", "y"), crs = 4326)
    cog_diff <- st_distance(cog_true_sf, cog_pred_sf)
    return(cog_diff)
}
cog_lat_pred_function <- function(x) {
    cog_pred <- COGravity(x = x$x, y = x$y, wt = x$Pred)
    return(cog_pred[3])
}
cog_lon_pred_function <- function(x) {
    cog_pred <- COGravity(x = x$x, y = x$y, wt = x$Pred)
    return(cog_pred[1])
}
cog_lat_true_function <- function(x) {
    cog_true <- COGravity(x = x$x, y = x$y, wt = x$Real)
    return(cog_true[3])
}
cog_lon_true_function <- function(x) {
    cog_true <- COGravity(x = x$x, y = x$y, wt = x$Real)
    return(cog_true[1])
}
cog_function <- function(x) {
    cog_out <- COGravity(x = x$x, y = x$y, wt = x$value)
    return(cog_out)
}
calib_function <- function(x) {
    calib_out <- round(getECE(x$Real, x$Pred, n_bins = 10), 2)
    return(calib_out)
}
optim_thresh_func<- function(x, method = "max.sensitivity+specificity"){
    optim_thresh_all<- SDMTools::optim.thresh(x$Real, x$Pred, threshold = 101)
    optim_thresh_out<- optim_thresh_all[[which(names(optim_thresh_all) == method)]]

    if(length(optim_thresh_out) > 1){
        optim_thresh_out<- median(optim_thresh_out)
    }

    return(optim_thresh_out)

}
conf_mat_func <- function(x, thresh) {
    mat_out <- SDMTools::confusion.matrix(x$Real, x$Pred, threshold = thresh)
    return(mat_out)
}
sens_func <- function(x) {
    sens_out <- x[2, 2] / sum(x[, 2])
    return(sens_out)
}
spec_func <- function(x) {
    spec_out <- x[1, 1] / sum(x[, 1])
    return(spec_out)
}
pr_auc_func <- function(x) {
    pr_auc_out <- MLmetrics::PRAUC(x$Pred, x$Real)
    return(pr_auc_out)
}

f1_func<- function(x) {
    pred_pa<- ifelse(x$Pred > 0.5, "1", "0")
    f1_out <- MLmetrics::F1_Score(pred_pa, x$Real, positive = "1")
    return(f1_out)
}
rmse_func <- function(x) {
    rmse_out <- MLmetrics::RMSE(x$Pred, x$Real)
    return(rmse_out)
}
raw_diff_func <- function(x) {
    diff_out <- x$Pred - x$HSI
    return(mean(diff_out, na.rm = TRUE))
}

#####
## Regions -- always run
#####
land <- st_read(here::here("data/ne_50m_land.shp"))
lme_files <- list.files(here::here("data/region_shapefiles_lme"), full.names = TRUE)
region_dat <- data.frame("Region" = c("cc","ne"), "Region_Long" = c("California_Current", "Northeast_US_Shelf"), "File_Path" = unlist(lme_files)) %>%
    as_tibble() %>%
    mutate(., "Shapefile" = map(File_Path, st_read)) %>%
    dplyr::select(., -File_Path)

#####
## Results -- getting prediction skill stats and environmental novelty measures
#####
# Need region_dat
scenarios <- c("res", "seas")

fore_summs_list <- vector("list", length(scenarios))
fits_path <- here::here("mods/")
test_path <- here::here("data/")
regions_vec <- c("cc", "ne") # Also update 595

tally_obs <- function(true_hsi, min, max) {
    true_hsi_temp <- true_hsi[!is.na(true_hsi)]
    obs_tally <- length(true_hsi_temp[true_hsi_temp < max & true_hsi_temp >= min]) / length(true_hsi_temp)
    return(round(obs_tally, 2))
}

diff_obs <- function(true_hsi, base_tally) {
    true_hsi_temp <- true_hsi[!is.na(true_hsi)]
    obs_tally <- length(true_hsi_temp[true_hsi_temp <= 0.7 & true_hsi_temp >= 0.3]) / length(true_hsi_temp)
    diff_tally <- obs_tally - unique(base_tally)
    return(round(diff_tally, 2))
}

for(g in seq_along(scenarios)){
    scenario_use <- scenarios[g]
    season_month_df <- data.frame("Month" = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), "Season" = c("Winter", "Winter", "Spring", "Spring", "Spring", "Summer", "Summer", "Summer", "Fall", "Fall", "Fall", "Winter"))
    
    fits_root <- paste0(fits_path, "/brt_fits_lme_", scenario_use)
    test_root <- paste0(test_path, "train_test_lme_", scenario_use)
    hab_suit_root <- paste0(here::here("data/vs_hab_suit_lme"), "_", scenario_use)
    
    for(i in seq_along(regions_vec)){

        res_ind <- 1
        
        region_use <- regions_vec[i]
        shapefile_use <- region_dat$Shapefile[[which(region_dat$Region == region_use)]]
        
        fits_temp <- list.files(paste(fits_root, region_use, sep = "/"), pattern = ".rds")
        fits_temp <- fits_temp[which(grepl("Base", fits_temp))]

        hab_suit <- readRDS(paste(hab_suit_root, paste0(region_use, "_vs_suit.rds"), sep = "/"))
        suit_rasts <- raster::stack(sapply(hab_suit[1:length(hab_suit) - 1], "[[", 3))
        suit_rasts<- suit_rasts[[which(names(suit_rasts) == "X1985.01.01"):which(names(suit_rasts) == "X2019.12.01")]]
        
        if(region_use == "cc"){
            suit_df <- as.data.frame(suit_rasts, xy = TRUE) %>%
                pivot_longer(., -c(x, y), names_to = "raster.layer", values_to = "HSI") %>%
                mutate(., "raster.layer" = gsub("X", "", raster.layer)) %>%
                separate(., col = raster.layer, into = c("Year", "Month", "Day"), sep = "[.]") %>%
                mutate(., "Date" = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
                arrange(., Date) %>%
                mutate(., "Region" = region_use) %>%
                dplyr::select(., x, y, Date, Region, HSI) %>%
                mutate("Region" = plyr::revalue(Region, c("cc" = "CCS")))

            train_hsi_prop <- suit_df %>%
                drop_na(HSI) %>%
                filter(., Date >= "1985-01-01" & Date < "2004-01-01") %>%
                mutate(.,
                    "Month" = format(Date, "%m")
                ) %>%
                group_by(., Month, Region) %>%
                summarize(., "Probs05Tally_Base" = length(HSI[HSI >= 0.4 & HSI <= 0.6]) / length(HSI))
                
        }

        if(region_use == "ne"){
            suit_df <- as.data.frame(suit_rasts, xy = TRUE) %>%
                pivot_longer(., -c(x, y), names_to = "raster.layer", values_to = "HSI") %>%
                mutate(., "raster.layer" = gsub("X", "", raster.layer)) %>%
                separate(., col = raster.layer, into = c("Year", "Month", "Day"), sep = "[.]") %>%
                mutate(., "Date" = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
                arrange(., Date) %>%
                mutate(., "Region" = region_use) %>%
                dplyr::select(., x, y, Date, Region, HSI) %>%
                mutate("Region" = plyr::revalue(Region, c("ne" = "NES")))
            
            train_hsi_prop <- suit_df %>%
                drop_na(HSI) %>%
                filter(., Date >= "1985-01-01" & Date < "2004-01-01") %>%
                mutate(.,
                    "Month" = format(Date, "%m")
                ) %>%
                group_by(., Month, Region) %>%
                summarize(., "Probs05Tally_Base" = length(HSI[HSI >= 0.4 & HSI <= 0.6]) / length(HSI))
        }
      

        for(j in seq_along(fits_temp)){
            
            # Scenario label
            scen_use <- gsub("_fit.rds", "", fits_temp[j])
            mod_fit <- readRDS(paste0(paste(fits_root, region_use, sep = "/"), "/", scen_use, "_fit.rds"))
            
            while (is.null(mod_fit)) {
                # Refit the model??
                mod_fit <- fit_brt(train_data = data.frame(readRDS(paste0(paste(test_root, region_use, sep = "/"), "/", scen_use, ".rds"))[["Training"]]), predictors_vec = c("depth", "oisst_daily"), response = "Real", family = "bernoulli", tree_complexity = 3, learning_rate = 0.01, bag_fraction = 0.6)
                saveRDS(mod_fit, paste0(paste(test_root, region_use, sep = "/"), "/", scen_use, "_fit.rds"))
                print("Model updated")
            }
            
            # Get the predictions together
            pred_dat_temp <- data.frame(readRDS(paste0(paste(test_root, region_use, sep = "/"), "/", scen_use, ".rds"))[["Testing"]]) %>%
                filter(., format(Date, "%Y") < 2020) %>%
                mutate(.,
                    "Region" = region_use,
                    "Scenario" = str_extract(scen_use, "[^_]+"),
                    "ModelTrainStart" = as.Date(str_extract(scen_use, "\\d{4}-\\d{2}-\\d{2}")),
                    "ModelTrainEnd" = as.Date(str_extract(sapply(strsplit(scen_use, "to"), "[", 2), "\\d{4}-\\d{2}-\\d{2}")),
                    "ForecastHorizonLength" = lubridate::interval(ymd(ModelTrainEnd), ymd(Date)) %/% months(1),
                    "DataExtent" = time_length(lubridate::interval(as.Date(ModelTrainStart), ModelTrainEnd), "year"))
            
            # Make predictions
            pred_dat_temp <- pred_dat_temp %>%
                    mutate(., "Pred" = predict(mod_fit, newdata = (.), type = "response", n.trees = mod_fit$gbm.call$best.trees))
            pred_dat_temp$Month <- format(pred_dat_temp$Date, "%m")
            pred_dat_temp <- pred_dat_temp %>%
                left_join(., season_month_df, by = c("Month" = "Month")) %>%
                rename(., c("ForecastHorizonSeason" = "Season")) %>%
                mutate(., "Year" = format(Date, "%Y"))

            # Add in BRT fitted curve...
            brt_sst_fit <- data.frame(
                "Region" = region_use,
                "Scenario" = str_extract(scen_use, "[^_]+"),
                "ModelTrainStart" = as.Date(str_extract(scen_use, "\\d{4}-\\d{2}-\\d{2}")),
                "ModelTrainEnd" = as.Date(str_extract(sapply(strsplit(scen_use, "to"), "[", 2), "\\d{4}-\\d{2}-\\d{2}")), 
                "BRT_SST_Fit" = plot.gbm.aja(mod_fit, i.var = "oisst_daily", i.use = pred_dat_temp$oisst_daily, n.sims = 1, return.grid = TRUE, continuous.resolution = 500, type = "response")
            ) %>%
                group_by(., Region, Scenario, ModelTrainStart, ModelTrainEnd) %>%
                    nest(.key = "BRT_SST_Fit")
                 
            # Training data...
            fit_sst_summs <- data.frame(readRDS(paste0(paste(test_root, region_use, sep = "/"), "/", scen_use, ".rds"))[["Training"]])
            
            # Monthly extrapolation...
            extrap_res <- fit_sst_summs %>%
                mutate(.,
                    "Region" = region_use,
                    "Month" = format(Date, "%m"), 
                    "Scenario" = str_extract(scen_use, "[^_]+")
                ) %>%
                group_by(., Scenario, Region) %>%
                nest() 

            # Prediction data...
            pred_dat_extrap <- pred_dat_temp %>%
                group_by(., Region, Scenario, Year, Month) %>%
                nest(.key = "fore_dat")
            extrap_res <- extrap_res %>%
                left_join(., pred_dat_extrap, by = c("Region" = "Region", "Scenario" = "Scenario")) %>%
                dplyr::select(., -data, -fore_dat)
           
            # Add in average training data SST
            fit_sst_summs <- fit_sst_summs %>%
                mutate(.,
                    "Region" = region_use,
                    "Month" = format(Date, "%m"),
                    "Year" = format(Date, "%Y"),
                    "Scenario" = str_extract(scen_use, "[^_]+")
                ) %>%
                group_by(., Scenario, Region) %>%
                summarize_at(., vars("oisst_daily"), c("FitSSTMean" = mean, "FitSSTSD" = sd))
            
            fit_sst_summs <- fit_sst_summs %>%
                left_join(., extrap_res) 
                
            pred_dat_temp <- pred_dat_temp %>%
                left_join(., fit_sst_summs, by = c("Scenario" = "Scenario", "Region" = "Region", "Year" = "Year", "Month" = "Month")) %>%
                left_join(brt_sst_fit)
                
            if (res_ind == 1) {
                all_out <- pred_dat_temp
            } else {
                all_out <- bind_rows(all_out, pred_dat_temp)
            }
            
            res_ind<- res_ind + 1
        }
        
        print(paste(regions_vec[i], " is done", sep = ""))
        
        #####
        ## Visualizing results
        #####
        all_out$DataExtent <- all_out$DataExtent + 1
        
        all_out_both <- all_out
        
        all_out_both <- all_out_both %>%
            mutate(., "Region" = plyr::revalue(Region, c("cc" = "CCS", "ne" = "NES")))

        # Bring in Suitability and nest
        fore_summs <- all_out_both %>%
            left_join(., suit_df) %>%
            left_join(., train_hsi_prop) %>%
            group_by(., Region, Scenario, Month, Year, ModelTrainStart, ModelTrainEnd, FitSSTMean, FitSSTSD, BRT_SST_Fit) %>%
            nest() %>%
            arrange(., Region, Scenario, Year, Month, ModelTrainStart, ModelTrainEnd, FitSSTMean, FitSSTSD)
        
        # Calculate a sweet of statistics
        fore_summs$PredSSTMean <- as.numeric(lapply(fore_summs$data, FUN = function(x) mean(x$oisst_daily, na.rm = TRUE)))
        fore_summs$PredSSTSD <- as.numeric(lapply(fore_summs$data, FUN = function(x) sd(x$oisst_daily, na.rm = TRUE)))
        fore_summs$AvgProbs <- as.numeric(lapply(fore_summs$data, FUN = function(x) tally_obs(x$HSI, min = 0.4, max = 0.6)))
        fore_summs$HighProbs<- as.numeric(lapply(fore_summs$data, FUN = function(x) tally_obs(x$HSI, min = 0.7, max = 1)))
        fore_summs$LowProbs<- as.numeric(lapply(fore_summs$data, FUN = function(x) tally_obs(x$HSI, min = 0, max = 0.3)))
        fore_summs$Probs05Diff <- as.numeric(lapply(fore_summs$data, FUN = function(x) diff_obs(x$HSI, x$Probs05Tally_Base)))
        fore_summs$OptimThresh<- as.numeric(lapply(fore_summs$data, FUN = function(x, method = "max.sensitivity+specificity") optim_thresh_func(x, method = "max.sensitivity+specificity")))
        fore_summs$MAE_PA <- round(as.numeric(lapply(fore_summs$data, FUN = function(x) MAE(x$Pred, x$Real))), 2)
        fore_summs$MAE_HSI <- round(as.numeric(lapply(fore_summs$data, FUN = function(x) MAE(x$Pred, x$HSI))), 2)
        fore_summs$AUC <- round(as.numeric(lapply(fore_summs$data, FUN = function(x) AUC(x$Pred, x$Real))), 2)
        fore_summs$Calib <- round(as.numeric(lapply(fore_summs$data, FUN = function(x) calib_function(x))), 2)
        fore_summs$Cor <- round(as.numeric(lapply(fore_summs$data, FUN = function(x) cor(x$Pred, x$Real))), 2)

        fore_summs <- fore_summs %>%
            mutate(., "Conf_Mat" = map2(data, list(0.5), conf_mat_func))
        fore_summs$Sens <- round(as.numeric(lapply(fore_summs$Conf_Mat, FUN = function(x) sens_func(x))), 2)
        fore_summs$Spec <- round(as.numeric(lapply(fore_summs$Conf_Mat, FUN = function(x) spec_func(x))), 2)
        fore_summs$PrAUC <- round(as.numeric(lapply(fore_summs$data, FUN = function(x) pr_auc_func(x))), 2)
        fore_summs$COG_Lat_Pred <- as.numeric(lapply(fore_summs$data, FUN = function(x) cog_lat_pred_function(x)))
        fore_summs$COG_Lon_Pred <- as.numeric(lapply(fore_summs$data, FUN = function(x) cog_lon_pred_function(x)))
        fore_summs$COG_Lat_True <- as.numeric(lapply(fore_summs$data, FUN = function(x) cog_lat_true_function(x)))
        fore_summs$COG_Lon_True <- as.numeric(lapply(fore_summs$data, FUN = function(x) cog_lon_true_function(x)))
        fore_summs$TSS <- fore_summs$Sens + fore_summs$Spec - 1
        fore_summs$RMSE <- as.numeric(lapply(fore_summs$data, FUN = function(x) rmse_func(x)))
        fore_summs$RawDiff <- round(as.numeric(lapply(fore_summs$data, FUN = function(x) raw_diff_func(x))), 2)
        fore_summs$F1 <- as.numeric(lapply(fore_summs$data, FUN = function(x) f1_func(x)))
        fore_summs$Prev <- as.numeric(lapply(fore_summs$data, FUN = function(x) round(sum(x$Real) / length(x$Real), 2)))
        
        hell_dist <- vector("numeric", length = nrow(fore_summs))
        for (k in seq_along(hell_dist)) {
            hell_dist[k] <- HellingerDist(Norm(mean = fore_summs$FitSSTMean[k], sd = fore_summs$FitSSTSD[k]), Norm(mean = fore_summs$PredSSTMean[k], sd = fore_summs$PredSSTSD[k]))
        }
        fore_summs$HellDist<- hell_dist

        if(i == 1){
            fore_summs_out<- fore_summs
        } else {
            fore_summs_out<- bind_rows(fore_summs_out, fore_summs)
        }
    }
    fore_summs_list[[g]]<- fore_summs_out
}

names(fore_summs_list)<- scenarios

# Save it
saveRDS(fore_summs_list, here::here("results/fore_summs_list.rds"))
