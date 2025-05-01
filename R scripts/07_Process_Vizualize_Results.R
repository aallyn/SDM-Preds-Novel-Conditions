#####
## Libraries
#####
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
# library(dsmextra)
library(distrEx)
library(patchwork)
library(binr)
library(SDMTools)
library(broom)
library(ggpmisc)
library(modelsummary)
library(gt)
library(webshot2)
library(CalibratR)
source("~/GitHub/ForecastingChallenge/scripts/SDM_PredValidation_Functions.R")
res_data_path <- "/Users/aallyn/Library/CloudStorage/Box-Box/RES_Data/"
proj_box_path<- "/Users/aallyn/Library/CloudStorage/Box-Box/Mills Lab/Projects/NASA_UNSDG19/"

season_month_df <- data.frame("Month" = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), "Season" = c("Winter", "Winter", "Spring", "Spring", "Spring", "Summer", "Summer", "Summer", "Fall", "Fall", "Fall", "Winter"))

colors_use<- c('#7570b3','#1b9e77', '#e7298a','#d95f02')


#####
## Species response functions
#####
norm_func <- function(x, mean, sd) {
    out <- dnorm(x, mean = mean, sd = sd)
    return(out)
}

#####
## Regions -- always run
#####
land <- st_read(here::here("data/ne_50m_land.shp"))
lme_files <- list.files(here::here("data/region_shapefiles_lme"), full.names = TRUE)
region_dat <- data.frame("Region" = c("cc", "ne"), "Region_Long" = c("California_Current", "Northeast_US_Shelf"), "File_Path" = unlist(lme_files)) %>%
    as_tibble() %>%
    mutate(., "Shapefile" = map(File_Path, st_read)) %>%
    dplyr::select(., -File_Path)

#####
## Global sea surface temperature patterns 
## !! WARNING THIS TAKES A WHILE TO RUN!!!!
#####
# Global time series and SOM??
# oisst_path <- "/Users/aallyn/Library/CloudStorage/Box-Box/RES_Data/OISST/oisst_mainstays/regional_timeseries/large_marine_ecosystems/"
# oisst_all <- list.files(oisst_path, full.names = TRUE)
# cc_sst <- read.csv(oisst_all[which(grepl("california_current", oisst_all))]) %>%
#     mutate(., "Year" = format(as.Date(time), "%Y")) %>%
#     group_by(., Year) %>%
#     summarize_at(., "area_wtd_anom", mean) %>%
#     mutate(.,
#         "Year_Plot" = as.numeric(Year),
#         "Region" = rep("CCS")
#     )
# ne_sst <- read.csv(oisst_all[which(grepl("northeast_us_continental_shelf", oisst_all))]) %>%
#     mutate(., "Year" = format(as.Date(time), "%Y")) %>%
#     group_by(., Year) %>%
#     summarize_at(., "area_wtd_anom", mean) %>%
#     mutate(.,
#         "Year_Plot" = as.numeric(Year),
#         "Region" = rep("NES")
#     )

# sst_anom <- bind_rows(cc_sst, ne_sst) %>%
#     mutate(., "Region" = factor(Region, levels = c("CCS", "NES"), labels = c("California Current", "Northeast US Continental Shelf"))) %>%
#     filter(., Year_Plot >= 1985 & Year_Plot <= 2020)
# sst_anom$DataSubset <- ifelse(sst_anom$Year_Plot <= 2004, "A", "B")

# breaks_use <- c(1985, 1995, 2005, 2015)
# limits_use <- c(1985, 2020)

# ## Monthly instead?
# cc_sst <- read.csv(oisst_all[which(grepl("california_current", oisst_all))]) %>%
#         mutate(., "Year" = format(as.Date(time), "%Y")) %>%
#         group_by(., Year) %>%
#         summarize(., "area_wtd_anom_mean" = mean(area_wtd_anom),
#         "area_wtd_anom_sd" = sd(area_wtd_anom)) %>%
#         mutate(.,
#             "Year_Plot" = as.numeric(Year),
#             "Region" = rep("CCS")
#         )

# ne_sst<- read.csv(oisst_all[which(grepl("northeast_us_continental_shelf", oisst_all))]) %>%
#         mutate(., "Year" = format(as.Date(time), "%Y")) %>%
#         group_by(., Year) %>%
#         summarize(., "area_wtd_anom_mean" = mean(area_wtd_anom),
#         "area_wtd_anom_sd" = sd(area_wtd_anom)) %>%
#         mutate(.,
#             "Year_Plot" = as.numeric(Year),
#             "Region" = rep("NES")
#         )

# sst_anom_plot_dat <- bind_rows(cc_sst, ne_sst) %>%
#     mutate(., "Year" = as.Date(Year, "%Y")) %>%
#     filter(., Year >= 1985) |>
#     mutate(Time_Group = ifelse(Year_Plot <= 2004, "A", "B"))

# sst_anom_plot_dat$Region <- factor(sst_anom_plot_dat$Region, levels = c("CCS", "NES"), labels = c("CC", "NES"))

# sst_anom_plot <- ggplot() +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "#d9d9d9", lwd = 1) +
#     geom_errorbar(data = subset(sst_anom_plot_dat, Year_Plot < 2021 & Year_Plot >= 1985), aes(x = Year, ymin = area_wtd_anom_mean - area_wtd_anom_sd, ymax = area_wtd_anom_mean + area_wtd_anom_sd, color = Region), lwd = 1, alpha = 0.5) +
#     geom_line(data = subset(sst_anom_plot_dat, Year_Plot < 2021 & Year_Plot >= 1985), aes(x = Year, y = area_wtd_anom_mean, color = Region), lwd = 1) +
#     geom_point(data = subset(sst_anom_plot_dat, Year_Plot < 2021 & Year_Plot >= 1985), aes(x = Year, y = area_wtd_anom_mean, fill = Region), pch = 21, size = 2) +
#     stat_poly_line(data = subset(sst_anom_plot_dat, Year_Plot < 2021 & Year_Plot >= 2004), aes(x = Year, y = area_wtd_anom_mean, color = Region), lty = "dashed") +
#     stat_poly_eq(data = subset(sst_anom_plot_dat, Year_Plot < 2021 & Year_Plot >= 2004), aes(x = Year, y = area_wtd_anom_mean, group = Region, label = paste(stat(eq.label), stat(rr.label), sep = "*\", \"*"))) +
#     scale_fill_manual(name = "Large marine ecosystem", values = colors_use) +
#     scale_fill_manual(name = "Large marine ecosystem", values = colors_use) +
#     scale_color_manual(name = "Large marine ecosystem", values = colors_use) +
#     ylab("SST Anomaly\n from 1982-2011 baseline") +
#     xlab("Year") +
#     facet_wrap(~Region) +
#     # geom_rect(data = sst_anom_plot_dat, inherit.aes = FALSE, aes(xmin = as.Date("2003-12-15"), xmax = as.Date("2021-02-15"), ymin = -1.55, ymax = 2.6, color = Region), fill = NA) +
#     # geom_segment(data = sst_anom_plot_dat, inherit.aes = FALSE, aes(x = as.Date("2003-12-15"), xend = as.Date("2000-01-01"), y = -1.55, yend = -3.5, color = Region), arrow = arrow(length = unit(0.5, "cm"))) +
#     # coord_cartesian(ylim = c(-1.75, 2.75), clip="off") +
#     theme_bw(base_size = 16) +
#     theme(
#         plot.margin = unit(c(1.2,1.2,1.2,1.2), "lines"),
#         legend.position = "none",
#         strip.background = element_blank(),
#         strip.text = element_text(size = 18)
#     )

# ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "SST_MonthlyAnomalies", ".jpg"), height = 8, width = 11, dpi = 300, sst_anom_plot)


# Differences in temperatures
# str(sst_anom_plot_dat)

#####
## SST Differences -- how much warming during the testing period?
#####
# Training SSTs
cc_train <- readRDS(here::here(paste0("data/train_test_lme_res/cc/Base_1985-01-01_to_2004-01-01.rds")))[[1]] |>
    mutate("Region" = factor("California Current", levels = c("California Current", "Northeast U.S. Shelf")))
nes_train <- readRDS(here::here(paste0("data/train_test_lme_res/ne/Base_1985-01-01_to_2004-01-01.rds")))[[1]] |>
    mutate("Region" = factor("Northeast U.S. Shelf", levels = c("California Current", "Northeast U.S. Shelf")))

# Testing SSTs
cc_test <- readRDS(here::here(paste0("data/train_test_lme_res/cc/Base_1985-01-01_to_2004-01-01.rds")))[[2]] |>
    mutate("Region" = factor("California Current", levels = c("California Current", "Northeast U.S. Shelf")))
nes_test <- readRDS(here::here(paste0("data/train_test_lme_res/ne/Base_1985-01-01_to_2004-01-01.rds")))[[2]] |>
    mutate("Region" = factor("Northeast U.S. Shelf", levels = c("California Current", "Northeast U.S. Shelf")))

# Combine
cc_sst <- bind_rows(cc_train, cc_test) |>
    filter(Date < "2020-01-01")
ne_sst<- bind_rows(nes_train, nes_test) |>
    filter(Date < "2020-01-01")

# Climatologies...
cc_clim <- cc_sst |>
    filter(between(Date, as.Date("1985-01-01"), as.Date("2003-12-31"))) |>
    mutate(Month = format(Date, "%m")) |>
    group_by(x, y, Month) |>
    summarize("oisst_clim" = mean(oisst_daily))

cc_anom <- cc_sst |>
    filter(between(Date, as.Date("2004-01-01"), as.Date("2019-12-31"))) |>
    mutate(Month = format(Date, "%m")) |>
    left_join(cc_clim) |>
    mutate(oisst_anom = oisst_daily - oisst_clim) |>
    group_by(Date) |>
    summarize("Monthly_Mean_Anom" = mean(oisst_anom)) |>
    mutate(Year = format(Date, "%y")) |>
    group_by(Year) |>
    summarize("Yearly_Mean_Anom" = mean(Monthly_Mean_Anom))
mean(cc_anom$Yearly_Mean_Anom)

# Climatologies...
ne_clim <- ne_sst |>
    filter(between(Date, as.Date("1985-01-01"), as.Date("2003-12-31"))) |>
    mutate(Month = format(Date, "%m")) |>
    group_by(x, y, Month) |>
    summarize("oisst_clim" = mean(oisst_daily))

ne_anom <- ne_sst |>
    filter(between(Date, as.Date("2004-01-01"), as.Date("2019-12-31"))) |>
    mutate(Month = format(Date, "%m")) |>
    left_join(ne_clim) |>
    mutate(oisst_anom = oisst_daily - oisst_clim) |>
    group_by(Date) |>
    summarize("Monthly_Mean_Anom" = mean(oisst_anom)) |>
    mutate(Year = format(Date, "%y")) |>
    group_by(Year) |>
    summarize("Yearly_Mean_Anom" = mean(Monthly_Mean_Anom))
ggplot() +
    geom_point(data = ne_anom, aes(x = Year, y = Yearly_Mean_Anom)) +
    geom_smooth(data = ne_anom, aes(x = Year, y = Yearly_Mean_Anom), formula = y ~ x, method = "lm", se = TRUE) +
    stat_poly_eq(formula = y ~ x, 
        label.x = "left",
        eq.with.lhs = "italic(hat(y))~`=`~",
        aes(label = paste(..eq.label.., sep = "~~~")), parse = TRUE)
mean(ne_anom$Yearly_Mean_Anom)

# Simpler?
cc_base <- cc_sst |>
    filter(between(Date, as.Date("1985-01-01"), as.Date("2003-12-31"))) |>
    summarize(Mean_SST = mean(oisst_daily))
cc_fut <- cc_sst |>
    filter(between(Date, as.Date("2004-01-01"), as.Date("2019-12-31"))) |>
    summarize(Mean_SST = mean(oisst_daily))
cc_fut - cc_base

ne_base <- ne_sst |>
    filter(between(Date, as.Date("1985-01-01"), as.Date("2003-12-31"))) |>
    summarize(Mean_SST = mean(oisst_daily))
ne_fut <- ne_sst |>
    filter(between(Date, as.Date("2004-01-01"), as.Date("2019-12-31"))) |>
    summarize(Mean_SST = mean(oisst_daily))
ne_fut - ne_base

#####
## Response curves
## !! WARNING THIS TAKES A WHILE TO RUN!!!!
#####
sst_rast <- raster::stack(here::here("data/habitat_covs/oisst/sst.grd"))
depth_rast <- raster::stack(here::here("data/habitat_covs/depth/depth.grd"))
base_years <- data.frame("Region" = region_dat$Region, "Start_Year_Base" = rep(as.Date("1985-01-01"), length(unique(region_dat$Region))), "End_Year_Base" = rep(as.Date("2004-01-01"), length(unique(region_dat$Region))), "Start_Year_Fore" = c(as.Date("2004-01-01"), as.Date("2004-01-01")), "End_Year_Fore" = c(as.Date("2020-01-01"), as.Date("2020-01-01")))

region_dat <- region_dat %>%
    left_join(., base_years)

## Read in species parameters
spp_params <- read.csv(here::here("data/SppEnvCurveParams.csv"))
spp_params_t_means <- spp_params %>%
    dplyr::select(., Region, Scenario, Mean) %>%
    pivot_wider(., names_from = Scenario, values_from = Mean)
names(spp_params_t_means)[c(2:4)] <- paste0(names(spp_params_t_means)[c(2:4)], "_Mean")

spp_params_t_sd <- spp_params %>%
    dplyr::select(., Region, Scenario, SD) %>%
    pivot_wider(., names_from = Scenario, values_from = SD)
names(spp_params_t_sd)[c(2:4)] <- paste0(names(spp_params_t_sd)[c(2:4)], "_SD")
spp_params_t <- spp_params_t_means %>%
    left_join(., spp_params_t_sd)

region_dat <- region_dat %>%
    left_join(., spp_params_t)

# Make the curves
spp_resp_curve<- function(rast, region, mask_use, start_year, end_year, mean_use, sd_use, sd_div = NULL, n_samps = 1000){
    
    if(FALSE){
        rast = sst_rast
        region = region_dat$Region_Long[[1]]
        mask_use = region_dat$Shapefile[[1]]
        start_year = region_dat$Start_Year_Base[[1]]
        end_year = region_dat$End_Year_Base[[1]]
        mean_use = region_dat$Res_Mean[[1]]
        sd_use = region_dat$Res_SD[[1]]
        sd_div = NULL
        n_samps = 1000
    }
    # Output
    out_list <- vector("list", length = 2)
    names(out_list) <- c("Vals", "Plot")
    
    # Subset global/full time series for raster stack
    if(is.null(sd_div)){
        if (nlayers(rast) > 1) {
                rast_ind_keep <- which(names(rast) >= paste0("X", start_year) & names(rast) < paste0("X", end_year))
                rast_temp <- raster::mask(rast[[rast_ind_keep]], mask_use)
                ext_temp <- extent(st_bbox(mask_use))
                new_bbox <- ext_temp
                rast_temp <- raster::crop(rast_temp, new_bbox)
                mean_base <- as.numeric(mean(getValues(rast_temp), na.rm = TRUE))
                sd_base <- sd(getValues(rast_temp), na.rm = TRUE)
                min_base <- as.numeric(min(getValues(rast_temp), na.rm = TRUE))
                max_base <- as.numeric(max(getValues(rast_temp), na.rm = TRUE))
                } else {
                rast_temp <- raster::mask(rast, mask_use)
                ext_temp <- extent(st_bbox(mask_use))
                new_bbox <- ext_temp
                rast_temp <- raster::crop(rast_temp, new_bbox)
                mean_base <- as.numeric(mean(getValues(rast_temp), na.rm = TRUE))
                sd_base <- sd(getValues(rast_temp), na.rm = TRUE)
                min_base <- as.numeric(min(getValues(rast_temp), na.rm = TRUE))
                max_base<- as.numeric(max(getValues(rast_temp), na.rm = TRUE))
            }
            
            } else {
            rast_temp <- raster::mask(rast, mask_use)
            mean_use <- as.numeric(mean(getValues(rast_temp), na.rm = TRUE))
            sd_use <- sd(getValues(rast_temp), na.rm = TRUE) / sd_div
            mean_base <- mean_use
            sd_base <- sd_use
            min_base <- as.numeric(min(getValues(rast_temp), na.rm = TRUE))
            max_base<- as.numeric(max(getValues(rast_temp), na.rm = TRUE))
        }
        
        # Get range in values
        mean_curves <- data.frame("MeanVar" = runif(n_samps, min = min(getValues(rast_temp), na.rm = TRUE), max = max(getValues(rast_temp), na.rm = TRUE)))

        # Get mean and sd, then run through normal function
        out_list[[1]] <- data.frame("Mean" = mean_base, "SD" = sd_base, "Min" = min_base, "Max" = max_base)

        # Create curve information
        mean_curves <- mean_curves %>%
            mutate(.,
                "NormPred" = pmap_dbl(list(x = MeanVar, mean = mean_use, sd = sd_use), norm_func),
                "NormPred" = scales::rescale(NormPred, to = c(0, 1))
            )
        
        # Dataframe
        mean_plot <- mean_curves %>%
            pivot_longer(., -MeanVar, names_to = "Function", values_to = "Prediction")
        mean_plot$Function <- factor(mean_plot$Function, levels = c("NormPred"))
        mean_plot$Region <- region
        
        # Return it
        out_list[[2]]<- mean_plot

        # Return it
        return(out_list)
    }
    
region_dat <- region_dat %>%
    mutate(.,
        "Depth_Curve_Dat" = pmap(list(rast = list(depth_rast), region = Region_Long, mask_use = Shapefile, start_year = Start_Year_Base, end_year = End_Year_Base, mean_use = list(NULL), sd_use = list(NULL), sd_div = list(3), n_samps = 1000), spp_resp_curve),
        "SST_Curve_Dat_Res" = pmap(list(rast = list(sst_rast), region = Region_Long, mask_use = Shapefile, start_year = Start_Year_Base, end_year = End_Year_Base, mean_use = Res_Mean, sd_use = Res_SD, sd_div = list(NULL), n_samps = 1000), spp_resp_curve),
        "SST_Curve_Dat_Seas" = pmap(list(rast = list(sst_rast), region = Region_Long, mask_use = Shapefile, start_year = Start_Year_Base, end_year = End_Year_Base, mean_use = SeasWarm_Mean, sd_use = SeasWarm_SD, sd_div = list(NULL), n_samps = 1000), spp_resp_curve)
    )

## Plot the depth curves...
all_depth_dat <- do.call(rbind.data.frame, lapply(region_dat$Depth_Curve_Dat, `[[`, 2)) %>%
    arrange(., Region, MeanVar)

# Bring over depth mean/sd...
join_dat <- do.call(rbind.data.frame, lapply(region_dat$Depth_Curve_Dat, `[[`, 1)) %>%
    mutate(., "Region_Long" = unique(all_depth_dat$Region))
all_depth_dat <- all_depth_dat %>%
    left_join(., join_dat, by = c("Region" = "Region_Long"))
all_depth_dat$Region <- factor(all_depth_dat$Region, levels = c("California_Current", "Northeast_US_Shelf"), labels = c("CC", "NES"))

# Filtering a bit because of the tails...
all_depth_dat <- all_depth_dat %>%
    filter(., Region == "CC" & MeanVar > 1500 | Region == "NES" & MeanVar < 425)


depth_out <- ggplot() +
        geom_path(data = all_depth_dat, aes(x = MeanVar, y = Prediction, group = Region), lwd = 2, color = "gray20") +
        # geom_label(data = subset(all_depth_dat, all_depth_dat$Region == "CC"), aes(x = 4500, y = 1.15, label = paste0("Mean = ", round(Mean, 0), "\nSD = ", round(SD, 0))), label.size = NA, size = 6) +
        # geom_label(data = subset(all_depth_dat, all_depth_dat$Region == "NES"), aes(x = 350, y = 1.15, label = paste0("Mean = ", round(Mean, 0), "\nSD = ", round(SD, 0))), label.size = NA, size = 6) +
        ylab("Habitat suitability") +
        xlab("Water depth (m)") +
        ylim(c(0, 1)) +
        theme_bw(base_size = 18) +
        facet_wrap(~ Region, scales = "free_x") +
        ggtitle("Depth species-response curves") +
        theme(
            strip.background = element_rect(colour = NA, fill = NA),
            strip.text = element_text(size = 28, face = "bold"),
            plot.caption = element_text(hjust = 0)
        )
ggsave(here::here("results/depth_curve_res.jpg"), plot = depth_out, height = 8, width = 11, dpi = 300)

## Now the SST curves. This is a bit more complicated because we want to add in an error bar that represents the "future" conditions the model is going to be extrapolated, too. For each month, what if we grab the min/max/mean temps.
# A function to help us subset...
get_sst_dat <- function(sst_stack, mask_use, start_year, end_year) {

    # Subset global/full time series
    sst_ind_keep <- which(format(as.Date(names(sst_stack), format = "X%Y.%m.%d")) >= format(as.Date(paste0("X", start_year), format = "X%Y-%m-%d")) & format(as.Date(names(sst_stack), format = "X%Y.%m.%d")) < format(as.Date(paste0("X", end_year), format = "X%Y-%m-%d")))
    sst_temp <- raster::mask(sst_stack[[sst_ind_keep]], mask_use)

    # Convert to a dataframe
    sst_df <- as.data.frame(sst_temp, xy = TRUE) %>%
        pivot_longer(., -c("x", "y"), names_to = "Date", values_to = "SST")

    # Return it
    return(sst_df)
}

get_fore_summ <- function(df, stat) {
    df_temp <- df %>%
        mutate(., "Month" = format(as.Date(Date, format = "X%Y.%m.%d"), "%m"))


    if (stat == "mean") {
        df_out <- df_temp %>%
            group_by(., Month) %>%
            summarize_at(., "SST", mean, na.rm = TRUE)
    }

    if (stat == "sd") {
        df_out <- df_temp %>%
            group_by(., Month) %>%
            summarize_at(., "SST", sd, na.rm = TRUE)
    }

    if (stat == "min") {
        df_out <- df_temp %>%
            group_by(., Month) %>%
            summarize_at(., "SST", min, na.rm = TRUE)
    }

    if (stat == "max") {
        df_out <- df_temp %>%
            group_by(., Month) %>%
            summarize_at(., "SST", max, na.rm = TRUE)
    }

    return(df_out)
}

region_dat <- region_dat %>%
    mutate(.,
        "Fore_SST_DF" = pmap(list(sst_stack = list(sst_rast), mask_use = Shapefile, start_year = Start_Year_Fore, end_year = End_Year_Fore), get_sst_dat),
        "Fore_SST_Mean" = map2(Fore_SST_DF, list("mean"), get_fore_summ),
        "Fore_SST_SD" = map2(Fore_SST_DF, list("sd"), get_fore_summ),
        "Fore_SST_Min" = map2(Fore_SST_DF, list("min"), get_fore_summ),
        "Fore_SST_Max" = map2(Fore_SST_DF, list("max"), get_fore_summ)
    )

all_sst_dat <- do.call(rbind.data.frame, lapply(region_dat$SST_Curve_Dat_Res, `[[`, 2)) %>%
    arrange(., Region, MeanVar)

# Bring over sst mean/sd...
join_dat <- do.call(rbind.data.frame, lapply(region_dat$SST_Curve_Dat_Res, `[[`, 1)) %>%
    mutate(., "Region_Long" = unique(all_sst_dat$Region))
    
all_sst_dat <- all_sst_dat %>%
    left_join(., join_dat, by = c("Region" = "Region_Long"))
all_sst_dat$Region <- factor(all_sst_dat$Region)

region_dat_join <- region_dat %>%
    dplyr::select(., Region_Long, Res_Mean, Res_SD)

all_sst_dat <- all_sst_dat %>%
    left_join(., region_dat_join, by = c("Region" = "Region_Long"))
 all_sst_dat_res<- all_sst_dat

# Rug instead?
# Also want this for the future...
future_ymon_means <- function(df, region) {
    df_out <- df %>%
        mutate(
            "Date" = as.Date(Date, format = "X%Y.%m.%d"),
            "Year_Mon" = as.yearmon(Date)
        ) %>%
        group_by(., Year_Mon) %>%
        summarize_at(., "SST", mean, na.rm = TRUE)
    df_out$Region <- region
    return(df_out)
}

future_ymon_sd <- function(df, region) {
    df_out <- df %>%
        mutate(
            "Date" = as.Date(Date, format = "X%Y.%m.%d"),
            "Year_Mon" = as.yearmon(Date)
        ) %>%
        group_by(., Year_Mon) %>%
        summarize_at(., "SST", sd, na.rm = TRUE)
    df_out$Region <- region
    return(df_out)
}

region_dat <- region_dat %>%
    mutate(.,
        "Fore_SST_YMon_Mean" = map2(Fore_SST_DF, Region_Long, future_ymon_means),
        "Fore_SST_YMon_SD" = map2(Fore_SST_DF, Region_Long, future_ymon_sd)
    )

future_sst_dat_rug <- do.call(rbind.data.frame, region_dat$Fore_SST_YMon_Mean)
colnames(future_sst_dat_rug)[2] <- "MeanSST"
future_sst_dat_rug$Month <- format(future_sst_dat_rug$Year_Mon, "%m")

future_sst_dat_rug <- future_sst_dat_rug %>%
    left_join(., season_month_df)
future_sst_dat_rug$Season <- factor(future_sst_dat_rug$Season, levels = c("Winter", "Spring", "Summer", "Fall"))
y_vals_match <- data.frame("Season" = c("Winter", "Spring", "Summer", "Fall"), "Y_val" = c(0.2, 0.14, 0.08, 0.02))
future_sst_dat_rug <- future_sst_dat_rug %>%
    left_join(., y_vals_match)
future_sst_dat_rug$Season <- factor(future_sst_dat_rug$Season, levels = c("Winter", "Spring", "Summer", "Fall"))

# Relabeling regions
all_sst_dat_res$Region <- factor(all_sst_dat_res$Region, levels = c("California_Current", "Northeast_US_Shelf"), labels = c("CC", "NES"))
all_sst_dat_res$Region <- factor(all_sst_dat_res$Region, levels = c("CC", "NES"), labels = c("California Current", "Northeast U.S. Shelf"))
future_sst_dat_rug$Region<- factor(future_sst_dat_rug$Region, levels = c("California_Current", "Northeast_US_Shelf"), labels = c("CC", "NES"))
future_sst_dat_rug$Region<- factor(future_sst_dat_rug$Region, levels = c("CC", "NES"), labels = c("California Current", "Northeast U.S. Shelf"))

sst_out_res_sd_rug <- ggplot() +
    geom_rect(data = all_sst_dat_res, aes(xmin = Mean - SD, xmax = Mean + SD, ymin = 0.01, ymax = 0.21), fill = "gray80") +
    geom_point(data = future_sst_dat_rug, aes(x = MeanSST, y = Y_val, color = Season), alpha = 0.5, inherit.aes = FALSE) +
    geom_path(data = all_sst_dat_res, aes(x = MeanVar, y = Prediction), lwd = 2, color = "gray20") +
    # geom_ribbon(data = all_sst_dat, aes(x = MeanVar, ymax = Prediction, ymin = 0), lwd = 2, color = "gray20", alpha = 0.5, fill = NA) +
    scale_color_manual(name = "Season", values = colors_use) +
    xlab("SST (deg C)") +
    geom_label(data = all_sst_dat_res, aes(x = 2.5, y = 0.95, label = paste0("Mean = ", round(Res_Mean, 2), "\nSD = ", round(Res_SD, 2))), label.size = NA) +
    ylab("Habitat suitability") +
    ylim(c(0, 1)) +
    xlim(c(-1, 30)) +
    theme_bw(base_size = 18) +
    facet_wrap(~Region) +
    ggtitle("Resident-moble species archetype") +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 18, face = "bold"),
        plot.caption = element_text(hjust = 0)
    )

## Now seasonal
all_sst_dat <- do.call(rbind.data.frame, lapply(region_dat$SST_Curve_Dat_Seas, `[[`, 2)) %>%
    arrange(., Region, MeanVar)

# Bring over sst mean/sd...
join_dat <- do.call(rbind.data.frame, lapply(region_dat$SST_Curve_Dat_Seas, `[[`, 1)) %>%
    mutate(., "Region_Long" = unique(all_sst_dat$Region))
all_sst_dat <- all_sst_dat %>%
        left_join(., join_dat, by = c("Region" = "Region_Long"))
all_sst_dat$Region <- factor(all_sst_dat$Region)

region_dat_join <- region_dat %>%
        dplyr::select(., Region_Long, SeasWarm_Mean, SeasWarm_SD)
all_sst_dat <- all_sst_dat %>%
    left_join(., region_dat_join, by = c("Region" = "Region_Long"))

all_sst_dat_seas<- all_sst_dat

# Also want this for the future...
future_sst_dat_mean <- do.call(rbind.data.frame, region_dat$Fore_SST_Mean) %>%
    mutate(., "Region" = rep(unique(region_dat$Region_Long), each = 12))
colnames(future_sst_dat_mean)[2] <- "MeanSST"
# future_sst_dat_sd <- do.call(rbind.data.frame, region_dat$Fore_SST_SD) %>%
#     mutate(., "Region" = rep(unique(region_dat$Region_Long), each = 12))
# colnames(future_sst_dat_sd)[2] <- "SDSST"
# future_sst_dat_max <- do.call(rbind.data.frame, region_dat$Fore_SST_Max) %>%
#     mutate(., "Region" = rep(unique(region_dat$Region_Long), each = 12))
# colnames(future_sst_dat_max)[2] <- "MaxSST"
# future_sst_dat_min <- do.call(rbind.data.frame, region_dat$Fore_SST_Min) %>%
#     mutate(., "Region" = rep(unique(region_dat$Region_Long), each = 12))
# colnames(future_sst_dat_min)[2] <- "MinSST"
future_sst_dat <- future_sst_dat_mean %>%
    # left_join(., future_sst_dat_sd) %>%
    # left_join(., future_sst_dat_max) %>%
    # left_join(., future_sst_dat_min) %>%
    left_join(., season_month_df)
future_sst_dat$Season <- factor(future_sst_dat$Season, levels = c("Winter", "Spring", "Summer", "Fall"))
future_sst_dat$Month_Order <- factor(future_sst_dat$Month, levels = rev(c("12", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11")))
future_sst_dat <- future_sst_dat %>%
    arrange(., Region, Month_Order)
    
future_sst_dat_rug <- do.call(rbind.data.frame, region_dat$Fore_SST_YMon_Mean)
colnames(future_sst_dat_rug)[2] <- "MeanSST"
future_sst_dat_rug$Month <- format(future_sst_dat_rug$Year_Mon, "%m")

# future_sst_dat_rug_sd <- do.call(rbind.data.frame, region_dat$Fore_SST_YMon_SD)
# colnames(future_sst_dat_rug_sd)[2] <- "SDSST"
# future_sst_dat_rug_sd$Month <- format(future_sst_dat_rug_sd$Year_Mon, "%m")

future_sst_dat_rug <- future_sst_dat_rug %>%
    # left_join(., future_sst_dat_rug_sd) %>%
    left_join(., season_month_df)
future_sst_dat_rug$Season <- factor(future_sst_dat_rug$Season, levels = c("Winter", "Spring", "Summer", "Fall"))
y_vals_match <- data.frame("Season" = c("Winter", "Spring", "Summer", "Fall"), "Y_val" = c(0.2, 0.14, 0.08, 0.02))
future_sst_dat_rug <- future_sst_dat_rug %>%
    left_join(., y_vals_match)
future_sst_dat_rug$Season <- factor(future_sst_dat_rug$Season, levels = c("Winter", "Spring", "Summer", "Fall"))

# Relabeling regions
all_sst_dat_seas$Region <- factor(all_sst_dat_seas$Region, levels = c("California_Current", "Northeast_US_Shelf"), labels = c("CC", "NES"))
all_sst_dat_seas$Region <- factor(all_sst_dat_seas$Region, levels = c("CC", "NES"), labels = c("California Current", "Northeast U.S. Shelf"))
future_sst_dat_rug$Region <- factor(future_sst_dat_rug$Region, levels = c("California_Current", "Northeast_US_Shelf"), labels = c("CC", "NES"))
future_sst_dat_rug$Region<- factor(future_sst_dat_rug$Region, levels = c("CC", "NES"), labels = c("California Current", "Northeast U.S. Shelf"))

sst_out_seas_sd_rug <- ggplot() +
    geom_rect(data = all_sst_dat_seas, aes(xmin = Mean - SD, xmax = Mean + SD, ymin = 0.01, ymax = 0.21), fill = "gray80") +
    geom_point(data = future_sst_dat_rug, aes(x = MeanSST, y = Y_val, color = Season), alpha = 0.5, inherit.aes = FALSE) +
    geom_path(data = all_sst_dat_seas, aes(x = MeanVar, y = Prediction), lwd = 2, color = "gray20") +
    scale_color_manual(name = "Season", values = colors_use) +
    xlab("SST (deg C)") +
    geom_label(data = all_sst_dat_seas, aes(x = 2.5, y = 0.95, label = paste0("Mean = ", round(SeasWarm_Mean, 2), "\nSD = ", round(SeasWarm_SD, 2))), label.size = NA) +
    ylab("Habitat suitability") +
    ylim(c(0, 1)) +
    xlim(c(-1, 30)) +
    theme_bw(base_size = 18) +
    facet_wrap(~Region) +
    ggtitle("Seasonally-migrating species archetype") +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 18, face = "bold"),
        plot.caption = element_text(hjust = 0)
    )

sst_out_rug <- sst_out_res_sd_rug / sst_out_seas_sd_rug + plot_layout(guides = "collect")
ggsave(paste0(here::here("results/sst_res_rug.jpg")), plot = sst_out_rug, height = 8, width = 11)

#####
## Habitat suitability
## !!! Warning this will also take a while to run !!!
#####
## Average across months for baseline
scenarios <- c("res", "seas")

plot_suit_out <- vector("list", length = length(unique(region_dat$Region)) * length(scenarios))
names(plot_suit_out) <- paste(unique(region_dat$Region), rep(scenarios, each = length(unique(region_dat$Region))), sep = "_")

suit_df_out <- vector("list", length = length(unique(region_dat$Region)) * length(scenarios))
names(suit_df_out) <- paste(unique(region_dat$Region), rep(scenarios, each = length(unique(region_dat$Region))), sep = "_")

res_ind<- 1

for(g in seq_along(scenarios)){
    scenario_use <- scenarios[g]
    # hab_suit_files <- list.files(paste0("/Users/aallyn/Library/CloudStorage/Box-Box/Mills Lab/Projects/NASA_UNSDG19/Temp Results/vs_hab_suit_lme_", scenario_use, "/"), full.names = TRUE, pattern = "vs_suit")
    hab_suit_files<- list.files(paste0(here::here("data/vs_hab_suit_lme"), "_", scenario_use, "/"), full.names = TRUE, pattern = "vs_suit")
   
    
    for(i in seq_along(hab_suit_files)){
        hab_suit_temp <- readRDS(hab_suit_files[i])
        suit_rasts <- raster::stack(sapply(hab_suit_temp[1:length(hab_suit_temp) - 1], "[[", 3))
        
        suit_df <- as.data.frame(suit_rasts, xy = TRUE) %>%
            pivot_longer(., -c(x, y), names_to = "raster.layer", values_to = "value") %>%
            mutate(., "raster.layer" = gsub("X", "", raster.layer)) %>%
            separate(., col = raster.layer, into = c("Year", "Month", "Day"), sep = "[.]") %>%
            mutate(., "Date" = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
            arrange(., Date)
        
        # Get region
        region_use <- gsub("_vs_suit.rds", "", stringr::str_remove(hab_suit_files[i], ".*\\/"))
        
        # Subset dates
        suit_df <- suit_df %>%
            filter(., Date >= region_dat$Start_Year_Base[[which(region_dat$Region == region_use)]] & Date < region_dat$End_Year_Base[[which(region_dat$Region == region_use)]]) %>%
            group_by(., x, y, Month) %>%
            summarize_at(., "value", mean, na.rm = TRUE)
        
        suit_df_out[[res_ind]] <- suit_df
        res_ind<- res_ind+1
        
        # Get bbox
        bbox <- st_bbox(region_dat$Shapefile[[which(region_dat$Region == region_use)]])
        
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
        
        plot_suit_out[[which(names(plot_suit_out) == paste(region_use, scenario_use, sep = "_"))]] <- plot_suit_temp
        ggsave(filename = here::here(paste0("results/", region_use, "_hab_suit_", scenario_use, ".jpg")), height = 8, width = 11, dpi = 300, plot_suit_temp)
    }
}

## New figure: 2 x 2 plot of January/July and then RES/Seasonally migrating
months_keep <- c("01", "07")

for(i in seq_along(suit_df_out)){

    df_temp <- suit_df_out[[i]]
    name_use <- names(suit_df_out)[i]

    df_keep <- df_temp %>%
        filter(., Month %in% months_keep) %>%
        mutate(., "Region_Species_Archetype" = name_use)
    
    if(i == 1){
        res_out<- df_keep
    } else {
        res_out<- bind_rows(res_out, df_keep)
    }
}

res_out <- res_out %>%
    separate(Region_Species_Archetype, into = c("Region", "Species_Archetype"))
    
res_out$Species_Archetype <- factor(res_out$Species_Archetype, levels = c("res", "seas"), labels = c("Mobile resident", "Seasonal migrant"))
res_out$Region <- factor(res_out$Region, levels = c("cc", "ne"), labels = c("CC", "NES"))
res_out$Region_Species_Archetype<- paste(res_out$Region, res_out$Species_Archetype, sep = "_")

plots_temp <- vector("list", 8)
res_ind<- 1
cc_bbox <- st_bbox(region_dat$Shapefile[[which(region_dat$Region == "cc")]])
ne_bbox <- st_bbox(region_dat$Shapefile[[which(region_dat$Region == "ne")]])
res_out$value <- ifelse(res_out$value > 1, 1, res_out$value)

month_title <- data.frame("Month_Match" = c("01", "07"), "Month_Title" = c("Jan", "July"))

for(i in seq_along(unique(res_out$Region_Species_Archetype))){
    filter_use <- unique(res_out$Region_Species_Archetype)[i]

    dat_temp <- res_out %>%
        filter(., Region_Species_Archetype == filter_use)
    
    bbox_use <- switch(as.character(unique(dat_temp$Region)),
        "CC" = cc_bbox,
        "NES" = ne_bbox
    )
    
    for(j in seq_along(unique(dat_temp$Month))){

        dat_plot <- dat_temp %>%
            filter(., Month == unique(dat_temp$Month)[j])

        # title_plot<- month_title$Month_Title[month_title$Month_Match == unique(dat_temp$Month)[j]]
        title_plot <- ifelse(dat_temp$Month[j] == "01", "January", "July")
        
        if(j ==1) {
            plots_temp[[res_ind]] <- ggplot() +
                geom_raster(data = dat_plot, aes(x = x, y = y, fill = value), na.rm = TRUE) +
                scale_fill_viridis(name = "Habitat suitability", na.value = "transparent", limits = c(0, 1)) +
                geom_sf(data = land, color = "#d9d9d9") +
                coord_sf(xlim = bbox_use[c(1, 3)], ylim = bbox_use[c(2, 4)]) +
                scale_x_continuous("", breaks = round(seq(from = bbox_use[c(1)], to = bbox_use[c(3)], length.out = 4), 0)) +
                xlab("Longitude") +
                ylab("Latitude") +
                ggtitle(title_plot) +
                theme_bw() +
                theme(
                    strip.background = element_rect(colour = NA, fill = NA),
                    strip.text = element_text(size = 16, face = "bold")
                )
        } else {
            plots_temp[[res_ind]] <- ggplot() +
                geom_raster(data = dat_plot, aes(x = x, y = y, fill = value), na.rm = TRUE) +
                scale_fill_viridis(name = "Habitat suitability", na.value = "transparent", limits = c(0, 1)) +
                geom_sf(data = land, color = "#d9d9d9") +
                coord_sf(xlim = bbox_use[c(1, 3)], ylim = bbox_use[c(2, 4)]) +
                scale_x_continuous("", breaks = round(seq(from = bbox_use[c(1)], to = bbox_use[c(3)], length.out = 4), 0)) +
                ggtitle(title_plot) +
                theme_bw() +
                theme(
                    strip.background = element_rect(colour = NA, fill = NA),
                    strip.text = element_text(size = 16, face = "bold"),
                    axis.text.x = element_blank(),
                    axis.text.y = element_blank()
                )
            
        }

        names(plots_temp)[res_ind] <- paste(filter_use, unique(dat_temp$Month)[j], sep = "_")

        res_ind<- res_ind+1

    }
}

res_row <- (plots_temp[[1]] + plots_temp[[2]] + plots_temp[[3]] + plots_temp[[4]]) + 
plot_layout(guides = "collect", nrow = 1) +
    plot_annotation(title = "Mobile resident species archetype", theme = theme(plot.title = element_text(size = 16)))
seas_row <- (plots_temp[[5]] + plots_temp[[6]] + plots_temp[[7]] + plots_temp[[8]]) +
plot_layout(guides = "collect", nrow = 1) +
    plot_annotation(title = "Seasonal migrant warm water species archetype", theme = theme(plot.title = element_text(size = 16)))

plot_out<- wrap_elements(res_row) / wrap_elements(seas_row) + plot_layout(guides = "collect")
ggsave(filename = here::here("results/ExampleSurfaces.jpg"), height = 8, width = 15, dpi = 300, plot_out)

# #####
# ## "TRUE" center of gravity
# #####
scenarios <- c("res", "seas")

suit_df_out <- vector("list", length = length(unique(region_dat$Region)) * length(scenarios))
names(suit_df_out) <- paste(unique(region_dat$Region), rep(scenarios, each = length(unique(region_dat$Region))), sep = "_")

res_ind <- 1
 
for(g in seq_along(scenarios)){
    scenario_use <- scenarios[g]
    # hab_suit_files <- list.files(paste0("/Users/aallyn/Library/CloudStorage/Box-Box/Mills Lab/Projects/NASA_UNSDG19/Temp Results/vs_hab_suit_lme_", scenario_use, "/"), full.names = TRUE, pattern = "vs_suit")
    hab_suit_files<- list.files(paste0(here::here("data/vs_hab_suit_lme"), "_", scenario_use, "/"), full.names = TRUE, pattern = "vs_suit")
   
    
    for(i in seq_along(hab_suit_files)){
        hab_suit_temp <- readRDS(hab_suit_files[i])
        suit_rasts <- raster::stack(sapply(hab_suit_temp[1:length(hab_suit_temp) - 1], "[[", 3))
        
        suit_df <- as.data.frame(suit_rasts, xy = TRUE) %>%
            pivot_longer(., -c(x, y), names_to = "raster.layer", values_to = "value") %>%
            mutate(., "raster.layer" = gsub("X", "", raster.layer)) %>%
            separate(., col = raster.layer, into = c("Year", "Month", "Day"), sep = "[.]") %>%
            mutate(., "Date" = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
            arrange(., Date)
        
        # Get region
        region_use <- gsub("_vs_suit.rds", "", stringr::str_remove(hab_suit_files[i], ".*\\/"))
        
        # Subset dates
        suit_df <- suit_df %>%
            filter(., Date >= region_dat$Start_Year_Base[[which(region_dat$Region == region_use)]] & Date < region_dat$End_Year_Fore[[which(region_dat$Region == region_use)]]) %>%
            group_by(., Year, Month) %>%
            nest()
        
        # Calculate COG
        suit_df <- suit_df %>%
            mutate(., "COG" = map(data, cog_function)) %>%
            dplyr::select(., Year, Month, COG)
        
        suit_df_out[[res_ind]] <- suit_df

        # Update result index
        res_ind<- res_ind + 1
    }
}

for(k in seq_along(suit_df_out)){
    temp <- suit_df_out[[k]] %>%
        unnest()
    temp$Variable<- rep(c("Long", "Long_SD", "Lat", "Lat_SD"), length.out = nrow(temp))
    temp$Region = rep(names(suit_df_out)[[k]])
    if(k == 1){
        out<- temp
    } else {
        out<- bind_rows(out, temp)
    }
}

out_wide <- out %>%
    pivot_wider(., names_from = Variable, values_from = COG) %>%
    mutate(., "Date" = as.Date(paste(Year, Month, "16", sep = "-"))) %>%
    separate(., Region, into = c("Region", "Species_Archetype"))

out_wide$Lat_Min<- ifelse(out_wide$Region == "cc", 26, 39)
out_wide$Lat_Max<- ifelse(out_wide$Region == "cc", 40, 43)

out_wide$Region <- factor(out_wide$Region, levels = c("cc", "ne"), labels = c("CC", "NES"))
out_wide$Species_Archetype <- factor(out_wide$Species_Archetype, levels = c("res", "seas"), labels = c("Mobile resident", "Seasonal migrant"))

res_lat_out <- ggplot() +
    geom_point(data = subset(out_wide, out_wide$Species_Archetype == "Mobile resident"), aes(x = Date, y = Lat), color = "light gray", size = 0.75) +
    geom_path(data = subset(out_wide, out_wide$Species_Archetype == "Mobile resident"), aes(x = Date, y = Lat), color = "light gray") +
    stat_smooth(data = subset(out_wide, out_wide$Species_Archetype == "Mobile resident"), aes(x = Date, y = Lat), method = "lm", formula = y ~ x, se = TRUE, color = "black") +
    ggtitle("Mobile resident species archetype") +
    facet_wrap(~ Region, scales = "free_y") +
    geom_blank(data = out_wide, aes(y = Lat_Min)) +
    geom_blank(data = out_wide, aes(y = Lat_Max)) +
    theme_bw(base_size = 16) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold"),
        plot.caption = element_text(hjust = 0) 
    ) 
seas_lat_out<- ggplot() +
    geom_point(data = subset(out_wide, out_wide$Species_Archetype == "Seasonal migrant"), aes(x = Date, y = Lat), color = "light gray", size = 0.75) +
    geom_path(data = subset(out_wide, out_wide$Species_Archetype == "Seasonal migrant"), aes(x = Date, y = Lat), color = "light gray") +
    stat_smooth(data = subset(out_wide, out_wide$Species_Archetype == "Seasonal migrant"), aes(x = Date, y = Lat), method = "lm", formula = y ~ x, se = TRUE, color = "black") +
    ggtitle("Seasonal migrant species archetype") +
    facet_wrap(~ Region, scales = "free_y") +
    geom_blank(data = out_wide, aes(y = Lat_Min)) +
    geom_blank(data = out_wide, aes(y = Lat_Max)) +
    theme_bw(base_size = 16) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold"),
        plot.caption = element_text(hjust = 0) 
    ) 

lat_out <- res_lat_out / seas_lat_out + plot_layout(guides = "collect")
ggsave(filename = here::here("results/COG_Lat.jpg"), height = 8, width = 11, dpi = 300, lat_out)

out_wide$Long_Min<- ifelse(out_wide$Region == "CCS", -118, -67)
out_wide$Long_Max<- ifelse(out_wide$Region == "CCS", -128, -73)

lon_out<- ggplot() +
    geom_point(data = out_wide, aes(x = Date, y = Long), color = "light gray", size = 0.75) +
    geom_path(data = out_wide, aes(x = Date, y = Long), color = "light gray") +
    stat_smooth(data = out_wide, aes(x = Date, y = Long), method = "lm", formula = y ~ x, se = TRUE, color = "black") +
    facet_wrap(~ Region + Species_Archetype, scales = "free_y") +
    geom_blank(data = out_wide, aes(y = Long_Min)) +
    geom_blank(data = out_wide, aes(y = Long_Max)) +
    theme_bw(base_size = 12) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )

ggsave(filename = here::here("results/COG_Lat.jpg"), height = 8, width = 11, dpi = 300, lat_out)
ggsave(filename = here::here("results/COG_Lon.jpg"), height = 8, width = 11, dpi = 300, lon_out)

#####
## Results -- getting prediction skill stats and environmental novelty measures
#####
# Read in results from 06_Pred_BRTs_Get_Env_Novelty.R
fore_summs_list <- readRDS(here::here("results/fore_summs_list.rds"))

# Unlist into one big dataframe and species archetype column
scenarios <- c("res", "seas")
names(fore_summs_list)<- scenarios 

# Get to a nested dataframe...
fore_summs <- dplyr::bind_rows(fore_summs_list, .id = "Species_Archetype")
fore_summs$Region<- factor(fore_summs$Region, levels = c("CCS", "NES"), labels = c("California Current", "Northeast U.S. Shelf"))
fore_summs$Region<- factor(fore_summs$Region, levels = c("California Current", "Northeast U.S. Shelf"), labels = c("California Current", "Northeast U.S. Shelf"))


##### Hellinger's distance timeseries
hell_dist <- fore_summs %>%
    ungroup() %>%
    left_join(., season_month_df) %>%
    dplyr::select(., Region, Month, Season, Year, HellDist) %>%
    mutate(., "Date" = as.Date(paste(Year, Month, "16", sep = "-"), "%Y-%m-%d")) %>%
    group_by(Region, Year, Season) %>%
    summarize(.,
        "Mean_HellDist" = mean(HellDist, na.rm = TRUE),
        "SD_HellDist" = sd(HellDist, na.rm = TRUE)
    ) %>%
    mutate(.,
        "Plot_Ymin" = Mean_HellDist - SD_HellDist,
        "Plot_Ymax" = Mean_HellDist + SD_HellDist
    )
hell_dist$Plot_Ymin<- ifelse(hell_dist$Plot_Ymin < 0, 0, hell_dist$Plot_Ymin)
hell_dist$Season<- factor(hell_dist$Season, levels = c("Winter", "Spring", "Summer", "Fall"), labels = c("Winter", "Spring", "Summer", "Fall"))

hell_dist$Plot_Date <- as.Date(paste(hell_dist$Year, ifelse(hell_dist$Season == "Winter", "01", ifelse(hell_dist$Season == "Spring", "04", ifelse(hell_dist$Season == "Summer", "07", "10"))), "16", sep = "-"))
hell_dist_plot<- ggplot(data = hell_dist, aes(x = Plot_Date, y = Mean_HellDist, fill = Season, color = Season)) +
    geom_errorbar(data = hell_dist, aes(x = Plot_Date, ymin = Plot_Ymin, ymax = Plot_Ymax, color = Season, group = Season), alpha = 0.4) +
    geom_point(data = hell_dist, aes(x = Plot_Date, y = Mean_HellDist, fill = Season, color = Season), size = 3, alpha = 0.4, pch = 21) +
    geom_smooth(method = "lm", se = FALSE) +
    stat_poly_eq(formula = y ~ x, 
        label.x = "left",
        label.y = rev(seq(from = 0, to = 0.2, length.out = 4)),
        eq.with.lhs = "italic(hat(y))~`=`~",
        aes(label = paste(..eq.label.., sep = "~~~")), parse = TRUE) +
    stat_fit_glance(method = 'lm',
                  method.args = list(formula = "y ~ x"),
                  #geom = 'text',
                  label.x = "right",
                  label.y = rev(seq(from = 0, to = 0.2, length.out = 4)), #added to prevent overplotting
                  aes(label = paste("~italic(p) ==", round(..p.value.., digits = 3),
                  "~italic(R)^2 ==", round(..r.squared.., digits = 2),
                  sep = "~")),
                  parse = TRUE) +
    # stat_smooth(data = hell_dist, aes(x = Plot_Date, y = Mean_HellDist, fill = Season, color = Season), method = "lm", formula = y ~ x, se = F, alpha = 0.4) +
    scale_fill_manual(name = "", values = colors_use) +
    scale_color_manual(name = "", values = colors_use) +
    scale_y_continuous(name = "Hellinger Distance environmental novelty\nrelative to 1985-2004", limits = c(-0.1, 0.125)) +
    xlab("Year") +
    facet_wrap(~ Region, ncol = 1) +
    theme_bw(base_size = 16) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold"),
        plot.caption = element_text(hjust = 0) 
    )
ggsave(filename = paste0(here::here("results/"), "HellDist_Month_TS.jpg"), height = 8, width = 11, dpi = 300, hell_dist_plot)

#####
## Deviance explained
#####
dev_expl_brt<- function(model){
    int.null.deviance = model$self.statistics$mean.null 
    int.residual.deviance = model$cv.statistics$deviance.mean
    (int.null.deviance - int.residual.deviance)/int.null.deviance
}

cc_res <- readRDS(here::here("mods/brt_fits_lme_res/cc/Base_1985-01-01_to_2004-01-01_fit.rds"))
dev_expl_brt(cc_res)

ne_res<- readRDS(here::here("mods/brt_fits_lme_res/ne/Base_1985-01-01_to_2004-01-01_fit.rds"))
dev_expl_brt(ne_res)

cc_seas <- readRDS(here::here("mods/brt_fits_lme_seas/cc/Base_1985-01-01_to_2004-01-01_fit.rds"))
dev_expl_brt(cc_seas)

ne_seas<- readRDS(here::here("mods/brt_fits_lme_seas/ne/Base_1985-01-01_to_2004-01-01_fit.rds"))
dev_expl_brt(ne_seas)


#####
## Estimation curves
#####
# Would need to get sst_curve_dat_res object
all_sst_dat_res$Species_Archetype <- "Resident-mobile"
all_sst_dat_seas$Species_Archetype <- "Seasonally-migrating warm water"

sst_curve_dat <- all_sst_dat_res %>%
    bind_rows(., all_sst_dat_seas)

sst_curve_dat_res <- sst_curve_dat %>%
    filter(., Species_Archetype == "Resident-mobile")
sst_curve_dat_seas <- sst_curve_dat %>%
    filter(., Species_Archetype == "Seasonally-migrating warm water")


sst_curve_dat_res$Region <- factor(sst_curve_dat_res$Region, levels = c("CC", "NES"), labels = c("California Current", "Northeast U.S. Shelf"))
sst_curve_dat_seas$Region<- factor(sst_curve_dat_seas$Region, levels = c("California Current", "Northeast U.S. Shelf"), labels = c("California Current", "Northeast U.S. Shelf"))
brt_fits_res <- fore_summs %>%
    filter(., Species_Archetype == "res") %>%
    ungroup() %>%
    distinct(Region, BRT_SST_Fit) %>%
    unnest(cols = c(BRT_SST_Fit))
names(brt_fits_res)[2:3]<- c("oisst_daily", "y")

brt_fits_seas <- fore_summs %>%
    filter(., Species_Archetype == "seas") %>%
    ungroup() %>%
    distinct(Region, BRT_SST_Fit) %>%
    unnest(cols = c(BRT_SST_Fit))
names(brt_fits_seas)[2:3]<- c("oisst_daily", "y")


# Get fitted data to add as a rug
cc_res <- readRDS(here::here(paste0("data/train_test_lme_res/cc/Base_1985-01-01_to_2004-01-01.rds")))[[1]] |>
    mutate("Region" = factor("California Current", levels = c("California Current", "Northeast U.S. Shelf")))
nes_res <- readRDS(here::here(paste0("data/train_test_lme_res/ne/Base_1985-01-01_to_2004-01-01.rds")))[[1]] |>
    mutate("Region" = factor("Northeast U.S. Shelf", levels = c("California Current", "Northeast U.S. Shelf")))

fit_sst_dat_res <- bind_rows(cc_res, nes_res) |>
    mutate(Month = format(Date, "%m")) |>
    left_join(season_month_df) |>
    mutate(
        Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")),
        y_plot = as.numeric(rescale(as.numeric(Season), to = c(-0.125, -0.01), from = range(as.numeric(Season))))
    )

sst_op_est_res <- ggplot() +
    geom_path(data = sst_curve_dat_res, aes(x = MeanVar, y = Prediction), lwd = 2.5, color = "gray20", alpha = 0.5) +
    geom_path(data = brt_fits_res, aes(x = oisst_daily, y = y), color = "#1b9e77", lwd = 1.5, alpha = 0.5) +
    geom_point(data = fit_sst_dat_res, aes(y = y_plot, x = oisst_daily, color = Season), shape = "|", size = 1) +
    # scale_fill_manual(name = "", values = colors_use) +
    scale_color_manual(name = "", values = colors_use) +
    xlab(expression("SST " ( degree*C))) +
    ylab("Habitat suitability") +
    ylim(c(-0.15, 1)) +
    xlim(c(-1, 30)) +
    theme_bw(base_size = 18) +
    facet_wrap(~Region, ncol = 2) +
    ggtitle("Resident-mobile species archetype") +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 18, face = "bold")
    )

# Get fitted data to add as a rug
cc_seas <- readRDS(here::here(paste0("data/train_test_lme_seas/cc/Base_1985-01-01_to_2004-01-01.rds")))[[1]] |>
    mutate("Region" = factor("California Current", levels = c("California Current", "Northeast U.S. Shelf")))
nes_seas <- readRDS(here::here(paste0("data/train_test_lme_seas/ne/Base_1985-01-01_to_2004-01-01.rds")))[[1]] |>
    mutate("Region" = factor("Northeast U.S. Shelf", levels = c("California Current", "Northeast U.S. Shelf")))

fit_sst_dat_seas <- bind_rows(cc_seas, nes_seas) |>
    mutate(Month = format(Date, "%m")) |>
    left_join(season_month_df) |>
    mutate(
        Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")),
        y_plot = as.numeric(rescale(as.numeric(Season), to = c(-0.125, -0.01), from = range(as.numeric(Season))))
    )

sst_op_est_seas <- ggplot() +
    geom_path(data = sst_curve_dat_seas, aes(x = MeanVar, y = Prediction), lwd = 2.5, color = "gray20", alpha = 0.5) +
    geom_path(data = brt_fits_seas, aes(x = oisst_daily, y = y), color = "#1b9e77", lwd = 1.5, alpha = 0.5) +
    geom_point(data = fit_sst_dat_seas, aes(y = y_plot, x = oisst_daily, color = Season), shape = "|", size = 1) +
    # geom_rug(data = fit_sst_dat_seas, aes(x = oisst_daily, color = Season), alpha = 0.75, sides = "b") +
    scale_color_manual(name = "", values = colors_use) +
        xlab(expression("SST " ( degree*C))) +
        ylab("Habitat suitability") +
        ylim(c(-0.15, 1)) +
        xlim(c(-1, 30)) +
        theme_bw(base_size = 18) +
        facet_wrap(~ Region, ncol = 2) +
        ggtitle("Seasonally-migrating warm water species archetype") +
        theme(
            strip.background = element_rect(colour = NA, fill = NA),
            strip.text = element_text(size = 18, face = "bold"),
            plot.caption = element_text(hjust = 0)
        ) 
sst_op_est_both <- (sst_op_est_res +
    guides(colour = guide_legend(override.aes = list(size = 8)))) / (sst_op_est_seas +
    guides(colour = guide_legend(override.aes = list(size = 8)))) 
sst_op_est_both<- sst_op_est_both + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(paste0(here::here("results/sst_op_est.jpg")), plot = sst_op_est_both, height = 8, width = 11) 

##### 
## Prediction statistics
#####
plot_dat <- fore_summs
plot_dat <- plot_dat %>%
    filter(., Region %in% c("California Current", "Northeast U.S. Shelf")) %>%
    left_join(., season_month_df)
plot_dat$Season <- factor(plot_dat$Season, levels = c("Winter", "Spring", "Summer", "Fall"))
plot_dat$Species_Archetype <- factor(plot_dat$Species_Archetype, levels = c("res", "seas"), labels = c("Resident-mobile species archetype", "Seasonally-migrating species archetype"))
mae_scales <- c(0.0, 0.4)
auc_scales <- c(0.7, 1)
calib_scales <- c(0.85, 1.001)
precis_scales <- c(0.2, 0.45)
hdist_scale <- c(0, 0.6)

#### Scaling PrAUC....we need to get an AUC minimum value, which relies on prevalence PrAUC Min = 1 + (1-prev) ln(1-prev)/prev
pr_auc_min<- function(prev){
    out <- 1 + (((1 - prev) * log(1 - prev))/prev)
    return(out)
}

plot_dat <- plot_dat %>%
    mutate(., "Min_PrAUC" = map_dbl(Prev, pr_auc_min))

pr_auc_table <- plot_dat %>%
    dplyr::select(, Species_Archetype, Region, Scenario, ModelTrainStart, ModelTrainEnd, Month, Year, Min_PrAUC, PrAUC) %>%
    group_by(., Species_Archetype, Region, Scenario, .drop = FALSE) %>%
    summarize(., "Overall_Min_PrAUC" = min(Min_PrAUC), "Overall_Max_PrAUC" = max(PrAUC))

pr_auc_table

pr_auc_min_use <- min(pr_auc_table$Overall_Min_PrAUC)
pr_auc_max_use<- max(pr_auc_table$Overall_Max_PrAUC)

pr_auc_rescale<- function(pr_auc_val, pr_auc_min, pr_auc_max){
    out<- (pr_auc_val - pr_auc_min)/(pr_auc_max - pr_auc_min)
    return(out)
}

plot_dat <- plot_dat %>%
    mutate("PrAUC_Scaled" = map_dbl(PrAUC, pr_auc_rescale, pr_auc_min = pr_auc_min_use, pr_auc_max = pr_auc_max_use))

plot_dat_use <- plot_dat %>%
    ungroup() %>%
    dplyr::select(., Species_Archetype, Region, Scenario, ModelTrainStart, ModelTrainEnd, Month, Year, Season, data, PrAUC_Scaled, AUC, Cor, RMSE, Calib, PrAUC_Scaled, HellDist) %>%
    distinct()

pr_auc_plot_res <- ggplot(data = subset(plot_dat_use, plot_dat_use$Species_Archetype == "Resident-mobile species archetype"), aes(x = HellDist, y = round(PrAUC_Scaled, 2), fill = Season, color = Season, shape = Season, group = Season, order = Season)) +
    geom_point(size = 3, pch = 21, alpha = 0.4) +
    geom_smooth(method = "lm", se = FALSE) +
    stat_poly_eq(formula = y ~ x, 
        label.x = "left",
        label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)),
        eq.with.lhs = "italic(hat(y))~`=`~",
        aes(label = paste(..eq.label.., sep = "~~~")), size = 6, parse = TRUE) +
    stat_fit_glance(method = 'lm',
                  method.args = list(formula = "y ~ x"),
                  #geom = 'text',
                #   label.x = "right",
                  label.x = 0.4,
                  label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), #added to prevent overplotting
                  aes(label = paste("~italic(p) ==", round(..p.value.., digits = 3),
                  "~italic(R)^2 ==", round(..r.squared.., digits = 2),
                  sep = "~")),
                  size = 6,
                  parse = TRUE) +
    scale_fill_manual(name = "", values = colors_use) +
    scale_color_manual(name = "", values = colors_use) +
    xlab("Hellinger Distance") +
    ylab("Scaled area under the precision-recall curve (PrAUC)") +
    ylim(c(0, 1.06)) +
    ggtitle("Resident-mobile species archetype") +
    facet_wrap(~ Region, ncol = 2) +
    theme_bw(base_size = 18) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 18, face = "bold"),
        plot.caption = element_text(hjust = 0) 
    )

pr_auc_plot_seas<- ggplot(data = subset(plot_dat_use, plot_dat_use$Species_Archetype == "Seasonally-migrating species archetype"), aes(x = HellDist, y = round(PrAUC_Scaled, 2), fill = Season, color = Season, shape = Season, group = Season, order = Season)) +
    geom_point(size = 3, pch = 21, alpha = 0.4) +
    geom_smooth(method = "lm", se = FALSE) +
    stat_poly_eq(formula = y ~ x, 
    label.x = "left",
    label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)),
    eq.with.lhs = "italic(hat(y))~`=`~",
    aes(label = paste(..eq.label.., sep = "~~~")), size = 6, parse = TRUE) +
    stat_fit_glance(method = 'lm',
                  method.args = list(formula = "y ~ x"),
                  #geom = 'text',
                #   label.x = "right",
                  label.x = 0.4,
                  label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), #added to prevent overplotting
                  aes(label = paste("~italic(p) ==", round(..p.value.., digits = 3),
                  "~italic(R)^2 ==", round(..r.squared.., digits = 2),
                  sep = "~")),
                  size = 6,
                  parse = TRUE) +
    scale_fill_manual(name = "", values = colors_use) +
    scale_color_manual(name = "", values = colors_use) +
    xlab("Hellinger Distance") +
    ylab("Scaled area under the precision-recall curve (PrAUC)") +
    ylim(c(0, 1.06)) +
    ggtitle("Seasonally-migrating species archetype") +
    facet_wrap(~ Region, ncol = 2) +
    theme_bw(base_size = 18) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 18, face = "bold"),
        plot.caption = element_text(hjust = 0) 
    )
pr_auc_out<- pr_auc_plot_res / pr_auc_plot_seas + plot_layout(guides = "collect") & theme(legend.position = 'bottom', legend.text=element_text(size=17))
ggsave(filename = paste0(here::here("results/"), "PrAUC_Scaled.jpg"), width = 18, height = 15, dpi = 300, pr_auc_out)

# AUC
auc_plot_res <- ggplot(data = subset(plot_dat_use, plot_dat_use$Species_Archetype == "Resident-mobile species archetype"), aes(x = HellDist, y = round(AUC, 2), fill = Season, color = Season, shape = Season, group = Season, order = Season)) +
    geom_point(size = 3, pch = 21, alpha = 0.4) +
    geom_smooth(method = "lm", se = FALSE) +
    stat_poly_eq(formula = y ~ x, 
    label.x = "left",
    label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)),
    eq.with.lhs = "italic(hat(y))~`=`~",
    aes(label = paste(..eq.label.., sep = "~~~")), parse = TRUE) +
    stat_fit_glance(method = 'lm',
                  method.args = list(formula = "y ~ x"),
                  #geom = 'text',
                  label.x = "right",
                  label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), #added to prevent overplotting
                  aes(label = paste("~italic(p) ==", round(..p.value.., digits = 3),
                  "~italic(R)^2 ==", round(..r.squared.., digits = 2),
                  sep = "~")),
                  parse = TRUE) +
    scale_fill_manual(name = "", values = colors_use) +
    scale_color_manual(name = "", values = colors_use) +
    xlab("Hellinger Distance") +
    ylab("AUC") +
    ylim(c(0.5, 1)) + 
    ggtitle("Resident-mobile species archetype") +
    facet_wrap(~ Region, ncol = 2) +
    theme_bw(base_size = 18) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 18, face = "bold"),
        plot.caption = element_text(hjust = 0) 
    )

auc_plot_seas<- ggplot(data = subset(plot_dat_use, plot_dat_use$Species_Archetype == "Seasonally-migrating species archetype"), aes(x = HellDist, y = round(AUC, 2), fill = Season, color = Season, shape = Season, group = Season, order = Season)) +
    geom_point(size = 3, pch = 21, alpha = 0.4) +
    geom_smooth(method = "lm", se = FALSE) +
    stat_poly_eq(formula = y ~ x, 
    label.x = "left",
    label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)),
    eq.with.lhs = "italic(hat(y))~`=`~",
    aes(label = paste(..eq.label.., sep = "~~~")), parse = TRUE) +
    stat_fit_glance(method = 'lm',
                  method.args = list(formula = "y ~ x"),
                  #geom = 'text',
                  label.x = "right",
                  label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), #added to prevent overplotting
                  aes(label = paste("~italic(p) ==", round(..p.value.., digits = 3),
                  "~italic(R)^2 ==", round(..r.squared.., digits = 2),
                  sep = "~")),
                  parse = TRUE) +
    scale_fill_manual(name = "", values = colors_use) +
    scale_color_manual(name = "", values = colors_use) +
    xlab("Hellinger Distance") +
    ylab("AUC") +
    ggtitle("Seasonally-migrating species archetype") +
    ylim(c(0.5, 1)) + 
    facet_wrap(~ Region, ncol = 2) +
    theme_bw(base_size = 18) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 18, face = "bold"),
        plot.caption = element_text(hjust = 0) 
    )
auc_out <- auc_plot_res / auc_plot_seas + plot_layout(guides = "collect") & theme(legend.position = 'bottom', legend.text=element_text(size=17))
ggsave(filename = paste0(here::here("results/"), "AUC_Month.jpg"), width = 18, height = 15, dpi = 300, auc_out)

# Calibration
calib_plot_res <- ggplot(data = subset(plot_dat_use, plot_dat_use$Species_Archetype == "Resident-mobile species archetype"), aes(x = HellDist, y = round(Calib, 2), fill = Season, color = Season, shape = Season, group = Season, order = Season)) +
    geom_point(size = 3, pch = 21, alpha = 0.4) +
    geom_smooth(method = "lm", se = FALSE) +
    stat_poly_eq(formula = y ~ x, 
    label.x = "left",
    label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)),
    eq.with.lhs = "italic(hat(y))~`=`~",
    aes(label = paste(..eq.label.., sep = "~~~")), parse = TRUE) +
    stat_fit_glance(method = 'lm',
                  method.args = list(formula = "y ~ x"),
                  #geom = 'text',
                  label.x = "right",
                  label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), #added to prevent overplotting
                  aes(label = paste("~italic(p) ==", round(..p.value.., digits = 3),
                  "~italic(R)^2 ==", round(..r.squared.., digits = 2),
                  sep = "~")),
                  parse = TRUE) +
    scale_fill_manual(name = "", values = colors_use) +
    scale_color_manual(name = "", values = colors_use) +
    xlab("Hellinger Distance") +
    ylab("Calibration") +
    ylim(c(-0.02, 0.08)) + 
    ggtitle("Resident-mobile species archetype") +
    facet_wrap(~ Region, ncol = 2) +
    theme_bw(base_size = 18) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 18, face = "bold"),
        plot.caption = element_text(hjust = 0) 
    )

calib_plot_seas<- ggplot(data = subset(plot_dat_use, plot_dat_use$Species_Archetype == "Seasonally-migrating species archetype"), aes(x = HellDist, y = round(Calib, 2), fill = Season, color = Season, shape = Season, group = Season, order = Season)) +
    geom_point(size = 3, pch = 21, alpha = 0.4) +
    geom_smooth(method = "lm", se = FALSE) +
    stat_poly_eq(formula = y ~ x, 
    label.x = "left",
    label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)),
    eq.with.lhs = "italic(hat(y))~`=`~",
    aes(label = paste(..eq.label.., sep = "~~~")), parse = TRUE) +
    stat_fit_glance(method = 'lm',
                  method.args = list(formula = "y ~ x"),
                  #geom = 'text',
                  label.x = "right",
                  label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), #added to prevent overplotting
                  aes(label = paste("~italic(p) ==", round(..p.value.., digits = 3),
                  "~italic(R)^2 ==", round(..r.squared.., digits = 2),
                  sep = "~")),
                  parse = TRUE) +
    scale_fill_manual(name = "", values = colors_use) +
    scale_color_manual(name = "", values = colors_use) +
    xlab("Hellinger Distance") +
    ylab("Calibration") +
    ggtitle("Seasonally-migrating species archetype") +
    ylim(c(-0.02, 0.08)) + 
    facet_wrap(~ Region, ncol = 2) +
    theme_bw(base_size = 18) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 18, face = "bold"),
        plot.caption = element_text(hjust = 0) 
    )
calib_out<- calib_plot_res / calib_plot_seas + plot_layout(guides = "collect")  & theme(legend.position = 'bottom', legend.text=element_text(size=17))
ggsave(filename = paste0(here::here("results/"), "Calib_Month.jpg"), width = 18, height = 15, dpi = 300, calib_out)

