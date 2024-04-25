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
## Global time series and SOM??
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
# sst_anom$DataSubset <- ifelse(sst_anom$Year_Plot <= 2004, "1", "0")

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
#     filter(., Year >= 1985) 

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
        geom_label(data = subset(all_depth_dat, all_depth_dat$Region == "CC"), aes(x = 4500, y = 1.15, label = paste0("Mean = ", round(Mean, 0), "\nSD = ", round(SD, 0))), label.size = NA, size = 6) +
        geom_label(data = subset(all_depth_dat, all_depth_dat$Region == "NES"), aes(x = 350, y = 1.15, label = paste0("Mean = ", round(Mean, 0), "\nSD = ", round(SD, 0))), label.size = NA, size = 6) +
        ylab("Habitat suitability") +
        xlab("Depth") +
        ylim(c(0, 1.2)) +
        theme_bw(base_size = 18) +
        facet_wrap(~ Region, scales = "free_x") +
        ggtitle("Depth species-response curves") +
        theme(
            strip.background = element_rect(colour = NA, fill = NA),
            strip.text = element_text(size = 16, face = "bold"),
            plot.caption = element_text(hjust = 0)
        )
ggsave(here::here("results/depth_curve_res.jpg"), plot = depth_out, height = 8, width = 11)

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
all_sst_dat_res$Region <- factor(all_sst_dat_res$Region, levels = c("California_Current", "Northeast_US_Shelf"), labels = c("CC", "NES"))
future_sst_dat_rug$Region<- factor(future_sst_dat_rug$Region, levels = c("California_Current", "Northeast_US_Shelf"), labels = c("CC", "NES"))

sst_out_res_sd_rug <- ggplot() +
    geom_rect(data = all_sst_dat_res, aes(xmin = Mean - SD, xmax = Mean + SD, ymin = 0.01, ymax = 0.21), fill = "gray80") +
    geom_point(data = future_sst_dat_rug, aes(x = MeanSST, y = Y_val, color = Season), alpha = 0.5, inherit.aes = FALSE) +
    geom_path(data = , aes(x = MeanVar, y = Prediction), lwd = 2, color = "gray20") +
    # geom_ribbon(data = all_sst_dat, aes(x = MeanVar, ymax = Prediction, ymin = 0), lwd = 2, color = "gray20", alpha = 0.5, fill = NA) +
    scale_color_manual(name = "Season", values = colors_use) +
    xlab("SST (deg C)") +
    geom_label(data = , aes(x = 2.5, y = 0.95, label = paste0("Mean = ", round(Res_Mean, 2), "\nSD = ", round(Res_SD, 2))), label.size = NA) +
    ylab("Habitat suitability") +
    ylim(c(0, 1)) +
    xlim(c(-1, 30)) +
    theme_bw(base_size = 16) +
    facet_wrap(~Region) +
    ggtitle("Mobile resident species archetype") +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold"),
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
future_sst_dat_rug$Region<- factor(future_sst_dat_rug$Region, levels = c("California_Current", "Northeast_US_Shelf"), labels = c("CC", "NES"))

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
    theme_bw(base_size = 16) +
    facet_wrap(~Region) +
    ggtitle("Seasonal migrant species archetype") +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold"),
        plot.caption = element_text(hjust = 0) 
    )

sst_out_rug <- sst_out_res_sd_rug / sst_out_seas_sd_rug + plot_layout(guides = "collect")
ggsave(paste0(here::here("results/sst_res_rug.jpg")), plot = sst_out_rug, height = 8, width = 11)

all_sst_dat_res$Species_Archetype <- "Resident-mobile"
all_sst_dat_seas$Species_Archetype <- "Seasonally-migrating warm water"

sst_curve_dat <-  all_sst_dat_res %>%
    bind_rows(., all_sst_dat_seas)
    
sst_curve_dat_res <- sst_curve_dat %>%
    filter(., Species_Archetype == "Resident-mobile")

sst_curve_res <- ggplot() +
    geom_path(data = sst_curve_dat_res, aes(x = MeanVar, y = Prediction), lwd = 2, color = "gray20") +
    xlab("SST (deg C)") +
    geom_label(data = sst_curve_dat_res, aes(x = 2.5, y = 0.95, label = paste0("Mean = ", round(Res_Mean, 2), "\nSD = ", round(Res_SD, 2)))) +
    ylab("Prediction") +
    ylim(c(0, 1)) +
    xlim(c(-1, 30)) +
    theme_bw(base_size = 14) +
    facet_wrap(~Region) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )

sst_curve_dat_seas <- sst_curve_dat %>%
    filter(., Species_Archetype == "Seasonally-migrating warm water")
sst_curve_dat_seas$Region<- factor(sst_curve_dat_seas$Region, levels = c("California_Current", "Northeast_US_Shelf"), labels = c("CC", "NES"))

sst_curve_both <- sst_curve_res +
    geom_path(data = sst_curve_dat_seas, aes(x = MeanVar, y = Prediction), lty = "dashed", lwd = 2, color = "gray20") +
    xlab("SST (deg C)") +
    geom_label(data = sst_curve_dat_seas, aes(x = 27, y = 0.95, label = paste0("Mean = ", round(SeasWarm_Mean, 2), "\nSD = ", round(SeasWarm_SD, 2)))) +
    ylab("Prediction") +
    ylim(c(0, 1)) +
    xlim(c(-1, 30)) +
    theme_bw(base_size = 14) +
    facet_wrap(~Region) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )
ggsave(paste0(here::here("results/sst_deriv_res.jpg")), plot = deriv_out, height = 8, width = 11)

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
ggsave(filename = here::here("pipelines/sim_spp/results/ExampleSurfaces.jpg"), height = 8, width = 15, dpi = 300, plot_out)

# ## Forecast years difference from baseline. This plot shows the difference for the forecast years by month from the baseline training conditions by month (monthly anomaly). Might want to lose month?
# scenarios <- c("res", "seas")

# plot_suit_out <- vector("list", length = length(unique(region_dat$Region)) * length(scenarios))
# names(plot_suit_out) <- paste(unique(region_dat$Region), rep(scenarios, each = length(unique(region_dat$Region))), sep = "_")

# suit_df_out <- vector("list", length = length(unique(region_dat$Region)) * length(scenarios))
# names(suit_df_out) <- paste(unique(region_dat$Region), rep(scenarios, each = length(unique(region_dat$Region))), sep = "_")

# for(g in seq_along(scenarios)){
#     scenario_use <- scenarios[g]
#     # hab_suit_files <- list.files(paste0("/Users/aallyn/Library/CloudStorage/Box-Box/Mills Lab/Projects/NASA_UNSDG19/Temp Results/vs_hab_suit_lme_", scenario_use, "/"), full.names = TRUE, pattern = "vs_suit")
#     hab_suit_files<- list.files(paste0(here::here("data/sim_spp/vs_hab_suit_lme"), "_", scenario_use, "/"), full.names = TRUE, pattern = "vs_suit")
   
    
#     for(i in seq_along(hab_suit_files)){
#         hab_suit_temp <- readRDS(hab_suit_files[i])
#         suit_rasts <- raster::stack(sapply(hab_suit_temp[1:length(hab_suit_temp) - 1], "[[", 3))
        
#         suit_df <- as.data.frame(suit_rasts, xy = TRUE) %>%
#             pivot_longer(., -c(x, y), names_to = "raster.layer", values_to = "value") %>%
#             mutate(., "raster.layer" = gsub("X", "", raster.layer)) %>%
#             separate(., col = raster.layer, into = c("Year", "Month", "Day"), sep = "[.]") %>%
#             mutate(., "Date" = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
#             arrange(., Date)
        
#         # Get region
#         region_use <- gsub("_vs_suit.rds", "", stringr::str_remove(hab_suit_files[i], ".*\\/"))
        
#         # Subset dates
#         suit_df_fore <- suit_df %>%
#             filter(., Date >= region_dat$Start_Year_Fore[[which(region_dat$Region == region_use)]] & Date < region_dat$End_Year_Fore[[which(region_dat$Region == region_use)]]) %>%
#             group_by(., x, y, Month) %>%
#             summarize_at(., "value", list(Fore = mean), na.rm = TRUE)

#         suit_df_base <- suit_df %>%
#             filter(., Date >= region_dat$Start_Year_Base[[which(region_dat$Region == region_use)]] & Date < region_dat$End_Year_Base[[which(region_dat$Region == region_use)]]) %>%
#             group_by(., x, y, Month) %>%
#             summarize_at(., "value", list(Base = mean), na.rm = TRUE)
        
#         suit_df <- suit_df_base %>%
#             left_join(suit_df_fore) %>%
#             mutate(., "Difference" = Fore - Base)
        
#         suit_df_out[[i]] <- suit_df
        
#         # Get bbox
#         bbox <- st_bbox(region_dat$Shapefile[[which(region_dat$Region == region_use)]])
        
#         plot_suit_temp <- ggplot() +
#             geom_raster(data = suit_df, aes(x = x, y = y, fill = Difference), na.rm = TRUE) +
#             scale_fill_gradient2(name = "Habitat suitability difference", na.value = "transparent", low = "#2166ac", high = "#b2182b") +
#             geom_sf(data = land, color = "#d9d9d9") +
#             coord_sf(xlim = bbox[c(1, 3)], ylim = bbox[c(2, 4)]) +
#             xlab("Longitude") +
#             ylab("Latitude") +
#             theme_bw() +
#             facet_wrap(~Month) +
#             theme(
#                 strip.background = element_rect(colour = NA, fill = NA),
#                 strip.text = element_text(size = 16, face = "bold")
#             )
        
#         plot_suit_out[[which(names(plot_suit_out) == paste(region_use, scenario_use, sep = "_"))]] <- plot_suit_temp
#         ggsave(filename = paste0("/Users/aallyn/Library/CloudStorage/Box-Box/Mills Lab/Projects/NASA_UNSDG19/Temp Results/", region_use, "_hab_suit_diff_", scenario_use, ".jpg"), height = 8, width = 11, dpi = 300, plot_suit_temp)
#     }
# }


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
    hab_suit_files<- list.files(paste0(here::here("data/sim_spp/vs_hab_suit_lme"), "_", scenario_use, "/"), full.names = TRUE, pattern = "vs_suit")
   
    
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
ggsave(filename = here::here("pipelines/sim_spp/results/COG_Lat.jpg"), height = 8, width = 11, dpi = 300, lat_out)

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

ggsave(filename = here::here("pipelines/sim_spp/results/COG_Lat.jpg"), height = 8, width = 11, dpi = 300, lat_out)
ggsave(filename = here::here("pipelines/sim_spp/results/COG_Lon.jpg"), height = 8, width = 11, dpi = 300, lon_out)

#####
## Results -- getting prediction skill stats and environmental novelty measures
#####
# Read in results from 06_Pred_BRTs_Get_Env_Novelty.R
fore_summs_list <- readRDS(here::here("results/fore_summs_list.rds"))

# Unlist into one big dataframe and species archetype column
names(fore_summs_list)<- scenarios
# Get to a nested dataframe...
fore_summs <- dplyr::bind_rows(fore_summs_list, .id = "Species_Archetype")
fore_summs$Region<- factor(fore_summs$Region, levels = c("CCS", "NES"), labels = c("CC", "NES"))


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
hell_dist_plot<- ggplot() +
    geom_errorbar(data = hell_dist, aes(x = Plot_Date, ymin = Plot_Ymin, ymax = Plot_Ymax, color = Season, group = Season), alpha = 0.4) +
    geom_point(data = hell_dist, aes(x = Plot_Date, y = Mean_HellDist, fill = Season, color = Season), size = 3, alpha = 0.4, pch = 21) +
    stat_smooth(data = hell_dist, aes(x = Plot_Date, y = Mean_HellDist, fill = Season, color = Season), method = "lm", formula = y ~ x, se = F, alpha = 0.4) +
    scale_fill_manual(name = "Prediction Target Season", values = colors_use) +
    scale_color_manual(name = "Prediction Target Season", values = colors_use) +
    scale_y_continuous(name = "Hellinger's Distance environmental novelty\nrelative to 1985-2004") +
    xlab("Year") +
    facet_wrap(~ Region) +
    theme_bw(base_size = 16) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA)
    )
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "HellDist_TS", plot_suff, ".jpg"), height = 8, width = 11, dpi = 300, hell_dist_plot)

out<- sst_anom_plot / hell_dist_plot + plot_layout(heights = c(0.6, 1))
ggsave(filename = paste0("~/Desktop/", "UncertFig3", ".png"), height = 8, width = 11, dpi = 600, out)


# sst_dat_temp <- fore_summs %>%
#     ungroup() %>%
#     dplyr::select(., Region, Month, Year, FitSSTMean, FitSSTSD, PredSSTMean, PredSSTSD) %>%
#     mutate(., "Date" = as.Date(paste(Year, Month, "16", sep = "-"), "%Y-%m-%d"))
# sst_dat_temp2 <- sst_dat_temp %>%
#     group_by(Region, Year) %>%
#     summarize(.,
#         "Mean_SST" = mean(PredSSTMean, na.rm = TRUE),
#         "SD_SST" = sd(PredSSTMean, na.rm = TRUE)
#     ) %>%
#     mutate(.,
#         "Year" = as.Date(Year, "%Y"),
#         "Plot_Ymin" = Mean_SST - SD_SST,
#         "Plot_Ymax" = Mean_SST + SD_SST
#     )

# ggplot() +
#     geom_line(data = sst_dat_temp, aes(x = Date, y = FitSSTMean, color = Region), alpha = 0.5, lwd = 4) +
#     geom_errorbar(data = sst_dat_temp2, aes(x = Year, ymin = Plot_Ymin, ymax = Plot_Ymax, color = Region)) +
#     scale_fill_manual(name = "Large marine ecosystem", values = colors_use) +
#     scale_color_manual(name = "Large marine ecosystem", values = colors_use) +
#     scale_y_continuous(name = "Sea surface temperature") +
#     facet_wrap(~Region) +
#     theme_bw(base_size = 16)

#####
## Deviance explained
#####
dev_expl_brt<- function(model){
    int.null.deviance = model$self.statistics$mean.null 
    int.residual.deviance = model$cv.statistics$deviance.mean
    (int.null.deviance - int.residual.deviance)/int.null.deviance
}

cc_res <- readRDS(here::here("data/sim_spp/brt_fits_lme_res/cc/Base_1985-01-01_to_2004-01-01_fit.rds"))
dev_expl_brt(cc_res)

ne_res<- readRDS(here::here("data/sim_spp/brt_fits_lme_res/ne/Base_1985-01-01_to_2004-01-01_fit.rds"))
dev_expl_brt(ne_res)

cc_seas <- readRDS(here::here("data/sim_spp/brt_fits_lme_seas/cc/Base_1985-01-01_to_2004-01-01_fit.rds"))
dev_expl_brt(cc_seas)

ne_seas<- readRDS(here::here("data/sim_spp/brt_fits_lme_seas/ne/Base_1985-01-01_to_2004-01-01_fit.rds"))
dev_expl_brt(ne_seas)

#####
## Parameter importance
#####
summary(cc_res)


#####
## Estimation curves
#####
# Would need to get sst_curve_dat_res object
# sst_curve_dat_res$Region <- factor(sst_curve_dat_res$Region, levels = c("California_Current", "Northeast_US_Shelf"), labels = c("CC", "NES"))
# sst_curve_dat_seas$Region<- factor(sst_curve_dat_seas$Region, levels = c("California_Current", "Northeast_US_Shelf"), labels = c("CC", "NES"))
# brt_fits_res <- fore_summs %>%
#     filter(., Species_Archetype == "res") %>%
#     ungroup() %>%
#     distinct(Region, BRT_SST_Fit) %>%
#     unnest(cols = c(BRT_SST_Fit))
# names(brt_fits_res)[2:3]<- c("oisst_daily", "y")
# brt_fits_res$Region <- factor(brt_fits_res$Region, levels = c("CC", "NES"), labels = c("CC", "NES"))

# brt_fits_seas <- fore_summs %>%
#     filter(., Species_Archetype == "seas") %>%
#     ungroup() %>%
#     distinct(Region, BRT_SST_Fit) %>%
#     unnest(cols = c(BRT_SST_Fit))
# names(brt_fits_seas)[2:3]<- c("oisst_daily", "y")
# brt_fits_seas$Region <- factor(brt_fits_seas$Region, levels = c("CC", "NES"), labels = c("CC", "NES"))

# sst_op_est_res<- ggplot() +
#         geom_path(data = sst_curve_dat_res, aes(x = MeanVar, y = Prediction), lwd = 2.5, color = "gray20", alpha = 0.5) +
#         geom_path(data = brt_fits_res, aes(x = oisst_daily, y = y), color = "#1b9e77", lwd = 1.5, alpha = 0.5) +
#         xlab("SST (deg C)") +
#         ylab("Habitat suitability") +
#         ylim(c(0, 1)) +
#         xlim(c(-1, 30)) +
#         theme_bw(base_size = 18) +
#         facet_wrap(~ Region, ncol = 2) +
#         ggtitle("Resident-mobile species archetype") +
#         theme(
#             strip.background = element_rect(colour = NA, fill = NA),
#             strip.text = element_text(size = 16, face = "bold"),
#             plot.caption = element_text(hjust = 0) 
#         ) 
# sst_op_est_seas<- ggplot() +
#         geom_path(data = sst_curve_dat_seas, aes(x = MeanVar, y = Prediction), lwd = 2.5, color = "gray20", alpha = 0.5) +
#         geom_path(data = brt_fits_seas, aes(x = oisst_daily, y = y), color = "#1b9e77", lwd = 1.5, alpha = 0.5) +
#         xlab("SST (deg C)") +
#         ylab("Habitat suitability") +
#         ylim(c(0, 1)) +
#         xlim(c(-1, 30)) +
#         theme_bw(base_size = 18) +
#         facet_wrap(~ Region, ncol = 2) +
#         ggtitle("Seasonally-migrating warm water species archetype") +
#         theme(
#             strip.background = element_rect(colour = NA, fill = NA),
#             strip.text = element_text(size = 16, face = "bold"),
#             plot.caption = element_text(hjust = 0)
#         ) 
# sst_op_est_both<- sst_op_est_res / sst_op_est_seas + plot_layout(guides = "collect")   
# ggsave(paste0(here::here("pipelines/sim_spp/images/sst_op_est.jpg")), plot = sst_op_est_both, height = 8, width = 11) 

##### 
## Prediction statistics
#####
plot_dat <- fore_summs
plot_dat$Region <- factor(plot_dat$Region, levels = c("CC", "NES"), labels = c("CC", "NES"))
plot_dat <- plot_dat %>%
    filter(., Region %in% c("CC", "NES")) %>%
    left_join(., season_month_df)
plot_dat$Season <- factor(plot_dat$Season, levels = c("Winter", "Spring", "Summer", "Fall"))
plot_dat$Species_Archetype <- factor(plot_dat$Species_Archetype, levels = c("res", "seas"), labels = c("Mobile resident species archetype", "Seasonal migrant species archetype"))
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
    select(., Species_Archetype, Region, Scenario, ModelTrainStart, ModelTrainEnd, Month, Year, Season, data, PrAUC_Scaled, Cor, RMSE, Calib, PrAUC_Scaled, HellDist) %>%
    distinct()

corr_plot_res <- ggplot(data = subset(plot_dat_use, plot_dat_use$Species_Archetype == "Mobile resident species archetype"), aes(x = HellDist, y = round(Cor, 2), fill = Season, color = Season, shape = Season, group = Season, order = Season)) +
    geom_point(size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(method = "lm", formula = y ~ x, se = F) +
    # stat_poly_eq(geom = "text_npc", formula = y ~ x,
    #            label.x = "left",
    #            label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), 
    #            eq.with.lhs = "",
    #            aes(label = paste("bold(\"", factor(c("Winter", "Spring", "Summer", "Fall"), levels = c("Winter", "Spring", "Summer", "Fall")),
    #                              " \")*",
    #                              "italic(hat(y))~`=`~",
    #                              stat(eq.label),
    #                              sep = "")),
    #            parse = TRUE) +
    scale_fill_manual(name = "Prediction Target Season", values = colors_use) +
    scale_color_manual(name = "Prediction Target Season", values = colors_use) +
    xlab("Hellinger's Distance") +
    ylab("Correlation Coefficient\nPredicted habitat suitability vs. True Presence/Absence") +
    ylim(c(0, 0.9)) +
    ggtitle("Mobile resident species archetype") +
    facet_wrap(~ Region) +
    theme_bw(base_size = 16) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold"), 
        plot.caption = element_text(hjust = 0)
    )

corr_plot_seas <- ggplot(data = subset(plot_dat_use, plot_dat_use$Species_Archetype == "Seasonal migrant species archetype"), aes(x = HellDist, y = round(Cor, 2), fill = Season, color = Season, shape = Season, group = Season, order = Season)) +
    geom_point(size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(method = "lm", formula = y ~ x, se = F) +
    # stat_poly_eq(geom = "text_npc", formula = y ~ x,
    #            label.x = "left",
    #            label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), 
    #            eq.with.lhs = "",
    #            aes(label = paste("bold(\"", factor(c("Winter", "Spring", "Summer", "Fall"), levels = c("Winter", "Spring", "Summer", "Fall")),
    #                              " \")*",
    #                              "italic(hat(y))~`=`~",
    #                              stat(eq.label),
    #                              sep = "")),
    #            parse = TRUE) +
    scale_fill_manual(name = "Prediction Target Season", values = colors_use) +
    scale_color_manual(name = "Prediction Target Season", values = colors_use) +
    xlab("Hellinger's Distance") +
    ylab("Correlation Coefficient\nPredicted habitat suitability vs. True Presence/Absence") +
    ylim(c(0, 0.9)) +
    ggtitle("Seasonal migrant species archetype") +
    facet_wrap(~ Region) +
    theme_bw(base_size = 16) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold"), 
        plot.caption = element_text(hjust = 0)
    )
    
corr_out<- corr_plot_res / corr_plot_seas + plot_layout(guides = "collect")
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "CorrCoeff.jpg"), width = 18, height = 15, dpi = 300, corr_out)

pr_auc_plot_res <- ggplot(data = subset(plot_dat_use, plot_dat_use$Species_Archetype == "Mobile resident species archetype"), aes(x = HellDist, y = round(PrAUC_Scaled, 2), fill = Season, color = Season, shape = Season, group = Season, order = Season)) +
    geom_point(size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(method = "lm", formula = y ~ x, se = F) +
    # stat_poly_eq(geom = "text_npc", formula = y ~ x,
    #            label.x = "left",
    #            label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), 
    #            eq.with.lhs = "",
    #            aes(label = paste("bold(\"", factor(c("Winter", "Spring", "Summer", "Fall"), levels = c("Winter", "Spring", "Summer", "Fall")),
    #                              " \")*",
    #                              "italic(hat(y))~`=`~",
    #                              stat(eq.label),
    #                              sep = "")),
    #            parse = TRUE) +
    scale_fill_manual(name = "Prediction Target Season", values = colors_use) +
    scale_color_manual(name = "Prediction Target Season", values = colors_use) +
    xlab("Hellinger's Distance") +
    ylab("Scaled area under the precision-recall curve (PrAUC)") +
    ylim(c(0, 1.06)) +
    ggtitle("Mobile resident species archetype") +
    facet_wrap(~ Region) +
    theme_bw(base_size = 16) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold"),
        plot.caption = element_text(hjust = 0) 
    )

pr_auc_plot_seas<- ggplot(data = subset(plot_dat_use, plot_dat_use$Species_Archetype == "Seasonal migrant species archetype"), aes(x = HellDist, y = round(PrAUC_Scaled, 2), fill = Season, color = Season, shape = Season, group = Season, order = Season)) +
    geom_point(size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(method = "lm", formula = y ~ x, se = F) +
    # stat_poly_eq(geom = "text_npc", formula = y ~ x,
    #            label.x = "left",
    #            label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), 
    #            eq.with.lhs = "",
    #            aes(label = paste("bold(\"", factor(c("Winter", "Spring", "Summer", "Fall"), levels = c("Winter", "Spring", "Summer", "Fall")),
    #                              " \")*",
    #                              "italic(hat(y))~`=`~",
    #                              stat(eq.label),
    #                              sep = "")),
    #            parse = TRUE) +
    scale_fill_manual(name = "Prediction Target Season", values = colors_use) +
    scale_color_manual(name = "Prediction Target Season", values = colors_use) +
    xlab("Hellinger's Distance") +
    ylab("Scaled area under the precision-recall curve (PrAUC)") +
    ylim(c(0, 1.06)) +
    ggtitle("Seasonal migrant species archetype") +
    facet_wrap(~ Region) +
    theme_bw(base_size = 16) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold"),
        plot.caption = element_text(hjust = 0) 
    )
pr_auc_out<- pr_auc_plot_res / pr_auc_plot_seas + plot_layout(guides = "collect")
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "PrAUC_Scaled.jpg"), width = 18, height = 15, dpi = 300, pr_auc_out)

rmse_plot_res <- ggplot(data = subset(plot_dat_use, plot_dat_use$Species_Archetype == "Mobile resident species archetype"), aes(x = HellDist, y = round(RMSE, 2), fill = Season, color = Season, shape = Season, group = Season, order = Season)) +
    geom_point(size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(method = "lm", formula = y ~ x, se = F) +
    # stat_poly_eq(geom = "text_npc", formula = y ~ x,
    #            label.x = "left",
    #            label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), 
    #            eq.with.lhs = "",
    #            aes(label = paste("bold(\"", factor(c("Winter", "Spring", "Summer", "Fall"), levels = c("Winter", "Spring", "Summer", "Fall")),
    #                              " \")*",
    #                              "italic(hat(y))~`=`~",
    #                              stat(eq.label),
    #                              sep = "")),
    #            parse = TRUE) +
    scale_fill_manual(name = "Prediction Target Season", values = colors_use) +
    scale_color_manual(name = "Prediction Target Season", values = colors_use) +
    xlab("Hellinger's Distance") +
    ylab("Root mean square error") +
    ylim(c(0.19, 0.5)) +
    ggtitle("Mobile resident species archetype") +
    facet_wrap(~ Region) +
    theme_bw(base_size = 16) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold"),
        plot.caption = element_text(hjust = 0) 
    )

rmse_plot_seas<- ggplot(data = subset(plot_dat_use, plot_dat_use$Species_Archetype == "Seasonal migrant species archetype"), aes(x = HellDist, y = round(RMSE, 2), fill = Season, color = Season, shape = Season, group = Season, order = Season)) +
    geom_point(size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(method = "lm", formula = y ~ x, se = F) +
    # stat_poly_eq(geom = "text_npc", formula = y ~ x,
    #            label.x = "left",
    #            label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), 
    #            eq.with.lhs = "",
    #            aes(label = paste("bold(\"", factor(c("Winter", "Spring", "Summer", "Fall"), levels = c("Winter", "Spring", "Summer", "Fall")),
    #                              " \")*",
    #                              "italic(hat(y))~`=`~",
    #                              stat(eq.label),
    #                              sep = "")),
    #            parse = TRUE) +
    scale_fill_manual(name = "Prediction Target Season", values = colors_use) +
    scale_color_manual(name = "Prediction Target Season", values = colors_use) +
    xlab("Hellinger's Distance") +
    ylab("Root mean square error") +
    ylim(c(0.19, 0.5)) +
    ggtitle("Seasonal migrant species archetype") +
    facet_wrap(~ Region) +
    theme_bw(base_size = 16) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold"),
        plot.caption = element_text(hjust = 0) 
    )
rmse_out<- rmse_plot_res / rmse_plot_seas + plot_layout(guides = "collect")
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "RMSE.jpg"), width = 18, height = 15, dpi = 300, rmse_out)

calib_plot_res <- ggplot(data = subset(plot_dat_use, plot_dat_use$Species_Archetype == "Mobile resident species archetype"), aes(x = HellDist, y = round(Calib, 2), fill = Season, color = Season, shape = Season, group = Season, order = Season)) +
    geom_point(size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(method = "lm", formula = y ~ x, se = F) +
    # stat_poly_eq(geom = "text_npc", formula = y ~ x,
    #            label.x = "left",
    #            label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), 
    #            eq.with.lhs = "",
    #            aes(label = paste("bold(\"", factor(c("Winter", "Spring", "Summer", "Fall"), levels = c("Winter", "Spring", "Summer", "Fall")),
    #                              " \")*",
    #                              "italic(hat(y))~`=`~",
    #                              stat(eq.label),
    #                              sep = "")),
    #            parse = TRUE) +
    scale_fill_manual(name = "Prediction Target Season", values = colors_use) +
    scale_color_manual(name = "Prediction Target Season", values = colors_use) +
    xlab("Hellinger's Distance") +
    ylab("Calibration") +
    ylim(c(0, 0.08)) + 
    ggtitle("Mobile resident species archetype") +
    facet_wrap(~ Region) +
    theme_bw(base_size = 16) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold"),
        plot.caption = element_text(hjust = 0) 
    )

calib_plot_seas<- ggplot(data = subset(plot_dat_use, plot_dat_use$Species_Archetype == "Seasonal migrant species archetype"), aes(x = HellDist, y = round(Calib, 2), fill = Season, color = Season, shape = Season, group = Season, order = Season)) +
    geom_point(size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(method = "lm", formula = y ~ x, se = F) +
    # stat_poly_eq(geom = "text_npc", formula = y ~ x,
    #            label.x = "left",
    #            label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), 
    #            eq.with.lhs = "",
    #            aes(label = paste("bold(\"", factor(c("Winter", "Spring", "Summer", "Fall"), levels = c("Winter", "Spring", "Summer", "Fall")),
    #                              " \")*",
    #                              "italic(hat(y))~`=`~",
    #                              stat(eq.label),
    #                              sep = "")),
    #            parse = TRUE) +
    scale_fill_manual(name = "Prediction Target Season", values = colors_use) +
    scale_color_manual(name = "Prediction Target Season", values = colors_use) +
    xlab("Hellinger's Distance") +
    ylab("Calibration") +
    ggtitle("Seasonal migrant species archetype") +
    ylim(c(0, 0.08)) + 
    facet_wrap(~ Region) +
    theme_bw(base_size = 16) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold"),
        plot.caption = element_text(hjust = 0) 
    )
calib_out<- calib_plot_res / calib_plot_seas + plot_layout(guides = "collect")
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "Calib.jpg"), width = 18, height = 15, dpi = 300, calib_out)

#####
## Modeling prediction skill as a function of other variables
#####
species_archetypes <- unique(plot_dat_use$Species_Archetype)

for(g in seq_along(species_archetypes)){
    cc_dat <- plot_dat_use %>%
        filter(., Region == "CC" & Species_Archetype == species_archetypes[g])
    # corr_lm <- lm(Cor ~ HellDist * Season, data = cc_dat)
    pr_auc_scaled_lm <- lm(PrAUC_Scaled ~ HellDist * Season, data = cc_dat)
    calib_lm <- lm(Calib ~ HellDist * Season, data = cc_dat)
    # rmse_lm <- lm(RMSE ~ HellDist * Season, data = cc_dat)

    # Marginal effects
    ggeffect(pr_auc_scaled_lm, terms = c("HellDist", "Season")) %>%
        plot()
    ggeffect(calib_lm, terms = c("HellDist", "Season")) %>%
        plot()
    
    
    models <- list("PrAUC_Scaled" = pr_auc_scaled_lm, "Calib" = calib_lm)
    cc_mod_summ <- modelsummary(models,
        fmt = 3,
        estimate = c("{estimate} ({std.error}){stars}"),
        stars = c("*" = 0.05),
        coef_rename = c(
            "HellDist" = "Hellinger Distance",
            "SeasonSpring" = "Spring", "SeasonSummer" = "Summer", "SeasonFall" = "Fall",
            "HellDist:SeasonSpring" = "Hellingers Distance:Spring",
            "HellDist:SeasonSummer" = "Hellingers Distance:Summer",
            "HellDist:SeasonFall" = "Hellingers Distance:Fall"
        ),
        statistic = NULL,
        coef_omit = "Intercept",
        gof_omit = c("Num.Obs|R2|BIC|Log.Lik.|F|RMSE|AIC"),
        output = "data.frame"
    )

    # GT testing
    cc_mod_summ <- cc_mod_summ %>%
        select(-part, -statistic) 
    
    which_sig <- sapply(rownames(cc_mod_summ), function(x) grep("\\*$", cc_mod_summ[x, ])) %>%
        enframe() %>%
        unnest()
    colnames(which_sig) <- c("Row", "Col")

    which_neg <- sapply(rownames(cc_mod_summ), function(x) grep("-", cc_mod_summ[x, ])) %>%
        enframe() %>%
        unnest()
    colnames(which_neg) <- c("Row_Neg", "Col_Neg")
    which_neg$Sign<- "Negative"

    # Changes
    changes <- which_sig %>%
        left_join(., which_neg, by = c("Row" = "Row_Neg", "Col" = "Col_Neg"))
    
    changes$Sign[is.na(changes$Sign)]<- "Positive"
    
    # PrAUC significant AND positive is good -- which_sig value = 2, which_pos = T and Calib significant AND negative is good -- which_sig value = 3, which_pos = F
    good_change_prauc <- changes %>%
        filter(., Col == 2 & Sign == "Positive")
    bad_change_prauc<- changes %>%
        filter(., Col == 2 & Sign == "Negative")
    good_change_calib <- changes %>%
        filter(., Col == 3 & Sign == "Negative")
    bad_change_calib <- changes %>%
        filter(., Col == 3 & Sign == "Positive")
    
    cc_mod_summ_num <- cc_mod_summ %>%
        gt() %>%
        tab_header(
            title = as.character(species_archetypes[g]),
            subtitle = "CC"
        ) %>%
        tab_style(
            style = list(
                cell_text(color = "#5AAE61", weight = "bold")
            ),
            locations = cells_body(
                columns = as.numeric(good_change_prauc$Col),
                rows = as.numeric(good_change_prauc$Row)
            )
        ) %>%
        tab_style(
            style = list(
                cell_text(color = "#5AAE61", weight = "bold")
            ),
            locations = cells_body(
                columns = as.numeric(good_change_calib$Col),
                rows = as.numeric(good_change_calib$Row)
            )
        ) %>%
        tab_style(
            style = list(
                cell_text(color = "#9970AB", weight = "bold")
            ),
            locations = cells_body(
                columns = as.numeric(bad_change_prauc$Col),
                rows = as.numeric(bad_change_prauc$Row)
            )
        ) %>%
        tab_style(
            style = list(
                cell_text(color = "#9970AB", weight = "bold")
            ),
            locations = cells_body(
                columns = as.numeric(bad_change_calib$Col),
                rows = as.numeric(bad_change_calib$Row)
            )
        )

    # Save numeric one
    gtsave(cc_mod_summ_num, here::here(paste0("pipelines/sim_spp/images/CC_", species_archetypes[g], "_PredSkill_LM_num.png")))
    
    icon_fun <- function(icon, fill, val) {
        fontawesome::fa(icon, fill = fill) %>%
            rep(., val) %>%
            gt::html()
    }

    changes_neut <- expand.grid("Row" = as.character(seq(1, 7)), "Col" = seq(2, 3)) %>%
        anti_join(., which_sig)
    
    cc_mod_summ_icon <- cc_mod_summ %>%
        gt() %>%
        tab_header(
            title = as.character(species_archetypes[g]),
            subtitle = "CC"
        ) %>%
        text_transform(
            locations = cells_body(
                columns = as.numeric(changes_neut$Col),
                rows = as.numeric(changes_neut$Row)
            ),
            fn = function(x) {
                icon_fun(icon = "fas fa-circle", fill = "gray", val = 1)
            }
        ) %>%
        text_transform(
            locations = cells_body(
                columns = as.numeric(good_change_prauc$Col),
                rows = as.numeric(good_change_prauc$Row)
            ),
            fn = function(x) {
                icon_fun(icon = "fas fa-thumbs-up", fill = "#5AAE61", val = 1)
            }
        ) %>%
        text_transform(
            locations = cells_body(
                columns = as.numeric(good_change_calib$Col),
                rows = as.numeric(good_change_calib$Row)
            ),
            fn = function(x) {
                icon_fun(icon = "fas fa-thumbs-up", fill = "#5AAE61", val = 1)
            }
        ) %>%
         text_transform(
            locations = cells_body(
                columns = as.numeric(bad_change_prauc$Col),
                rows = as.numeric(bad_change_prauc$Row)
            ),
            fn = function(x) {
                icon_fun(icon = "fas fa-thumbs-down", fill = "#9970AB", val = 1)
            }
        ) %>%
        text_transform(
            locations = cells_body(
                columns = as.numeric(bad_change_calib$Col),
                rows = as.numeric(bad_change_calib$Row)
            ),
            fn = function(x) {
                icon_fun(icon = "fas fa-thumbs-down", fill = "#9970AB", val = 1)
            }
        ) 
    gtsave(cc_mod_summ_icon, here::here(paste0("pipelines/sim_spp/images/CC_", species_archetypes[g], "_PredSkill_LM_icon.png")))

    rm(models)
    ne_dat <- plot_dat_use %>%
        filter(., Region == "NES" & Species_Archetype == species_archetypes[g])
    
    # corr_lm <- lm(Cor ~ HellDist * Season, data = ne_dat)
    pr_auc_scaled_lm <- lm(PrAUC_Scaled ~ HellDist * Season, data = ne_dat)
    calib_lm <- lm(Calib ~ HellDist * Season, data = ne_dat)
    # rmse_lm <- lm(RMSE ~ HellDist * Season, data = ne_dat)
    
    models<- list("PrAUC_Scaled" = pr_auc_scaled_lm, "Calib" = calib_lm)
    ne_mod_summ <- modelsummary(models,
        fmt = 2,
        estimate = c("{estimate} ({std.error}){stars}"),
        stars = c("*" = 0.05),
        coef_rename = c(
            "HellDist" = "Hellinger Distance",
            "SeasonSpring" = "Spring", "SeasonSummer" = "Summer", "SeasonFall" = "Fall",
            "HellDist:SeasonSpring" = "Hellingers Distance:Spring",
            "HellDist:SeasonSummer" = "Hellingers Distance:Summer",
            "HellDist:SeasonFall" = "Hellingers Distance:Fall"
        ),
        statistic = NULL,
        coef_omit = "Intercept",
        gof_omit = c("Num.Obs|R2|BIC|Log.Lik.|F|RMSE|AIC"),
        output = "data.frame"
    )
    
    ne_mod_summ <- ne_mod_summ %>%
        select(-part, -statistic)
    
    which_sig <- sapply(rownames(ne_mod_summ), function(x) grep("\\*$", ne_mod_summ[x, ])) %>%
        enframe() %>%
        unnest()
    colnames(which_sig) <- c("Row", "Col")

    which_neg <- sapply(rownames(ne_mod_summ), function(x) grep("-", ne_mod_summ[x, ])) %>%
        enframe() %>%
        unnest()
    colnames(which_neg) <- c("Row_Neg", "Col_Neg")
    which_neg$Sign<- "Negative"

    # Changes
    changes <- which_sig %>%
        left_join(., which_neg, by = c("Row" = "Row_Neg", "Col" = "Col_Neg"))
    
    changes$Sign[is.na(changes$Sign)]<- "Positive"
    
    # PrAUC significant AND positive is good -- which_sig value = 2, which_pos = T and Calib significant AND negative is good -- which_sig value = 3, which_pos = F
    good_change_prauc <- changes %>%
        filter(., Col == 2 & Sign == "Positive")
    bad_change_prauc<- changes %>%
        filter(., Col == 2 & Sign == "Negative")
    good_change_calib <- changes %>%
        filter(., Col == 3 & Sign == "Negative")
    bad_change_calib <- changes %>%
        filter(., Col == 3 & Sign == "Positive")
    
    ne_mod_summ_num <- ne_mod_summ %>%
        gt() %>%
        tab_header(
            title = as.character(species_archetypes[g]),
            subtitle = "NES"
        ) %>%
        tab_style(
            style = list(
                cell_text(color = "#5AAE61", weight = "bold")
            ),
            locations = cells_body(
                columns = as.numeric(good_change_prauc$Col),
                rows = as.numeric(good_change_prauc$Row)
            )
        ) %>%
        tab_style(
            style = list(
                cell_text(color = "#5AAE61", weight = "bold")
            ),
            locations = cells_body(
                columns = as.numeric(good_change_calib$Col),
                rows = as.numeric(good_change_calib$Row)
            )
        ) %>%
        tab_style(
            style = list(
                cell_text(color = "#9970AB", weight = "bold")
            ),
            locations = cells_body(
                columns = as.numeric(bad_change_prauc$Col),
                rows = as.numeric(bad_change_prauc$Row)
            )
        ) %>%
        tab_style(
            style = list(
                cell_text(color = "#9970AB", weight = "bold")
            ),
            locations = cells_body(
                columns = as.numeric(bad_change_calib$Col),
                rows = as.numeric(bad_change_calib$Row)
            )
        ) %>%
        cols_hide("term")

    # Save numeric one
    gtsave(ne_mod_summ_num, here::here(paste0("pipelines/sim_spp/images/NES_", species_archetypes[g], "_PredSkill_LM_num.png")))
    
    icon_fun <- function(icon, fill, val) {
        fontawesome::fa(icon, fill = fill) %>%
            rep(., val) %>%
            gt::html()
    }

    changes_neut <- expand.grid("Row" = as.character(seq(1, 7)), "Col" = seq(2, 3)) %>%
        anti_join(., which_sig)
    
    ne_mod_summ_icon <- ne_mod_summ %>%
        gt() %>%
        tab_header(
            title = as.character(species_archetypes[g]),
            subtitle = "NES"
        ) %>%
        text_transform(
            locations = cells_body(
                columns = as.numeric(changes_neut$Col),
                rows = as.numeric(changes_neut$Row)
            ),
            fn = function(x) {
                icon_fun(icon = "fas fa-circle", fill = "gray", val = 1)
            }
        ) %>%
        text_transform(
            locations = cells_body(
                columns = as.numeric(good_change_prauc$Col),
                rows = as.numeric(good_change_prauc$Row)
            ),
            fn = function(x) {
                icon_fun(icon = "fas fa-thumbs-up", fill = "#5AAE61", val = 1)
            }
        ) %>%
        text_transform(
            locations = cells_body(
                columns = as.numeric(good_change_calib$Col),
                rows = as.numeric(good_change_calib$Row)
            ),
            fn = function(x) {
                icon_fun(icon = "fas fa-thumbs-up", fill = "#5AAE61", val = 1)
            }
        ) %>%
         text_transform(
            locations = cells_body(
                columns = as.numeric(bad_change_prauc$Col),
                rows = as.numeric(bad_change_prauc$Row)
            ),
            fn = function(x) {
                icon_fun(icon = "fas fa-thumbs-down", fill = "#9970AB", val = 1)
            }
        ) %>%
        text_transform(
            locations = cells_body(
                columns = as.numeric(bad_change_calib$Col),
                rows = as.numeric(bad_change_calib$Row)
            ),
            fn = function(x) {
                icon_fun(icon = "fas fa-thumbs-down", fill = "#9970AB", val = 1)
            }
        ) %>%
        cols_hide("term")
    gtsave(ne_mod_summ_icon, here::here(paste0("pipelines/sim_spp/images/NES_", species_archetypes[g], "_PredSkill_LM_icon.png")))

    # Combined tables
    library(cowplot)
    num_plot_cc <- ggdraw() + draw_image(here::here(paste0("pipelines/sim_spp/images/CC_", species_archetypes[g], "_PredSkill_LM_num.png")))
    num_plot_nes <- ggdraw() + draw_image(here::here(paste0("pipelines/sim_spp/images/NES_", species_archetypes[g], "_PredSkill_LM_num.png")))
    num_combo <- plot_grid(num_plot_cc, num_plot_nes, rel_widths = c(1, 0.998), rel_heights = c(1, 0.998))
    save_plot(here::here(paste0("pipelines/sim_spp/images/", species_archetypes[g], "_PredSkill_LM_num.png")), num_combo)

    icon_plot_cc <- ggdraw() + draw_image(here::here(paste0("pipelines/sim_spp/images/CC_", species_archetypes[g], "_PredSkill_LM_icon.png")))
    icon_plot_nes <- ggdraw() + draw_image(here::here(paste0("pipelines/sim_spp/images/NES_", species_archetypes[g], "_PredSkill_LM_icon.png")))
    icon_combo <- plot_grid(icon_plot_cc, icon_plot_nes, rel_widths = c(1, 0.998), rel_heights = c(1, 0.998))
    save_plot(here::here(paste0("pipelines/sim_spp/images/", species_archetypes[g], "_PredSkill_LM_icon.png")), icon_combo)
}

## Marginal effects
install.packages("ggeffects")
library(ggeffects)
ggeffect(calib.lm, terms = c("HellDist", "Season")) %>% 
  plot()








# How does novelty influence the proportion of good/bad habitat by season?
plot_dat_temp <- plot_dat %>%
    ungroup() %>%
    dplyr::select(., Species_Archetype, Region, Scenario, Season, HellDist, AvgProbs, HighProbs, LowProbs) %>%
    gather(., key = "Probs", value = "Value", -Species_Archetype, -Region, -Scenario, -Season, -HellDist)
plot_dat_temp$Probs <- factor(plot_dat_temp$Probs, levels = c("LowProbs", "AvgProbs", "HighProbs"))
plot_dat_temp$Species_Archetype<- gsub(" warm water", "", plot_dat_temp$Species_Archetype)

props_prob_plot<- ggplot(data = plot_dat_temp, aes(x = HellDist, y = Value, color = Probs, group = Probs)) +
    geom_point(aes(fill = Probs), size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(data = plot_dat_temp, method = "lm", formula = y ~ x, se = F) +
    stat_poly_eq(geom = "text_npc", formula = y ~ x,
               label.x = "left",
               label.y = rev(seq(from = 0.7, to = 1, length.out = 3)), 
               eq.with.lhs = "",
               aes(label = paste("italic(hat(y))~`=`~",
                                 stat(eq.label),
                                 sep = "")),
               parse = TRUE) +
    scale_fill_manual(name = "True Probability Value Bin", values = c("#313695", "#636363", "#a50026")) +
    scale_color_manual(name = "True Probability Value Bin", values = c("#313695", "#636363", "#a50026")) +
    scale_y_continuous("", limits = c(-0.15, 1.4)) +
    facet_wrap(~ Species_Archetype + Region + Season) +
    theme_bw(base_size = 14) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 14, face = "bold")
    )
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "PropProbs", plot_suff, ".jpg"), height = 11, width = 15, dpi = 300, props_prob_plot)

## Not just about Hellinger's Distance, but also WHERE that novelty occurs -- or where you fall on the normal curve
curve_loc_func <- function(mean_use, sd_use, pred_use) {
    pnorm_temp <- pnorm(pred_use, mean = mean_use, sd = sd_use)
    curve_loc_out <- abs(0.5 - pnorm_temp)
    return(curve_loc_out)
}

curve_loc <- fore_summs %>%
    ungroup() %>%
    dplyr::select(., Species_Archetype, Region, Scenario, Month, Year, FitSSTMean, FitSSTSD, PredSSTMean) %>%
    mutate(., "CurveLoc" = pmap_dbl(list(mean_use = FitSSTMean, sd_use = FitSSTSD, pred_use = PredSSTMean), curve_loc_func))

max_hdist_table <- plot_dat %>%
    dplyr::select(., Region, Scenario, HellDist) %>%
    group_by(., Region, Scenario) %>%
    summarize(., "Max_HellDist" = max(HellDist),
    "Min_HellDist" = min(HellDist))

curve_loc <- curve_loc %>%
    left_join(., max_hdist_table) %>%
    group_by(., Region) %>%
    nest()

rescale_group<- function(data){
    scale_out <- rescale(data$CurveLoc, min = unique(data$Min_HellDist), max = unique(data$Max_HellDist))
    return(scale_out)
}

curve_loc <- curve_loc %>%
    mutate(., "RescaledCurveLoc" = map(data, rescale_group)) %>%
    dplyr::select(., Region, RescaledCurveLoc) %>%
    unnest()

plot_dat$RescaledCurveLoc<- curve_loc$RescaledCurveLoc
plot_dat<- plot_dat %>%
    mutate(., "HellDistCurveLoc" = HellDist + RescaledCurveLoc)

## GLMM?
corr_mod <- lm(Cor ~ Species_Archetype * Region + HellDist * CurveLoc, data = plot_dat)
f1_mod <- lm(F1 ~ Species_Archetype * Region + HellDist * CurveLoc, data = plot_dat)
rmse_mod<- lm(RMSE ~ Species_Archetype * Region + HellDist * CurveLoc, data = plot_dat)

prev_plot <- ggplot(data = plot_dat, aes(x = HellDist, y = Prev, fill = Season, shape = Season, group = Season, color = Season)) +
    geom_point(, size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(data = plot_dat, aes(x = HellDist, y = Prev, color = Season, group = Season), method = "lm", formula = y ~ x, se = F) +
    stat_poly_eq(geom = "text_npc", formula = y ~ x,
               label.x = "left",
               label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), 
               eq.with.lhs = "",
               aes(label = paste("bold(\"", factor(c("Winter", "Spring", "Summer", "Fall"), levels = c("Winter", "Spring", "Summer", "Fall")),
                                 " \")*",
                                 "italic(hat(y))~`=`~",
                                 stat(eq.label),
                                 sep = "")),
               parse = TRUE) +
    scale_fill_manual(name = "Forecast Target Season", values = colors_use) +
    scale_color_manual(name = "Forecast Target Season", values = colors_use) +
    xlab("Hellinger's Distance") +
    ylab("Prevalence") +
    facet_wrap(~ Region + Species_Archetype) +
    theme_bw(base_size = 12) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "Prev", plot_suff, ".jpg"), height = 8, width = 11, dpi = 300, prev_plot)

tally_plot <- ggplot(data = plot_dat, aes(x = HellDist, y = Probs05, fill = Season, shape = Season, group = Season, color = Season)) +
    geom_point(, size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(data = plot_dat, aes(x = HellDist, y = Probs05, color = Season, group = Season), method = "lm", formula = y ~ x, se = F) +
    stat_poly_eq(geom = "text_npc", formula = y ~ x,
               label.x = "left",
               label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), 
               eq.with.lhs = "",
               aes(label = paste("bold(\"", factor(c("Winter", "Spring", "Summer", "Fall"), levels = c("Winter", "Spring", "Summer", "Fall")),
                                 " \")*",
                                 "italic(hat(y))~`=`~",
                                 stat(eq.label),
                                 sep = "")),
               parse = TRUE) +
    scale_fill_manual(name = "Forecast Target Season", values = colors_use) +
    scale_color_manual(name = "Forecast Target Season", values = colors_use) +
    xlab("Hellinger's Distance") +
    ylab("Proportion of cells\nwith true habitat suitability between 0.4 and 0.6") +
    facet_wrap(~ Region + Species_Archetype) +
    theme_bw(base_size = 12) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "Tally", plot_suff, ".jpg"), height = 8, width = 11, dpi = 300, tally_plot)

plot_dat<- plot_dat %>%
    select(., Species_Archetype, Region, Scenario, ModelTrainStart, ModelTrainEnd, Month, Year, Season, data, Cor, MAE_PA, MAE_HSI, TSS, AUC, PrAUC, Sens, Spec, HellDist, HellDistCurveLoc) %>%
    distinct()



corr_plot <- ggplot(data = plot_dat, aes(x = HellDist, y = round(Cor, 2), fill = Season, color = Season, shape = Season, group = Season, order = Season)) +
    geom_point(size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(method = "lm", formula = y ~ x, se = F) +
    stat_poly_eq(geom = "text_npc", formula = y ~ x,
               label.x = "left",
               label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), 
               eq.with.lhs = "",
               aes(label = paste("bold(\"", factor(c("Winter", "Spring", "Summer", "Fall"), levels = c("Winter", "Spring", "Summer", "Fall")),
                                 " \")*",
                                 "italic(hat(y))~`=`~",
                                 stat(eq.label),
                                 sep = "")),
               parse = TRUE) +
    scale_fill_manual(name = "Forecast Target Season", values = colors_use) +
    scale_color_manual(name = "Forecast Target Season", values = colors_use) +
    xlab("Hellinger's Distance") +
    ylab("Correlation Coefficient\nPredicted probability of presence vs. True Presence/Absence") +
    facet_wrap(~ Region + Species_Archetype) +
    theme_bw(base_size = 12) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "Cor", plot_suff, ".jpg"), height = 8, width = 11, dpi = 300, corr_plot)

tss_plot <- ggplot(data = plot_dat, aes(x = HellDist, y = round(TSS, 2), fill = Season, color = Season, shape = Season, group = Season, order = Season)) +
    geom_point(size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(method = "lm", formula = y ~ x, se = F) +
    stat_poly_eq(geom = "text_npc", formula = y ~ x,
               label.x = "left",
               label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), 
               eq.with.lhs = "",
               aes(label = paste("bold(\"", factor(c("Winter", "Spring", "Summer", "Fall"), levels = c("Winter", "Spring", "Summer", "Fall")),
                                 " \")*",
                                 "italic(hat(y))~`=`~",
                                 stat(eq.label),
                                 sep = "")),
               parse = TRUE) +
    scale_fill_manual(name = "Forecast Target Season", values = colors_use) +
    scale_color_manual(name = "Forecast Target Season", values = colors_use) +
    xlab("Hellinger's Distance") +
    ylab("True Skill Statistic\nSensitivity+Specificity -1") +
    facet_wrap(~ Region + Species_Archetype) +
    theme_bw(base_size = 12) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "TSS", plot_suff, ".jpg"), height = 8, width = 11, dpi = 300, tss_plot)


auc_plot <- ggplot(data = plot_dat, aes(x = HellDist, y = AUC, fill = Season, shape = Season, color = Season, group = Season)) +
    geom_point(size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(method = "lm", formula = y ~ x, se = F) +
    stat_poly_eq(geom = "text_npc", formula = y ~ x,
               label.x = "left",
               label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), 
               eq.with.lhs = "",
               aes(label = paste("bold(\"", factor(c("Winter", "Spring", "Summer", "Fall"), levels = c("Winter", "Spring", "Summer", "Fall")),
                                 " \")*",
                                 "italic(hat(y))~`=`~",
                                 stat(eq.label),
                                 sep = "")),
               parse = TRUE) +
    scale_fill_manual(name = "Forecast Target Season", values = colors_use) +
    scale_color_manual(name = "Forecast Target Season", values = colors_use) +
    xlab("Hellinger's Distance") +
    ylab("AUC") +
    facet_wrap(~Region+Species_Archetype) +
    theme_bw(base_size = 12) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "AUC", plot_suff, ".jpg"), height = 8, width = 11, dpi = 300, auc_plot)

f1_plot <- ggplot(data = plot_dat, aes(x = HellDist, y = F1, fill = Season, shape = Season, color = Season, group = Season)) +
    geom_point(size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(method = "lm", formula = y ~ x, se = F) +
    stat_poly_eq(geom = "text_npc", formula = y ~ x,
               label.x = "left",
               label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), 
               eq.with.lhs = "",
               aes(label = paste("bold(\"", factor(c("Winter", "Spring", "Summer", "Fall"), levels = c("Winter", "Spring", "Summer", "Fall")),
                                 " \")*",
                                 "italic(hat(y))~`=`~",
                                 stat(eq.label),
                                 sep = "")),
               parse = TRUE) +
    scale_fill_manual(name = "Forecast Target Season", values = colors_use) +
    scale_color_manual(name = "Forecast Target Season", values = colors_use) +
    xlab("Hellinger's Distance") +
    ylab("F1 Measure") +
    facet_wrap(~Region+Species_Archetype) +
    theme_bw(base_size = 12) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "F1", plot_suff, ".jpg"), height = 8, width = 11, dpi = 300, f1_plot)

raw_diff_plot <- ggplot(data = plot_dat, aes(x = HellDist, y = RawDiff, fill = Season, shape = Season, color = Season, group = Season)) +
    geom_point(size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(method = "lm", formula = y ~ x, se = F) +
    stat_poly_eq(geom = "text_npc", formula = y ~ x,
               label.x = "left",
               label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), 
               eq.with.lhs = "",
               aes(label = paste("bold(\"", factor(c("Winter", "Spring", "Summer", "Fall"), levels = c("Winter", "Spring", "Summer", "Fall")),
                                 " \")*",
                                 "italic(hat(y))~`=`~",
                                 stat(eq.label),
                                 sep = "")),
               parse = TRUE) +
    scale_fill_manual(name = "Forecast Target Season", values = colors_use) +
    scale_color_manual(name = "Forecast Target Season", values = colors_use) +
    xlab("Hellinger's Distance") +
    ylab("Raw Difference Between Predicted and True HSI") +
    facet_wrap(~Region+Species_Archetype) +
    theme_bw(base_size = 12) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "RawDiff", plot_suff, ".jpg"), height = 8, width = 11, dpi = 300, raw_diff_plot)

rmse_plot <- ggplot(data = plot_dat, aes(x = HellDist, y = round(RMSE, 2), fill = Season, shape = Season, color = Season, group = Season)) +
    geom_point(size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(method = "lm", formula = y ~ x, se = F) +
    stat_poly_eq(geom = "text_npc", formula = y ~ x,
               label.x = "left",
               label.y = rev(seq(from = 0.01, to = 0.19, length.out = 4)), 
               eq.with.lhs = "",
               aes(label = paste("bold(\"", factor(c("Winter", "Spring", "Summer", "Fall"), levels = c("Winter", "Spring", "Summer", "Fall")),
                                 " \")*",
                                 "italic(hat(y))~`=`~",
                                 stat(eq.label),
                                 sep = "")),
               parse = TRUE) +
    scale_fill_manual(name = "Forecast Target Season", values = colors_use) +
    scale_color_manual(name = "Forecast Target Season", values = colors_use) +
    scale_y_continuous(name = "Root mean square error", limits = c(0.15, 0.5)) +
    xlab("Hellinger's Distance") +
    facet_wrap(~Region+Species_Archetype) +
    theme_bw(base_size = 12) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "RMSE", plot_suff, ".jpg"), height = 8, width = 11, dpi = 300, rmse_plot)


plot_dat$MAE_HSI<- round(plot_dat$MAE_HSI)
mae_hsi_plot <- ggplot() +
    geom_point(data = plot_dat, aes(x = HellDist, y = round(MAE_HSI, 2), fill = Season, shape = Season, group = Season), size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(data = plot_dat, aes(x = HellDist, y = round(MAE_HSI, 2), color = Season, group = Season), method = "lm", formula = y ~ x, se = F) +
    scale_fill_manual(name = "Forecast Target Season", values = colors_use) +
    scale_color_manual(name = "Forecast Target Season", values = colors_use) +
    xlab("Hellinger's Distance\n*Note axis scale change*") +
    ylab("Mean Absolute Error\nPredicted vs. True Habitat Suitability") +
    facet_wrap(~Region+Species_Archetype) +
    theme_bw(base_size = 12) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "MAE_HSI", plot_suff, ".jpg"), height = 8, width = 11, dpi = 300, mae_hsi_plot)



pr_auc_plot1 <- ggplot() +
    geom_point(data = plot_dat[1,], aes(x = HellDist, y = PrAUC, fill = Season, shape = Season, group = Season), size = 3, pch = 21, alpha = 0.4) +
    # stat_smooth(data = plot_dat, aes(x = HellDist, y = PrAUC, color = Season, group = Season), method = "lm", formula = y ~ x, se = F) +
    scale_fill_manual(name = "Forecast Target Season", values = colors_use) +
    scale_color_manual(name = "Forecast Target Season", values = colors_use) +
    xlab("Hellinger's Distance\n*Note axis scale change*") +
    ylab("PR-AUC") +
    facet_wrap(~Region+Species_Archetype, scales = "free_x") +
    theme_bw(base_size = 12) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "PRAUC1", plot_suff, ".jpg"), height = 8, width = 11, dpi = 300, pr_auc_plot1)

plot_dat_simp <- plot_dat
plot_dat_simp$Region <- ifelse(plot_dat_simp$Region == "CCS", "California Current", "Northeast Shelf")
pr_auc_plot_simp <- ggplot() +
    geom_point(data = subset(plot_dat_simp, Species_Archetype == "Resident"), aes(x = HellDist, y = PrAUC, fill = Region), size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(data = plot_dat_simp, aes(x = HellDist, y = PrAUC, color = Region), method = "lm", formula = y ~ x, se = F) +
    scale_fill_manual(name = "Large marine ecosystem", values = colors_use) +
    scale_color_manual(name = "Large marine ecosystem", values = colors_use) +
    scale_x_continuous(name = "Environmental novelty", labels = c("None", "Mild", "Extreme"), breaks = c(0, 0.25, 0.5), limits = c(0, 0.5)) +
    xlab("Hellinger's Distance") +
    ylab("PR-AUC") +
    facet_wrap(~Region) +
    theme_bw(base_size = 16) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 18),
        legend.position = "none"
    )
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "PRAUC_Simp", plot_suff, ".png"), height = 8, width = 11, dpi = 600, pr_auc_plot_simp)
ggsave(filename = paste0("~/Desktop/", "PRAUC_Simp", plot_suff, ".png"), height = 8, width = 11, dpi = 600, pr_auc_plot_simp)

pr_auc_plot <- ggplot() +
    geom_point(data = plot_dat, aes(x = HellDist, y = PrAUC, fill = Season, shape = Season, group = Season), size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(data = plot_dat, aes(x = HellDist, y = PrAUC, color = Season, group = Season), method = "lm", formula = y ~ x, se = F) +
    scale_fill_manual(name = "Forecast Target Season", values = colors_use) +
    scale_color_manual(name = "Forecast Target Season", values = colors_use) +
    xlab("Hellinger's Distance") +
    ylab("PR-AUC") +
    facet_wrap(~Region+Species_Archetype) +
    theme_bw(base_size = 12) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "PRAUC", plot_suff, ".jpg"), height = 8, width = 11, dpi = 300, pr_auc_plot)

tss_plot <- ggplot() +
    geom_point(data = plot_dat, aes(x = HellDist, y = TSS, fill = Season, shape = Season, group = Season), size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(data = plot_dat, aes(x = HellDist, y = TSS, color = Season, group = Season), method = "lm", formula = y ~ x, se = F) +
    scale_fill_manual(name = "Forecast Target Season", values = colors_use) +
    scale_color_manual(name = "Forecast Target Season", values = colors_use) +
    xlab("Hellinger's Distance") +
    ylab("True skill statistic\n(Sensitivity + Specificity - 1)") +
    facet_wrap(~Region+Species_Archetype) +
    theme_bw(base_size = 12) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "TSS", plot_suff, ".jpg"), height = 8, width = 11, dpi = 300, tss_plot)

#####
### Need a plot that shows the complexity in the relationship among novel conditions, temperature-response curve and prediction skill
#####














mae_plot <- ggplot() +
    geom_point(data = plot_dat, aes(x = HellDist, y = MAE_PA, fill = Season, shape = Season, group = Season), size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(data = plot_dat, aes(x = HellDist, y = MAE_PA, color = Season, group = Season), method = "lm", formula = y ~ x, se = F) +
    scale_fill_manual(name = "Forecast Target Season", values = colors_use) +
    scale_color_manual(name = "Forecast Target Season", values = colors_use) +
    # scale_shape_manual(name = "Forecast Target Season", values = c(21, 22, 23, 24)) +
    xlab("Hellinger's Distance\n*Note axis scale change*") +
    ylab("Mean Absolute Error") +
    # ylim(mae_scales) +
    # xlim(hdist_scale) +
    facet_wrap(~Region+Species_Archetype) +
    theme_bw(base_size = 12) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "MAE_PA", plot_suff, ".jpg"), height = 8, width = 11, dpi = 300, mae_plot)

sens_plot <- ggplot() +
    geom_point(data = plot_dat, aes(x = HellDist, y = Sens, fill = Season, shape = Season, group = Season), size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(data = plot_dat, aes(x = HellDist, y = Sens, color = Season, group = Season), method = "lm", formula = y ~ x, se = F) +
    scale_fill_manual(name = "Forecast Target Season", values = colors_use) +
    scale_color_manual(name = "Forecast Target Season", values = colors_use) +
    # scale_shape_manual(name = "Forecast Target Season", values = c(21, 22, 23, 24)) +
    xlab("Hellinger's Distance\n*Note axis scale change*") +
    ylab("Sensitivity") +
    # ylim(mae_scales) +
    # xlim(hdist_scale) +
    facet_wrap(~Region+Species_Archetype, scales = "free_x") +
    theme_bw(base_size = 12) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "Sens", plot_suff, ".jpg"), height = 8, width = 11, dpi = 300, sens_plot)


spec_plot <- ggplot() +
    geom_point(data = plot_dat, aes(x = HellDist, y = Spec, fill = Season, shape = Season, group = Season), size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(data = plot_dat, aes(x = HellDist, y = Spec, color = Season, group = Season), method = "lm", formula = y ~ x, se = F) +
    scale_fill_manual(name = "Forecast Target Season", values = colors_use) +
    scale_color_manual(name = "Forecast Target Season", values = colors_use) +
    # scale_shape_manual(name = "Forecast Target Season", values = c(21, 22, 23, 24)) +
    xlab("Hellinger's Distance\n*Note axis scale change*") +
    ylab("Specificity") +
    # ylim(mae_scales) +
    # xlim(hdist_scale) +
    facet_wrap(~Region+Species_Archetype, scales = "free_x") +
    theme_bw(base_size = 12) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "Spec", plot_suff, ".jpg"), height = 8, width = 11, dpi = 300, spec_plot)

## Temperatures for HD...
t <- plot_dat %>%
    filter(., Region == "NES") %>%
    distinct(FitSSTMean, FitSSTSD)
base_plot <- ggplot(data.frame(x = c(-1, 30)), aes(x = x)) +
    stat_function(fun = dnorm, args = list(mean = unique(t$FitSSTMean), sd = unique(t$FitSSTSD)), lwd = 2, color = "gray20") +
    xlab("Sea Surface Temperature") +
    ylab("Density") +
    theme_bw(base_size = 16) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "BaseSSTCurve", plot_suff, ".jpg"), height = 8, width = 11, dpi = 300, base_plot)

t2 <- plot_dat %>%
    filter(., Region == "NES")
fut_plot <- ggplot(data.frame(x = c(-1, 30)), aes(x = x)) +
    stat_function(fun = dnorm, args = list(mean = unique(t$FitSSTMean)+5, sd = unique(t$FitSSTSD)), lwd = 2, color = "#1b9e77") +
    xlab("Sea Surface Temperature") +
    ylab("Density") +
    theme_bw(base_size = 16) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "FutureSSTCurve", plot_suff, ".jpg"), height = 8, width = 11, dpi = 300, fut_plot)

both_plot <- base_plot +
    stat_function(fun = dnorm, args = list(mean = unique(t$FitSSTMean) + 5, sd = unique(t$FitSSTSD)), lwd = 2, color = "#1b9e77") +
    xlab("Sea Surface Temperature") +
    ylab("Density") +
    theme_bw(base_size = 16) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )
ggsave(filename = paste0(here::here("pipelines/sim_spp/results/"), "BothSSTCurve", plot_suff, ".jpg"), height = 8, width = 11, dpi = 300, both_plot)


#####
## Sampling variability
#####

var_exp <- plot_dat %>%
    select(., Species_Archetype, Region, Scenario, Season, ModelTrainStart, ModelTrainEnd, Month, Year, data, HellDist) %>%
    distinct()

rbinom_samp_func<- function(val){
    samps <- rbinom(100, size = 1, prob = val)
    var_out <- var(samps)
    return(var_out)
}

rbinom_var_mean<- function(data){
    var_return<- sapply(data$HSI, rbinom_samp_func)
    var_out <- var(var_return)
    return(var_out)
}

var_exp <- var_exp %>%
    mutate(., "RBinom_Variance" = map_dbl(data, rbinom_var_mean))

ggplot() +
    geom_point(data = var_exp, aes(x = HellDist, y = RBinom_Variance, fill = Season, shape = Season, group = Season), size = 3, pch = 21, alpha = 0.4) +
    stat_smooth(data = var_exp, aes(x = HellDist, y = RBinom_Variance, color = Season, group = Season), method = "lm", formula = y ~ x, se = F) +
    scale_fill_manual(name = "Forecast Target Season", values = colors_use) +
    scale_color_manual(name = "Forecast Target Season", values = colors_use) +
    # scale_shape_manual(name = "Forecast Target Season", values = c(21, 22, 23, 24)) +
    xlab("Hellinger's Distance\n*Note axis scale change*") +
    ylab("rbinom() sample variance") +
    # ylim(mae_scales) +
    # xlim(hdist_scale) +
    facet_wrap(~Region+Species_Archetype, scales = "free_x") +
    theme_bw(base_size = 12) +
    theme(
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text = element_text(size = 16, face = "bold")
    )
