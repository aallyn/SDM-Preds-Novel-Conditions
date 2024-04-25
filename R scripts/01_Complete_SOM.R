#####
## OISST Self Organizing Maps
#####

# Preliminaries -----------------------------------------------------------
# Loading libraries
library(tidyverse)
library(sf)
library(raster)
library(gmRi)
library(biscale)
library(MASS)
library(kohonen)
library(SOMbrero)
library(forecast)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("map", "purrr")

# Connecting to GMRI box folders
res_data_path<- "/Users/aallyn/Library/CloudStorage/Box-Box/RES_Data/"
proj_box_path<- "/Users/aallyn/Library/CloudStorage/Box-Box/Mills Lab/Projects/NASA_UNSDG19/"

# Regions -----------------------------------------------------------------
# Read in land
land<- st_read(paste(res_data_path, "Shapefiles/ne_50m_land/ne_50m_land.shp", sep = "")) 

# Read in just the LMEs
lme_files <- list.files(paste0(res_data_path, "Shapefiles/large_marine_ecosystems"), full.names = TRUE, pattern = "exterior.geojson")
lmes <- lapply(lme_files, st_read)
lmes <- do.call(bind_rows, lmes)

# SOM analysis ------------------------------------------------------------
# There's a lot going on with this stuff and not really sure if I am exactly tracking the core piece, which is setting up the SOM grid, in the "somgrid" function. Within that function we need to specify the xdimensions and the ydimensions, the topology of the grid (rectangular or hexagonal), the neighborhood type (bubble or gaussian) and whether the grid is toroidal or not. Okay, let's start with the grid dimensions.

# From https://www.polarmicrobes.org/tutorial-self-organizing-maps-in-r/:
# You want about 5 elements from the training data per map unit,
# You want to use a symmetrical map unless you have a very small training dataset, hexagonal map units, and a toroidal shape. 

# From Miguel: 3x4 hexagonal grid (potentially just for the short example).

# Neither too helpful for right now. Here (https://stats.stackexchange.com/questions/71812/kohonen-self-organizing-maps-determining-the-number-of-neurons-and-grid-size) suggests using a Sammon's map. Here (https://stats.stackexchange.com/questions/282288/som-grid-size-suggested-by-vesanto) suggests "5 * sqrt(# rows)." Going beyond that a bit, a function here looks like it could also be useful for determining the map dimensions. https://stackoverflow.com/questions/19163214/kohonen-self-organizing-maps-determining-the-number-of-neurons-and-grid-size and based on the MatLab function here: http://cda.psych.uiuc.edu/matlab_class/martinez/edatoolbox/Docs/som_topol_struct.html. I tried a few of these different approaches, ending up with a 7x6 grid. But, that's a bit excessive I think. Going to try just a simple 2x2 and focus on the LMEs.
# A function to read in each LMEs OISST data
oisst_stem<- paste0("/Users/aallyn/Library/CloudStorage/Box-Box/RES_Data/", "OISST/oisst_mainstays/regional_timeseries/large_marine_ecosystems/")
dat_read_wrapper<- function(file, stem_path = oisst_stem){
  if(FALSE){
    file = lmes$name[[47]]
    stem_path = "~/Box/RES_Data/OISST/Global/RegionNetCDFs/Processed/"
  }
  # Set netcdf file title and output
  file_name_temp <- gsub(" - ", " ", tolower(file))
  file_name_temp2 <- gsub("[.]", "", gsub(" ", "_", file_name_temp))
  file_name_temp3<- gsub("-", "_", file_name_temp2)
  file_name<- paste0("OISSTv2_anom_", file_name_temp3, ".csv")
  
  # Read in the file...
  dat <- read.csv(paste0(stem_path, file_name, sep = ""))
  
  # Some quick formatting
  dat<- dat %>%
    mutate(., "Plot_Date" = as.Date(dat$time)) %>%
    separate(., Plot_Date, c("YYYY", "MM", "DD"), remove = FALSE) %>%
    filter(., YYYY >= 1985 & YYYY <= 2021) %>%
    data.frame()
}

# Map the function to each of the rows in ocean_nest
lmes <- lmes %>%
  mutate(., "Data" = pmap(list(file = name, stem_path = oisst_stem), possibly(dat_read_wrapper, NA)))

# Actually just need yearly. Let's do that as a different process
dat_process_wrapper<- function(df){
  if(FALSE){
    df<- lmes$Data[[1]]
  }
  # Getting yearly data ready to model
  dat_yearly<- df %>%
    group_by(YYYY) %>%
    dplyr::summarize(., Mean_Value = mean(area_wtd_anom, na.rm = T)) %>%
    mutate(., Plot_Date = as.Date(paste(YYYY, "06", "15", sep = "-"))) %>%
    filter(., YYYY != format(Sys.time(), "%Y")) %>%
    data.frame()
  
  dat_yearly$Year_Model<- as.integer(dat_yearly$YYYY) - as.integer(min(dat_yearly$YYYY))
  
  return(dat_yearly)
}

lmes<- lmes %>%
  mutate(., "ModData" = purrr::map(Data, dat_process_wrapper))

# Next need a wrapper a wrapper function to fit the linear models
lm_fit_wrapper<- function(df, dates, resp_col, time_col){
  # Details
  # This function is designed to map to a nested data frame or apply over a named list. For each of the data sets, it then subsets the data given the input dates and fits a basic linear model to the time series data using the named response and time columns.
  
  # Arguments
  # df = Dataframe (column name for nested data frames)
  # dates = Dates to use to fit the linear model. Should be supplied as a vector as c("YYYY-MM-DD", "YYYY-MM-DD")
  # resp.col = Name of the response column in the dataframe
  # time.col = Name of the time column (predictor variable) in the data frame
  
  # For debugging
  if(FALSE){
    df = lmes$ModData[[1]]
    dates = c("1982-01-16", "2019-12-31")
    resp_col = "Mean_Value"
    time_col = "Year_Model"
  }
  
  # Get data based on dates -- allows for fitting lm during different time periods
  dates_new<- as.Date(dates, "%Y-%m-%d")
  
  dat_mod<- df %>%
    filter(., between(Plot_Date, dates_new[1], dates_new[2]))
  
  # Matching up column names
  resp_col_ind<- which(colnames(dat_mod) == resp_col)
  time_col_ind<- which(colnames(dat_mod) == time_col)
  
  colnames(dat_mod)[resp_col_ind]<- "Response"
  colnames(dat_mod)[time_col_ind]<- "Predictor"
  
  # Fit and return model
  lm_out<- lm(Response ~ Predictor, data = dat_mod)
  return(lm_out)
}

# Map function to each of the nested datasets, using future to run in parallel
lmes <- lmes %>%
  mutate(., "LMFit" = pmap(list(df = ModData, dates = list(c("1985-01-16", "2019-12-31")), resp_col = "Mean_Value", time_col = "Year_Model"), lm_fit_wrapper))

lmes_sub <- lmes %>%
  dplyr::filter(., name %in% c("California Current", "Northeast U.S. Continental Shelf")) %>%
  mutate(., "LMFit" = pmap(list(df = ModData, dates = list(c("1985-01-16", "2019-12-31")), resp_col = "Mean_Value", time_col = "Year_Model"), lm_fit_wrapper))


# Now extract this information, where we are going to essentially be looking for common patterns in the annual time series across the LMEs. Overall, there is interest in "recent" climate change, though, there is also interest in getting out to "multi-decadal" time scales. So...let's make the cut off 2000 and see how things group out.
lme_yearly <- lmes %>%
  dplyr::select(., name, ModData) %>%
  unnest(., cols = c(ModData)) %>%
  dplyr::select(., name, YYYY, Mean_Value) %>%
  st_drop_geometry()

# Adjusting data to only use the "recent" or not...
recent <- FALSE
if(recent){
    lme_yearly_use<- lme_yearly %>%
    filter(., YYYY >= 2000 & YYYY <= 2019) %>%
    pivot_wider(., names_from = YYYY, values_from = Mean_Value)
}
lme_yearly_use <- lme_yearly %>%
      pivot_wider(., names_from = YYYY, values_from = Mean_Value)
  
# Now to a matrix, ignoring Name column
lme_yearly_mat <- as.matrix(lme_yearly_use[, -c(1)])

# Trying simple 2x2 grid
lme_grid<- somgrid(xdim = 2, ydim = 2, topo = "hexagonal", neighbourhood.fct = "gaussian")
lme_som<- som(X = lme_yearly_mat, grid = lme_grid, rlen = 10000, alpha = c(0.05, 0.01), dist.fcts = "euclidean")
plot(lme_som, type="counts", shape = "straight") # number of lme's in each node

# Somewhat better. Still not sure about this grid business...
plot(lme_som, type = "codes", shape = "straight") # patterns ordered 1:12 left to right bottom to top
plot(lme_som, type = "dist.neighbours", shape = "straight") # how different are neighbor nodes, larger is more different
plot(lme_som, type = "quality", shape = "straight") # measure of clusteryness of node, smaller is tighter cluster

patterns<- getCodes(lme_som)

lme_yearly_use$cluster<- lme_som$unit.classif

membernumber<- table(lme_yearly_use$cluster)
membernumber

# pattern 1 (lower left corner) and its members
plot(patterns[1,], type = "l", ylim = c(-3,3))
for(i in 1:membernumber[1]){
  lines(as.numeric(lme_yearly_use[which(lme_yearly_use$cluster == 1)[i], ]), col= "lightgray")
}

lines(patterns[1,], lwd=3)

# pattern 4 (top right corner) and its members
plot(patterns[4,], type = "l", ylim = c(-3,3))
for(i in 1:membernumber[4]){
  print(i)
  lines(as.numeric(lme_yearly_use[which(lme_yearly_use$cluster == 4)[i], ]), col="lightgray")
}

lines(patterns[4,], lwd=3)

# Visualize these using ggplot
if(recent){
  plot_title_adj<- "Recent"
} else {
  plot_title_adj<- "Full"
}

lme_som_plot<- lmes %>%
  filter(., name %in% lmes$name) %>%
  left_join(., lme_yearly_use, by = c("name" = "name")) %>%
  arrange(., cluster)
lme_som_plot$cluster_fac<- factor(lme_som_plot$cluster, levels = c(1, 2, 3, 4), labels = c("A", "B", "C", "D"))

plot_map<- ggplot() +
  geom_sf(data = lme_som_plot, aes(fill = cluster_fac)) +
  scale_fill_manual(name = "SOM Cluster", values = c("#c65999", "#7aa456", "#777acd", "#c96d44")) +
  labs(title = "SST patterns across Large Marine Ecosystems",
       subtitle = "Results from self organizing maps") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Have patterns with the map??
pat_plot<- data.frame(patterns) 
pat_plot$cluster_fac<- c("A", "B", "C", "D")
pat_plot<- pat_plot %>%
  pivot_longer(-cluster_fac, names_to = "Year", values_to = "SST_SOM_Anomaly")
pat_plot$Year_Plot<- as.numeric(gsub("X", "", pat_plot$Year))

# Need a similar cluster.fac for the actual data and likely want to visualize things over the full TS even if SOM run on more recent years. So, need to bring over lme_yearly_use info to lme_yearly...
# lme_yearly<- lme_yearly %>%
#   left_join(., lme_yearly_use)
lme_yearly<- lme_yearly_use %>%
  arrange(., cluster)
lme_yearly$cluster_fac<- factor(lme_yearly$cluster, levels = c(1, 2, 3, 4), labels = c("A", "B", "C", "D"))
line_colors<- c("#c65999", "#7aa456", "#777acd", "#c96d44")
pat_plots_out<- vector("list", length = length(unique(lme_yearly$cluster_fac)))

# Focal systems...
focal_ecos<- c("California Current", "Northeast U.S. Continental Shelf")

# Breaks
breaks_use<- c(1985, 1995, 2005, 2015)
limits_use<- c(1985, 2020)
# breaks_use<- c(2004, 2009, 2014, 2019)
# limits_use<- c(2002, 2020) 

for(i in seq_along(levels(lme_yearly$cluster_fac))){
  obs_dat_temp<- lme_yearly %>%
    filter(., cluster_fac == levels(lme_yearly$cluster_fac)[i]) %>%
    pivot_longer(-c(name, cluster, cluster_fac), names_to = "Year", values_to = "SST_SOM_Anomaly")
  obs_dat_temp$Year_Plot<- as.numeric(gsub("X", "", obs_dat_temp$Year))
  
  pat_dat_temp<- pat_plot %>%
    filter(., cluster_fac == levels(lme_yearly$cluster_fac)[i])
  
  # Focal?
  if(any(focal_ecos %in% unique(obs_dat_temp$name))){
    pat_plots_out[[i]]<- ggplot() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#d9d9d9", size = 1) +
      geom_line(data = obs_dat_temp, aes(x = Year_Plot, y = SST_SOM_Anomaly, group = name, color = name), alpha = 0.2) +
      scale_color_manual(name = "Observed LME SSTs", values = rep(line_colors[i], length(unique(obs_dat_temp$name)))) +
      geom_line(data = pat_dat_temp, aes(x = Year_Plot, y = SST_SOM_Anomaly), color = line_colors[i]) +
      geom_line(data = subset(obs_dat_temp, name == focal_ecos), aes(x = Year_Plot, y = SST_SOM_Anomaly, group = name, color = name), lty = "dotdash") +
      scale_x_continuous(name = "Year", breaks = breaks_use, limits = limits_use) +
      ylim(c(-2.5, 2.5)) +
      ylab("SST Anomaly") +
      xlab("Year") +
      theme_bw() +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12, face="bold"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none")
  } else {
    pat_plots_out[[i]]<- ggplot() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#d9d9d9", size = 1) +
      geom_line(data = obs_dat_temp, aes(x = Year_Plot, y = SST_SOM_Anomaly, group = name, color = name), alpha = 0.3) +
      scale_color_manual(name = "Observed LME SSTs", values = rep(line_colors[i], length(unique(obs_dat_temp$name)))) +
      geom_line(data = pat_dat_temp, aes(x = Year_Plot, y = SST_SOM_Anomaly), color = line_colors[i]) +
      scale_x_continuous(name = "Year", breaks = breaks_use, limits = limits_use) +
      ylim(c(-2.5, 2.5)) +
      ylab("SST Anomaly") +
      xlab("Year") +
      theme_bw() +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=12, face="bold"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none")
  }
 
  
}

# Custom layout
layout <- "
AAAAAA
AAAAAA
##BC##
##DE##
"

final_map<- plot_map + pat_plots_out[[3]] + pat_plots_out[[4]] + 
  pat_plots_out[[1]] + pat_plots_out[[2]] + 
  plot_layout(design = layout) 

ggsave(paste0(proj_box_path, "Temp Results/LMESOMRes_WithPatterns_Gaussian.jpg"), plot = final_map, width = 15, height = 10)



oo