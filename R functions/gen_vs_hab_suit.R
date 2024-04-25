library(terra)
library(virtualspecies)
library(tidyverse)
library(here)

sf::sf_use_s2(FALSE)

#####
## Functions
#####

#' @title Create a habitat covariates list.
#'
#' @description This function generates a habitat covariates list. This list is eventually used by our `sim_vs_hab_suit` function, which is mainly a a wrapper that uses `virtualspecies::generateSpFromFun`.
#'
#' @param habitat_covs_dir = A directory path to a habitat_covs folder, which has subfolders for each habitat covariate of interest stored as .grd raster layers or raster stacks.
#' @param region_sf = Either NULL or an sf polygon that will be used as as a crop and mask to focus on a specific region. When NULL, no masking will be done.
#'
#' @return A list with habitat covariate rasters (layers or stacks) formatted for use with `sim_vs_hab_suit` function and `virtualspecies::generateSpFromFun`
#'
#' @export

create_vs_habitat_covs_list <- function(habitat_covs_dir, region_sf) {

  # For debugging
  if (FALSE) {
    # habitat_covs_dir <- here::here("data/sim_spp/habitat_covs")
    region_sf <- st_read(here::here("data/sim_spp/region_shapefiles_lme/cc.geojson"))
  }

  # Get the list of subfolders within the habitat covariates directory
  habitat_covs_folds_temp <- list.files(habitat_covs_dir, full.names = TRUE)
  # Don't include the "processed" subfolder
  habitat_covs_folds <- habitat_covs_folds_temp[!grepl("processed", habitat_covs_folds_temp)]

  # Empty storage list
  vs_habitat_covs_list <- vector("list", length(habitat_covs_folds))

  # Loop through the habitat covariate subfolders and add each of them to our vs_habitat_covs_list object
  for (i in seq_along(habitat_covs_folds)) {
    # Get the raster file...
    rast_file_temp <- list.files(habitat_covs_folds[i], pattern = ".grd", full.names = TRUE)
    rast_stack_temp <- raster::stack(rast_file_temp)

    if (!is.null(region_sf)) {
      # Crop and apply a mask...
      mask_temp <- region_sf
      rast_stack_temp <- raster::mask(raster::crop(rast_stack_temp, mask_temp), mask_temp)
      vs_habitat_covs_list[[i]] <- rast_stack_temp
    } else {
      vs_habitat_covs_list[[i]] <- rast_stack_temp
    }
    names(vs_habitat_covs_list)[i] <- gsub(".grd", "", sub(".*\\/", "", rast_file_temp))
  }

  # Return the vs_habitat_covs_list
  return(vs_habitat_covs_list)
}


#' @title Format virtual species functions
#'
#' @description This function generates a list object of the formatted functions for each variable as is expected by `virtualspecies::generateSpFromFun`. This list object is identical to that generated manually by `virtualspecies::formatFunctions`. Currently, only implemented the beta function and logistic function.
#'
#' @param spp_env_params = A data frame (one row) file with the species-environment function parameters.
#'
#' @return A list with correctly formatted functions, ready for use in `virtualspecies::generateSpFromFun`
#'
#' @export

format_vs_funcs <- function(spp_env_params) {

  # For debugging
  if (FALSE) {
    # spp_env_params_path <- read.csv(paste(spp_env_params_dir, "/spp_env_params.csv", sep = ""))
    # region <- "cc"
  }

  vs_func_params <- spp_env_params

  # The functions and arguments are all saved in a list...
  vs_func_form_out <- vector("list", length = max(seq_along(vs_func_params$variable)))

  # Need populate the list for each of the variables
  for (i in seq_along(vs_func_params$variable)) {

    # List element name based on variable name
    names(vs_func_form_out)[i] <- vs_func_params$variable[i]

    # Formula largely depends on function_name...
    function_name_use <- vs_func_params$function_name[i]

    ## Normal function
    if (function_name_use == "norm_func") {
      # Check param values
      norm_params_check <- c("mean", "sd")
      if (!complete.cases(vs_func_params[i, norm_params_check])) {
        print(paste("Check parameter input values for normal function. Some values are NA"))
        stop()
      }

      vs_func_form_out[[i]]$fun <- c("fun" = function_name_use)
      vs_func_form_out[[i]]$args <- c("mean" = vs_func_params$mean[i], "sd" = vs_func_params$sd[i])
    }

    ## Beta function
    if (function_name_use == "beta_func") {
      # Check param values
      beta_params_check <- c("alpha", "p1", "p2", "gamma")
      if (!complete.cases(vs_func_params[i, beta_params_check])) {
        print(paste("Check parameter input values for beta function. Some values are NA"))
        stop()
      }

      vs_func_form_out[[i]]$fun <- c("fun" = function_name_use)
      vs_func_form_out[[i]]$args <- c("p1" = vs_func_params$p1[i], "p2" = vs_func_params$p2[i], "alpha" = vs_func_params$alpha[i], "gamma" = vs_func_params$gamma[i])
    }

    ## Logistic function
    if (function_name_use == "logistic_func") {
      # Check param values
      logistic_params_check <- c("alpha", "beta")
      if (!complete.cases(vs_func_params[i, logistic_params_check])) {
        print(paste("Check parameter input values for logistic function. Some values are NA"))
        stop()
      }

      vs_func_form_out[[i]]$fun <- c("fun" = function_name_use)
      vs_func_form_out[[i]]$args <- c("alpha" = vs_func_params$alpha[i], "beta" = vs_func_params$beta[i])
    }
  }

  # Return the list
  return(vs_func_form_out)
}

#' @title Simulate virtual species habitat suitability
#'
#' @description This function leverages the `virtualspecies::generateSpFromFun` function to generate virtual species habitat suitabilities.
#'
#' @param habitat_list = A list of habitat covariate raster layers or stacks with the environmental habitat information across the spatial domain of interest.
#' @param sim_dates = A date vector with the time steps to use in the simulation, should also match the resolution of the dynamic habitat layers in `habitat_stack`
#' @param vs_params = A list with formatted functions specifying species response to environmental variables. This is generated by the `format_vs_funcs` function.
#' @param habitat_formula = The RHS model formula specifying the functional form, with coefficients, for estimating the habitat suitability at each location based on its environmental characteristics and the species' environmental preferences as governed by `vs_params`.
#' @param vs_rescale = Default = FALSE
#' @param vs_species.type = Default = NULL
#' @param vs_resecale.each.response = Default = FALSE
#' @param vs_plot = Default = FALSE
#'
#' @return A list that has raster layer (one time step) or stack (multiple time steps) with a species habitat suitability estimated at every grid cell across the spatial domain of interest as governed by `habitat_stack`. The final item in the list contains simulation information.
#'
#' @export

sim_vs_hab_suit <- function(habitat_list, vs_params, habitat_formula, vs_rescale = FALSE, vs_species.type = NULL, vs_rescale.each.response = FALSE, vs_plot = FALSE, ...) {

  # For debugging
  if (FALSE) {
    # tar_load(vs_habitat_covs_list)
    # habitat_list <- vs_habitat_covs_list
    # sim_tsteps <- seq(ymd("1981-09-01"), ymd("2020-04-01"), by = "month")
    # sim_tsteps <- as.Date(gsub("X", "", gsub("[.]", "-", names(vs_habitat_covs_list[[2]]))))
    # tar_load(vs_funcs)
    # vs_params <- vs_funcs
    # habitat_formula <- habitat_formula
    # vs_rescale <- FALSE
    # vs_species.type <- NULL
    # vs_rescale.each.response <- FALSE
    # vs_plot <- FALSE
  }

  # Get simulation dates -- not very robust!
  sim_dates_temp <- names(habitat_list[[2]])
  sim_dates <- as.Date(gsub("X", "", gsub("[.]", "-", sim_dates_temp)))

  # Generate storage for output
  vs_hab_suit_out <- vector("list", length(sim_dates) + 1)

  # Get coefficient info
  coeffs_temp <- c(str_extract_all(habitat_formula, "[A-z]+", simplify = TRUE))
  # coeffs_vals_temp<- as.numeric(str_extract_all(habitat_formula, "\\d+", simplify = TRUE))
  coeffs_vals_temp <- c(0.1, 1)

  # Get the reference maximum value if vs_rescale = FALSE
  if (!vs_rescale) {
    ref_max_vals <- data.frame("Coeff" = names(habitat_list), "Ref_Max" = rep(NA, length(habitat_list)))

    for (i in seq_along(ref_max_vals$Coeff)) {
      # Get the approrpriate information from vs_params
      params_use <- vs_params[[match(ref_max_vals$Coeff[i], names(vs_params))]]

      # Get the appropriate raster layer or stack from habitat_list
      habitat_rast_use <- habitat_list[[match(ref_max_vals$Coeff[i], names(habitat_list))]]
      habitat_rast_interval <- c(min(minValue(habitat_rast_use)), max(maxValue(habitat_rast_use)))

      # Get max value, which will depend on the fun. Optimize wasn't working...
      if (params_use$fun == "norm_func") {
        norm_vals_df <- data.frame("Var_Value" = seq(from = min(minValue(habitat_rast_use)), to = max(maxValue(habitat_rast_use)), length.out = 10000))
        norm_vals_df <- norm_vals_df %>%
          mutate(., "Norm_Pred" = pmap_dbl(list(x = Var_Value, mean = params_use$args["mean"], sd = params_use$args["sd"]), norm_func))
        ref_max_vals$Ref_Max[i] <- norm_func(x = norm_vals_df$Var_Value[which.max(norm_vals_df$Norm_Pred)], mean = params_use$args["mean"], sd = params_use$args["sd"])
      }

      if (params_use$fun == "beta_func") {
        beta_vals_df <- data.frame("Var_Value" = seq(from = min(minValue(habitat_rast_use)), to = max(maxValue(habitat_rast_use)), length.out = 10000))
        beta_vals_df <- beta_vals_df %>%
          mutate(., "Beta_Pred" = pmap_dbl(list(x = Var_Value, p1 = params_use$args["p1"], p2 = params_use$args["p2"], alpha = params_use$args["alpha"], gamma = params_use$args["gamma"]), beta_func))
        ref_max_vals$Ref_Max[i] <- beta_func(x = beta_vals_df$Var_Value[which.max(beta_vals_df$Beta_Pred)], p1 = params_use$args["p1"], p2 = params_use$args["p2"], alpha = params_use$args["alpha"], gamma = params_use$args["gamma"])
      }

      if (params_use$fun == "logistic_func") {
        logistic_vals_df <- data.frame("Var_Value" = seq(from = min(minValue(habitat_rast_use)), to = max(maxValue(habitat_rast_use)), length.out = 10000))
        logistic_vals_df <- logistic_vals_df %>%
          mutate(., "Logistic_Pred" = pmap_dbl(list(x = Var_Value, alpha = params_use$args["alpha"], beta = params_use$args["beta"]), logistic_func))
        ref_max_vals$Ref_Max[i] <- logistic_func(logistic_vals_df$Var_Value[which.max(logistic_vals_df$Logistic_Pred)], alpha = params_use$args["alpha"], beta = params_use$args["beta"])
      }
    }
    # Calculate overall max
    ref_max_all <- switch(as.character(length(coeffs_temp)),
      "2" = coeffs_vals_temp[1] * ref_max_vals$Ref_Max[1] + coeffs_vals_temp[2] * ref_max_vals$Ref_Max[2],
      "3" = coeffs_vals_temp[1] * ref_max_vals$Ref_Max[1] + coeffs_vals_temp[2] * ref_max_vals$Ref_Max[2] + coeffs_vals_temp[3] * ref_max_vals$Ref_Max[3]
    )
  }

  # Likely a faster way to do this, for now, just going with a loop over each of the sim_steps
  # Make a stack from the habitat_list
  hab_stack <- raster::stack(habitat_list)

  for (i in seq_along(sim_dates)) {
    rast_name_use <- paste("X", gsub("-", ".", sim_dates[i]), sep = "")
    sst_use <- raster(subset(hab_stack, which(names(hab_stack) == rast_name_use)))
    stack_use <- c(terra::rast(hab_stack[[1]]), terra::rast(sst_use))
    names(stack_use) <- c("depth", "sst")

    # Generate species
    vs_temp <- generateSpFromFun(raster.stack = stack_use, parameters = vs_params, rescale = vs_rescale, species.type = vs_species.type, formula = habitat_formula, rescale.each.response = vs_rescale.each.response, plot = vs_plot)

    # Adjust suitability...
    vs_temp$suitab.raster <- (1 / ref_max_all) * vs_temp$suitab.raster
    vs_temp$suitab.raster[vs_temp$suitab.raster > 1]<- 1
    # Store it
    vs_hab_suit_out[[i]] <- vs_temp
    names(vs_hab_suit_out)[i] <- rast_name_use

    # Update
    print(paste(rast_name_use, " is done!", sep = ""))
  }

  # Add simulation information
  vs_hab_suit_out[[length(sim_dates) + 1]] <- vs_params
  names(vs_hab_suit_out)[length(sim_dates) + 1] <- "Sim_Parameters"

  # Return it
  return(vs_hab_suit_out)
}

#' @title Main function to simulate and save virtual species habitat suitability
#'
#' @description This main function is a wrapper around `sim_vs_hab_suit`, which will generate virtual species habitat suitability given habitat covariate raster layers/stacks (found in the habitat_covs_dir) and the species response to these habitat covariates depending on species-response curves and habitat formula.
#'
#' @param habitat_covs_dir = A directory path to a habitat_covs folder, which has subfolders for each habitat covariate of interest stored as .grd raster layers or raster stacks.
#' @param spp_env_params_path = A file path to the csv file with the species-environment function parameters. This csv file has one row per environmental variable of interest (e.g., depth, SST) and the corresponding parameter values filled in.
#' @param habitat_formula = The RHS model formula specifying the functional form, with coefficients, for estimating the habitat suitability at each location based on its environmental characteristics and the species' environmental preferences as governed by `vs_params`.
#' @param region_path = Either NULL or file path for the shapefile that will be used as as a crop and mask to focus on a specific region. This shapefile character vector has to be in the data/sim_spp/region_shapefiles folder. When NULL, no masking will be done.
#' @param out_file = The path and file name to save a list that includes the virtual species habitat suitability raster layer or stack and information about the simulation as the last item in the list.
#'
#' @return Saves a list with the virtual species habitat suitability raster layer (one time step) or stack (multiple time steps) with a species habitat suitability estimated at every grid cell across the spatial domain of interest as governed by the extent and grain of habitat covariate raster layers/stacks.
#'
#' @export

main_sim_vs_hab_suit <- function(habitat_covs_dir, spp_env_params_path, habitat_formula, region_path, out_file) {

  # Load in region
  region_sf <- st_read(region_path)

  # Format habitat covariates
  habitat_list <- create_vs_habitat_covs_list(habitat_covs_dir = habitat_covs_dir, region_sf = region_sf)

  # Load and format species-environment functions
  spp_env_params <- read.csv(spp_env_params_path)
  vs_params <- format_vs_funcs(spp_env_params = spp_env_params)

  # Simulate virtual species habitat suitability
  vs_hab_suit <- sim_vs_hab_suit(habitat_list = habitat_list, vs_params = vs_params, habitat_formula = habitat_formula)

  # Save it
  saveRDS(vs_hab_suit, file = out_file)
}

#####
## Non-interactive running
#####
if (!interactive()) {
  # Build up our command line argument parser
  p <- arg_parser("Simulate virtual species habitat suitability")
  # Our first argument, the directory holding habitat covariate raster layers or stacks
  p <- add_argument(p, "habitat_covs_dir", help = "A directory path to a habitat_covs folder, which has subfolders for each habitat covariate of interest stored as .grd raster layers or raster stacks. ")
  # Our second argument, the directory holding information on species-environment responses
  p <- add_argument(p, "spp_env_params_path", help = "A file path to the csv file with the species-environment function parameters. This csv file has one row per environmental variable of interest (e.g., depth, SST) and the corresponding parameter values filled in.")
  # Our third argument, the right hand side habitat formula dictating the influence of each habitat covariate on virtual species probability of presence
  p <- add_argument(p, "habitat_formula", help = "The RHS model formula specifying the functional form, with coefficients, for estimating the habitat suitability at each location based on its environmental characteristics and the species' environmental preferences as governed by `vs_params`. ")
  # Our sixth argument, a character vector to read in a shapefile and then crop and mask the habitat layers to a region of interest
  p <- add_argument(p, "region_path", help = "Either NULL or file path for the shapefile that will be used as as a crop mask to focus on a specific region. This shapefile character vector has to be in the data/sim_spp/region_shapefiles folder. When NULL, no masking will be done.")
  #  Our fifth argument, the path and name to save a list with virtual species raster layer/stack and simulation parameter information
  p <- add_argument(p, "out_file", help = "The path and file name to save a list that includes the virtual species habitat suitability raster layer or stack and information about the simulation as the last item in the list.")

  # parse the arguments
  args <- parse_args(p)
  for (i in 1:length(args)) {
    log_info(paste0("Args include ", names(args)[i], ": ", args[[i]]))
  }

  # execute the function
  main_sim_vs_hab_suit(habitat_covs_dir = args$habitat_covs_dir, spp_env_params_path = args$spp_env_params_path, habitat_formula = args$habitat_formula, region_path = args$region_path, out_file = args$out_file)

  # update logger
  log_info("Virtual species habitat suitability raster layer/stacks generated and saved")
}