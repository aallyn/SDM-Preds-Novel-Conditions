#####
## Functions
#####

#' @title Prepare presence/absence training and testing data
#'
#' @description This function splits a dataset into training and testing slices and returns them in a list
#'
#' @param all_data = A dataframe with all of the data 
#' @param train_date_start = The start date for training data
#' @param train_date_end = The end date for training data
#'
#' @return A list, with one slot for the training data and then another slot with the testing data.
#'
#' @export

split_train_test<- function(all_data, train_date_start, train_date_end){
  
  # For debugging
  if(FALSE){
  }
  
  all_data <- all_data %>%
    drop_na()
  
  # Empty list for storage
  train_test_list<- vector("list", length = 2)
  names(train_test_list)<- c("Training", "Testing")

  # Filter by date and then save in correct slot
  train_test_list[["Training"]]<- all_data %>% filter(., Date >= train_date_start & Date < train_date_end)
  train_test_list[["Testing"]]<- all_data %>% filter(., Date >= train_date_end)

  # Return it
  return(train_test_list)
}

#' @title Main split testing and training data function 
#'
#' @description This function reads in a full data set, as created by the 'enhance_sample_vs_pa.R' process, runs 'split_train_test' based on dates and then saves the list with trianing/testing data slices using the scenario name provided. 
#' 
#' @param all_data_path = A path to dataframe with all of the data 
#' @param train_date_start = The start date for training data
#' @param train_date_end = The end date for training data
#' @param out_file = The path and file name to save the list with training and testing slices.

#' @return Saves a list with sampled presence/absences.
#'
#' @export

main_split_train_test<- function(all_data_path, train_date_start, train_date_end, out_file){
  
  # For debugging
  if(FALSE){
  }
  
  # Load the data
  all_data<- readRDS(all_data_path)

  # Run prepare function
  train_test_out<- split_train_test(all_data = all_data, train_date_start = train_date_start, train_date_end = train_date_end)

  # Save it
  saveRDS(train_test_out, file = out_file)
}

#####
## Non-interactive running
#####
if(!interactive()){
    # Build up our command line argument parser
    p<- arg_parser("Split training and testing data")
    # Our first argument, the path to the enahnced species presence/absence data
    p<- add_argument(p, "all_data_path", help = "A path to the enahnced species presence/absence data.")
    # Our second argument, the training data start date
    p<- add_argument(p, "train_date_start", help = "Training data start date.")
    # Our third arugment, the training data end date
     p<- add_argument(p, "train_date_end", help = "Training data end date.")
    # Our fourth argument, the output file name and path 
    p<- add_argument(p, "out_file", help = "The path and file name to save the list with training and testing slices.")
    # parse the arguments
    args <- parse_args(p)
    for (i in 1:length(args)){
        log_info(paste0('Args include ', names(args)[i],': ', args[[i]]))
    }
    
    # execute the function
    main_split_train_test(all_data_path = args$all_data_path, train_date_start = args$train_date_start, train_date_end = args$train_date_end, out_file = args$out_file)
    
    # update logger
    log_info('Data split into training and testing sets and saved as a list.')
}