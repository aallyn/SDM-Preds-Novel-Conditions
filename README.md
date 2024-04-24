# SDM-Preds-Novel-Conditions
Code to support the manuscript "Species distribution model predictability doesn’t always decline under novel temperature conditions"

# Workflow
  # sim_spp_gen_vs_hab_suit:
  #   foreach:
  #     - "cc"
  #     - "ne"
  #   do:
  #     cmd: |
  #       R CMD pipelines/sim_spp/R/gen_vs_hab_suit.R ./data/sim_spp/habitat_covs ./data/sim_spp/spp_env_params_eez/${item}_spp_env_params.csv "0.1*depth + 1*sst" ./data/sim_spp/region_shapefiles_eez/${item}.geojson ./data/sim_spp/vs_hab_suit_eez/${item}_vs_suit.rds
  #     deps:
  #       - pipelines/sim_spp/R/spp_env_funcs.R
  #       - ./data/sim_spp/habitat_covs
  #       - ./data/sim_spp/spp_env_params_eez/${item}_spp_env_params.csv
  #       - ./data/sim_spp/region_shapefiles_eez/${item}.geojson
  #     outs:
  #       - data/sim_spp/vs_hab_suit_eez/${item}_vs_suit.rds
  
  # sim_spp_convert_vs_hab_suit:
  #   foreach:
  #     - "cc"
  #     - "ne"
  #   do: 
  #     cmd: |
  #       R CMD pipelines/sim_spp/R/convert_vs_hab_suit.R ./data/sim_spp/vs_hab_suit_eez/${item}_vs_suit.rds ./data/sim_spp/convert_vs_hab_suit_params/convert_vs_hab_suit_params.csv ./data/sim_spp/vs_hab_suit_eez/${item}_vs_pa.rds
  #     deps:
  #       - ./data/sim_spp/vs_hab_suit_eez/${item}_vs_suit.rds
  #       - ./data/sim_spp/convert_vs_hab_suit_params/convert_vs_hab_suit_params.csv
  #     outs:
  #       - data/sim_spp/vs_hab_suit_eez/${item}_vs_pa.rds

  # sim_spp_sample_vs_pa:
  #   foreach:
  #     - "cc"
  #     - "ne"
  #   do: 
  #     cmd: |
  #       R CMD pipelines/sim_spp/R/sample_vs_pa.R ./data/sim_spp/vs_hab_suit_eez/${item}_vs_pa.rds ./data/sim_spp/region_shapefiles_eez/${item}.geojson ./data/sim_spp/sample_vs_pa_params/${item}_sample_vs_pa_params.csv ./data/sim_spp/vs_hab_suit_eez/${item}_vs_samp.rds
  #     deps:
  #       - ./data/sim_spp/vs_hab_suit_eez/${item}_vs_pa.rds
  #       - ./data/sim_spp/sample_vs_pa_params/${item}_sample_vs_pa_params.csv
  #     outs:
  #       - data/sim_spp/vs_hab_suit_eez/${item}_vs_samp.rds
  
  # sim_spp_enhance_sample_vs_pa:
  #   foreach:
  #     - "cc"
  #     - "ne"
  #   do: 
  #     cmd: |
  #       R CMD pipelines/sim_spp/R/enhance_sample_vs_pa.R ./data/sim_spp/vs_hab_suit_eez/${item}_vs_samp.rds ./data/sim_spp/habitat_covs ./data/sim_spp/vs_hab_suit_eez/${item}_vs_enhanced.rds 
  #     deps:
  #       - ./tools/enhance_r/enhance_r_funcs.R
  #       - ./data/sim_spp/vs_hab_suit_eez/${item}_vs_samp.rds
  #       - ./data/sim_spp/habitat_covs
  #     outs:
  #       - ./data/sim_spp/vs_hab_suit_eez/${item}_vs_enhanced.rds 
  
  # sim_spp_split_train_test:
  #   foreach: ${sim_spp_split_train_test_datasets}
  #   do: 
  #     cmd: |
  #       R CMD pipelines/sim_spp/R/split_train_test.R ./data/sim_spp/vs_hab_suit_eez/cc_vs_enhanced.rds ${item.train_start_date} ${item.train_end_date} ./data/sim_spp/train_test_eez/cc/${item.name}.rds 
  #     deps:
  #       - ./data/sim_spp/vs_hab_suit_eez/cc_vs_enhanced.rds
  #     outs:
  #       - ./data/sim_spp/train_test_eez/cc/${item.name}.rds 
  
  # sim_spp_fit_brt:
  #   foreach: ${sim_spp_fit_brt}

  #   do: 
  #     cmd: |
  #       R CMD pipelines/sim_spp/R/fit_brt.R ./data/sim_spp/train_test_eez/cc/${item.dataset} ${item.predictors_vec} ${item.response} ${item.family} ${item.tree_complexity} ${item.learning_rate} ${item.bag_fraction} ./data/sim_spp/brt_fits_eez/cc/${item.model_name}.rds 
  #     deps:
  #       - ./data/sim_spp/train_test_eez/cc/${item.dataset}
  #     outs:
  #       - ./data/sim_spp/brt_fits_eez/cc/${item.model_name}.rds  

  # sim_spp_gen_vs_hab_suit:
  #   foreach:
  #     - "cc"
  #     - "ne"
  #     # - "seaus"
  #     # - "goa"
  #     # - "wcentaus"
  #   do:
  #     cmd: |
  #       R CMD pipelines/sim_spp/R/gen_vs_hab_suit.R ./data/sim_spp/habitat_covs ./data/sim_spp/spp_env_params_lme_res/${item}_spp_env_params.csv "1*depth + 1*sst" ./data/sim_spp/region_shapefiles_lme/${item}.geojson ./data/sim_spp/vs_hab_suit_lme_res/${item}_vs_suit.rds
  #     deps:
  #       - pipelines/sim_spp/R/spp_env_funcs.R
  #       - ./data/sim_spp/habitat_covs
  #       - ./data/sim_spp/spp_env_params_lme_res/${item}_spp_env_params.csv
  #       - ./data/sim_spp/region_shapefiles_lme/${item}.geojson
  #     outs:
  #       - data/sim_spp/vs_hab_suit_lme_res/${item}_vs_suit.rds
  
  # sim_spp_convert_vs_hab_suit:
  #   foreach:
  #     - "cc"
  #     - "ne"
  #     # - "seaus"
  #     # - "goa"
  #     # - "wcentaus"
  #   do: 
  #     cmd: |
  #       R CMD pipelines/sim_spp/R/convert_vs_hab_suit.R ./data/sim_spp/vs_hab_suit_lme_res/${item}_vs_suit.rds ./data/sim_spp/convert_vs_hab_suit_params/convert_vs_hab_suit_params.csv ./data/sim_spp/vs_hab_suit_lme_res/${item}_vs_pa.rds
  #     deps:
  #       - ./data/sim_spp/vs_hab_suit_lme_res/${item}_vs_suit.rds
  #       - ./data/sim_spp/convert_vs_hab_suit_params/convert_vs_hab_suit_params.csv
  #     outs:
  #       - data/sim_spp/vs_hab_suit_lme_res/${item}_vs_pa.rds

  # sim_spp_sample_vs_pa:
  #   foreach:
  #     - "cc"
  #     - "ne"
  #     # - "seaus"
  #     # - "goa"
  #     # - "wcentaus"
  #   do: 
  #     cmd: |
  #       R CMD pipelines/sim_spp/R/sample_vs_pa.R ./data/sim_spp/vs_hab_suit_lme_res/${item}_vs_pa.rds ./data/sim_spp/region_shapefiles_lme/${item}.geojson ./data/sim_spp/sample_vs_pa_params/${item}_sample_vs_pa_params.csv ./data/sim_spp/vs_hab_suit_lme_res/${item}_vs_samp.rds
  #     deps:
  #       - ./data/sim_spp/vs_hab_suit_lme_res/${item}_vs_pa.rds
  #       - ./data/sim_spp/sample_vs_pa_params/${item}_sample_vs_pa_params.csv
  #     outs:
  #       - data/sim_spp/vs_hab_suit_lme_res/${item}_vs_samp.rds
  
  # sim_spp_enhance_sample_vs_pa:
  #   foreach:
  #     - "cc"
  #     - "ne"
  #     # - "seaus"
  #     # - "goa"
  #     # - "wcentaus"
  #   do: 
  #     cmd: |
  #       R CMD pipelines/sim_spp/R/enhance_sample_vs_pa.R ./data/sim_spp/vs_hab_suit_lme_res/${item}_vs_samp.rds ./data/sim_spp/habitat_covs ./data/sim_spp/vs_hab_suit_lme_res/${item}_vs_enhanced.rds 
  #     deps:
  #       - ./tools/enhance_r/enhance_r_funcs.R
  #       - ./data/sim_spp/vs_hab_suit_lme_res/${item}_vs_samp.rds
  #       - ./data/sim_spp/habitat_covs
  #     outs:
  #       - ./data/sim_spp/vs_hab_suit_lme_res/${item}_vs_enhanced.rds 
  
  sim_spp_split_train_test:
    foreach: ${sim_spp_split_train_test_datasets}
    do: 
      cmd: |
        R CMD pipelines/sim_spp/R/split_train_test.R ./data/sim_spp/vs_hab_suit_lme_res/cc_vs_enhanced.rds ${item.train_start_date} ${item.train_end_date} ./data/sim_spp/train_test_lme_res/cc/${item.name}.rds 
      deps:
        - ./data/sim_spp/vs_hab_suit_lme_res/cc_vs_enhanced.rds
      outs:
        - ./data/sim_spp/train_test_lme_res/cc/${item.name}.rds 
  
  sim_spp_fit_brt:
    foreach: ${sim_spp_fit_brt}

    do: 
      cmd: |
        R CMD pipelines/sim_spp/R/fit_brt.R ./data/sim_spp/train_test_lme_res/cc/${item.dataset} ${item.predictors_vec} ${item.response} ${item.family} ${item.tree_complexity} ${item.learning_rate} ${item.bag_fraction} ./data/sim_spp/brt_fits_lme_res/cc/${item.model_name}.rds 
      deps:
        - ./data/sim_spp/train_test_lme_res/cc/${item.dataset}
      outs:
        - ./data/sim_spp/brt_fits_lme_res/cc/${item.model_name}.rds 
