# SDM-Preds-Novel-Conditions
This repository includes the code to support the manuscript "Species distribution model predictability doesn’t always decline under novel temperature conditions." Before moving forward, interested users will also need the "data" folder, which can be accessed [here](10.6084/m9.figshare.25687137) or requested by sending an email to [Andrew Allyn](mailto:aallyn@gmri.org). After receiving the data folder, it should be downloaded and added to this repo in the main folder (i.e., the path `SDM-PREDS-NOVEL-CONDITIONS/data/` must exist).

# Workflow
Following the manuscript, the general workflow for this analysis includes the following steps and running code in the corresponding scripts:
1. Select focal large marine ecosystems (LMEs) based on sea surface temperature (SST) trends and patterns, with an accompanying self organizing maps analysis (`01_Complete_SOM.R`) to understand how the California Current and Northeast U.S. Shelf Large Marine Ecosystems relate to the 66 other global LMEs.
2. Within each LME, complete a search of possible mean and standard deviation values to identify the combination that generates a resident-mobile and seasonally-migrating warm water speices archetype (`02_Search_Species_Curve_Parameters.R`).
3. After selecitng species-response curve parameters, simulate monthly species' habitat suitability for each species archetype in each ecosystem (`03_Simulate_HabSuit.R`).
4. Gather training and testing datasets by converting habitat suitability (step 3) into presence/absences, gathering sample data, enhancing sample data with covariates, and then splitting it into training and testing data (`04_Split_Train_Test_Data.R`). This step in particular can take a long time with enhancing "observations" with covariates.
5. Fit boosted regression tree SDM to 1985-2004 training data (`05_Fit_BRTs.R`).
6. Predict monthly probability of presence from 2005-2020 using fitted models and observed SST and then calculate prediction performance metrics (`06_Pred_BRTs_Get_Env_Novelty.R`).
7. Calculate environmental novelty of prediction target conditions (step 5) relative to baseline training conditions (step 4).
8. Assess relationships between SDM prediction performance and environmental novelty (`07_Process_Vizualize_Results.R`).
 
# Interacting with the repo
Depending on computing abilities, it will take a while to run through all of the steps for each of the large marine ecosystems and species archetypes. As a result, we have made all of the raw and interim datasets, model fits, and predictions available. This will allow folks to pick up towards the end of the workflow. While, also allowing the oppportunity to review specific workflow steps and corresponding functions as needed without requiring everything is run from the beginning to the end. or reference, R session info is also pasted below.

## R session info


# Questions or suggestions?
Please feel free to reach out to [Andrew Allyn](mailto:aallyn@gmri.org) with any questions or suggestions!