**Code for: "Simplifying mosquito sampling to estimate community biting exposure and indoor malaria vector control impact".**

\*\*Authors: Nilani Chandradeva, Elizabeth Pretorius, Eunice Teixeira da Silva, Harry Hutchims, Fatucha Barri, Isaac J. Stopard, Hugo Soubrier, Cesario Martins, Amabelia Rodrigues, Jose Ernesto Nante, Paulo Djata, Joseph D. Challenger, Thomas S. Churcher\*, Ellie Sherrard-Smith\* and Anna Last \*

(\*) senior authors

The repo is structured into 3 sections (listed below). Within each section, scripts are sequentially numbered in the order that they should be run in. Any data required to run scripts is called from the `data_in` folder, or the `output` folder if it is generated within the analysis. The expected output can be found in the `output` folder within each of the 3 folders listed below: 

Folder `1-segmented-sampling-validation`: contains the analysis for validating the segmented sampling method by revisiting the systematic review by Sherrard-Smith et al. (2019) <https://www.pnas.org/doi/10.1073/pnas.1820646116>

Folder `2-field-data`: analysing the data collected in the Bijagos archipelago in November 2023.

Folder `3-model-ITN-impact`: code for using the ICDMM model to predict the impact of ITNs in a Bijagos-like setting, using estimates of phi-Bed generated from field data.

The analysis is run using R version 4.4.1. Each script will be relatively quick to run. Note that `1-segmented-sampling-validation/2.ITN_impact_sampling_method.R`,  `2-field-data/2.sleeping_glmm.R`, `2-field-data/4.ento_glmm.R` and `3-model-ITN-impact/1.scenario_modelling_bijagos.R` will take slightly longer to run due to the run time of the statistical and mathematical models.

This version of the ICDMM R package is required to run the code. Please follow these installation instructions below (and for other packages required to run the analysis). These packages should take a few minutes to install. 

``` r
#need to install the ICDMM package (this version) 
install.packages( "odin", repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org")) 
devtools::install_github("nilani-chandradeva1/odin-malaria-model")
#rstanarm and bayesplot will require a C++ toolchain, which can be installed by following the Stan installation guideline for R: https://mc-stan.org/rstan/
install.packages(c("tidyverse", "ggpattern", "scales", "hms", "rstanarm", "bayesplot", "lubridate", "sf", "sp"))
#ggpattern requires package "fastmap" version ≥ 1.2.0. If this is not installed automatically, ensure that it is: install_packages("fastmap", dependencies = TRUE)
remotes::install_github("r-tmap/map")

```
