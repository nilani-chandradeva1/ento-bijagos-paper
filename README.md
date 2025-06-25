**Code for: "Simplifying mosquito sampling to estimate community biting exposure and indoor malaria vector control impact".**

\*\*Authors: Nilani Chandradeva, Elizabeth Pretorius, Eunice Teixera da Silva, Harry Hutchims, Fatucha Barri, Isaac J. Stopard, Hugo Soubrier, Cesario Martins, Amabelia Rodrigues, Jose Ernesto Nante, Paulo Djata, Joseph D. Challenger, Thomas S. Churcher\*, Ellie Sherrard-Smith\* and Anna Last \*

(\*) senior authors

The repo is structured into 3 sections (listed below). Within each section, scripts are sequentially numbered in the order that they should be run in. Any data required to run scripts is called from the `data_in` folder, or the `output` folder if it is generated within the analysis.

Folder `1-segmented-sampling-validation`: contains the analysis for validating the segmented sampling method by revisiting the systematic review by Sherrard-Smith et al. (2019) <https://www.pnas.org/doi/10.1073/pnas.1820646116>

Folder `2-field-data`: analysing the data collected in the Bijagos archipelago in November 2023.

Folder `3-model-ITN-impact`: code for using the ICDMM model to predict the impact of ITNs in a Bijagos-like setting, using estimates of phi-Bed generated from field data.

This version of the ICDMM R package is required to run the code. Please follow these installation instructions below.

``` r
#need to install the ICDMM package (this version) 
install.packages( "odin", repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org")) 
devtools::install_github("nilani-chandradeva1/odin-malaria-model")
```
