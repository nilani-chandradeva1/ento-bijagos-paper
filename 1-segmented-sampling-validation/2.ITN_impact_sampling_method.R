#use the ICDMM model to predict ITN impact, depending on whether bites_Bed (phi-Bed) is estimated using hourly or segmented method

require(ICDMM)
require(tidyverse)

# create a vector of age categories
init_age <- c(0, 1, 2, 3.5, 5, 7.5, 10, 15, 20, 30, 40, 50, 60)

# provide a value of the annual EIR for this model run
init_EIR <- 30 # low transmission setting for Bijagos-like setting

# provide the length of time (in days) that you want to run the model for (long so get to equilibrium by time ITNs are distributed)
time_period <- 365*15

# provide a value for the proportion of cases that are treated (referred to as ft in the paper)
prop_treated <- 0

# Define time for turning on interventions
ITN_IRS_on <- (365*5) + (6*30) #5.5 years

#from all studies from review (hourly and segmented phis)
study_dev <- read.csv("1-segmented-sampling-validation/output/phi-b-review-analysis.csv") %>%
  select(-c(new_PHI_B_L, new_PHI_B_U)) #remove the uncertainty estimates generated from taking min and max of human activity data

#take the phi-B estimates from each study (hourly, segmented method and uncertainty)
phi_B_seg <-  round(study_dev$new_PHI_B_seg,4)
phi_B_seg_L <- round(study_dev$new_PHI_B_seg_L, 4)
phi_B_seg_U <- round(study_dev$new_PHI_B_seg_U, 4)

phi_B_hourly <- round(study_dev$new_PHI_B,4)


phi_B_hourly_L2 <- round(study_dev$new_PHI_B_L2, 4) #use the uncertainty from taking 20th and 80th percentile from human activity data
phi_B_hourly_U2 <- round(study_dev$new_PHI_B_U2, 4) #use the uncertainty from taking 20th and 80th percentile from human activity data


study_dev2 <- study_dev %>%
  select(!X) %>%
  pivot_longer(!c(Ref_name, residuals_phiB), names_to = "parameter", values_to = "value") %>%
  mutate(segment = case_when(grepl("seg", parameter) ~ "segment",
                             TRUE ~ "hourly"),
         estim_type = case_when(grepl("L", parameter) ~ "lower",
                                grepl("U", parameter) ~ "upper",
                                TRUE ~ "point"),
         phi_param = case_when(
           grepl("PHI_B", parameter) ~ "bites_Bed")) %>%
 # select(!parameter) %>%
  pivot_wider(names_from = phi_param, values_from = value)


bites_bed_vec <- c(phi_B_seg, phi_B_seg_L, phi_B_seg_U, phi_B_hourly, phi_B_hourly_L2, phi_B_hourly_U2)

bites_df <- data.frame(study_dev$Ref_name, phi_B_seg, phi_B_seg_L, phi_B_seg_U, phi_B_hourly, phi_B_hourly_L2, phi_B_hourly_U2)

bites_df2 <- data.frame(study_dev$Ref_name, bites_bed_vec)

#get net efficacies: from Sherrard-Smith et al (2022) LancetPH work
#this work estimated the probability that a mosquito is killed (dn0), repelled (rn0) or able to successfully feed in experimental hut trials, for a given phenotypic resistance level
pyr_df <- read.csv("1-segmented-sampling-validation/data_in/ESS_pyrethroid_only_nets.csv")


pyr_df <- pyr_df %>%
  filter(resistance == 0.55) #based on Moss 2024, phenotypic resistance is 55%

#can explore uncertainty of resistance as well (although we do not show this in paper, so just select the median estimates)
dn0_vec_pyr <- pyr_df$dn0_med
rn0_vec_pyr <- pyr_df$rn0_med

gamman_vec_pyr <-  pyr_df$gamman_med

ento_df_pyr <- data.frame(bites_bed_vec, dn0_vec_pyr)
names(ento_df_pyr)<- c("bites_Bed", "d_ITN0") #these are the names of the parameters in the transmission model


ento_df_pyr <- ento_df_pyr %>%
  mutate(r_ITN0 = case_when(
                            d_ITN0 == pyr_df$dn0_med ~ rn0_vec_pyr[1],
                           
                            TRUE ~ NA_real_),
         itn_half_life = case_when(
                                   d_ITN0 == pyr_df$dn0_med ~ gamman_vec_pyr[1]*365, #convert to days
                                   
                                   TRUE ~ NA_real_)) %>%
  distinct() #32 rows. 

#create list, model will be run over these parameter combinations
ento_param_list_pyr <- list()
for (i in seq_len(nrow(ento_df_pyr))){
  ento_param_list_pyr[[i]] <- as.numeric(ento_df_pyr[i,])
}

out_nets_pyr <- function(itn_input){
  bites_Bed_in <- itn_input[1]
  d_ITN0_in <- itn_input[2]
  r_ITN0_in <- itn_input[3]
  itn_half_life_in <- itn_input[4]
  output <- run_model(het_brackets = 5,
                      age = init_age,
                      time = time_period,
                      init_EIR = init_EIR,
                      num_int = 2, #specifying ITNs only
                      itn_cov = 0.86, #From Hutchins et al 2020 for Bijagos 
                      ITN_IRS_on = ITN_IRS_on,
                      init_ft = prop_treated,
                      bites_Bed = bites_Bed_in,
                      itn_half_life = itn_half_life_in,
                      d_ITN0 = d_ITN0_in,
                      r_ITN0 = r_ITN0_in,
                      country = "Guinea-Bissau",
                      admin2 = "Bolama") #admin 2 unit in which Bijagos is situated
  return(output)
}

my_sim_nets_pyr <- function(){
  ento_out <- lapply(ento_param_list_pyr, out_nets_pyr)
  ento_out_mod <- do.call(rbind, sapply(1:(nrow(ento_df_pyr)), function(x){
    df <- as.data.frame(ento_out[[x]])
    df2 <- as.data.frame(dplyr::select(.data = df,t, mu, mv, itn_cov, EIR_tot, prev,
                                       d_ITN0, r_ITN0, bites_Bed, Q0,inc05,s_ITN, d_ITN, r_ITN))
    df3 <- as.data.frame(dplyr::mutate(.data = df2, ref = x, net_type = "pyr_only"))}, simplify = F))
  return(ento_out_mod)
  
}

ento_nets_pyr <- my_sim_nets_pyr()

ento_nets_pyr$bites_Bed <- round(ento_nets_pyr$bites_Bed, 4)

#baseline scenario: no interventions
init_EIR_in <- 30
ento_baseline_df <- data.frame(init_EIR_in)
ento_param_list_baseline <- list()
for (i in seq_len(nrow(ento_baseline_df))){
  ento_param_list_baseline[[i]] <- as.numeric(ento_baseline_df[i,]) #some of this gets overwritten
}

out_baseline <- function(itn_input){
  init_EIR_in <- as.numeric(itn_input)
  output <- run_model(het_brackets = 5,
                      age = init_age,
                      time = time_period,
                      init_EIR = init_EIR_in,
                      num_int = 1,
                      #itn_cov = itn_cov_in,
                      #ITN_IRS_on = ITN_IRS_on,
                      init_ft = prop_treated,
                      country = "Guinea-Bissau",
                      admin2 = "Bolama")
  return(output)
}

my_sim_baseline <- function(){
  #pyr_out_list_antag_ITN <- purrr::map2(y, x, antag_ITN_cov_loop) #loop through all parameter values
  ento_out <- lapply(ento_param_list_baseline, out_baseline)
  #res_ento_out <- lapply(ento_out, runfun) #put these values into the model
  ento_out_mod <- do.call(rbind, sapply(1:(nrow(ento_baseline_df)), function(x){
    df <- as.data.frame(ento_out[[x]])
    df2 <- as.data.frame(dplyr::select(.data = df,t, mu, mv, EIR_tot, prev,
                                       Q0, inc05))
    df3 <- as.data.frame(dplyr::mutate(.data = df2, ref = x, net_type = "baseline"))}, simplify = F))
  return(ento_out_mod)
  
}

ento_baseline <- my_sim_baseline()

#dynamics are due to seasonality
ggplot(ento_baseline, aes(x = t, y = prev))+
  geom_point()+
  facet_wrap(vars(ref))+
  ylim(0, 1)

ento_baseline <- ento_baseline %>%
  mutate(net_type = "baseline",
         itn_cov = 0.86,
         segment_type = "baseline") #just for ease to bind dfs

ento_nets_data <- ento_nets_pyr

#save 
saveRDS(ento_nets_data, file = "1-segmented-sampling-validation/output/ento_nets_impact_sampling_methods.rds")


#read in
ento_nets_data <- readRDS("1-segmented-sampling-validation/output/ento_nets_impact_sampling_methods.rds")


ento_nets_data2 <- ento_nets_data %>%
  select(t, inc05,net_type, bites_Bed)

ento_baseline_rbind <- ento_baseline %>%
  mutate(net_type = "baseline",
         bites_Bed = "baseline") %>%
  select(t, inc05, net_type, bites_Bed)

nets_data <- rbind(ento_nets_data2, ento_baseline_rbind)

#compare the cases averted according to each sampling method

cases_baseline <- ento_baseline %>%
  filter(between(t, 365*11.5, 365*14.5)) %>% #a 3-year from a ITN distribution at 11.5 years into simulation
  summarise(tot_cases = sum(inc05)) #4.21..


cases_pyr <- nets_data %>% #modify to separate out the studies here. Want efficacy by the study.
  filter(between(t, 365*11.5, 365*14.5)) %>% #a 3-year from a ITN distribution at 11.5 years into simulation
  group_by(bites_Bed) %>%
  summarise(tot_cases = sum(inc05))%>% #compute the total number of clinical cases in under 5s in this period
  mutate(total_case_baseline = cases_baseline$tot_cases, #append with the 
         cases_averted = total_case_baseline - tot_cases,
         cases_averted_percent = (cases_averted/total_case_baseline)*100,
         net_type = "pyrethroid only")

cases_pyr$bites_Bed <- as.numeric(cases_pyr$bites_Bed)

#remove the last row (the baseline scenario)
cases_pyr <- cases_pyr[1:32, ]
cases_pyr <- cases_pyr %>%
  select(-net_type)

#now need to map these bites_Bed values onto study_dev2

study_dev2$bites_Bed <- round(study_dev2$bites_Bed, 4)

case_summary <- left_join(study_dev2, cases_pyr, by = "bites_Bed")

case_summary_rel<- case_summary %>%
  ungroup() %>%
  select(segment, Ref_name, estim_type,cases_averted_percent) %>%
  pivot_wider(names_from = estim_type, values_from = cases_averted_percent)

study_dev3 <- study_dev2 %>%
  select(Ref_name, residuals_phiB) %>%
  unique()

#relative difference in cases averted in u5s according to each sampling method
case_summary_rel2 <- left_join(case_summary_rel, study_dev3, by = "Ref_name")


write.csv(case_summary_rel2, file = "1-segmented-sampling-validation/output/case_summary_rel.csv")
