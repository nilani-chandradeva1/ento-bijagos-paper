#modelling impact of ITNs in different phi-B settings in the Bijagos islands
#differences in phi-B are driven by different assumptions on human sleeping patterns

# A) make a dynamics with the mean estimate from each scenario (prevalence in under 5s)
# B) make a plot with cases averted from each scenario

#show the extent to which assumptions about sleeping patterns may influence the impact of a given coverage of ITNs

require(ICDMM)
require(tidyverse)
#read in the phi estimates (incl. senstivity analysis)
phi_vals <- read.csv("2-field-data/output/phi_estimates_bijagos.csv")
# create a vector of age categories
init_age <- c(0, 1, 2, 3.5, 5, 7.5, 10, 15, 20, 30, 40, 50, 60)

# provide the length of time (in days) that you want to run the model for
time_period <- 365*15

# provide a value for the proportion of cases that are treated (referred to as ft in the paper)
prop_treated <- 0

# Define time for turning on interventions
ITN_IRS_on <- (365*5) + (6*30) #5.5 years


#get net efficacies
pyr_df <- read.csv("1-segmented-sampling-validation/data_in/ESS_pyrethroid_only_nets.csv")

pyr_df <- pyr_df %>%
  filter(resistance == 0.55) #phenotypic resistance measured by Moss et al 2024. 

dn0_vec_pyr <- c(pyr_df$dn0_lo10, pyr_df$dn0_med, pyr_df$dn0_up90)
rn0_vec_pyr <- c(pyr_df$rn0_lo10, pyr_df$rn0_med, pyr_df$rn0_up90)

#vector of bites_bed for each sleeping pattern scenario, and the lower and upper bounds
bites_bed_vec <- c(phi_vals$mean, phi_vals$lower, phi_vals$upper)

#first get model in the right space - which transmission intensity gets us closest to prevalence estimates from McGregor et al 2020 survey (~6%)

init_EIR_in <- c(15, 20, 30)
bites_bed_in <- phi_vals$mean[1] #for the observed data
gamman_in <- pyr_df$gamman_med*365 #taking in median estimates
rn0_in <- pyr_df$rn0_med #taking in median estimates
dn0_in <- pyr_df$dn0_med #taking in median estimates

space_mod <- data.frame(init_EIR_in, bites_bed_in, gamman_in, rn0_in, dn0_in)
names(space_mod) <- c("init_EIR", "bites_Bed", "itn_half_life", "r_ITN0", "d_ITN0")

space_mod_list <- list()

for (i in seq_len(nrow(space_mod))){
  space_mod_list[[i]] <- as.numeric(space_mod[i,])
}

out_space_mod <- function(itn_input){
  init_EIR_in <- itn_input[1]
  bites_Bed_in <- itn_input[2]
  itn_half_life_in <- itn_input[3]
  r_ITN0_in <- itn_input[4]
  d_ITN0_in <- itn_input[5]
  output <- run_model(het_brackets = 5,
                      age = init_age,
                      time = time_period,
                      init_EIR = init_EIR_in,
                      num_int = 2,
                      itn_cov = 0.86, #ITN usage for Bijagos from Hutchins et al (2020)
                      ITN_IRS_on = ITN_IRS_on,
                      init_ft = prop_treated,
                      bites_Bed = bites_Bed_in,
                      itn_half_life = itn_half_life_in,
                      d_ITN0 = d_ITN0_in,
                      r_ITN0 = r_ITN0_in,
                      country = "Guinea_Bissau",
                      admin2 = "Boloma")
  return(output)
}

my_sim_space_mod <- function(){
  ento_out <- lapply(space_mod_list, out_space_mod)
  ento_out_mod <- do.call(rbind, sapply(1:(nrow(space_mod)), function(x){
    df <- as.data.frame(ento_out[[x]])
    df2 <- as.data.frame(dplyr::select(.data = df,t, mu, mv, itn_cov, EIR_tot, prev,
                                       d_ITN0, r_ITN0, bites_Bed, Q0,inc05,s_ITN, d_ITN, r_ITN))
    df3 <- as.data.frame(dplyr::mutate(.data = df2, ref = x, net_type = "space_mod"))}, simplify = F))
  return(ento_out_mod)
  
}

ento_space_mod <- my_sim_space_mod()

ggplot(ento_space_mod, aes(x = t, y  = prev))+
  geom_point()+
  facet_wrap(vars(ref))+
  geom_vline(aes(xintercept = (12*365)+60), col = "red")+ #time of survey
  geom_vline(aes(xintercept = ITN_IRS_on), col = "blue")+ #time nets go on
  ylim(0, 0.8)+
  geom_hline(aes(yintercept = 0.06)) #6% from McGregor et al survey

###based on this, use init_EIR of 30
init_EIR <- 30


gamman_vec_pyr <- c(pyr_df$gamman_lo10, pyr_df$gamman_med, pyr_df$gamman_up90)
ento_df_pyr <- expand.grid(bites_Bed = bites_bed_vec,
                           d_ITN0 = dn0_vec_pyr[2])

ento_df_pyr <- ento_df_pyr %>%
  mutate(r_ITN0 = case_when(d_ITN0 == pyr_df$dn0_med ~ rn0_vec_pyr[2],
                            TRUE ~ NA_real_),
         itn_half_life = case_when(d_ITN0 == pyr_df$dn0_med ~ gamman_vec_pyr[2]*365,
                                   TRUE ~ NA_real_))
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
                      num_int = 2,
                      itn_cov = 0.86, #from Hutchins et al 2020
                      ITN_IRS_on = ITN_IRS_on,
                      init_ft = prop_treated,
                      bites_Bed = bites_Bed_in,
                      itn_half_life = itn_half_life_in,
                      d_ITN0 = d_ITN0_in,
                      r_ITN0 = r_ITN0_in,
                      country = "Guinea-Bissau",
                      admin2 = "Bolama")
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

#add on information about scenario etc
ento_nets_pyr <- ento_nets_pyr %>%
  mutate(scenario = case_when(bites_Bed == bites_bed_vec[1] ~ "data_sleep_M",
                              bites_Bed == bites_bed_vec[2] ~ "80_sleep_M",
                              bites_Bed == bites_bed_vec[3] ~ "60_sleep_M",
                              bites_Bed == bites_bed_vec[4] ~ "data_sleep_L",
                              bites_Bed == bites_bed_vec[5] ~ "80_sleep_L",
                              bites_Bed == bites_bed_vec[6] ~ "60_sleep_L",
                              bites_Bed == bites_bed_vec[7] ~ "data_sleep_U",
                              bites_Bed == bites_bed_vec[8] ~ "80_sleep_U",
                              bites_Bed == bites_bed_vec[9] ~ "60_sleep_U"))

ento_nets_data <- ento_nets_pyr

saveRDS(ento_nets_data, file = "3-model-ITN-impact/output/bijagos_scenarios.rds")

#baseline scenario
init_EIR_in <- 30
ento_baseline_df <- data.frame(init_EIR_in)
ento_param_list_baseline <- list()
for (i in seq_len(nrow(ento_baseline_df))){
  ento_param_list_baseline[[i]] <- as.numeric(ento_baseline_df[i,]) #some of this gets overwritten
}

out_baseline <- function(itn_input){
  output <- run_model(het_brackets = 5,
                      age = init_age,
                      time = time_period,
                      init_EIR = init_EIR,
                      num_int = 1,
                      #itn_cov = itn_cov_in,
                      #ITN_IRS_on = ITN_IRS_on,
                      init_ft = prop_treated,
                      country = "Guinea-Bissau",
                      admin2 = "Bolama")
  return(output)
}

my_sim_baseline <- function(){
  ento_out <- lapply(ento_param_list_baseline, out_baseline)
  ento_out_mod <- do.call(rbind, sapply(1:(nrow(ento_baseline_df)), function(x){
    df <- as.data.frame(ento_out[[x]])
    df2 <- as.data.frame(dplyr::select(.data = df,t, mu, mv, EIR_tot, prev,
                                       Q0, inc05))
    df3 <- as.data.frame(dplyr::mutate(.data = df2, ref = x, net_type = "baseline"))}, simplify = F))
  return(ento_out_mod)
  
}

ento_baseline <- my_sim_baseline()


ento_baseline <- ento_baseline %>%
  mutate(
    itn_cov = 0.86,
    sleeping_scenario = "No interventions",
    net_type = "Baseline (no interventions)") #just for ease to help with facet plot later

require(tidyverse)

#read in, to save running again if need to
ento_nets_data <- readRDS("3-model-ITN-impact/output/bijagos_scenarios.rds")

ento_nets_data2 <- ento_nets_data %>%
  filter(net_type == "pyr_only") %>%
  mutate(sleeping_scenario = case_when(grepl("data", scenario) ~ "Observed data (100%) on population in bed at night",
                                       grepl("80", scenario) ~ "80% population in bed at night",
                                       grepl("60", scenario) ~ "60% population in bed at night"),
         phi_uncertainty = case_when(grepl("_M", scenario) ~ "mean",
                                     grepl("_L", scenario) ~ "lower",
                                     grepl("_U", scenario) ~ "upper"),
         net_type = case_when(
           net_type == "pyr_only" ~ "Pyrethroid-only"))

#extract epi output derived from mean phi-B estimate
ento_nets_data3 <- ento_nets_data2 %>%
  filter(phi_uncertainty == "mean") %>% 
  select(t, inc05,prev,net_type, sleeping_scenario)

#prevalence over time####
ento_baseline_rbind <- ento_baseline %>%
  mutate(
    itn_cov = 0.86,
    sleeping_scenario = "No interventions",
    net_type = "Baseline (no interventions)") %>% #just for ease to help with facet plot later
  select(t, inc05, prev,net_type, sleeping_scenario)

nets_data_phi <- rbind(ento_nets_data3, ento_baseline_rbind) %>%
  rename(LLIN = net_type)

write.csv(nets_data_phi, file = "3-model-ITN-impact/output/incidence_bijagos.csv", row.names = FALSE)

#cases averted####
cases_baseline <- ento_baseline %>%
  filter(between(t, 365*11.5, 365*14.5)) %>%
  reframe(tot_cases = sum(inc05)) #4.210176 per person

cases_pyr <- ento_nets_data2 %>%
  group_by(scenario) %>%
  filter(between(t, 365*11.5, 365*14.5), 
         itn_cov == 0.86) %>%
  summarise(tot_cases = sum(inc05))%>%
  mutate(total_case_baseline = cases_baseline$tot_cases,
         cases_averted = total_case_baseline - tot_cases,
         cases_averted_percent = (cases_averted/total_case_baseline)*100, #rel difference in cases between intervention and baseline scenarios
         net_type = "pyrethroid only")


case_summary <-cases_pyr

case_summary_wide <- case_summary %>%
  mutate(sleeping_scenario = case_when(grepl("data", scenario) ~ "Observed data on population in bed at night",
                                       grepl("80", scenario) ~ "80% population in bed at night",
                                       grepl("60", scenario) ~ "60% population in bed at night"),
         phi_uncertainty = case_when(grepl("_M", scenario) ~ "mean",
                                     grepl("_L", scenario) ~ "lower",
                                     grepl("_U", scenario) ~ "upper"))

case_summary_rel<- case_summary_wide %>%
  group_by(sleeping_scenario, net_type) %>%
  select(sleeping_scenario, phi_uncertainty,cases_averted_percent, net_type) %>%
  pivot_wider(names_from = phi_uncertainty, values_from = cases_averted_percent)

write.csv(case_summary_rel, file = "3-model-ITN-impact/output/case_summary_rel_bijagos_scenario.csv")

