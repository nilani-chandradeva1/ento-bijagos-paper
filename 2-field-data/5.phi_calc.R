#calculating phi-Bed from the biting rate and sleeping pattern data for Bijagos archipelago
#use posterior distributions of prop bites and prop people in bed. 
#apply age-weighting to the posterior distr. of prop people in bed (age-stratified) to account for local age distribution
#take 2.5% and 97.5% quantiles of combined posterior distribution of phi-Bed to get 95% CrIs. 

require(tidyverse)

human_model <- readRDS("2-field-data/output/human_glm_fit0.rds")
dim(human_model)
ento_model <- readRDS("2-field-data/output/ento_model_fit1.rds")
dim(ento_model)

#compute lambda (prop bites indoors or outdoors for each time period) for the ento data
lambda_calc <- ento_model %>%
  rowwise() %>%
  mutate(row_tot = sum(c_across(day_out_exp:night_in_exp)), 
         lambda_in_day = day_in_exp/row_tot, 
         lambda_out_day = day_out_exp/row_tot, 
         lambda_in_eve = eve_in_exp/row_tot, 
         lambda_out_eve = eve_out_exp/row_tot, 
         lambda_in_night = night_in_exp/row_tot, 
         lambda_out_night = night_out_exp/row_tot) %>%
  select(lambda_in_day, lambda_out_day, lambda_in_eve, lambda_out_eve, lambda_in_night, lambda_out_night)

human_u5 <- data.frame(human_model$day_in_u5, human_model$eve_in_u5, human_model$night_in_u5)
human_5_14 <- data.frame(human_model$day_in_5_14, human_model$eve_in_5_14, human_model$night_in_5_14)
human_over_15 <- data.frame(human_model$day_in_15_plus, human_model$eve_in_15_plus, human_model$night_in_15_plus)

dim(human_u5) #4000 rows, 3 col

#function to estimate phi-Bed from lambda and sleeping pattern distribution
phi_calculator <- function(lambda_calc, sleeping_pattern){
  
  #validate input 
  if (!is.matrix(sleeping_pattern) && !is.data.frame(sleeping_pattern)) {
    stop("Error: sleeping_pattern must be a matrix or dataframe with dimensions 4000 x 3.")
  }
  if (!all(dim(sleeping_pattern) == c(4000, 3))) {
    stop("Error: sleeping_pattern must have exactly 4000 rows and 3 columns.")
  }
  
  numerator_phi_day = lambda_calc$lambda_in_day*sleeping_pattern[,1]
  numerator_phi_eve = lambda_calc$lambda_in_eve*sleeping_pattern[,2]
  numerator_phi_night = lambda_calc$lambda_in_night*sleeping_pattern[,3]
  
  denominator_phi_day = ((1-sleeping_pattern[,1])*lambda_calc$lambda_out_day) + (sleeping_pattern[,1]*lambda_calc$lambda_in_day)
  denominator_phi_eve = ((1-sleeping_pattern[,2])*lambda_calc$lambda_out_eve) + (sleeping_pattern[,2]*lambda_calc$lambda_in_eve)
  denominator_phi_night = ((1-sleeping_pattern[,3])*lambda_calc$lambda_out_night) + (sleeping_pattern[,3]*lambda_calc$lambda_in_night)
  
  total_numerator = numerator_phi_day + numerator_phi_eve + numerator_phi_night
  total_denominator = denominator_phi_day + denominator_phi_eve + denominator_phi_night
  phi <- total_numerator/total_denominator
  return(phi)
  
}

#calculate phi for each age group (using age-specific sleeping pattern data)
phi_u5 <- phi_calculator(lambda_calc = lambda_calc, sleeping_pattern = human_u5)
range(phi_u5)
phi_5_14 <- phi_calculator(lambda_calc = lambda_calc, sleeping_pattern = human_5_14)
phi_over_15 <- phi_calculator(lambda_calc = lambda_calc, sleeping_pattern = human_over_15)

#then pipe through the age-weighting process
age <- seq(0, 80, 5)
na <- as.integer(length(age))
age_rate <- age_width <- age_mid_point <- den <- c()
for (i in 1:(na-1))
{
  age_width[i] <- age[i+1] - age[i]
  age_rate[i] <- 1/(age[i + 1] - age[i])  # vector of rates at which people leave each age group (1/age group width)
  age_mid_point[i] <- 0.5 * (age[i] + age[i + 1])  # set age group vector to the midpoint of the group
  
}
age_rate[na] = 0

av_age_GB <- 19.4 #av age GB 2023 https://population.un.org/dataportal/data/indicators/61/locations/624,834/start/1990/end/2024/table/pivotbylocation?df=ee706f12-9ffd-4f3e-9524-ba165f18c207
den <- 1/(1 + age_rate[1]/(1/av_age_GB))
for (i in 1:(na-1))
{
  den[i+1] <- age_rate[i] * den[i]/(age_rate[i+1] + (1/av_age_GB))  # proportion in each age_vector group
}

prop_comp_GB <- den
sum(den) #check sums to 1

age_model <- data.frame(age, prop_comp_GB)

#prop of pop under 5
prop_u5 <- age_model %>%
  filter(age < 5)

prop_u5_pop_GB <- prop_u5$prop_comp_GB

#prop of pop 5-14y
prop_5_14 <- age_model %>%
  filter(age >= 5 & age < 15)

prop_5_14_pop_GB <- sum(prop_5_14$prop_comp_GB)

#prop of pop 15y+
prop_15_plus <- age_model %>%
  filter(age >= 15)

prop_15_plus_pop_GB <- sum(prop_15_plus$prop_comp_GB)

age_group <- c("U5", "5-14", "15-plus")
prop_GB <- c(prop_u5_pop_GB, prop_5_14_pop_GB, prop_15_plus_pop_GB)
sum(prop_GB) #correctly sums to 1

#apply the weighting by multiplying the age-specific phi-bed estimates by proportion of pop in each age group

phi_u5_weight <- phi_u5*prop_u5_pop_GB
phi_5_14_weight <- phi_5_14*prop_5_14_pop_GB
phi_over_15_weight <- phi_over_15*prop_15_plus_pop_GB

phi_all = phi_u5_weight + phi_5_14_weight + phi_over_15_weight
range(phi_all)
phi_all_mean <- mean(phi_all) #0.953
phi_all_lower <- quantile(phi_all, 0.025) #0.835
phi_all_upper <- quantile(phi_all, 0.975) #0.995

#then apply sensitivity analysis, 
#scale evening and night props in bed down by 80% and 60%

#80% of those indoors in baseline analysis stay in bed

night_prop_80_u5 <- human_model$night_in_u5*0.8
eve_prop_80_u5 <- human_model$eve_in_u5*0.8

night_prop_80_5_14 <- human_model$night_in_5_14*0.8
eve_prop_80_5_14 <- human_model$eve_in_5_14*0.8

night_prop_80_15_plus <- human_model$night_in_15_plus*0.8
eve_prop_80_15_plus <- human_model$eve_in_15_plus*0.8

#60% of those in baseline analysis stay in bed

night_prop_60_u5 <- human_model$night_in_u5*0.6
eve_prop_60_u5 <- human_model$eve_in_u5*0.6

night_prop_60_5_14 <- human_model$night_in_5_14*0.6
eve_prop_60_5_14 <- human_model$eve_in_5_14*0.6

night_prop_60_15_plus <- human_model$night_in_15_plus*0.6
eve_prop_60_15_plus <- human_model$eve_in_15_plus*0.6


human_u5_SM80 <- data.frame(human_model$day_in_u5, human_model$eve_in_u5, night_prop_80_u5)
human_5_14_SM80 <- data.frame(human_model$day_in_5_14, human_model$eve_in_5_14, night_prop_80_5_14)
human_over_15_SM80 <- data.frame(human_model$day_in_15_plus, human_model$eve_in_15_plus, night_prop_80_15_plus)

human_u5_SM60 <- data.frame(human_model$day_in_u5, human_model$eve_in_u5, night_prop_60_u5)
human_5_14_SM60 <- data.frame(human_model$day_in_5_14, human_model$eve_in_5_14, night_prop_60_5_14)
human_over_15_SM60 <- data.frame(human_model$day_in_15_plus, human_model$eve_in_15_plus, night_prop_60_15_plus)

#phi estimates for sensitivity

phi_u5_SM80 <- phi_calculator(lambda_calc = lambda_calc, sleeping_pattern = human_u5_SM80)
phi_5_14_SM80 <- phi_calculator(lambda_calc = lambda_calc, sleeping_pattern = human_5_14_SM80)
phi_over_15_SM80 <- phi_calculator(lambda_calc = lambda_calc, sleeping_pattern = human_over_15_SM80)

phi_u5_SM80_weight <- phi_u5_SM80*prop_u5_pop_GB
phi_5_14_SM80_weight <- phi_5_14_SM80*prop_5_14_pop_GB
phi_over_15_SM80_weight <- phi_over_15_SM80*prop_15_plus_pop_GB

phi_all_SM80 = phi_u5_SM80_weight+phi_5_14_SM80_weight+phi_over_15_SM80_weight
phi_SM80_mean <- mean(phi_all_SM80) #0.7688...
phi_SM80_lower <- quantile(phi_all_SM80, 0.025) #0.6099..
phi_SM80_upper <- quantile(phi_all_SM80, 0.975) #0.885..

phi_u5_SM60 <- phi_calculator(lambda_calc = lambda_calc, sleeping_pattern = human_u5_SM60)
phi_5_14_SM60 <- phi_calculator(lambda_calc = lambda_calc, sleeping_pattern = human_5_14_SM60)
phi_over_15_SM60 <- phi_calculator(lambda_calc = lambda_calc, sleeping_pattern = human_over_15_SM60)

phi_u5_SM60_weight <- phi_u5_SM60*prop_u5_pop_GB
phi_5_14_SM60_weight <- phi_5_14_SM60*prop_5_14_pop_GB
phi_over_15_SM60_weight <- phi_over_15_SM60*prop_15_plus_pop_GB

phi_all_SM60 = phi_u5_SM60_weight+phi_5_14_SM60_weight+phi_over_15_SM60_weight
phi_SM60_mean <- mean(phi_all_SM60) #0.6119..
phi_SM60_lower <- quantile(phi_all_SM60, 0.025) #0.4329..
phi_SM60_upper <- quantile(phi_all_SM60, 0.975) #0.7720..

phi_mean <- c(phi_all_mean, phi_SM80_mean, phi_SM60_mean)
phi_lower <- c(phi_all_lower,phi_SM80_lower, phi_SM60_lower)
phi_upper <- c(phi_all_upper, phi_SM80_upper, phi_SM60_upper)
label <- c("Data", "80%", "60%")
phi_estimates <- data.frame(label, phi_mean, phi_lower, phi_upper)
names(phi_estimates) <- c("scenario","mean", "lower", "upper")

#save final output of phi-bed estimates
write.csv(phi_estimates, file = "2-field-data/output/phi_estimates_bijagos.csv")
