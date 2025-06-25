#statistical analysis to estimate proportion of people in bed in each time segment, by age group

#ITN usage is measured by age group, so useful to know sleeping activity in age groups
#sleeping activity in time segments useful too for pairing with ento data and estimating phi-B

require(tidyverse)
require(rstanarm)
require(bayesplot)

df1 <- readRDS("2-field-data/output/sleeping_data_for_glmm.rds")

#number of people per age group
df1 %>%
  filter(segment == "night") %>%
  group_by(age_group) %>%
  summarise(tot_people = sum(nTot))

#binomial logistic regression 

#age and segment as fixed effects. Also explore model with interaction
#cluster as random effect, or without the cluster at all. (NB: cluster = sampling site)

df1 <- df1 %>%
  mutate(duration = case_when(segment == "evening" ~ 3, 
                              segment == "night" ~ 9, 
                              segment == "day" ~ 11), 
         segment_lab = case_when(segment == "evening" ~ 0, 
                                 segment == "night" ~ 1, 
                                 segment == "day" ~ 2), 
         age_group_lab = case_when(age_group == "0_4" ~ 0, 
                                   age_group == "5_14" ~ 1, 
                                   age_group == "15_plus" ~ 2))

df1 %>%
  group_by(age_group, segment) %>%
  summarise(mean_prop = mean(nIn/nTot))


nrow(df1) #42

#Explore different models 

#age and segment as fixed effects
fit_0 <- stan_glm(cbind(nIn, nTot-nIn) ~ as.factor(age_group_lab) + as.factor(segment_lab), 
                  data = df1,
                  family = binomial(link = "logit"))

#age, segment and interaction as fixed effects
fit_1 <- stan_glm(cbind(nIn, nTot-nIn) ~ as.factor(age_group_lab) + as.factor(segment_lab) 
                  + as.factor(age_group_lab):as.factor(segment_lab), 
                  data = df1, 
                  family = binomial(link = "logit"))

#age and segment as fixed, cluster as random
fit_2 <- stan_glmer(cbind(nIn, nTot-nIn) ~ as.factor(age_group_lab) + as.factor(segment_lab) 
                  + (1|cluster), 
                  data = df1, 
                  family = binomial(link = "logit"))

#age, segment and interaction as fixed effects and cluster as random
fit_3 <- stan_glmer(cbind(nIn, nTot-nIn) ~ + as.factor(age_group_lab) + as.factor(segment_lab) 
                  + as.factor(age_group_lab):as.factor(segment_lab) + (1|cluster), 
                  data = df1, 
                  family = binomial(link = "logit"))

loo0 <- loo(fit_0, save_psis = TRUE, k_threshold = 0.7) #excl 4
loo1 <- loo(fit_1, save_psis = TRUE, k_threshold = 0.7) #excl 14
loo2 <- loo(fit_2, save_psis = TRUE, k_threshold = 0.7) #excl 5 obs
loo3 <- loo(fit_3, save_psis = TRUE, k_threshold = 0.7) #excl 16 obs

loo_0_1 <- loo::loo_compare(loo0, loo1) #fit_0 is the better model. but se_diff*2 ~ elpd_diff so better by a small amount
loo_0_2 <- loo::loo_compare(loo0, loo2) #fit 0 is the better model, but marginally 
loo_0_3 <- loo::loo_compare(loo0, loo3) #fit 0 is the better model, marginally


#in SM table, fit_0 maps to model 1, fit_1 to model 4, fit_2, to model 3 and fit_3 to model 2

#then process model output from fit_0 to get posterior distribution for prop in bed for each time segment and age group

draws_0 <- as.data.frame(fit_0)
dim(draws_0) #4000 it

store <- rep(0,4000)
day_in_u5 <- rep(0, 4000)
eve_in_u5 <- rep(0, 4000)
night_in_u5 <- rep(0, 4000)

day_in_5_14 <- rep(0,4000)
eve_in_5_14 <- rep(0,4000)
night_in_5_14 <- rep(0,4000)

day_in_15_plus <- rep(0,4000)
eve_in_15_plus <- rep(0,4000)
night_in_15_plus <- rep(0,4000)

for (i in 1:4000){
  
  
  eve_in_u5[i] <- draws_0$`(Intercept)`[i] 
  night_in_u5[i] <- draws_0$`(Intercept)`[i] + draws_0$`as.factor(segment_lab)1`[i]
  day_in_u5[i] <- draws_0$`(Intercept)`[i] +draws_0$`as.factor(segment_lab)2`[i]
  
  
  eve_in_5_14[i] <- draws_0$`(Intercept)`[i] + draws_0$`as.factor(age_group_lab)1`[i] 
  night_in_5_14[i] <- draws_0$`(Intercept)`[i]+draws_0$`as.factor(age_group_lab)1`[i] + draws_0$`as.factor(segment_lab)1`[i]
  day_in_5_14[i] <- draws_0$`(Intercept)`[i] + draws_0$`as.factor(age_group_lab)1`[i] + draws_0$`as.factor(segment_lab)2`[i]
  
  
  eve_in_15_plus[i] <- draws_0$`(Intercept)`[i] + draws_0$`as.factor(age_group_lab)2`[i] 
  night_in_15_plus[i] <- draws_0$`(Intercept)`[i] + draws_0$`as.factor(age_group_lab)2`[i] + draws_0$`as.factor(segment_lab)1`[i]
  day_in_15_plus[i] <- draws_0$`(Intercept)`[i] + draws_0$`as.factor(age_group_lab)2`[i] + + draws_0$`as.factor(segment_lab)2`[i]
  
  
}



df_params_human <- data.frame(day_in_u5, eve_in_u5, night_in_u5, 
                              day_in_5_14, eve_in_5_14, night_in_5_14, 
                              day_in_15_plus, eve_in_15_plus, night_in_15_plus)

#convert log odds to probability with invlogit()
df_params_human_invlogit <- as.data.frame(lapply(df_params_human, invlogit))

#save best-fitting model 
write_rds(df_params_human_invlogit, file = "2-field-data/output/human_glm_fit0.rds")

df_params_human_invlogit <- readRDS("2-field-data/output/human_glm_fit0.rds")

#proportion of people in bed for each age group and time segment (and uncertainty)

mean(df_params_human_invlogit$night_in_u5) #0.998
quantile(df_params_human_invlogit$night_in_u5, prob = c(0.025, 0.975)) #0.9863..,0.99999...

mean(df_params_human_invlogit$night_in_5_14) #0.9969..
quantile(df_params_human_invlogit$night_in_5_14, prob = c(0.025, 0.975)) #0.97916..., 0.99985...
mean(df_params_human_invlogit$night_in_15_plus) #0.99667..
quantile(df_params_human_invlogit$night_in_15_plus, prob = c(0.025, 0.975)) #0.9786..., 0.999..

#process the proportions and put in data frame

#u5 output
mean_day_in_u5 <- mean(df_params_human_invlogit$day_in_u5)
day_in_u5_quant <- quantile(df_params_human_invlogit$day_in_u5, prob=c(0.025, 0.975))
day_in_u5_quant[[1]] #lower 

mean_eve_in_u5 <- mean(df_params_human_invlogit$eve_in_u5)
eve_in_u5_quant <- quantile(df_params_human_invlogit$eve_in_u5, prob=c(0.025, 0.975))

mean_night_in_u5 <- mean(df_params_human_invlogit$night_in_u5)
night_in_u5_quant <- quantile(df_params_human_invlogit$night_in_u5, prob=c(0.025, 0.975))

#5-14 output
mean_day_in_5_14 <- mean(df_params_human_invlogit$day_in_5_14)
day_in_5_14_quant <- quantile(df_params_human_invlogit$day_in_5_14, prob=c(0.025, 0.975))

mean_eve_in_5_14 <- mean(df_params_human_invlogit$eve_in_5_14)
eve_in_5_14_quant <- quantile(df_params_human_invlogit$eve_in_5_14, prob=c(0.025, 0.975))

mean_night_in_5_14 <- mean(df_params_human_invlogit$night_in_5_14)
night_in_5_14_quant <- quantile(df_params_human_invlogit$night_in_5_14, prob=c(0.025, 0.975))

#adult output
mean_day_in_15_plus <- mean(df_params_human_invlogit$day_in_15_plus)
day_in_15_plus_quant <- quantile(df_params_human_invlogit$day_in_15_plus, prob=c(0.025, 0.975))

mean_eve_in_15_plus <- mean(df_params_human_invlogit$eve_in_15_plus)
eve_in_15_plus_quant <- quantile(df_params_human_invlogit$eve_in_15_plus, prob=c(0.025, 0.975))

mean_night_in_15_plus <- mean(df_params_human_invlogit$night_in_15_plus)
night_in_15_plus_quant <- quantile(df_params_human_invlogit$night_in_15_plus, prob=c(0.025, 0.975))

mean_val <- c(mean_day_in_u5, mean_eve_in_u5, mean_night_in_u5, 
              mean_day_in_5_14, mean_eve_in_5_14, mean_night_in_5_14,
              mean_day_in_15_plus, mean_eve_in_15_plus, mean_night_in_15_plus)

lower_val <- c(day_in_u5_quant[[1]], eve_in_u5_quant[[1]], night_in_u5_quant[[1]], 
               day_in_5_14_quant[[1]], eve_in_5_14_quant[[1]], night_in_5_14_quant[[1]], 
               day_in_15_plus_quant[[1]], eve_in_15_plus_quant[[1]], night_in_15_plus_quant[[1]])

upper_val <- c(day_in_u5_quant[[2]], eve_in_u5_quant[[2]], night_in_u5_quant[[2]], 
               day_in_5_14_quant[[2]], eve_in_5_14_quant[[2]], night_in_5_14_quant[[2]], 
               day_in_15_plus_quant[[2]], eve_in_15_plus_quant[[2]], night_in_15_plus_quant[[2]])

parameter <- c("Day (u5)", "Evening (u5)", "Night (u5)", 
               "Day (5-14y)", "Evening (5-14y)", "Night (5-14y)", 
               "Day (15 plus)", "Evening (15 plus)", "Night (15 plus)")

df_fit0 <- data.frame(parameter, mean_val, lower_val, upper_val)

df_fit0 <- df_fit0 %>%
  mutate(period = case_when(grepl("Day", parameter) ~ "Day", 
                            grepl("Evening", parameter) ~ "Evening", 
                            grepl("Night", parameter) ~ "Night") , 
         age_group = case_when(grepl("u5", parameter) ~ "Under 5y", 
                               grepl("5-14y", parameter) ~ "5 to 14 years", 
                               grepl("15 plus", parameter) ~ "Over 15 years")
         )


dat_summary <- df1 %>%
  rename(period = segment) %>%
  mutate(period = case_when(period == "day" ~ "Day", 
                            period == "evening" ~ "Evening", 
                            period == "night" ~ "Night"), 
         age_group = case_when(age_group == "0_4" ~ "Under 5y", 
                               age_group == "5_14" ~ "5 to 14 years", 
                               age_group == "15_plus" ~ "Over 15 years")) %>%
  mutate(propIn = nIn/nTot)

dat_summary %>%
  filter(period == "Night")

df_fit0_data <- left_join(df_fit0, dat_summary)



df_fit0_data <- df_fit0_data %>%
  mutate(age_group = factor(age_group, levels = c("Under 5y", 
                                                  "5 to 14 years", 
                                                  "Over 15 years")))


write_rds(df_fit0_data, file = "2-field-data/output/human_glm_fit0_data.rds") #this has the data too

df_fit0_data <- readRDS("2-field-data/output/human_glm_fit0_data.rds")


#inspect plot of the model output and data together

#colour pal for age group
age_cols <-  c('#1b9e77','#d95f02','#7570b3') 

sleeping_glm_plot <- ggplot(df_fit0_data, aes(x = period, weight = mean_val, ymin = lower_val, ymax = upper_val))+
  geom_point(aes(x = period, y = mean_val, shape = "Mean proportion of people in bed (model-estimated)", 
                 col = as.factor(age_group)),
             position = position_dodge(width = 0.9), size = 6, stroke = 2)+
  geom_point(aes(x = period, y = propIn, shape = "Proportion of people in bed (survey data)", 
             col = as.factor(age_group)), position = position_dodge(width = 0.9), size = 3)+
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.3, linewidth = 1, 
                aes(col = as.factor(age_group)))+
  ylab("Proportion of people in bed")+
  xlab("Time period")+
  theme_bw()+
  scale_shape_manual(name ="", 
                     values = c(4,1))+
  scale_colour_manual(name = "Age group", values = age_cols)+
  guides(col = guide_legend(override.aes = list(linetype = 0, shape = 4)))+
  theme(legend.position = c(0.7, 0.2))
  
#model diagnostics to inspect convergence of chains in best-fitting model
#for supplementary material

posterior_0 <- as.array(fit_0)
names <- dimnames(posterior_0)
param <- names$parameters

#trace plot
color_scheme_set("brightblue")
trace_plot <- mcmc_trace(posterior_0, pars = param) 
ggsave(trace_plot, file = "2-field-data/plots/human_trace_plot.pdf")
#chains appear well-mixed

#rhats
#potential scale reduction statistics
#has chain converged? Compare the behaviour to another randomly initialised chain 
#the split Rhat measures ratio of the average variance of draws within each chain to variance of pooled draws across chains
#if all chains are at eq, these will be the same and Rhat will be one. 
#if chains have not converged to a common distrib, Rhat will be greater than 1
rhats <- rhat(fit_0)
print(rhats) #all ~ 1

rhat_plot <- mcmc_rhat(rhats) + yaxis_text(hjust = 1)
ggsave(rhat_plot, file = "2-field-data/plots/human_rhat_plot.pdf")

#Neff
#an estimate of the numbver of independent draws from the posterior distrib of the estimand of interst
#neff measured is based on the ability of draws to estimate the true mean of a parameter, which is related to estimating other functions of the draws
#draws within a chain are not independent if there is autocorrelation, so neff is usually smaller than the total sample size
#larger ratio of Neff to N, the better
ratios <- neff_ratio(fit_0)
neff_plot <- mcmc_neff(ratios, size = 2) + yaxis_text(hjust = 1)
ggsave(neff_plot, file = "2-field-data/plots/human_neff_plot.pdf")