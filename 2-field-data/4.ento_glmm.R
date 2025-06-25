#negative binomial glm and glmers to estimate number of mosquitoes caught per trap per hour, in different sampling locations and time periods
#use offset to account for fact that in different sampling locations and time periods, traps are run for different time durations, which could affect number of mosquitoes caught
#i.e. could collect more mosquitoes in some time periods/locations because of duration trap run 

require(tidyverse)
require(rstanarm)
require(bayesplot)
#setwd("C:/Users/nc1115/Documents/github/ento-bijagos")
df_stats <- read.csv("2-field-data/output/ento_all_cleaned_stats.csv")

hist(df_stats$AnophelesF)
mean(df_stats$AnophelesF) #1.64
var(df_stats$AnophelesF) #36.09

#very overdispersed

df_stats %>%
  filter(period == "day")

#assign numbers to fixed effects

df_stats_long <- df_stats %>%
  mutate(location_fac = case_when(location == "outside" ~ 0, 
                                  location == "inside" ~ 1), 
         period_fac = case_when(period == "day" ~ 0, 
                                period == "evening" ~ 1, 
                                period == "night" ~ 2)) %>%
  rename(Count = AnophelesF)

BAYES_SEED <- 1234

#explore different model compositions

fit_4 <- stan_glm(Count ~ as.factor(location_fac) + as.factor(period_fac), 
                  offset = log(duration), 
                  data = df_stats_long, 
                  family = neg_binomial_2(), 
                  seed = BAYES_SEED
                  )

fit_3 <- stan_glmer(Count ~ as.factor(location_fac) + as.factor(period_fac) 
                  + (1|island), 
                  offset = log(duration), 
                  data = df_stats_long, 
                  family = neg_binomial_2(), 
                  seed = BAYES_SEED
)

fit_2 <- stan_glm(Count ~ as.factor(location_fac) + as.factor(period_fac) + 
                    as.factor(location_fac):as.factor(period_fac), 
                  offset = log(duration), 
                  data = df_stats_long, 
                  family = neg_binomial_2(), 
                  seed = BAYES_SEED)

fit_1 <- stan_glmer(Count ~ as.factor(location_fac) + as.factor(period_fac) +
                    as.factor(location_fac):as.factor(period_fac) + (1|island), 
                  offset = log(duration), 
                  data = df_stats_long, 
                  family = neg_binomial_2(), 
                  seed = BAYES_SEED)

#leave-one-out cross-validation. 

loo4 <- loo(fit_4, k_threshold = 0.7)
loo3 <- loo(fit_3, k_threshold = 0.7)
loo2 <- loo(fit_2, k_threshold = 0.7)
loo1 <- loo(fit_1, k_threshold = 0.7)

loo_4_1 <- loo::loo_compare(loo4, loo1)
loo_3_1 <- loo::loo_compare(loo3, loo1)
loo_2_1 <- loo::loo_compare(loo2, loo1)

#fit_1 is the best-fitting model 

model1 <- data.frame(fit_1)
head(names(model1))
# Extract reciprocal dispersion to check whether negative binomial was appropriate distribution to use
recip_phi <- model1$reciprocal_dispersion

# Convert to phi
phi <- 1 / recip_phi
summary(phi)
quantile(phi, c(0.025, 0.5, 0.975)) #phi is small, so high overdispersion. Report this


#extract posterior distribution for best-fitting model
store <- rep(0,4000)

day_out_exp <- rep(0,4000)
day_in_exp <- rep(0,4000)

eve_in_exp <- rep(0,4000)
eve_out_exp <- rep(0,4000)

night_out_exp <- rep(0,4000)
night_in_exp <- rep(0,4000)

for(i in 1:4000){
  store[i] <- model1$X.Intercept.[i] + 
    model1$as.factor.location_fac.1[i]
  
  
  #do this one
  day_out_exp[i] <- exp(model1$X.Intercept.[i])
  day_in_exp[i] <- exp(model1$X.Intercept.[i] + 
                         model1$as.factor.location_fac.1[i])
  
  eve_out_exp[i] <- exp(model1$X.Intercept.[i] + 
                          model1$as.factor.period_fac.1[i])
  eve_in_exp[i] <- exp(model1$X.Intercept.[i] + 
                         model1$as.factor.period_fac.1[i] + 
                         model1$as.factor.location_fac.1[i] +
                         model1$as.factor.location_fac.1.as.factor.period_fac.1[i])
  
  night_out_exp[i]  <- exp(model1$X.Intercept.[i] + 
                             model1$as.factor.period_fac.2[i])
  
  night_in_exp[i] <- exp(model1$X.Intercept.[i] + 
                           model1$as.factor.location_fac.1[i]+
                           model1$as.factor.period_fac.2[i]
                         )  
  
}

colnames(model1)

distr_vals <- c(day_out_exp, day_in_exp, 
                eve_out_exp, eve_in_exp, 
                night_out_exp, night_in_exp)

df_params <- data.frame(day_out_exp, day_in_exp, 
                        eve_out_exp, eve_in_exp, 
                        night_out_exp, night_in_exp)

write_rds(df_params, file = "2-field-data/output/ento_model_fit1.rds") #save output of posterior distr

df_params <- readRDS("2-field-data/output/ento_model_fit1.rds")

parameter <- c("Day (out)", "Day (in)", "Evening (out)", "Evening (in)", "Night (out)", "Night (in)")

mean_vals <- c(mean(df_params$day_out_exp), mean(df_params$day_in_exp), mean(df_params$eve_out_exp), mean(df_params$eve_in_exp), 
               mean(df_params$night_out_exp), mean(df_params$night_in_exp))


lower_vals <- c(quantile(df_params$day_out_exp, 0.025)[[1]], quantile(df_params$day_in_exp, 0.025)[[1]], 
           quantile(df_params$eve_out_exp, 0.025)[[1]], quantile(df_params$eve_in_exp, 0.025)[[1]], 
           quantile(df_params$night_out_exp, 0.025)[[1]], quantile(df_params$night_in_exp, 0.025)[[1]])



upper_vals <- c(quantile(df_params$day_out_exp, 0.975)[[1]], quantile(df_params$day_in_exp, 0.975)[[1]], 
                quantile(df_params$eve_out_exp, 0.975)[[1]], quantile(df_params$eve_in_exp, 0.975)[[1]], 
                quantile(df_params$night_out_exp, 0.975)[[1]], quantile(df_params$night_in_exp, 0.975)[[1]])

mean(df_params$night_in_exp) #0.375
quantile(df_params$night_in_exp, 0.025)[[1]] #0.07536
quantile(df_params$night_in_exp, 0.975)[[1]] #1.21976

mean(df_params$night_out_exp) #0.564
quantile(df_params$night_out_exp, 0.025)[[1]] #0.1129..
quantile(df_params$night_out_exp, 0.975)[[1]] #1.8187..

df_fit1 <- data.frame(parameter, mean_vals, lower_vals, upper_vals)


df_fit1 <- df_fit1 %>%
  mutate(location = case_when(grepl("out", parameter) ~ "Outside", 
                              grepl("in", parameter) ~ "Inside"), 
         period = case_when(grepl("Day", parameter) ~ "Day", 
                            grepl("Evening", parameter) ~ "Evening", 
                            grepl("Night", parameter) ~ "Night"))

df_stats_long %>%
  filter(period == "day")

dat_summary <- df_stats_long %>%
  mutate(location = case_when(location == "inside" ~ "Inside", 
                              location == "outside" ~ "Outside"), 
         period = case_when(period == "day" ~ "Day", 
                            period == "evening" ~ "Evening", 
                            period == "night" ~ "Night"))

dat_summary %>%
  filter(period == "Day")

df_fit1_dat <- left_join(dat_summary, df_fit1, by = c("period", "location"))

#save df with stan model output and the data

write_rds(df_fit1_dat, file = "2-field-data/output/ento_glm_dat.rds")

pals <- c('#984ea3','#ff7f00')



#sqrt y axis but label with actual values so easier to visualise
neg_binom_model_plot <- ggplot(df_fit1_dat, aes(x = period, weight = sqrt(mean_vals), ymin = sqrt(lower_vals), 
                                                 ymax = sqrt(upper_vals)))+
  #geom_bar(position = position_dodge(width = 0.9), aes(y = mean_val,
  #     fill = as.factor(location)), 
  #        stat = "identity", alpha = 0.008)+
  
  geom_point(aes(x = period, y = sqrt(mean_vals), shape = "Mean number of mosquitoes in each time period, per sampling location (model-estimated)",
                 col = as.factor(location)),position = position_dodge(width = 0.9), size = 6, stroke = 2)+
  geom_point(aes(x = period, y = sqrt(Count/duration), shape = "Number of mosquitoes caught per trap (survey data)",
                 col = as.factor(location)),
             position = position_dodge(width = 0.9), size = 3)+
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.1, linewidth = 1, aes(col = as.factor(location)))+
  theme_bw()+
  scale_shape_manual(name ="", 
                     values = c(4,1))+
  scale_color_manual(name = "Trapping station location",values = pals)+
  #scale_fill_manual(name = "Mean number of mosquitoes (model) caught in trapping station", values = pals)+
  guides(col = guide_legend(override.aes = list(linetype = 0, shape = 4)))+
  xlab("Time period")+
  #labs(col = "Trapping station location")+
  scale_linetype_manual(values = "solid", label = "95% CrI (model)",name = "")+
  scale_y_continuous(labels = c("0", "1", "4", "9", "16", "25"), 
                     name = "Number of mosquitoes caught per trap per hour", 
                     limits = c(0, 5))+
  theme(legend.position = c(0.4, 0.8), 
        
        legend.background = element_rect(fill=NA), 
        text=element_text(size=14), #change font size of all text
        axis.text=element_text(size=14), #change font size of axis text
        axis.title=element_text(size=14), #change font size of axis titles
        plot.title=element_text(size=14), #change font size of plot title
        legend.text=element_text(size=14), #change font size of legend text
        legend.title=element_text(size=14),
        axis.ticks.length = unit(5, "pt"))


#model diagnositics to check convergence of chains

posterior_1 <- as.array(fit_1)
names <- dimnames(posterior_1)
param <- names$parameters

#trace plot
color_scheme_set("brightblue")
trace_plot <- mcmc_trace(posterior_1, pars = param) 
ggsave(trace_plot, file = "2-field-data/plots/ento_trace.pdf")
#chains appear well-mixed

#rhats
#potential scale reduction statistics
#has chain converged? Compare the behaviour to another randomly initialised chain 
#the split Rhat measures ratio of the average variance of draws within each chain to variance of pooled draws across chains
#if all chains are at eq, these will be the same and Rhat will be one. 
#if chains have not converged to a common distrib, Rhat will be greater than 1
rhats <- rhat(fit_1)
print(rhats) #all ~ 1

rhat_plot <- mcmc_rhat(rhats) + yaxis_text(hjust = 1)
ggsave(rhat_plot, file = "2-field-data/plots/ento_rhat_plot.pdf")

#Neff
#an estimate of the numbver of independent draws from the posterior distrib of the estimand of interst
#neff measured is based on the ability of draws to estimate the true mean of a parameter, which is related to estimating other functions of the draws
#draws within a chain are not independent if there is autocorrelation, so neff is usually smaller than the total sample size
#larger ratio of Neff to N, the better
ratios <- neff_ratio(fit_1)
neff_plot <- mcmc_neff(ratios, size = 2) + yaxis_text(hjust = 1)
ggsave(neff_plot, file = "2-field-data/plots/ento_neff_plot.pdf")

