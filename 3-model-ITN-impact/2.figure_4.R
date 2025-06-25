require(tidyverse)

#A: prevalence in under 5s for each scenario
#B: cases averted for each scenario

prev_dynamics <- read.csv("3-model-ITN-impact/output/incidence_bijagos.csv")
case_summary_rel <- read.csv("3-model-ITN-impact/output/case_summary_rel_bijagos_scenario.csv")

#ITN distribution is every 3y from y5.5. So 5.5y, 8.5 and 11.5y

#60%, 80%, no int, obs proportion
sleeping_pals <- c('#bebada','#fb8072','#80b1d3','#fdb462')
time_labs <- c("-1", "0", "1", "2", "3")
breaks <- c(10.5, 11.5, 12.5, 13.5, 14.5)


phi_dynamics <- prev_dynamics %>%
  filter(between(t, 365*10.5, 365*14.5)) %>% #11.5y to 14.5y is what we want to show
  ggplot()+
  aes(x = t/365, y = prev*100, col = sleeping_scenario)+
  geom_line(size = 2)+
  theme_bw()+
  scale_colour_manual(values = c("No interventions" = '#80b1d3' , 
                                 "60% population in bed at night" = '#bebada',
                                 "80% population in bed at night" = '#fb8072' , 
                                 "Observed data (100%) on population in bed at night" = '#fdb462'
                                 ), 
                      name = "Scenario", 
                      breaks = c("No interventions", 
                                 "60% population in bed at night",
                                 "80% population in bed at night", 
                                 "Observed data (100%) on population in bed at night"), 
                      labels = c("No interventions", 
                                 "60% of observed proportion of people in bed at night", 
                                 "80% of observed proportion of people in bed at night", 
                                 "Observed proportion of people in bed at night"))+
  ylab("Slide prevalence (%) in children under 5-years-old")+
  scale_x_continuous(labels = time_labs, name = "Time (years) since the ITN distribution \n used to measure epidemiological impact",
                     breaks = breaks)+
  theme(legend.position = c(0.45, 0.87),
        legend.direction = "vertical",
        text=element_text(size=12), #change font size of all text
        axis.text=element_text(size=12), #change font size of axis text
        axis.title=element_text(size=12), #change font size of axis titles
        plot.title=element_text(size=12), #change font size of plot title
        legend.text=element_text(size=12), #change font size of legend text
        legend.title=element_text(size=12),
        axis.ticks.length = unit(5, "pt"))+
  scale_y_continuous(limits = c(0, 85), n.breaks = 17)+
  geom_vline(aes(xintercept = 11.5), linetype = "dashed", colour = "black", size = 1)+
  geom_vline(aes(xintercept = 14.5), linetype = "dashed", colour = "black", size = 1)


sleeping_pals2 <- c(sleeping_pals[1], sleeping_pals[2], sleeping_pals[4])
cases_averted_percent <- ggplot(case_summary_rel,aes(x = sleeping_scenario, y = mean,fill = as.factor(sleeping_scenario)))+
  #geom_point(position = position_dodge(width = 0.5),
  #           shape = 4,
  #           size = 5)+
  geom_bar(stat = "identity", position = "dodge", width = 0.5)+
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5),
                width = 0.2, size = 1)+
  scale_fill_manual(values = sleeping_pals2, name = "Sleeping pattern scenario")+
  ylab("Cases averted (%) in children \n under 5-years-old due to ITNs \n compared to baseline scenario of no intervention")+
  theme_bw()+
  xlab("Net type")+
  scale_x_discrete(labels = c("60%", "80%", "Observed"), 
                   name = "Scenario of observed proportion of people in bed at night")+
  guides(fill = "none")+
  ylim(0, 100)+
  theme(legend.position = c(0.35, 0.8),
        legend.direction = "vertical",
        text=element_text(size=12), #change font size of all text
        axis.text=element_text(size=12), #change font size of axis text
        axis.title=element_text(size=12), #change font size of axis titles
        plot.title=element_text(size=12), #change font size of plot title
        legend.text=element_text(size=12), #change font size of legend text
        legend.title=element_text(size=12),
        axis.ticks.length = unit(5, "pt"),
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
        )

fig_4 <- cowplot::plot_grid(phi_dynamics, cases_averted_percent,
                            labels = c("A", "B"))

ggsave(fig_4, file = "3-model-ITN-impact/plots/fig_4.pdf")


((case_summary_rel[3,5] - case_summary_rel[2,5])/case_summary_rel[3,5])*100 #17.76% diff  between obs and 80%
((case_summary_rel[3,5] - case_summary_rel[1,5])/case_summary_rel[3,5])*100 #35.33% diff between obs and 60% scenario


#for presentation: bigger fonts
phi_dynamics_pres <- prev_dynamics %>%
  filter(between(t, 365*10.5, 365*14.5)) %>% #11.5 to 14.5 is what we want to show
  ggplot()+
  aes(x = t/365, y = prev*100, col = sleeping_scenario)+
  geom_line(size = 2)+
  theme_bw()+
  scale_colour_manual(values = c("No interventions" = '#80b1d3' , 
                                 "60% population in bed at night" = '#bebada',
                                 "80% population in bed at night" = '#fb8072' , 
                                 "Observed data (100%) on population in bed at night" = '#fdb462'
  ), 
  name = "Scenario", 
  breaks = c("No interventions", 
             "60% population in bed at night",
             "80% population in bed at night", 
             "Observed data (100%) on population in bed at night"), 
  labels = c("No interventions", 
             "60% of observed proportion of people in bed at night", 
             "80% of observed proportion of people in bed at night", 
             "Observed proportion of people in bed at night"))+
  ylab("Slide prevalence (%) in children under 5-years-old")+
  scale_x_continuous(labels = time_labs, name = "Time (years) since the ITN distribution \n used to measure epidemiological impact",
                     breaks = breaks)
  xlab("Time (years) since the last ITN distribution")+
  theme(legend.position = c(0.4, 0.8),
        legend.direction = "vertical",
        text=element_text(size=18), #change font size of all text
        axis.text=element_text(size=18), #change font size of axis text
        axis.title=element_text(size=18), #change font size of axis titles
        plot.title=element_text(size=18), #change font size of plot title
        legend.text=element_text(size=18), #change font size of legend text
        legend.title=element_text(size=18),
        axis.ticks.length = unit(5, "pt"))+
    scale_y_continuous(limits = c(0, 85), n.breaks = 17)+
    geom_vline(aes(xintercept = 11.5), linetype = "dashed", colour = "black", size = 1)+
    geom_vline(aes(xintercept = 14.5), linetype = "dashed", colour = "black", size = 1)
  

sleeping_pals2 <- c(sleeping_pals[1], sleeping_pals[2], sleeping_pals[4])

cases_averted_percent_pres <- ggplot(case_summary_rel,aes(x = sleeping_scenario, y = mean,fill = as.factor(sleeping_scenario)))+
  geom_bar(stat = "identity", position = "dodge", width = 0.5)+
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5),
                width = 0.2, size = 1)+
  scale_fill_manual(values = sleeping_pals2, name = "Sleeping pattern scenario")+
  ylab("Cases averted (%) in children under 5-years-old due to ITNs \n compared to baseline scenario of no intervention")+
  theme_bw()+
  xlab("Net type")+
  scale_x_discrete(labels = c("60%", "80%", "Observed"), 
                   name = "Scenario of observed proportion of people in bed at night")+
  guides(fill = "none")+
  ylim(0, 100)+
  theme(legend.position = c(0.3, 0.8),
        legend.direction = "vertical",
        text=element_text(size=18), #change font size of all text
        axis.text=element_text(size=18), #change font size of axis text
        axis.title=element_text(size=18), #change font size of axis titles
        plot.title=element_text(size=18), #change font size of plot title
        legend.text=element_text(size=18), #change font size of legend text
        legend.title=element_text(size=18),
        axis.ticks.length = unit(5, "pt"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

