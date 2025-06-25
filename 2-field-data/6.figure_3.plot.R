#plotting field data 

#panel A: ento stan model output and data
#panel B: ento outdoor trapping data
#panel C: human sleeping pattern stan model output and data

#panel A

ento_glm <- readRDS("2-field-data/output/ento_glm_dat.rds")

pals <- c('#984ea3','#ff7f00') #colour pal for sampling location


#sqrt y axis but label with actual values so easier to visualise
neg_binom_model_plot <- ggplot(ento_glm, aes(x = period, weight = sqrt(mean_vals), ymin = sqrt(lower_vals), 
                                                ymax = sqrt(upper_vals)))+
  geom_point(aes(x = period, y = sqrt(mean_vals), shape = "Mean number of mosquitoes (model-estimated)",
                 col = as.factor(location)),position = position_dodge(width = 0.9), size = 6, stroke = 2)+
  geom_point(aes(x = period, y = sqrt(Count/duration), shape = "Number of mosquitoes (survey data)",
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
  scale_y_continuous(labels = c("0", "1", "4", "9", "16"), 
                     name = "Number of mosquitoes caught per trap per hour", 
                     limits = c(0, 4))+
  theme(legend.position = c(0.53, 0.85), 
        
        legend.background = element_rect(fill=NA), 
        text=element_text(size=12), #change font size of all text
        axis.text=element_text(size=12), #change font size of axis text
        axis.title=element_text(size=12), #change font size of axis titles
        plot.title=element_text(size=12), #change font size of plot title
        legend.text=element_text(size=12), #change font size of legend text
        legend.title=element_text(size=12),
        axis.ticks.length = unit(5, "pt"))



#panel B - outdoor trapping data

basic_time_plot <- readRDS(file  = "2-field-data/output/out_trapping_data.rds")

time_order <- c("19_20","19_22" ,"20_21", "21_22", "21_23_restricted_out", 
                "22_01", "23_01_restricted_out", "01_04", "04_07", 
                "07_08", "07_10", "08_09", "09_10", "17_19", "18_19")
#give proper time labels
times2 <- c("19:00-20:00","19:00-22:00" ,"20:00-21:00", "21:00-22:00", "21:00-23:00", 
            "22:00-01:00", "23:00-01:00", "01:00-04:00", "04:00-07:00", 
            "07:00-08:00", "07:00-10:00", "08:00-09:00", "09:00-10:00", "17:00-19:00", "18:00-19:00")

island_pals <- c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854')
time_pals <- c('#ffffb3','#80b1d3','#8dd3c7')

background_data <- basic_time_plot %>%
  mutate(period = case_when(segment2 %in% c("22_01", "01_04", "04_07","22_07",  
                                            "23_01_restricted_out") ~ "night",
                            segment2 %in% c("07_10","17_19", "18_19", 
                                            "07_08", "08_09", "09_10") ~ "day", 
                            segment2 %in% c("19_22", "19_20", "20_21", "21_23_restricted_out", "21_22") ~ "evening", 
                            TRUE ~ NA_character_)) %>%
  group_by(period) %>%
  summarise(xmin = min(segment2), xmax = max(segment2)) 

time_plots <- ggplot(basic_time_plot, aes(x = factor(segment2, level = time_order), y = mean_mosq)) +
  # Background color for different time periods
  geom_rect(data = background_data, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period), 
            alpha = 0.3, inherit.aes = FALSE) +  
  # Bars for mosquito counts by island
  geom_bar(aes(fill = as.factor(island)), col = "black", stat = "identity", 
           position = position_dodge(width = 0.9), width = 0.9) +
  #show min and mosquito count (each night)
  geom_errorbar(aes(ymin = min_mosq, ymax = max_mosq, col = as.factor(island)), 
                position = position_dodge(width = 0.9), 
                width = 2) +
  theme_bw() +
  scale_x_discrete(labels = times2) +
  # Combine time_pals and island_pals into one fill scale
  scale_fill_manual(values = c(island_pals, time_pals), name = "Sampling island or period") +  
  scale_color_manual(values = island_pals, name = "Island", guide = "none") +
  xlab("Trapping collection period (h)") +
  ylab(expression(atop("Mean number of  " * italic("Anopheles") * " mosquitoes", 
                       "caught in outdoor trapping stations"))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        legend.position = c(0.8, 0.8), 
        legend.direction = "vertical", 
        text=element_text(size=12), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12), 
        plot.title=element_text(size=12), 
        legend.text=element_text(size=12), 
        legend.title=element_text(size=12),
        axis.ticks.length = unit(5, "pt")) +
  ylim(0,30)


#panel C
human_glm <- readRDS("2-field-data/output/human_glm_fit0_data.rds")
age_cols <-  c('#1b9e77','#d95f02','#7570b3')

sleeping_glm_plot <- ggplot(human_glm, aes(x = period, weight = mean_val, ymin = lower_val, ymax = upper_val))+
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
  theme(legend.position = c(0.52, 0.5), 
        legend.direction = "vertical", 
        text=element_text(size=12), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12), 
        plot.title=element_text(size=12), 
        legend.text=element_text(size=12), 
        legend.title=element_text(size=12),
        axis.ticks.length = unit(5, "pt"))

fig_3_plot <- cowplot::plot_grid(neg_binom_model_plot, time_plots, sleeping_glm_plot, 
                   ncol = 3, labels = c("A", "B", "C"))
ggsave(fig_3_plot, file = "2-field-data/plots/fig_3.pdf")
