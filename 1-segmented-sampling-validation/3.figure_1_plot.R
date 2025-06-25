#Figure 1 in paper

#A: systematic review hourly vs segmented plot linear regression
#B: systematic review hourly vs segmented plot cases averted

#panel A
#calc R2
require(tidyverse)
require(ggpattern) #NB: need package "fastmap" version >= 1.2.0. if does not install automatically, ensure it is installed.
require(scales)
#read in hourly and segmented phi-B estimates
phi_b_df <- read.csv("1-segmented-sampling-validation/output/phi-b-review-analysis.csv")
case_summary_rel <- read.csv("1-segmented-sampling-validation/output/case_summary_rel.csv")


#residuals for y = x between segmented and hourly method
fitted_phiB <- phi_b_df$new_PHI_B
residuals_phiB <- phi_b_df$new_PHI_B_seg-fitted_phiB #distance of each point from y = x line
study_index <- which.max(abs(residuals_phiB)) #study 1, Cooke, has the largest difference in hourly and segmented method
phi_b_df$residuals_phiB <- abs(residuals_phiB) #these are the residuals for the linear model

study_index_min <- which.min(abs(residuals_phiB)) #study 7 (smallest difference between methods)

#get bites_Bed (segmented estimates) for the study with biggest difference
study_ref_max_resid <- phi_b_df[study_index,]$Ref_name
phi_b_seg_max_resid <-  phi_b_df[study_index,]$new_PHI_B_seg #0.768
phi_b_hourly_max_resid <- phi_b_df[study_index,]$new_PHI_B #0.869

#get bites_Bed (segmented estimates) for the study with smallest difference difference
study_ref_min_resid <- phi_b_df[study_index_min,]$Ref_name #SeyoumC
phi_b_seg_min_resid <- phi_b_df[study_index_min,]$new_PHI_B_seg 
phi_b_hourly_min_resid <- phi_b_df[study_index_min,]$new_PHI_B 

#calculate overall R-squared from line y = x. Goodness of fit: how much of variation in y is explained by x. 
calc_Rsq <- function(segment, hourly){
  res <- segment - hourly #for assumption that there is a linear relationship
  res_sq <- res^2
  SS_res <- sum(res_sq) #sum of the squares of the residuals
  
  SS_tot <- sum((segment-mean(segment))^2) #total sum of squares
  R_sq <- 1 - (SS_res/SS_tot)
  
  return(R_sq)
}


r_sq <- calc_Rsq(phi_b_df$new_PHI_B_seg, phi_b_df$new_PHI_B)
lab_phi_B <- paste("R² =", round(r_sq, 2))

study_pals <- hue_pal()(8)

phi_b <- ggplot(phi_b_df, aes(x = new_PHI_B, y = new_PHI_B_seg, col = as.factor(Ref_name)))+
  xlim(0, 1)+
  ylim(0, 1)+
  #xlab("Phi-B with hourly ento data")+
  #ylab("Phi_B with segmented ento data")+
  geom_abline(slope = 1, intercept = 0, col = "red", linetype =  "dashed")+
  #geom_smooth(method = "lm", col = "blue") +
  geom_text(aes(x = 0.1, y = 0.7, label = lab_phi_B), color = "red")+
  geom_point(size = 4)+
  geom_errorbar(aes(ymin = new_PHI_B_seg_L, ymax = new_PHI_B_seg_U), width = 0.02, linewidth = 1.2)+
  geom_errorbarh(aes(xmin = new_PHI_B_L, xmax = new_PHI_B_U), height  = 0.02)+
  geom_errorbarh(aes(xmin = new_PHI_B_L2, xmax = new_PHI_B_U2), linewidth = 1.2, height = 0.02)+
  labs(colour = "Study", 
       x = expression(phi[Bed] ~ "with hourly mosquito data"), 
       y = expression(phi[Bed] ~ "with segmented mosquito data"))+
  theme_bw()+
  theme(legend.position = c(0.55, 0.18), 
        legend.direction = "vertical",
        text=element_text(size=14),
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12), 
        plot.title=element_text(size=12),
        legend.text=element_text(size=12), 
        legend.title=element_text(size=12),
        axis.ticks.length = unit(5, "pt"))

cases_averted_percent <- ggplot(case_summary_rel,aes(x = reorder(Ref_name, residuals_phiB), y = point, pattern = as.factor(segment), 
                                                          fill = as.factor(Ref_name)))+
  #geom_point(position = position_dodge(width = 0.5))+
  geom_bar_pattern(stat = "identity", position = "dodge", width = 0.5, col = "black", size = 1, 
                   pattern_fill = "grey", pattern_spacing = 0.1)+
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5),
                width = 0.2, col = "black", size = 1.2)+
  ylab("Relative clinical cases averted (%) \n in children under 5-years-old due to ITNs")+
  theme_bw()+
  xlab("Study from Sherrard-Smith et al. (2019) review")+
  labs(fill = "Study", pattern = "Mosquito sampling method")+
  scale_pattern_manual(values = c("hourly" = "none", "segment" = "stripe"))+
  
  guides(fill = "none")+
  scale_fill_manual(values = study_pals)+
  theme(legend.position = c(0.5, 0.9),
        legend.direction = "vertical",
        text=element_text(size=14), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12), 
        plot.title=element_text(size=12), 
        legend.text=element_text(size=12), 
        legend.title=element_text(size=12),
        axis.ticks.length = unit(5, "pt"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylim(0, 100)+
  guides(
    pattern = guide_legend(
      override.aes = list(
        fill = "white",     # Change background color of the legend box here
        color = "grey", 
        pattern = c("none", "stripe"), 
        pattern_spacing = 0.04
      )
    )
  )


fig_1 <- cowplot::plot_grid(phi_b, cases_averted_percent, labels = c("A", "B"))

#summary stats: relative difference in clinical incidence between hourly and segmented method, for each study

rel_diff_studies <- case_summary_rel %>%
  select(segment, Ref_name, point) %>%
  pivot_wider(names_from = segment, values_from = point) %>%
  mutate(rel_diff = ((hourly-segment)/hourly)*100)

mean(rel_diff_studies$rel_diff)
range(rel_diff_studies$rel_diff)

ggsave(fig_1, file = "1-segmented-sampling-validation/plots/fig_1.pdf")

#for presentations - everything in bigger font (if needed)

phi_b_pres <- ggplot(phi_b_df, aes(x = new_PHI_B, y = new_PHI_B_seg, col = as.factor(Ref_name)))+
  xlim(0, 1)+
  ylim(0, 1)+
  #xlab("Phi-B with hourly ento data")+
  #ylab("Phi_B with segmented ento data")+
  geom_abline(slope = 1, intercept = 0, col = "red", linetype =  "dashed")+
  #geom_smooth(method = "lm", col = "blue") +
  geom_text(aes(x = 0.1, y = 0.7, label = lab_phi_B), color = "red")+
  geom_point(size = 4)+
  geom_errorbar(aes(ymin = new_PHI_B_seg_L, ymax = new_PHI_B_seg_U), width = 0.02, linewidth = 1.2)+
  geom_errorbarh(aes(xmin = new_PHI_B_L, xmax = new_PHI_B_U), height  = 0.02)+
  geom_errorbarh(aes(xmin = new_PHI_B_L2, xmax = new_PHI_B_U2), linewidth = 1.2, height = 0.02)+
  labs(colour = "Study", 
       x = expression(phi[Bed] ~ "with hourly mosquito data"), 
       y = expression(phi[Bed] ~ "with segmented mosquito data"))+
  theme_bw()+
  theme(legend.position = c(0.55, 0.18), 
        legend.direction = "vertical",
        text=element_text(size=14), #change font size of all text
        axis.text=element_text(size=16), #change font size of axis text
        axis.title=element_text(size=16), #change font size of axis titles
        plot.title=element_text(size=16), #change font size of plot title
        legend.text=element_text(size=16), #change font size of legend text
        legend.title=element_text(size=16),
        axis.ticks.length = unit(5, "pt"))

cases_averted_percent_pres <- ggplot(case_summary_rel,aes(x = reorder(Ref_name, residuals_phiB), y = point, pattern = as.factor(segment), 
                                                     fill = as.factor(Ref_name)))+
  #geom_point(position = position_dodge(width = 0.5))+
  geom_bar_pattern(stat = "identity", position = "dodge", width = 0.5, col = "black", size = 1, 
                   pattern_fill = "grey", pattern_spacing = 0.1)+
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5),
                width = 0.2, col = "black", size = 1.2)+
  ylab("Relative clinical cases averted (%) \n in children under 5-years-old due to ITNs")+
  theme_bw()+
  xlab("Study from Sherrard-Smith et al. (2019) review")+
  labs(fill = "Study", pattern = "Mosquito sampling method")+
  scale_pattern_manual(values = c("hourly" = "none", "segment" = "stripe"))+
  
  guides(fill = "none")+
  scale_fill_manual(values = study_pals)+
  theme(legend.position = c(0.5, 0.9),
        legend.direction = "vertical",
        text=element_text(size=14), #change font size of all text
        axis.text=element_text(size=16), #change font size of axis text
        axis.title=element_text(size=16), #change font size of axis titles
        plot.title=element_text(size=16), #change font size of plot title
        legend.text=element_text(size=16), #change font size of legend text
        legend.title=element_text(size=16),
        axis.ticks.length = unit(5, "pt"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylim(0, 100)+
  guides(
    pattern = guide_legend(
      override.aes = list(
        fill = "white",     # Change background color of the legend box here
        color = "grey", 
        pattern = c("none", "stripe"), 
        pattern_spacing = 0.04
      )
    )
  )

fig_1_pres <- cowplot::plot_grid(phi_b_pres, cases_averted_percent_pres, labels = c("A", "B"))

ggsave(fig_1_pres, file = "1-segmented-sampling-validation/plots/fig_1_pres.png")

ggsave(fig_1_pres, file = "1-segmented-sampling-validation/plots/fig_1_pres_poster.svg")
