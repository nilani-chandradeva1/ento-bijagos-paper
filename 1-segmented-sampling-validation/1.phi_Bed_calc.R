#estimating phi-B from ESS systematic review using segmented method (and verifying their phi-B hourly estimates)
#also estimating uncertainty for the hourly and segmented method
#the uncertainty estimates for both methods are estimated by taking the 20th and 80th percentile of prop people indoors and in bed
#we also estimate uncertainty around hourly method using min and max proportion of people indoors and in bed



#prepare dataset for plot in 3.figure_1.plot.R
#for the horizontal errorbars, pair the hourly ento data with the min and max prop people in bed, and 20th and 80th percentile
#for the vertical errorbars, pair the segmented mosquito data with the 20% and 80% percentile from the same segment

require(tidyverse)

#reading in data from ESS systematic review of matched mosquito and human data
df <- read.csv("1-segmented-sampling-validation/data_in/ESS-PNAS-review.csv", header = TRUE)

#checking step for ESS hourly estimates
check <- df %>%
  group_by(Ref_name) %>%#
  summarise(PHI_I_check = sum(numeratorPhiI)/sum(denominatorPhiI), 
            PHI_B_check = sum(numerator_phiB)/sum(denominator_PhiB), 
            PHI_I = PHI_I, 
            PHI_B) %>%
  select(Ref_name, PHI_B_check, PHI_B) %>%
  filter(!is.na(PHI_B))

#create the time segments: evening collections are 7pm-10pm, 10pm-7am and 7am to 7pm so mosquitoes will be collected in 19-21, 22-06 and 07-18h slots
df2 <- df %>%
  group_by(Ref_name, Hour) %>%
  mutate(segments = case_when(
    Hour %in% c(19, 20, 21) ~ "19_21", 
    Hour %in% c(22, 23, 24, 1, 2, 3, 4, 5, 6) ~ "22_06", 
    Hour %in% c(7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18) ~ "07_18"))

#read in the datasets for time people go to bed and indoors, from ALL studies in the ESS review
#generate uncertainty computing the min and max proportion of people in bed, at each hour, across all studies

df_bed_review <- read.csv("1-segmented-sampling-validation/data_in/human_in_bed_times_ESS.csv", header = TRUE)
studies <- names(df_bed_review[2:8])
bed_summary <- df_bed_review %>%
  rowwise() %>%
  mutate(prop_Bed_hourly_L = min(c_across(studies)), #min prop people in bed for a given hour across all studies
         prop_Bed_hourly_U = max(c_across(studies))) %>% #max prop people in bed for a given hour across all studies
  select(Hour, prop_Bed_hourly_L, prop_Bed_hourly_U)

df_in_review <- read.csv("1-segmented-sampling-validation/data_in/human_in_times_ESS.csv", header = TRUE)
studies_in <- names(df_in_review[2:23])
in_summary <- df_in_review %>%
  rowwise() %>%
  mutate(prop_In_hourly_L = min(c_across(studies_in)), #min prop people indoors for a given hour across all studies
         prop_In_hourly_U = max(c_across(studies_in))) %>% #max prop people indoors for a given hour across all studies
  select(Hour, prop_In_hourly_L, prop_In_hourly_U)


sleeping_summaries <- left_join(bed_summary, in_summary, by = "Hour")

df2 <- left_join(df2, sleeping_summaries)

df3_hourly <- df2 %>%
  group_by(Ref_name) %>%
  mutate(
    #uncertainty for hourly phi-B
    numeratorPhiB_L = prop_Bed_hourly_L*Inside_mosq,
    denominatorPhiB_L = ((1 - prop_In_hourly_L)*Outside_mosq) + (prop_In_hourly_L*Inside_mosq),
    numeratorPhiB_U = prop_Bed_hourly_U*Inside_mosq, 
    denominatorPhiB_U = ((1 - prop_In_hourly_U)*Outside_mosq) + (prop_In_hourly_U*Inside_mosq)) 
 # summarise(PhiB_L  = sum(numeratorPhiB_L)/sum(denominatorPhiB_L), 
 #           PhiB_U = sum(numeratorPhiB_U)/sum(denominatorPhiB_U), 
 #           PHI_B = sum(numerator_phiB)/sum(denominator_PhiB))
#

#great, all match
df3_hourly$PHI_B == check$PHI_B_check

df3_seg <- df2 %>%
  group_by(Ref_name, segments) %>%
  summarise(Inside_mosq_seg = sum(Inside_mosq), 
            Outside_mosq_seg = sum(Outside_mosq), 
            prop_In_seg = mean(Prop_In), ##mean
            prop_Bed_seg = mean(Prop_Bed),
            prop_Bed_seg_L = quantile(Prop_Bed, 0.2), #for uncertainty for segmented, take the 20 and 80% percentile of human activity for that segment. 
            prop_Bed_seg_U = quantile(Prop_Bed, 0.8),
            prop_In_seg_L = quantile(Prop_In, 0.2), 
            prop_In_seg_U = quantile(Prop_In, 0.8),
            
            #segmented phi-B
            numeratorPhiB_seg = prop_Bed_seg*Inside_mosq_seg, 
            denominator_PhiB_seg = ((1 - prop_In_seg)*Outside_mosq_seg) + (prop_In_seg*Inside_mosq_seg),
            numeratorPhiB_seg_L = prop_Bed_seg_L*Inside_mosq_seg, 
            denominator_PhiB_seg_L = ((1 - prop_In_seg_L)*Outside_mosq_seg) + (prop_In_seg_L*Inside_mosq_seg),
            numeratorPhiB_seg_U = prop_Bed_seg_U*Inside_mosq_seg, 
            denominator_PhiB_seg_U = ((1 - prop_In_seg_U)*Outside_mosq_seg) + (prop_In_seg_U*Inside_mosq_seg)
          
            
  )

##also generate uncertainty for hourly method by pairing hourly ento data with 20th and 80th percentiles of people in bed/indoors

df3_seg_uncertainty <- df3_seg %>%
  select(Ref_name, segments, prop_In_seg, prop_Bed_seg, prop_In_seg_L, prop_Bed_seg_L, 
         prop_In_seg_U, prop_Bed_seg_U)

df3_hourly_uncertainty <- left_join(df3_hourly, df3_seg_uncertainty) %>%
  mutate(numeratorPhiB_L2 = prop_Bed_seg_L*Inside_mosq,
         denominatorPhiB_L2 = ((1 - prop_In_seg_L)*Outside_mosq) + (prop_In_seg_L*Inside_mosq),
         numeratorPhiB_U2 = prop_Bed_seg_U*Inside_mosq, 
         denominatorPhiB_U2 = ((1 - prop_In_seg_U)*Outside_mosq) + (prop_In_seg_U*Inside_mosq)) %>%
   summarise(PhiB_L  = sum(numeratorPhiB_L)/sum(denominatorPhiB_L), 
             PhiB_U = sum(numeratorPhiB_U)/sum(denominatorPhiB_U), 
             
             PHI_B = sum(numerator_phiB)/sum(denominator_PhiB), 
             
             PhiB_L2 = sum(numeratorPhiB_L2)/sum(denominatorPhiB_L2), 
             PhiB_U2 = sum(numeratorPhiB_U2)/sum(denominatorPhiB_U2))

#get lower, upper and point estimates of phi-B from segmented method, for each study
df4_seg <- df3_seg %>%
  group_by(Ref_name) %>%
  summarise(
    PHI_B_seg = sum(numeratorPhiB_seg)/sum(denominator_PhiB_seg), 
    PHI_B_seg_L  = sum(numeratorPhiB_seg_L)/sum(denominator_PhiB_seg_L), 
    PHI_B_seg_U = sum(numeratorPhiB_seg_U)/sum(denominator_PhiB_seg_U),
    
  ) %>%
  ungroup()

df5 <- left_join(df3_hourly_uncertainty, df4_seg, by = "Ref_name")


#summarise into df that can be used for transmission modelling.
df6 <- df5 %>%
  group_by(Ref_name) %>%
  summarise(Ref_name = Ref_name,
  
            new_PHI_B_seg = median(c(PHI_B_seg, PHI_B_seg_L, PHI_B_seg_U)), 
            new_PHI_B_seg_L  = min(PHI_B_seg, PHI_B_seg_L, PHI_B_seg_U), 
            new_PHI_B_seg_U = max(PHI_B_seg, PHI_B_seg_L, PHI_B_seg_U),
            
            new_PHI_B = median(c(PHI_B, PhiB_L, PhiB_U)),
            
            new_PHI_B_L = min(PHI_B, PhiB_L, PhiB_U),
            new_PHI_B_U = max(PHI_B, PhiB_L, PhiB_U), 
            
            new_PHI_B_L2 = min(PHI_B, PhiB_L2, PhiB_U2), #uncertainty generated by same methodology as segmented approach (20th and 80th percentile)
            new_PHI_B_U2 = max(PHI_B, PhiB_L2, PhiB_U2)
  )


#residuals for y = x
fitted_phiB <- df6$new_PHI_B
residuals_phiB <- df6$new_PHI_B_seg-fitted_phiB
study_index <- which.max(abs(residuals_phiB)) #study 1, Cooke
df6$residuals_phiB <- abs(residuals_phiB) #these are the residuals for the linear model
write.csv(df6, file = "1-segmented-sampling-validation/output/phi-b-review-analysis.csv")
