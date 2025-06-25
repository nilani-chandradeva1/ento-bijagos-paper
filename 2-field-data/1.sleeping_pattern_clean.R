#cleaning the sleeping pattern data collected in surveys

require(tidyverse)
require(hms)

#read in the anonymised sleeping pattern data
sleeping_dat <- read.csv("2-field-data/data_in/MATAMAL_Ento_obs_anonymised.csv")

#data cleaning
sleeping_dat2 <- sleeping_dat %>%
  mutate(time_in = as_hms(sub("Z", "", time_in)), 
         time_out = as_hms(sub("Z", "", time_out))) %>%
  select(cluster, age, time_in, time_out, notes)

#assign age groups 
sleeping_dat3 <- sleeping_dat2 %>%
  mutate(age_group = case_when(age >= 0 & age <= 4 ~ "0_4", 
                               age >= 5 & age <= 14 ~ "5_14", 
                               age >= 15 ~ "15_plus"))

# proportion of people in bed in each of these times
as_hms(range(sleeping_dat3$time_in)) #can see ranges from 07:51:00 to 23:00:00
as_hms(range(sleeping_dat3$time_out)) #can see ranges from 05:59:00 to 22:00:00

bubaque <- sleeping_dat %>%
  filter(cluster == "bubaque")

range(bubaque$age) #0to60

canhabaque <- sleeping_dat %>%
  filter(cluster == "canhabaque")
range(canhabaque$age) #1to84
 
canogo <- sleeping_dat %>%
  filter(cluster == "canogo")
range(canogo$age) #2to75

orangogrande <- sleeping_dat %>%
  filter(cluster == "orangogrande")
range(orangogrande$age) #6to75

orangozinho <- sleeping_dat %>%
  filter(cluster == "orangozinho")
range(orangozinho$age) #0to79

#there is a person in bubaque who enters house at 08:20 and leaves at 10:30. Unclear if this is a data entry error (wrong time in and time out) or issue with 24h clock
#going to remove row 88 and 89 from dataset as times don't make sense and likely data entry errors which cannot be resolved

sleeping_dat4 <-sleeping_dat3[-c(88,89), ]

#then, add on 12 hour for the time ins that are less than 20:00. This corrects some data entry errors were times were entered as AM instead of PM

sleeping_dat5 <- sleeping_dat4 %>%
  mutate(time_in_clean = case_when(
    time_in < as_hms("20:00:00") ~ as_hms(as.numeric(time_in) + 12 * 3600),
    TRUE ~ time_in),  # Keep original if the condition isn't met
    time_out_clean = time_out)

#remove row 8 and 61 - same entry/exit time in both time_in and time_out columns 
sleeping_dat6 <- sleeping_dat5[-c(8, 61),]
#and remove row 55, got time in and time out as 22h
sleeping_dat6 <- sleeping_dat6[-55,] #this is the final, cleaned sleeping dataset
#write.csv(sleeping_dat6, file = "2-field-data/sleeping_data.csv")

#number of people in each age group in the final dataset
sleeping_dat6 %>%
  group_by(age_group) %>%
  summarise( 
            num_peple = n())

                                                  
#assumptions: some people entered house before 8pm or leave after 7am and are not recorded by observer

#if leave after 7am, can make assumption that left SOON after 7am. daytime biting risk is small so hardly matters

#if enter house before 8:30pm, can assume that they entered within the evening segment (say entered at 8pm)

#if entered after 10pm, matters more for biting risk. Going to assume they entered the house by midnight. 
notes_times <- unique(sleeping_dat6$notes)
after_22h <- c(notes_times[6], notes_times[11], notes_times[12], notes_times[19])

after_07h <- c(notes_times[2], notes_times[7], notes_times[8], notes_times[9], notes_times[10], 
               notes_times[13], notes_times[14])

sleeping_dat7 <- sleeping_dat6 %>%
  mutate(time_in_clean2 = case_when(notes %in% after_22h ~ as_hms("23:59:59"), #not midnight as this breaks code
                                     TRUE ~ time_in_clean), 
         time_out_clean2 = case_when(notes %in% after_07h ~ as_hms("07:01:00"), 
                                     TRUE ~ time_out_clean))

#create a vector of times in 30 minute increments from 20:30 to 07:00
times <- seq(from = as.POSIXct("20:30", format = "%H:%M"),
             to = as.POSIXct("07:30", format = "%H:%M") + 86400,  # Add 1 day to handle overnight
             by = "30 min")

# Format as "HHMM_HHMM"
time_ID <- paste0("in_",format(head(times, -1), "%H%M"))
time_ID2 <- paste0(format(head(times, -1), "%H:%M:%S"))


sleeping_dat8 <- sleeping_dat7 %>%
  mutate(in_2030 = case_when(time_in_clean2 >= as_hms("19:00:00") & time_in_clean2 <= as_hms(time_ID2[1]) ~ 1, #in between 19h and 2030
                                TRUE ~ 0), 
         in_2100 = case_when(time_in_clean2  >= as_hms("19:00:00") & time_in_clean2 <= as_hms(time_ID2[2]) ~ 1, #in between 19h and 2100
                                TRUE ~ 0), 
         in_2130 = case_when(time_in_clean2 >= as_hms("19:00:00") & time_in_clean2 <= as_hms(time_ID2[3]) ~ 1, #in between 1900 and 2130
                                TRUE ~ 0), 
         in_2200 = case_when(time_in_clean2 >= as_hms("19:00:00") & time_in_clean2 <= as_hms(time_ID2[4]) ~  1, #in between 1900 and 2200
                             TRUE ~ 0), 
         in_0530 = case_when(time_in_clean2 >= as_hms("19:00:00") & time_in_clean2 <= as_hms("23:59:59") ~ 1, #if they entered house at all
                             TRUE ~ 0),
         in_0600 = case_when(time_out_clean2 >= as_hms(time_ID2[19]) & time_out_clean2 <= as_hms(time_ID2[20]) ~ 0, #in between 0530 and 0600
                                TRUE ~ 1), 
         in_0630 = case_when(time_out_clean2 >= as_hms(time_ID2[19]) & time_out_clean2 <= as_hms(time_ID2[21]) ~ 0, #in between 0530 and 0630
                                 TRUE ~ 1), 
         in_0700 = case_when(time_out_clean2 >= as_hms(time_ID2[19]) & time_out_clean2 <= as_hms(time_ID2[22]) ~ 0, #in between 0530 and 0700
                                 TRUE ~ 1), 
         in_0730 = case_when(time_out_clean2 >= as_hms(time_ID2[19])  & time_out_clean2 <= as_hms("07:30:00") ~ 0, #in between 0530 and 0730 (left after 0700)
                             TRUE ~ 1))
head(sleeping_dat8)

sleeping_dat8 %>%
  sample_n(10)

sleeping_dat9 <- sleeping_dat8 %>%
  group_by(age_group, cluster) %>%
  summarise(in_by_2030 = sum(in_2030), #these are the number of people per age group and cluster in by this time
            in_by_2100 = sum(in_2100), 
            in_by_2130 = sum(in_2130), 
            in_by_2200 = sum(in_2200), 
            in_by_0530 = sum(in_0530),
            in_by_0600 = sum(in_0600), 
            in_by_0630 = sum(in_0630), 
            in_by_0700 = sum(in_0700), 
            in_by_0730 = sum(in_0730)) %>%
  rowwise() %>%
  mutate(num_age_group_cluster = max(c_across(in_by_2030:in_by_0730)))

sleeping_dat10 <- sleeping_dat9 %>%
  pivot_longer(!c(age_group, cluster, num_age_group_cluster), names_to = "times", values_to = "num_in")

age_cols <-  c('#1b9e77','#d95f02','#7570b3')

#quick inspection of sleeping pattern data
ggplot(sleeping_dat10, aes(x = times, y = num_in/num_age_group_cluster, col = age_group, group = age_group))+
  geom_line(size = 2, alpha = 0.6)+
  facet_wrap(vars(cluster))+
  scale_color_manual(values = age_cols)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ylab("Total number of people indoors per age group")


#then need to put this into the time segments: 7- 10pm, 10pm to 7am, 7am to 7pm to prepare for the glmm in script 2.sleep_glmm.R

df11 <- sleeping_dat10 %>%
  group_by(age_group, cluster) %>%
  filter(times %in% c("in_by_2200", "in_by_0530", "in_by_0730")) %>% #retain evening, night and day data
  mutate(times = case_when(times == "in_by_2200" ~ "evening", 
                           times == "in_by_0530" ~ "night", 
                           times == "in_by_0730" ~ "day")) %>%
  rename(nIn = num_in, 
         nTot = num_age_group_cluster, 
         segment = times)

write_rds(df11, file = "2-field-data/output/sleeping_data_for_glmm.rds")
