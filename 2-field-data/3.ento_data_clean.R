#cleaning the field ento data
require(tidyverse)
require(lubridate)
#setwd("C:/Users/nc1115/Documents/github/ento-bijagos")

ento_df <- read.csv("2-field-data/data_in/MATAMAL_Ento_Sample_summary_23.csv")
ento_df$cluster
head(ento_df)
names(ento_df)
#filter to just the essential data 
unique(ento_df$SubmitterName)
ento_df_mal <- ento_df %>%
  select(date_of_collection, cluster, village, house, position, on, off, AnophelesF)

#class of the data and time vars
class(ento_df_mal$date_of_collection)
class(ento_df_mal$on)
class(ento_df_mal$off)

unique(ento_df_mal$cluster)

#remove the 000Z from times in R

time_string_on <- ento_df_mal$on
time_string_off <- ento_df_mal$off

ento_df_mal <- ento_df_mal %>%
  mutate(on = gsub("\\.000Z", "", time_string_on), 
         off = gsub("\\.000Z", "", time_string_off), 
         date_of_collection = as.Date(dmy(date_of_collection)))

#village codes
#Bub_bidjana = Bubaque, Bidjana
#Can_amnequem = Canogo, Amnequem 
#Can_indina = Canhabque, Indena
#Oranzinho_acanho = Orangozinho, Ancanho
#OrGr_Eticoga = Orange Grande, Eticoga

#for each day, split into the time segments, based on times (19-22h, 22-07h and 07-19h).

#indoor segments:
#19-22
#22-07
#based on little outdoor daytime biting we assume assume no daytime biting


#out1 segments:
#18-19, 19-20, 20-21, 21-22, 22-01, 01-04, 04-07, 07-08, 08-09, 09-10

#out2 segments:
#19-22, 22-01, 01-04, 04-07, 07-10

#round the ON times (start time of the trap)

time_on <- as.POSIXct(ento_df_mal$on, format = "%H:%M:%S")

rounded_time_on <- as.POSIXct(round(as.numeric(time_on) / 3600) * 3600, origin = "1970-01-01")

# Format to display only time
formatted_time_on <- format(rounded_time_on, "%H:%M:%S")

#round the off times (end time for the trap)
time_off <- as.POSIXct(ento_df_mal$off, format = "%H:%M:%S")

rounded_time_off <- as.POSIXct(round(as.numeric(time_off) / 3600) * 3600, origin = "1970-01-01")

# Format to display only time
formatted_time_off <- format(rounded_time_off, "%H:%M:%S")

#add the formatted times onto df
ento_df_mal <- ento_df_mal %>%
  mutate(on_round = formatted_time_on, 
         off_round = formatted_time_off)

#correcting some more data entry errors
ento_df_mal2 <-  ento_df_mal %>%
  mutate(on_round = case_when(village == "Can_indina" & on_round == "09:00:00" & off_round == "07:00:00" ~ "07:00:00", #swapping on and off times
                              village == "Orazinho_ancanho" & on_round == "16:00:00" & off_round == "19:00:00" ~ "04:00:00", #correcting AM and PM times
                              village == "Can_indina" & on_round == "13:00:00" ~ "01:00:00", #correcting AM and PM times
                              TRUE ~ on_round), 
         off_round = case_when(village == "Can_indina"  & on_round == "07:00:00" & off_round == "07:00:00" ~ "09:00:00", #swapping on and off times
                               village == "Orazinho_ancanho" & on_round == "04:00:00" & off_round == "19:00:00" ~ "07:00:00", #correcting AM and PM times
                               TRUE ~ off_round), 
         note = case_when(village == "Can_indina" & on_round == "19:00:00" & off_round == "19:00:00" ~ "rm", #rows where same on time and off time erroneously put in. 
                          village == "Can_indina" & on_round == "22:00:00" & off_round == "22:00:00" ~ "rm", #the correct data is there
                          TRUE ~ "keep"))
nrow(ento_df_mal2) #290

unique(ento_df_mal2$house)
table(ento_df_mal2$house, ento_df_mal2$cluster) #canhabaque has a house 6. House 0-4 in Bubaque are CDC-LT, house 5-9 are yeast

table(ento_df_mal2$cluster, ento_df_mal2$village)

unique(ento_df_mal2$cluster)

ento_df_mal3 <- ento_df_mal2 %>%
  filter(note == "keep") %>%
  mutate(cluster = case_when(village == "Bub_bidjana" ~ "bubaque", 
                             TRUE ~ cluster)) %>%
  filter(position != "farmland") %>%
  filter(!(cluster == "bubaque" & house %in% c(6,7,8,9))) #remove yeast traps (only in Bidjana)

table(ento_df_mal3$house, ento_df_mal3$cluster) #NB canhabaque has a house 6
table(ento_df_mal3$village, ento_df_mal3$cluster) #now all correctly labelled
unique(ento_df_mal3$cluster)

nrow(ento_df_mal3)#these leaves 218
sum(ento_df_mal3$AnophelesF) #352 mosquitoes
unique(ento_df_mal3$village)
ento_df_mal3 %>%
  filter(cluster == "canogo") #house 6 inside should be house 5. 

ento_df_mal3 <- ento_df_mal3 %>%
  mutate(house = case_when(cluster == "canogo" & house == 6 ~ 5, 
                           TRUE ~ house))

table(ento_df_mal3$house, ento_df_mal3$cluster) #NB canhabaque has a house 6

 bubaque <- ento_df_mal3 %>%
  filter(cluster == "bubaque")
 
 unique(bubaque$house)

#17 time categories 
ento_df_mal4 <- ento_df_mal3 %>%
  mutate(segment = case_when(position == "inside" & on_round == "22:00:00" & off_round == "07:00:00" ~ "22_07",
                             position == "inside" & on_round == "22:00:00" & off_round == "08:00:00" ~ "22_08", #put in the nighttime collection segment
                             position == "inside" & on_round == "19:00:00" & off_round == "22:00:00" ~ "19_22", 
                             position == "outside" & on_round == "19:00:00" & off_round == "22:00:00" ~ "19_22", #the evening chunks (hourly collections not possible)
                             position == "outside" & on_round == "07:00:00" & off_round == "10:00:00" ~ "07_10", #morning chunks (hourly collections not possible)
                             position == "outside" & on_round == "18:00:00" & off_round == "19:00:00" ~ "18_19", 
                             position == "outside" & on_round == "19:00:00" & off_round == "20:00:00" ~ "19_20", 
                             position == "outside" & on_round == "20:00:00" & off_round == "21:00:00" ~ "20_21", 
                             position == "outside" & on_round == "21:00:00" & off_round == "22:00:00" ~ "21_22", 
                             position == "outside" & on_round == "22:00:00" & off_round == "01:00:00" ~ "22_01", 
                             position == "outside" & on_round == "01:00:00" & off_round == "04:00:00" ~ "01_04", 
                             position == "outside" & on_round == "04:00:00" & off_round == "07:00:00" ~ "04_07", 
                             position == "outside" & on_round == "07:00:00" & off_round == "08:00:00" ~ "07_08", 
                             position == "outside" & on_round == "08:00:00" & off_round == "09:00:00" ~ "08_09",
                             position == "outside" & on_round == "09:00:00" & off_round == "10:00:00" ~ "09_10", 
                             position == "outside" & on_round == "22:00:00" & off_round == "01:00:00" ~ "22_01", 
                             position == "outside" & on_round == "01:00:00" & off_round == "04:00:00" ~ "01_04", 
                             position == "outside" & on_round == "04:00:00" & off_round == "07:00:00" ~ "04_07",
                             position == "outside" & on_round == "17:00:00" & off_round == "19:00:00" ~ "17_19", 
                             position == "inside" & on_round == "07:00:00" & off_round == "22:00:00" ~ "day_07_22",
                             position == "inside" & on_round == "19:00:00" & off_round == "07:00:00" ~ "storm_tide_19_07",
                             position == "inside" & on_round == "19:00:00" & off_round == "21:00:00" ~ "19_21",
                             position == "inside" & on_round == "20:00:00" & off_round == "22:00:00" ~ "20_22",
                             position == "outside" & on_round == "07:00:00" & off_round == "09:00:00" ~ "07_09",
                             position == "outside" & on_round == "21:00:00" & off_round == "23:00:00" ~ "21_23",
                             position == "outside" & on_round == "23:00:00" & off_round == "01:00:00" ~ "23_01",
                             position == "outside" & on_round == "20:00:00" & off_round == "22:00:00" ~ "20_22",
                             TRUE ~ NA_character_),
         segment2 = case_when(position == "inside" & on_round == "22:00:00" & off_round == "07:00:00" ~ "22_07",
                              position == "inside" & on_round == "22:00:00" & off_round == "08:00:00" ~ "22_07", #put in the nighttime collection segment
                              position == "inside" & on_round == "19:00:00" & off_round == "22:00:00" ~ "19_22", 
                              position == "outside" & on_round == "19:00:00" & off_round == "22:00:00" ~ "19_22", #the evening chunks (hourly collections not possible)
                              position == "outside" & on_round == "07:00:00" & off_round == "10:00:00" ~ "07_10", #morning chunks (hourly collections not possible)
                              position == "outside" & on_round == "18:00:00" & off_round == "19:00:00" ~ "18_19", 
                              position == "outside" & on_round == "19:00:00" & off_round == "20:00:00" ~ "19_20", 
                              position == "outside" & on_round == "20:00:00" & off_round == "21:00:00" ~ "20_21", 
                              position == "outside" & on_round == "21:00:00" & off_round == "22:00:00" ~ "21_22", 
                              position == "outside" & on_round == "22:00:00" & off_round == "01:00:00" ~ "22_01", 
                              position == "outside" & on_round == "01:00:00" & off_round == "04:00:00" ~ "01_04", 
                              position == "outside" & on_round == "04:00:00" & off_round == "07:00:00" ~ "04_07", 
                              position == "outside" & on_round == "07:00:00" & off_round == "08:00:00" ~ "07_08", 
                              position == "outside" & on_round == "08:00:00" & off_round == "09:00:00" ~ "08_09",
                              position == "outside" & on_round == "09:00:00" & off_round == "10:00:00" ~ "09_10", 
                              position == "outside" & on_round == "22:00:00" & off_round == "01:00:00" ~ "22_01", 
                              position == "outside" & on_round == "01:00:00" & off_round == "04:00:00" ~ "01_04", 
                              position == "outside" & on_round == "04:00:00" & off_round == "07:00:00" ~ "04_07",
                              position == "outside" & on_round == "17:00:00" & off_round == "19:00:00" ~ "17_19", 
                              position == "inside" & on_round == "07:00:00" & off_round == "22:00:00" ~ "day_07_22",
                              position == "inside" & on_round == "19:00:00" & off_round == "07:00:00" ~ "storm_tide_19_07",
                              position == "inside" & on_round == "19:00:00" & off_round == "21:00:00" ~ "19_22",
                              position == "inside" & on_round == "20:00:00" & off_round == "22:00:00" ~ "19_22",
                              position == "outside" & on_round == "07:00:00" & off_round == "09:00:00" ~ "07_10",
                              position == "outside" & on_round == "21:00:00" & off_round == "23:00:00" ~ "21_23_restricted_out", #this is quite a small segment of the outdoor times
                              position == "outside" & on_round == "23:00:00" & off_round == "01:00:00" ~ "23_01_restricted_out",
                              position == "outside" & on_round == "20:00:00" & off_round == "22:00:00" ~ "19_22",
                              TRUE ~ NA_character_)) 
ento_df_mal4 <- ento_df_mal4[order(ento_df_mal4$house, ento_df_mal4$date_of_collection, ento_df_mal4$position, ento_df_mal4$on_round),]

ento_df_mal4 %>%
  filter(is.na(segment)) #remove this entry - unable to assign to segment, likely due to a data entry error. 

ento_df_mal4 <- ento_df_mal4 %>%
  filter(!is.na(segment))

ento_df_mal4 <- ento_df_mal4 %>%
  filter(segment2 != "day_07_22") #removing the daytime collection from the dataset - don't know if this biting spanned into the night

sum(ento_df_mal4$AnophelesF) #347 mosquitoes

#break down by village

village_labs <- unique(ento_df_mal4$village)

bubaque_ento <- ento_df_mal4 %>%
  filter(village == village_labs[1])
unique(bubaque_ento$house) #5 houses

canhabaque_ento <- ento_df_mal4 %>%
  filter(village == village_labs[2])
unique(canhabaque_ento$house) #5 houses

orangozinho_ento <- ento_df_mal4 %>%
  filter(village == village_labs[3])
unique(orangozinho_ento$house) #5 houses

canogo_ento <- ento_df_mal4 %>%
  filter(village == village_labs[4])
unique(canogo_ento$house) #5 houses

orango_grande_ento <- ento_df_mal4 %>%
  filter(village == village_labs[5])
unique(orango_grande_ento$house) #5 houses

out_house_bubaque <- bubaque_ento %>%
  filter(position == "outside") #some data entry errors, outdoor traps only in house 1 and 2
unique(out_house_bubaque$house)

bubaque_ento <- bubaque_ento %>%
  select(date_of_collection, house, position, AnophelesF, segment2) %>%
  rename(location = position) %>%
  distinct()  %>% #removes any duplicate entries
  filter(!(location == "outside" & house %in% c(4, 3))) #these are data entry errors

out_house_bubaque <- bubaque_ento %>%
  filter(location == "outside")
unique(out_house_bubaque$house)


bubaque_ento <- bubaque_ento[order(bubaque_ento$date_of_collection),]

bubaque_segments <- bubaque_ento %>%
  mutate(period = case_when(segment2 %in% c("22_01", "01_04", "04_07","22_07") ~ "night",
                            segment2 %in% c("07_10","17_19", "18_19") ~ "day", 
                            segment2 %in% c("19_22") ~ "evening"))

#bubaque_segments <- bubaque_segments %>%
#  group_by(house, date_of_collection, period, segment2) %>%
#  summarise(tot_out = sum(outside, na.rm = TRUE), 
#            tot_in = sum(inside, na.rm = TRUE))

sum(bubaque_segments$AnophelesF) #30

bubaque_segments %>%
  group_by(location) %>%
  summarise(num_mosq = sum(AnophelesF))

canhabaque_ento <- canhabaque_ento %>%
  select(date_of_collection, house, position, AnophelesF, segment2) %>%
  rename(location = position)

unique(canhabaque_ento$date_of_collection)
unique(canhabaque_ento$house)

#canhabaque_ento_wide <- canhabaque_ento %>%
#  pivot_wider(names_from = position, values_from = tot_AnF)

out_canha <- canhabaque_ento %>%
  filter(location == "outside") 

unique(out_canha$house) #2 houses with outdoor collection

canhabaque_ento <- canhabaque_ento[order(canhabaque_ento$date_of_collection),]
#for canhabaque 19th Nov there was a storm which affected trapping, apply some imputation (indoor traps), using data from the 20th 

canhabaque_dates <- unique(canhabaque_ento$date_of_collection)

imputation <- canhabaque_ento%>%
  filter(date_of_collection == canhabaque_dates[2] & segment2 %in% c("19_22", "22_07")) %>%
  group_by(segment2, location) %>%
  summarise(tot = sum(AnophelesF)) %>%
  ungroup() %>%
  group_by(location) %>%
  summarise(prop = tot/sum(tot))

imp_eve <- round(imputation$prop[1],3)
imp_night <- round(imputation$prop[2],3)


canhabaque_ento <- as.data.frame(canhabaque_ento) %>%
  add_row(segment2 = "19_22", date_of_collection =as.Date("2023-11-19"), house = 2, location = "inside", AnophelesF = 17*imp_eve) %>% #imputing the night of storm from night of 20th
  add_row(segment2 = "22_07", date_of_collection =as.Date("2023-11-19"), house = 2, location = "inside", AnophelesF = 17*imp_night) %>% #imputing the night of storm from night of 20th
  add_row(segment2 = "19_22", date_of_collection =as.Date("2023-11-19"), house = 4, location = "inside", AnophelesF = 23*imp_eve) %>% #imputing the night of storm from night of 20th
  add_row(segment2 = "22_07", date_of_collection =as.Date("2023-11-19"), house = 4, location = "inside", AnophelesF = 23*imp_night) %>% #imputing the night of storm from night of 20th
  add_row(segment2 = "22_07", date_of_collection =as.Date("2023-11-21"), house = 1, location = "inside", AnophelesF = (1/imp_eve) - 1) %>% #imputing nighttime biting
  add_row(segment2 = "22_07", date_of_collection =as.Date("2023-11-21"), house = 2, location = "inside", AnophelesF = (0/imp_eve) - 0) %>%#imputing nighttime biting
  add_row(segment2 = "22_07", date_of_collection =as.Date("2023-11-21"), house = 3, location = "inside", AnophelesF = (4/imp_eve) - 4) %>%#imputing nighttime biting
  add_row(segment2 = "22_07", date_of_collection =as.Date("2023-11-21"), house = 4, location = "inside", AnophelesF = (0/imp_eve) - 0) #imputing nighttime biting

canhabaque_ento<- canhabaque_ento %>%
  filter(segment2 != "storm_tide_19_07")%>%#remove the original segment
  mutate(AnophelesF = round(AnophelesF))

canhabaque_ento %>%
  filter(segment2 == "07_10") #1 mosquito caught in day


canhabaque_segments <- canhabaque_ento %>%
  group_by(house, date_of_collection) %>%
  mutate(period = case_when(segment2 %in% c("22_01", "01_04", "04_07","22_07") ~ "night",
                            segment2 %in% c("07_10","17_19", "18_19", "07_08", "08_09", "09_10") ~ "day", 
                            segment2 %in% c("19_22", "19_20", "20_21", "21_22") ~ "evening"))

sum(canhabaque_segments$AnophelesF) #178 mosquitoes
canhabaque_segments %>%
  group_by(location) %>%
  summarise(tot = sum(AnophelesF))

#canhabaque_segments <- canhabaque_segments %>%
#  group_by(house, date_of_collection, period, segment2) %>%
#  summarise(tot_out = sum(outside, na.rm = TRUE), 
#            tot_in = sum(inside, na.rm = TRUE))
#
#sum(canhabaque_segments$tot_in, canhabaque_segments$tot_out) #178

orangoz_ento <- orangozinho_ento %>%
  select(date_of_collection, house, position, AnophelesF, segment2) %>%
  rename(location = position)

#orangoz_ento_wide <- orangoz_ento %>%
#  pivot_wider(names_from = position, values_from = tot_AnF)

orangoz_ento<- orangoz_ento[order(orangoz_ento$date_of_collection),]

orangoz_segments <- orangoz_ento %>%
  mutate(period = case_when(segment2 %in% c("22_01", "01_04", "04_07","22_07") ~ "night",
                            segment2 %in% c("07_10","17_19", "18_19", "07_08", "08_09", "09_10") ~ "day", 
                            segment2 %in% c("19_22", "19_20", "20_21", "21_23_restricted_out", "21_22") ~ "evening"))

sum(orangoz_segments$AnophelesF)#124
orangoz_segments %>%
  group_by(location) %>%
  summarise(tot = sum(AnophelesF))
#orangoz_segments <- orangoz_segments %>%
#  group_by(house, date_of_collection, period, segment2) %>%
#  summarise(tot_out = sum(outside, na.rm = TRUE), 
#            tot_in = sum(inside, na.rm = TRUE))

orangogr_ento <- orango_grande_ento %>%
  select(date_of_collection, house, position, AnophelesF, segment2) %>%
  rename(location = position)

#orangogr_ento_wide <- orangogr_ento %>%
#  pivot_wider(names_from = position, values_from = tot_AnF)

orangogr_ento<- orangogr_ento[order(orangogr_ento$date_of_collection),]

orangogr_segments <- orangogr_ento %>%
  group_by(house, date_of_collection) %>%
  mutate(period = case_when(segment2 %in% c("22_01", "01_04", "04_07","22_07") ~ "night",
                            segment2 %in% c("07_10","17_19", "18_19", "07_08", "08_09", "09_10") ~ "day", 
                            segment2 %in% c("19_22", "19_20", "20_21", "21_23_restricted_out", "21_22") ~ "evening"))

sum(orangogr_segments$AnophelesF) #3

orangogr_segments %>%
  group_by(location) %>%
  summarise(tot = sum(AnophelesF))

#orangogr_segments <- orangogr_segments %>%
 # group_by(house, date_of_collection, period, segment2) %>%
  #summarise(tot_out = sum(outside, na.rm = TRUE), 
   #         tot_in = sum(inside, na.rm = TRUE))

canogo_ento <- canogo_ento %>%
  select(date_of_collection, house, position, AnophelesF, segment2) %>%
  rename(location = position)

#canogo_ento_wide <- canogo_ento %>%
 # pivot_wider(names_from = position, values_from = tot_AnF)

canogo_ento<- canogo_ento[order(canogo_ento$date_of_collection),]

canogo_segments <- canogo_ento%>%
  mutate(period = case_when(segment2 %in% c("22_01", "01_04", "04_07","22_07") ~ "night",
                            segment2 %in% c("07_10","17_19", "18_19", "07_08", "08_09", "09_10") ~ "day", 
                            segment2 %in% c("19_22", "19_20", "20_21", "21_22", "21_22", "23_01_restricted_out") ~ "evening"))
sum(canogo_segments$AnophelesF) #16
canogo_segments %>%
  group_by(location) %>%
  summarise(tot = sum(AnophelesF))

#canogo_segments <- canogo_segments %>%
 # group_by(house, date_of_collection, period, segment2) %>%
  #summarise(tot_out = sum(outside, na.rm = TRUE), 
   #         tot_in = sum(inside, na.rm = TRUE))

#then rbind all the files together
bubaque_segments <- bubaque_segments %>%
  mutate(island = "Bubaque")
canhabaque_segments <- canhabaque_segments %>%
  mutate(island = "Canhabaque")
orangogr_segments <- orangogr_segments %>%
  mutate(island = "Orange Grande")
orangoz_segments <- orangoz_segments %>%
  mutate(island = "Orangozinho")
canogo_segments <- canogo_segments %>%
  mutate(island = "Canogo")

list_all <- list(bubaque_segments, canhabaque_segments, canogo_segments, orangogr_segments, orangoz_segments)

df_all <- do.call("rbind", list_all)  
#this is the cleaned dataset
#df_all <- df_all %>%
 # rowwise() %>%
  #mutate(tot_AnF =sum(tot_in, tot_out, na.rm = TRUE))

dur_vec <- unique(df_all$segment2)

df_all <- df_all %>%
  mutate(duration = case_when(segment2 %in% c(dur_vec[1], dur_vec[3], dur_vec[4], dur_vec[5], 
                                              dur_vec[8]) ~ "3", 
                              segment2 %in% c(dur_vec[6],dur_vec[9], dur_vec[10], dur_vec[11], 
                                              dur_vec[12], dur_vec[13], dur_vec[14]) ~ "1", 
                              segment2 %in% c(dur_vec[2]) ~ "9", 
                              segment2 %in% c(dur_vec[7], dur_vec[15], dur_vec[16]) ~ "2"))
tot_num_mosq <- sum(df_all$AnophelesF) #351 mosquitoes

df_all %>%
  group_by(island, location) %>%
  summarise(total_mosq_caught_here = sum(AnophelesF), 
           percent_mosq_caught_here = (total_mosq_caught_here/tot_num_mosq)*100) #% caught in each location and island


write.csv(df_all, file = "2-field-data/output/ento_all_cleaned_stats.csv", row.names = FALSE) #this is the cleaned dataset for stan modelling

#also prepare outdoor trapping data for time plots. 
df_out <- df_all %>%
  filter(location == "outside")

reassign_house <- function(data){
  unique_houses <- unique(data$house)  # Detect unique houses for each island
  # Create a mapping to alternate between "house 1" and "house 2"
  house_map <- setNames(rep(c("Station 1", "Station 2"), length.out = length(unique_houses)), unique_houses)
  # Apply the mapping to the original data
  data$house_ID <- house_map[as.character(data$house)]
  return(data)
}

df_out2 <- df_out %>%
  group_by(island) %>%
  group_modify(~ reassign_house(.x)) %>%
  ungroup()


df_out3 <- df_out2 %>%
  mutate(period = case_when(segment2 %in% c("22_01", "01_04", "04_07","22_07",  
                                            "23_01_restricted_out") ~ "Night",
                            segment2 %in% c("07_10","17_19", "18_19", 
                                            "07_08", "08_09", "09_10") ~ "Day", 
                            segment2 %in% c("19_22", "19_20", "20_21", "21_23_restricted_out", "21_22") ~ "Evening", 
                            TRUE ~ NA_character_))

df_out3 %>%
  filter(period == "night" & island == "Canhabaque")


basic_time_plot <- df_out %>%
  group_by(island, segment2) %>%
  summarise(mean_mosq = mean(AnophelesF), 
            min_mosq = min(AnophelesF), 
            max_mosq = max(AnophelesF))

basic_time_plot %>%
  filter(island == "Canhabaque")

island_pals <- c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854')

basic_time_plot %>%
  ggplot(aes(x = segment2, y = mean_mosq, fill = as.factor(island))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = min_mosq, ymax = max_mosq), 
                position = position_dodge(width = 0.9), 
                width = 0.2) +
  theme_bw()+
  ylim(0, 30)

background_data <- df_out3 %>%
  group_by(period) %>%
  summarise(xmin = min(segment2), xmax = max(segment2)) %>%
  filter(period != "Night") %>%
  add_row(period = "Night", xmin = "22_01", xmax = "04_07")

#plot of outdoor biting

df_out3
basic_time_plot$segment2

time_order <- c("19_20","19_22" ,"20_21", "21_22", "21_23_restricted_out", 
                "22_01", "23_01_restricted_out", "01_04", "04_07", 
                "07_08", "07_10", "08_09", "09_10", "17_19", "18_19")
times2 <- c("19:00-20:00","19:00-22:00" ,"20:00-21:00", "21:00-22:00", "21:00-23:00", 
            "22:00-01:00", "23:00-01:00", "01:00-04:00", "04:00-07:00", 
            "07:00-08:00", "07:00-10:00", "08:00-09:00", "09:00-10:00", "17:00-19:00", "18:00-19:00")
length(time_order)
length(times2)
#may modify this colour scheme later
island_pals <- c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854')
time_pals <- c('#ffffb3','#80b1d3','#8dd3c7')


basic_time_plot %>%
  filter(segment2 == "07_10")

write_rds(basic_time_plot, file  = "2-field-data/output/out_trapping_data.rds")

time_plots <- ggplot(basic_time_plot, aes(x = factor(segment2, level = time_order), y = mean_mosq)) +
  # Background color for different time periods
  geom_rect(data = background_data, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period), 
            alpha = 0.3, inherit.aes = FALSE) +  
  # Bars for mosquito counts by island
  geom_bar(aes(fill = as.factor(island)), col = "black", stat = "identity", 
           position = position_dodge(width = 0.9), width = 0.9) +
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
        legend.position = c(0.8, 0.6), 
        legend.direction = "vertical", 
        text=element_text(size=14), 
        axis.text=element_text(size=14), 
        axis.title=element_text(size=14), 
        plot.title=element_text(size=14), 
        legend.text=element_text(size=14), 
        legend.title=element_text(size=14),
        axis.ticks.length = unit(5, "pt")) +
  ylim(0,30)
