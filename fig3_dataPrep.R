rm(list=ls())

library(ggplot2)
library(ppcor)
library(mgcv)
library(visreg)
library(scatterplot3d)
library(metR)
library(imager)
library(tidyverse)
library(lmerTest)
library(lavaan)
library(car)
library(data.table)

########
# MURI #
########

setwd('/Users/dalezhou/Box/contemplativeControlCode/daleManuscriptCodeReplication/')

ema_df = read.csv('data/SHINE_EMA_May19_2020.csv', header=T, stringsAsFactors=F)
networkStat = as.data.frame(read.csv("/Users/dalezhou/Desktop/sharedFilesCostFlexibility/networkStats100.txt", header=T, sep=' '))
aveControl = as.data.frame(read.csv("results/aveControl_individual_400.txt", header=F, sep=','))
colnames(aveControl) <- c('pID', 'aveCont')

df1 = read.csv("data/master_baseline_and_prescan_040120.csv", header=T, stringsAsFactors=F)

QA_df <- as.data.frame(df1)
QA_df <- merge(QA_df, networkStat, by="pID")
QA_df <- merge(QA_df, aveControl, by='pID')

QA_df$college_year <- as.factor(QA_df$college_year)
QA_df$gender_numeric <- as.factor(QA_df$gender_numeric)

###########
EMA = read.csv("data/SHINE_EMA_May19_2020.csv", stringsAsFactors = FALSE) %>%
  mutate(shineid = tolower(SHINEID)) %>% #make ids lowercase 
  filter(!shineid == "muri035") #remove 1ppt with no completion due to technical app difficulties 

baseline =  read.csv("data/master_baseline_and_prescan_040120.csv", stringsAsFactors = FALSE) %>%
  select(pID, group_att_alc_num, amount_self, age, race, college_year, rung_group, freq_self, drunk_percent, drunk_freq, gender_numeric, groupID, group_type, income_num, educ_mother_years, alc_att_happy) %>%
  mutate(shineid = pID,
         freq_self_text = car::recode(freq_self, "'-1'= 'I never drank any alcohol in my life ';
                                      '0' = 'I did not drink alcohol in the last 6 months, but I did drink in the past';
                                      '1.5'='1-2 times in the past 6 months';
                                      '7' ='3-5 times in the past 6 months';
                                      '12' = 'Once a month ';
                                      '30' = '2-3 times a month';
                                      '52' = 'Once a week' ;
                                      '104'= 'Twice a week ';
                                      '182' = '3-4 times a week ';
                                      '286'  = '5-6 times a week ';
                                      '365'= 'Every day'"),
         freq_self_ord = factor(freq_self_text, ordered = TRUE, levels = c('I never drank any alcohol in my life ',
                                                                           'I did not drink alcohol in the last 6 months, but I did drink in the past',
                                                                           '1-2 times in the past 6 months',
                                                                           '3-5 times in the past 6 months',
                                                                           'Once a month ',
                                                                           '2-3 times a month',
                                                                           'Once a week', 
                                                                           'Twice a week ', 
                                                                           '3-4 times a week ', 
                                                                           '5-6 times a week ',
                                                                           'Every day')),
         amount_self_text = car::recode(amount_self, "'0'= '0 drinks ';
                                        '1' = '1 drink'; 
                                        '2' ='2 drinks'; 
                                        '3.5' = '3-4 drinks';
                                        '5.5' = '5-6 drinks'; 
                                        '7.5' = '7-8 drinks' ;
                                        '10'= '9-11 drinks ';
                                        '13.5' = '12-15 drinks';
                                        '17' = '16-18 drinks';
                                        '21.5' = '19-24 drinks';
                                        '25'= '25 or more drinks'"),
         amount_self_ord = factor(amount_self_text, ordered = TRUE, levels = c('0 drinks', 
                                                                               '1 drink', 
                                                                               '2 drinks',
                                                                               '3-4 drinks',
                                                                               '5-6 drinks',
                                                                               '7-8 drinks', 
                                                                               '9-11 drinks', 
                                                                               '12-15 drinks ', 
                                                                               '16-18 drinks ', 
                                                                               '19-24 drinks ',
                                                                               '25 or more drinks'))) %>%
  filter(shineid %in% unique(EMA$SHINEID)) %>%
  mutate(amount_self_c = scale(amount_self, center = TRUE, scale = TRUE),
         freq_self_c = scale(freq_self, center = TRUE, scale = TRUE),
         age = ifelse(is.na(age), mean(age, na.rm = TRUE), age), #impute mean age for those missing
         age_c = scale(age, scale = FALSE))

# tidy data
EMA1 = EMA %>%
  left_join(., baseline) %>% #merge with control variables
  mutate(drinks_number = ifelse(HadAlcohol == 1, Num_Beer + Num_Wine + Num_Liquor, #sum drinks if HadAlcohol = 1
                                ifelse(HadAlcohol == 0, 0, #recode NA as 0 if HadAlcohol = 0
                                       NA))) #center age

# calculate extreme outliers at the signal level
extreme_drinking = EMA1 %>% 
  filter(HadAlcohol == 1) %>%
  summarize(median = median(drinks_number, na.rm = TRUE),
            sd3 = 3 * sd(drinks_number, na.rm = TRUE),
            median_3sd = median + sd3)

# get correct day numbers
day = EMA %>%
  filter(grepl("Morning|Evening", Session.Name)) %>% #select only alcohol prompts
  extract(Notification.Time, "date", "(.*) .*", remove = FALSE) %>%
  select(shineid, date) %>%
  unique() %>%
  group_by(shineid) %>%
  mutate(day = row_number() - 1) #start at 0

# finish tidying and winsorize extreme outliers
model_data = EMA1 %>%
  filter(!is.na(Notification.Time)) %>%
  mutate(day_retro = ifelse(grepl("^Morning", Session.Name), lag(DayOfWeek),
                            ifelse(grepl("FirstMorning", Session.Name) & DayOfWeek == "Sunday", "Saturday",
                                   ifelse(grepl("FirstMorning", Session.Name) & DayOfWeek == "Monday", "Sunday",
                                          ifelse(grepl("FirstMorning", Session.Name) & DayOfWeek == "Tuesday", "Monday",
                                                 ifelse(grepl("FirstMorning", Session.Name) & DayOfWeek == "Wednesday", "Tuesday",
                                                        ifelse(grepl("FirstMorning", Session.Name) & DayOfWeek == "Thursday", "Wednesday",
                                                               ifelse(grepl("FirstMorning", Session.Name) & DayOfWeek == "Friday", "Thursday",
                                                                      ifelse(grepl("FirstMorning", Session.Name) & DayOfWeek == "Saturday", "Friday", DayOfWeek)))))))), #get correct day for retrospective reports
         weekend = ifelse(DayOfWeek %in% c("Friday", "Saturday"), "FriSat",
                          ifelse(DayOfWeek %in% c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"), "SunThurs", DayOfWeek)),
         weekend_retro = ifelse(day_retro %in% c("Friday", "Saturday"), "FriSat",
                                ifelse(day_retro %in% c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"), "SunThurs", day_retro)), #weekend variable for retrospective reports
         drinks_number_win = ifelse(drinks_number > extreme_drinking$median_3sd, round(extreme_drinking$median_3sd, 1), drinks_number), #winsorize extreme values
         intervention = ifelse(grepl("PROMPTA", Session.Name), "no",
                               ifelse(grepl("PROMPTB|PROMPTC", Session.Name), "yes", NA)),
         Alc_Encounter = ifelse(HadAlcohol == 1, 1, Alc_Encounter)) %>% #fix responses where they said they drank but did not encounter alcohol
  extract(Notification.Time, "date", "(.*) .*", remove = FALSE) %>%
  group_by(shineid, date) %>%
  fill(intervention, .direction = "up") %>%
  fill(intervention, .direction = "down") %>%
  ungroup() %>%
  filter(grepl("Morning|Evening", Session.Name)) %>% #select only alcohol prompts
  mutate(time_of_day = ifelse(grepl("Morning", Session.Name), "morning",
                              ifelse(grepl("Evening", Session.Name), "evening", "other")),
         encounter_time = ifelse(time_of_day == "morning", "evening",
                                 ifelse(time_of_day == "evening", "morning", NA)),
         condition_num = ifelse(Condition == "perspective", -.5,
                                ifelse(Condition == "mindful", .5,  NA)),
         condition_combined = ifelse(Condition == "control", "control", "intervention"),
         intervention_num = ifelse(intervention == "no", -.5,
                                   ifelse(intervention == "yes", .5,  NA))) %>% 
  filter(grepl("Morning|Evening", Session.Name)) %>% #select only alcohol prompts
  group_by(shineid) %>% #grouping by SHINEID
  mutate(date_retro = ifelse(grepl("^Morning", Session.Name), lag(date),
                             ifelse(grepl("FirstMorning", Session.Name), NA, date)),
         signal = row_number() - 1, #start at 0
         mean_craving = mean(Craving_Alc, na.rm = TRUE),
         alc_encounter_previous = lag(Alc_Encounter),
         alc_encounter_previous = ifelse(alc_encounter_previous == 0, "no",
                                         ifelse(alc_encounter_previous == 1, "yes", NA)),
         craving_previous = lag(Craving_Alc),
         resp_intervention = ifelse(Condition == "mindful", Alc_React_Mindful,
                                    ifelse(Condition == "perspective", Alc_React_Perspective, NA)),
         resp_natural = Alc_React_Natural,
         diff_response_mindful = Alc_React_Mindful - Alc_React_Natural,
         diff_response_perspective = Alc_React_Perspective - Alc_React_Natural) %>% 
  ungroup() %>%
  rename("condition" = Condition) %>%
  left_join(., day) %>%
  select(shineid, condition, condition_num, condition_combined,
         signal, day, date, date_retro, DayOfWeek, weekend, day_retro, weekend_retro, intervention, time_of_day,
         encounter_time, drinks_number, drinks_number_win, Alc_Encounter, alc_encounter_previous, Craving_Alc, craving_previous,
         resp_intervention, resp_natural, diff_response_mindful, diff_response_perspective, Alc_React_Natural, Alc_React_Mindful, Alc_React_Perspective, everything())

# create dataframe for day rather than signal
# get correct day for retrospective reports (i.e. drinking in the morning)
day_retro = model_data %>%
  select(shineid, date_retro) %>%
  unique() %>%
  filter(!is.na(date_retro)) %>% #ignore first morning responses because we don't have full retrospective data for that day
  group_by(shineid) %>%
  mutate(day_retro = row_number() - 1) #start at 0

model_data_day = model_data %>%
  select(shineid, condition, condition_num, condition_combined,
         signal, Responded, date_retro, weekend_retro,
         drinks_number, drinks_number_win, Alc_Encounter,
         amount_self_c, freq_self_c, age_c, gender_numeric, intervention) %>%
  left_join(., day_retro) %>%
  group_by(shineid, day_retro) %>%
  mutate(n_encounters = sum(Alc_Encounter, na.rm = TRUE),
         encounter = ifelse(n_encounters > 0, 1, 0),
         n_responses = sum(Responded),
         drinks_number = ifelse(n_responses == 0, NA, sum(drinks_number, na.rm = TRUE)),
         drinks_number_win = ifelse(n_responses == 0, NA, sum(drinks_number_win, na.rm = TRUE)),
         intervention = intervention[1],
         intervention_num = ifelse(intervention == "no", -.5,
                                   ifelse(intervention == "yes", .5,  NA))) %>%
  select(-signal, -Responded, -Alc_Encounter, -n_encounters) %>%
  unique() %>%
  filter(!is.na(date_retro)) #ignore first morning responses because we don't have full retrospective data for that day

# calculate percentage of drinking days and encounters dataframe
# across all days
prop_data = model_data_day %>%
  filter(!is.na(drinks_number)) %>%
  mutate(had_alcohol = ifelse(drinks_number > 0, 1, 0)) %>%
  group_by(shineid, condition, condition_num, condition_combined,
           amount_self_c, freq_self_c) %>%
  summarize(n_encounters = sum(encounter, na.rm = TRUE),
            n_drinking = sum(had_alcohol, na.rm = TRUE),
            n_responses = n(),
            percentage_encounters = (n_encounters / n_responses) * 100,
            percentage_drinking = (n_drinking / n_responses) * 100)

# as a function of intervention week
prop_data_int = model_data_day %>%
  filter(!is.na(drinks_number)) %>%
  mutate(had_alcohol = ifelse(drinks_number > 0, 1, 0)) %>%
  group_by(shineid, condition, condition_num, condition_combined, intervention, intervention_num,
           amount_self_c, freq_self_c) %>%
  summarize(n_encounters = sum(encounter, na.rm = TRUE),
            n_drinking = sum(had_alcohol, na.rm = TRUE),
            n_responses = n(),
            percentage_encounters = (n_encounters / n_responses) * 100,
            percentage_drinking = (n_drinking / n_responses) * 100)

colnames(prop_data)[colnames(prop_data) == "shineid"] <- "pID"
colnames(prop_data_int)[colnames(prop_data_int) == "shineid"] <- "pID"
QA_df <- merge(QA_df, prop_data_int, by=c('pID'))

prop_data_int <- merge(prop_data_int, networkStat, by="pID")

# remove MURIP173 with missing data
QA_df<- QA_df[!(QA_df$pID=="muri173"),]

df_proc <- aggregate(n_drinking~pID, data=QA_df, FUN=function(x) c(mean=mean(x)))
df_proc <- aggregate(percentage_drinking~pID, data=QA_df, FUN=function(x) c(mean=mean(x)))
df_proc$avecont <- unlist(aggregate(avecont~pID, data=QA_df, FUN=function(x) c(mean=mean(x)))[2])
#df_proc$Yeo7_flexEfficiency <- unlist(aggregate(Yeo7_flexEfficiency~pID, data=QA_df, FUN=function(x) c(mean=mean(x)))[2])
#df_proc$Yeo346_flexEfficiency <- unlist(aggregate(Yeo346_flexEfficiency~pID, data=QA_df, FUN=function(x) c(mean=mean(x)))[2])

df_proc$freq_self_c <- unlist(aggregate(freq_self_c~pID, data=QA_df, FUN=function(x) c(mean=mean(x)))[2])
temp_QA_df <- QA_df[!duplicated(QA_df[,c('pID')]),]
temp_QA_df <- temp_QA_df[, !(names(temp_QA_df) %in% c('percentage_drinking', 'n_drinking', 'avecont', 'freq_self_c'))]
df_proc <- merge(df_proc, temp_QA_df, by='pID')

df_proc$condition.x <- as.factor(df_proc$condition.x)
df_proc$condition_combined <- as.factor(df_proc$condition_combined)

save(object=df_proc, file="data/df_proc.RData")
