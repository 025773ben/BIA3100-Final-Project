library(forcats)
library(dplyr)
library(rpart)
library(rpart.plot)

##Filtering and Cleaning personStats-------------------------------------------

dfP <- read.csv("C:\\Users\\Ben_1\\Downloads\\OH2403251338204021BD6XS\\PersonStatistics.csv")
dfP <- dfP %>% 
  select(LocalReportNumber, Age, Gender, Injury, IsAlcoholSuspected) %>% 
  na.omit(dfP) %>% 
  filter(Age >= 16 & Age < 100 & Gender != "Other/Unknown" & Gender != "" & 
           Injury != "" & Injury != "Injury Possible") %>% 
  mutate(Gender = as.factor(Gender), Injury = as.factor(Injury),
         Injury = fct_recode(Injury, "Serious Injury" = "Fatal"),
         Injury = fct_recode(Injury, "Serious Injury" = "Serious Injury Suspected"),
         Injury = fct_recode(Injury, "Minor Injury" = "Minor Injury Suspected")) %>%
  group_by(Injury) %>%
  slice_sample(n = 2451) %>%
  ungroup()
str(dfP)
table(dfP$Gender)


##Filter and clean CrashStats------------------------------------------------

dfC <- read.csv("C:\\Users\\Ben_1\\Downloads\\OH2403251338204021BD6XS\\CrashStatistics.csv")
dfC <- dfC %>% 
  select(LocalReportNumber, Weather, LightCondition, SpeedRelated) %>% 
  filter(LightCondition != "Other/Unknown") %>% 
  mutate(Weather = as.factor(Weather), LightCondition = as.factor(LightCondition)) %>% 
  mutate(LightCondition = fct_recode(LightCondition,"Dark" = "Dark - Lighted Roadway"),
         LightCondition = fct_recode(LightCondition,"Dark" = "Dark - Roadway Not Lighted"),
         LightCondition = fct_recode(LightCondition,"Dark" = "Dark - Unknown Roadway Lighting"),
         LightCondition = fct_recode(LightCondition,"Dark" = "Dawn/Dusk"))
str(dfC)
table(dfC$Weather)


##Add vehicle year-------------------------------------------------------------

dfU <- read.csv("C:\\Users\\Ben_1\\Downloads\\OH2403251338204021BD6XS\\UnitStatistics.csv")
dfU <- dfU %>% 
  select(LocalReportNumber, VehicleYear) %>% 
  na.omit(dfU) %>% 
  filter(VehicleYear < 2025 & VehicleYear > 1993) ##Q1 is 2007, Q3 is 2016, IQR=9, so lower bound is 1993(Q1-1.5*IQR)

boxplot(dfU$VehicleYear)
summary(dfU)


##Combine the two---------------------------------------------------------------

dfT <- left_join(dfP, dfC, by = "LocalReportNumber")
dfT <- left_join(dfT, dfU, by = "LocalReportNumber")
dfT <- dfT %>% 
  na.omit(dfT)
dfT <- distinct(dfT, LocalReportNumber, .keep_all = T)

str(dfT)
table(dfT$Injury)


##Decision Tree----------------------------------------------------------------

tree <- rpart(Injury ~ . - LocalReportNumber, 
                    data = dfT, 
                    method = "class", 
                    control = rpart.control(minsplit = 10))
rpart.plot(tree)
