library(readr)
library(tidyverse)
codeathon_RECODE <- read_csv("scratch/codeathon_RECODE.csv")
View(codeathon_RECODE)


'
Frequency Table: Rural Urban Continuum Code 
-------------------------------------------
'
rural_urban_freq <- codeathon_RECODE %>%
  count(Rural.Urban.Continuum.Code.2013) %>%
  group_by('Rural.Urban.Continuum.Code.2013') %>%
  mutate(prop = prop.table(n))

rural_urban_freq

'
Frequency Table: Median Household
---------------------------------
'
med_household_freq <- codeathon_RECODE %>%
  count(Median.household.income..in.tens..ACS.2013.2017) %>%
  group_by(Median.household.income..in.tens..ACS.2013.2017) %>%
  mutate(prop = prop.table(n))

med_household_freq

'
Frequency Table: Median Household Income [Quartiles]
----------------------------------------------------
Multiply all values by 10
'
med_household_freq$med_household_10 <- med_household_freq$Median.household.income..in.tens..ACS.2013.2017*10
med_household_freq$med_household_10

codeathon_RECODE$Median.household.income..in.tens..ACS.2013.2017 <- codeathon_RECODE$Median.household.income..in.tens..ACS.2013.2017*10
codeathon_RECODE <- codeathon_RECODE %>% arrange(X1)
View(codeathon_RECODE)

#Assign Quartile to each median income
#-------------------------------------
med_household_freq$med_household_10_quartile <- ntile(med_household_freq$med_household_10, 4)  
med_household_freq

'
Recode Column: Marital Status (Marital.status.at.diagnosis)
-----------------------------------------------------------
Married : Married (including common law),  Separated
Unmarried : Single (never married), Unmarried, Domestic Partner, Divorced, Widowed
'
marital <-  codeathon_RECODE %>% distinct(Marital.status.at.diagnosis)
marital$Marital.status.at.diagnosis

codeathon_RECODE <- codeathon_RECODE %>% mutate(Marital.status.at.diagnosis = ifelse(Marital.status.at.diagnosis == "Married (including common law)", "Married",
                                                                                     ifelse(Marital.status.at.diagnosis == "Separated", "Married", "Unmarried")))

'
Recode Column: Urban vs Rural
-----------------------------
Guidance on recoding: rural.urban.continuum.code.2013     
to a variable representing "rural vs. non-rural” with output rows 1 and 2 = Rural, Rows 3, 4, 5, 8, 9, 10,11  = Non-Rural;  Rows 6, 7 = Missing)
'
ru <-  rural_urban_freq %>% distinct(Rural.Urban.Continuum.Code.2013)
ru$Rural.Urban.Continuum.Code.2013
rural_urban_freq

codeathon_RECODE <- codeathon_RECODE %>% 
  mutate(Rural.Urban.Continuum.Code.2013 = ifelse(Rural.Urban.Continuum.Code.2013 == "Comp rural lt 2,500 urban pop, adjacent to a metro area", "Rural",
                                                  ifelse(Rural.Urban.Continuum.Code.2013 == "Comp rural lt 2,500 urban pop, not adjacent to metro area ", "Rural",
                                                         ifelse(Rural.Urban.Continuum.Code.2013 == "Counties in metropolitan areas of lt 250 thousand pop", "Missing",
                                                                ifelse(Rural.Urban.Continuum.Code.2013 == "Unknown/missing/no match", "Missing", "Non-Rural")))))


#Check Recoded column
ru <-  codeathon_RECODE %>% distinct(Rural.Urban.Continuum.Code.2013)
ru$Rural.Urban.Continuum.Code.2013



'
Recode Column: ER Status
------------------------
Recode Borderline to Positive
'
er <-  codeathon_RECODE %>% distinct(ER.Status.Recode.Breast.Cancer..1990..)
er$ER.Status.Recode.Breast.Cancer..1990..

codeathon_RECODE <- codeathon_RECODE %>% mutate(ER.Status.Recode.Breast.Cancer..1990.. = ifelse(ER.Status.Recode.Breast.Cancer..1990.. == "Borderline", "Positive", ER.Status.Recode.Breast.Cancer..1990..))

#Check Recode 
er <-  codeathon_RECODE %>% distinct(ER.Status.Recode.Breast.Cancer..1990..)
er$ER.Status.Recode.Breast.Cancer..1990..


'
Save Data
'
write.table(codeathon_RECODE, file="scratch/codeathon_RECODE_Day3_v01.csv", sep=",", row.names = FALSE, quote = FALSE)

