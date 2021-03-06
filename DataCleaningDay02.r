# DATA CLEANING DAY 2
SEER_Data<- read.csv("scratch/codeathon1.csv", header=TRUE)
library(ggplot2)


# DATA CLEANING 

drops <- c("Site.recode.B.ICD.O.3.WHO.2008", "Lymphoma.subtype.recode.WHO.2008", "Laterality", "Histology.recode...Brain.groupings", 
           "Summary.stage.2000..1998..", "SEER.summary.stage.1977..1995.2000.", "AJCC.stage.3rd.edition..1988.2003." , "SEER.modified.AJCC.stage.3rd..1988.2003.",
           "RX.Summ..Surg.Prim.Site..1998..", "Site.specific.surgery..1973.1997.varying.detail.by.year.and.site.", "Radiation.to.Brain.or.CNS.Recode..1988.1997.", 
           "CS.extension..2004.2015.", "Tumor.marker.1..1990.2003." , "Tumor.marker.2..1990.2003."  , "Tumor.marker.3..1990.2003." ,
           "Type.of.Reporting.Source", "Primary.Site...labeled", "Race.recode..W..B..AI..API."   ,                                  
           "Origin.recode.NHIA..Hispanic..Non.Hisp." )
SEER_Data<- SEER_Data[ , !(names(SEER_Data) %in% drops)]

SEER_Data<-SEER_Data[SEER_Data$Year.of.diagnosis>=1997 & SEER_Data$Year.of.diagnosis<=2016,]

SEER_Data$Survival.months.flag.new<- ifelse(SEER_Data$Survival.months.flag=="Complete dates are available and there are more than 0 days of survival",1,0)

SEER_Data$Survival.months.new<- as.integer(ifelse(SEER_Data$Survival.months.flag.new==0, NA, SEER_Data$Survival.months))

SEER_Data$Radiation.code.new <- ifelse(SEER_Data$Radiation.recode!="None/Unknown", 1,0)

SEER_Data$Site.recode.ICD.O.3.WHO.2008 <- ifelse(SEER_Data$Site.recode.ICD.O.3.WHO.2008 %in% c("NHL - Nodal","NHL - Extranodal" ), "NHL", SEER_Data$Site.recode.ICD.O.3.WHO.2008)
SEER_Data$Site.recode.ICD.O.3.WHO.2008 <- ifelse(SEER_Data$Site.recode.ICD.O.3.WHO.2008 %in% c("Rectum", "Sigmoid Colon", "Cecum", "Rectosigmoid Junction", "Ascending Colon", "Transverse Colon", "Descending Colon", "Large Intestine, NOS",
                                                                                               "Hepatic Flexure", "Splenic Flexure"), "Colorectal", SEER_Data$Site.recode.ICD.O.3.WHO.2008)

SEER_Data<- SEER_Data[ , !(names(SEER_Data) %in% c("Survival.months.flag", "Survival.months","Radiation.recode"))]

names(SEER_Data)[names(SEER_Data) == 'Survival.months.flag.new'] <- 'Survival.months.flag'
names(SEER_Data)[names(SEER_Data) == 'Survival.months.new'] <- 'Survival.months'
names(SEER_Data)[names(SEER_Data) == 'Radiation.code.new'] <- 'Radiation.recode'

write.csv(SEER_Data, "scratch/codeathon_RECODE.csv")

### UPDATES ### 

## ("scratch/codeathon_RECODE_Day3_v02.csv") --> v01 is the same as v02 but I had to re-run it as I don't have permision to re-write the v01. 
SEER_Cleaned <- read_csv("scratch/codeathon_RECODE_Day3_v02.csv")
SEER_Cleaned_updated <- SEER_Cleaned
#SEER_Cleaned_updated<- SEER_Cleaned[!SEER_Cleaned$X....High.school.education.ACS.2013.2017=="Blank(s)",]
#SEER_Cleaned_updated<- SEER_Cleaned_updated[SEER_Cleaned_updated$Age.recode.with..1.year.olds %in% c("35-39 years", "25-29 years", "30-34 years", "20-24 years", "15-19 years"),]
SEER_Cleaned_updated$X..Unemployed.ACS.2013.2017 <- as.double(SEER_Cleaned_updated$X..Unemployed.ACS.2013.2017)/100
SEER_Cleaned_updated$X....High.school.education.ACS.2013.2017<-as.double(SEER_Cleaned$X....High.school.education.ACS.2013.2017)/100

## NEW VARIABLES 
SEER_Cleaned_updated<- SEER_Cleaned_updated %>% mutate(HighSchoolEdCat = cut(X....High.school.education.ACS.2013.2017, breaks = c(0,10,20,30, 100)),
                                                       Unemployed_cat = cut(X..Unemployed.ACS.2013.2017, breaks = c(0,5,10,15, 100)),
                                                       median_income_household_group = ntile(Median.household.income..in.tens..ACS.2013.2017 , 4))  


# AGE GROUP COUNTS 
#SEER_Cleaned_updated %>% group_by(Decade,Age.recode.with..1.year.olds) %>% summarize(count=n()) %>% mutate(freq = count/sum(count)) 
write.csv(SEER_Cleaned_updated, "scratch/codeathon_RECODE_v03.csv")


