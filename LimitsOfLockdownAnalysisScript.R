#Limits of lockdown: characterising essential contacts during strict physical distancing

#Amy Thomas, University of Bristol, 12/03/2021

library(tidyverse)
library(haven) #for importing .dta STATA files #Y
library(labelled) #for handling labelled variables
library(sjlabelled) #handling labelled variables
library(sjmisc) #Y #use frq() to print a summary, includes labelled vectors in frequency table
library(naniar)
library(lubridate)
library(cowplot)
#library(plyr)

#Read in .dta file and inspect
df <- read_dta(file = "COVID_ALL_V1.0_plusG1occ.dta")

##########################################
#Removal of specific cases 
##########################################

#remove one case so that it matches the datanote: COVID_ID 9142
#case removed as completely new to the study, although eligble had never taken part. 
df <- df %>% filter(!COVID_ID == 9142)

#find which cases relate to Trips/Quads for age and check they are G1
tripsquads <- df %>% filter(covid1_9650 == -11)

#all responses from participants who are a Triplet or Quad are coded as this to retain anonymity, remove these cases from data
df <- df %>% filter(covid1_9650 > 0)

##########################################
#Weekend vs weekday responses and contacts 
##########################################

#create date survey completed by concatenating 9620, 9621, 9622 
df$date_ymd <- paste(df$covid1_9622,"-",df$covid1_9621,"-", df$covid1_9620)
str_replace_all(df$date_ymd, fixed(" "), "")
df$date_ymd <- ymd(df$date_ymd)

#calculate day of the week survey was conducted 
df$weekday <- wday(df$date_ymd, label = TRUE, abbr = TRUE)

#create new variable indicating if survey was completed on a weekday or weekend 
df <- mutate(df, weekday2 = ifelse(weekday == "Sat" | weekday == "Sun", "Weekend", "Weekday"))
df$weekday2 <- as.factor(df$weekday2)

#plot the number of responses over time - how many cluster in the beginning? 
summary(df$date_ymd)
ggplot(df, aes(date_ymd, ..count..)) + geom_histogram() + xlab("Date") + scale_x_date(date_minor_breaks = "1 day") + ylab("Count") + theme_minimal_grid()

#summarise the number of responses for weeks 1 to 6 of Q1 (is Monday start of week? tried to use start.on.monday = 4 to denote starts on Thursday, but function not recognised?)
df$Q1week <- cut(df$date_ymd, "weeks", labels = c("1", "2", "3", "4", "5", "6"))
df %>% dplyr::select(Q1week) %>% frq()
df %>% dplyr::select(weekday) %>% frq()
df %>% dplyr::select(weekday2) %>% frq()
df %>% dplyr::select(date_ymd) %>% frq()
levels(df$Q1week)

#how many contacts were reported for the weekend vs weekday 
#need to know what yesterday was... 
df$yesterday <- df$date_ymd-1
df$yesterday <- wday(df$yesterday, label = TRUE, abbr = TRUE)
df %>% select(yesterday) %>% frq()

#how many contacts were on weekdays or weekends?
df <- mutate(df, yesterday2 = ifelse(yesterday == "Sat" | yesterday == "Sun", "Weekend", "Weekday"))
df %>% dplyr::select(yesterday2) %>% frq()

#######################
#Ethnicity and Geography
#######################

#ethnicity 
df %>% select(ethnicity) %>% frq()

#geography 
#how many pp's live abroad? covid1_1500
df %>% select(covid1_1500) %>% frq()

##########################################
#Creation of age categories 
##########################################

# bin ages into groups/categories corresponding to contact q's using mutate()
# most participants are 18 - 69 so loose granularity of data with this approach
# consider different age categories? Or impute for ages which aren't surveyed?
# 0 to 4
# 5 - 17 
# 18 - 69
# 70+ 

df %>% dplyr::select(covid1_9650) %>% frq()

df <- df %>% filter(covid1_9650 > 0) %>% 
  mutate(age = cut(covid1_9650, breaks = c(0, 4, 17, 69, Inf), 
                   labels = c("0 - 4", "5 - 17", "18 - 69", "70+ ")))

all_ages_tally <- df %>% group_by(covid1_9650) %>% tally()
df %>% group_by(age) %>% tally()

#increase number of age categories to capture granularity
df <- df %>% filter(covid1_9650 > 0) %>% 
  mutate(age2
         = cut(covid1_9650, breaks = c(0, 4, 17, 29, 49, 69, Inf), 
               labels = c("0 - 4", "5 - 17", "18 - 29", "30 - 49", "50 - 69", "70+")))

#increase number of age categories further; these categories mirror POLYMOD and CoMix 
df <- df %>% filter(covid1_9650 > 0) %>% 
  mutate(age3
         = cut(covid1_9650, breaks = c(0, 4, 17, 29, 39, 49, 59, 69, Inf), 
               labels = c("0 - 4", "5 - 17", "23 - 29", "30 - 39", "40 - 49", "50 - 59", "60 - 69", "70+")))

#age categories to allow for modelling non-linear trend with groups of unequal sizes 
df <- df %>% filter(covid1_9650 > 0) %>% 
  mutate(age4
         = cut(covid1_9650, breaks = c(0, 49, Inf), 
               labels = c("0 - 49", "50+")))

##########################################################################################################
#Calculate total number of face-to-face contacts; face-to-face w/ physical contact; total contacts
##########################################################################################################

#ignore missing responses coded -1, -11, -10, -9

####################################
#Calculate total number of face-to-face contacts for each respondent: variable = total_face_to_face

#covid1_3100 - How many 0-4y.o. pp spoke to yesterday with physical contact
#covid1_3101 - How many 5-17y.o. pp spoke to yesterday with physical contact
#covid1_3102 - How many 18-69y.o. pp spoke to yesterday with physical contact
#covid1_3103 - How many 70+y.o. pp spoke to yesterday with physical contact

#dplyr::select responses for how many contacts were made yesterday which involved face-to-face contact only
#replace missing values coded as negative numbers with NA  

df <- df %>% replace_with_na(replace = list(covid1_3100 = c(-11,-10, -9, -1),
                                            covid1_3101 = c(-11,-10, -9, -1),
                                            covid1_3102 = c(-11,-10, -9, -1),
                                            covid1_3103 = c(-11,-10, -9, -1)))
df %>% filter(covid1_3100>=0) %>% tally()
is.na(df$covid1_3100)
df %>% dplyr::select(., covid1_3100, covid1_3101, covid1_3102, covid1_3103) %>% rowSums(na.rm=TRUE) -> df$total_face_to_face


####################################
#Calculate total number of physical contacts

#Calculate total number of physical contacts for each respondent: variable = total_physical 
#covid1_3130 - How many 0-4y.o. pp spoke to yesterday with physical contact
#covid1_3131 - How many 5-17y.o. pp spoke to yesterday with physical contact
#covid1_3132 - How many 18-69y.o. pp spoke to yesterday with physical contact
#covid1_3133 - How many 70+y.o. pp spoke to yesterday with physical contact

#dplyr::select responses for how many contacts were made yesterday which involved physical touch 
#replace missing values coded as negative numbers with NA  

df <- df %>% replace_with_na(replace = list(covid1_3130 = c(-11,-10, -9, -1),
                                            covid1_3131 = c(-11,-10, -9, -1),
                                            covid1_3132 = c(-11,-10, -9, -1),
                                            covid1_3133 = c(-11,-10, -9, -1)))
df %>% dplyr::select(., covid1_3130, covid1_3131, covid1_3132, covid1_3133) %>% rowSums(na.rm=TRUE) -> df$total_physical

############################################################################################################
#Calculate total number of contacts: sum of face-to-face and face-to-face with physical contact

#total contacts
df %>% dplyr::select(., total_face_to_face, total_physical) %>% rowSums(na.rm=TRUE) -> df$total_contacts

#######################
# Face-to-face contacts
#######################

#ignore missing responses coded -1, -11, -10, -9
#missing data?
df %>% group_by(covid1_3100) %>% tally()
#Missed Section (-9): 177; Missing (-1): 301; Total missing: 478
df %>% group_by(age) %>% tally()
#18-69: 6737; 70+: 71

#how many respondents reported zero face-to-face contacts aged 0-4 years
df %>% filter(covid1_3100 == 0) %>% group_by(age) %>% tally()
#how many respondents reported at least one face-to-face contact aged 0-4 years
df %>% filter(covid1_3100 > 0) %>% group_by(age) %>% tally()
#how many respondents missed the section for face-to-face contact aged 0-4 years
df %>% filter(covid1_3100 < 0) %>% group_by(age) %>% tally()


#Mean number of f2f contacts aged 0-4; 5-17; 18-69; 70+

#mean face-to-face contacts aged 0-4; total respondents which answered zero contacts or above: 6329
f2f0_4age3 <- df %>% filter(covid1_3100 >= 0) %>% 
  group_by(age.respondent = age3) %>% summarise(raw.mean = mean(covid1_3100), median = median(covid1_3100), sd = sd(covid1_3100), total.contacts = sum(covid1_3100), 
                                                Quant25 = quantile(covid1_3100, probs = (0.25)), 
                                                Quant75 = quantile(covid1_3100, probs = (0.75)),
                                                n= n()) %>% mutate(age.contact = "0 - 4")

#mean face-to-face contacts aged 5-17 
f2f5_17age3 <- df %>% filter(covid1_3101 >= 0) %>% 
  group_by(age.respondent = age3) %>% summarise(raw.mean = mean(covid1_3101), median = median(covid1_3101),sd = sd(covid1_3101), total.contacts = sum(covid1_3101),
                                                Quant25 = quantile(covid1_3101, probs = (0.25)), 
                                                Quant75 = quantile(covid1_3101, probs = (0.75)), n=n()) %>% mutate(age.contact = "5 - 17")

#mean face-to-face contacts aged 18-69 
f2f18_69age3 <- df %>% filter(covid1_3102 >= 0) %>% 
  group_by(age.respondent = age3) %>% summarise(raw.mean = mean(covid1_3102), median = median(covid1_3102),sd = sd(covid1_3102), total.contacts = sum(covid1_3102),
                                                Quant25 = quantile(covid1_3102, probs = (0.25)), 
                                                Quant75 = quantile(covid1_3102, probs = (0.75)), n=n()) %>% mutate(age.contact = "18 - 69")

#mean face-to-face contacts aged 70+
f2f70age3 <-df %>% filter(covid1_3103 >= 0) %>% 
  group_by(age.respondent = age3) %>% summarise(raw.mean = mean(covid1_3103), median = median(covid1_3103),sd = sd(covid1_3103),total.contacts = sum(covid1_3103),
                                                Quant25 = quantile(covid1_3103, probs = (0.25)), 
                                                Quant75 = quantile(covid1_3103, probs = (0.75)), n=n()) %>% mutate(age.contact = "70+")
##############################
#Total face-to-face contacts

#Calculate total number of face-to-face contacts for each respondent: variable = total_face_to_face
#covid1_3100 - How many 0-4y.o. pp spoke to yesterday with physical contact
#covid1_3101 - How many 5-17y.o. pp spoke to yesterday with physical contact
#covid1_3102 - How many 18-69y.o. pp spoke to yesterday with physical contact
#covid1_3103 - How many 70+y.o. pp spoke to yesterday with physical contact

#select responses for how many contacts were made yesterday which involved face-to-face contact only
#replace missing values coded as negative numbers with NA  

df <- df %>% replace_with_na(replace = list(covid1_3100 = c(-11,-10, -9, -1),
                                            covid1_3101 = c(-11,-10, -9, -1),
                                            covid1_3102 = c(-11,-10, -9, -1),
                                            covid1_3103 = c(-11,-10, -9, -1)))
df %>% filter(covid1_3100>=0) %>% tally()
is.na(df$covid1_3100)
df %>% select(., covid1_3100, covid1_3101, covid1_3102, covid1_3103) %>% rowSums(na.rm=TRUE) -> df$total_face_to_face
#mean number of face-to-face contacts for all respondents
df %>% summarise(mean = mean(total_face_to_face, na.rm = TRUE), median = median(total_face_to_face, na.rm = TRUE), sd = sd(total_face_to_face, na.rm = TRUE), min = min(total_face_to_face, na.rm = TRUE), max = max(total_face_to_face, na.rm = TRUE), Quant25 = quantile(total_face_to_face, probs = (0.25), na.rm =TRUE), Quant75 = quantile(total_face_to_face, probs = (0.75), na.rm =TRUE), N = n()) 

#mean number of face-to-face contacts for six age groups
df %>% group_by(age3) %>% summarise(mean = mean(total_face_to_face), SD = sd(total_face_to_face), N = n())

#mean number of face-to-face contacts stratified by sex
df %>% group_by(sex) %>% summarise(mean = mean(total_face_to_face), SD= sd(total_face_to_face), N = n())

#mean number of face-to-face contacts stratified by occupation 
df %>% group_by(occupation) %>% summarise(mean = mean(total_face_to_face), SD= sd(total_face_to_face), N = n())

#how many respondents reported zero face-to-face contacts for each of the four age categories; how many reported at least one; count of missing values?
df %>% filter(covid1_9650 > 0) %>% select(covid1_3100) %>% frq()
df %>% filter(covid1_9650 > 0) %>% select(covid1_3101) %>% frq()
df %>% filter(covid1_9650 > 0) %>% select(covid1_3102) %>% frq()
df %>% filter(covid1_9650 > 0) %>% select(covid1_3103) %>% frq()

#how many respondents reported zero face-to-face contacts
df %>% filter(total_face_to_face == 0) %>% tally()

#how many respondents reported at least one face-to-face contact 
#N=3944
df %>% filter(total_face_to_face > 0) %>% select(total_face_to_face) %>% tally()

#total f2f contacts by G0 and G1 
df %>% group_by(covid1_0005) %>% summarise(mean = mean(total_face_to_face), SD = sd(total_face_to_face), N = n())


##########################
# Physical contacts
##########################

#Mean number of f2f contacts w/ physical contact aged 0-4; 5-17; 18-69; 70+

#how many respondents reported zero physical contacts aged 0-4 years
df %>% filter(covid1_3130 == 0) %>% group_by(age) %>% tally()
#how many respondents reported at least one physical contact aged 0-4 years
df %>% filter(covid1_3130 > 0) %>% group_by(age) %>% tally()
#how many respondents missed the section for physical contact aged 0-4 years
df %>% filter(covid1_3130 < 0) %>% group_by(age) %>% tally()

#mean 0-4 physical
phys0_4age3 <- df %>% filter(covid1_3130 >= 0) %>% group_by(age.respondent = age3) %>% summarise(mean = mean(covid1_3130), median = median(covid1_3130), sd = sd(covid1_3130), 
                                                                                                 IQR = IQR(covid1_3130),total.contacts = sum(covid1_3130), n= n()) %>% mutate(age.contact = "0 - 4")

#mean 5-17 physical
phys5_17age3 <- df %>% filter(covid1_3131 >= 0) %>% group_by(age.respondent = age3) %>% summarise(mean = mean(covid1_3131), median = median(covid1_3131), sd = sd(covid1_3131), 
                                                                                                  IQR = IQR(covid1_3131),total.contacts = sum(covid1_3131), n= n()) %>% mutate(age.contact = "5 - 17")

#mean 18-69 physical
phys18_69age3 <- df %>% filter(covid1_3132 >= 0) %>% group_by(age.respondent = age3) %>% summarise(mean = mean(covid1_3132), median = median(covid1_3132), sd = sd(covid1_3132), 
                                                                                                   IQR = IQR(covid1_3132),total.contacts = sum(covid1_3132), n= n()) %>% mutate(age.contact = "18 - 69")

#mean 70+ physical
phys70age3 <- df %>% filter(covid1_3133 >= 0) %>% group_by(age.respondent = age3) %>% summarise(mean = mean(covid1_3133), median = median(covid1_3133), sd = sd(covid1_3133), 
                                                                                                IQR = IQR(covid1_3133),total.contacts = sum(covid1_3133), n= n()) %>% mutate(age.contact = "70+")

########################
# Total physical contacts

#Calculate total number of physical contacts for each respondent: variable = total_physical 
#covid1_3130 - How many 0-4y.o. pp spoke to yesterday with physical contact
#covid1_3131 - How many 5-17y.o. pp spoke to yesterday with physical contact
#covid1_3132 - How many 18-69y.o. pp spoke to yesterday with physical contact
#covid1_3133 - How many 70+y.o. pp spoke to yesterday with physical contact

#select responses for how many contacts were made yesterday which involved physical touch 
#replace missing values coded as negative numbers with NA  

df <- df %>% replace_with_na(replace = list(covid1_3130 = c(-11,-10, -9, -1),
                                            covid1_3131 = c(-11,-10, -9, -1),
                                            covid1_3132 = c(-11,-10, -9, -1),
                                            covid1_3133 = c(-11,-10, -9, -1)))
df %>% select(., covid1_3130, covid1_3131, covid1_3132, covid1_3133) %>% rowSums(na.rm=TRUE) -> df$total_physical


#mean number of physical contacts for all respondents
df %>% summarise(mean = mean(total_physical), sd = sd(total_physical), min = min(total_physical), max = max(total_physical))
df %>% summarise(mean = mean(total_physical, na.rm = TRUE), median = median(total_physical, na.rm = TRUE), sd = sd(total_physical, na.rm = TRUE), min = min(total_physical, na.rm = TRUE), max = max(total_physical, na.rm = TRUE), Quant25 = quantile(total_physical, probs = (0.25), na.rm =TRUE), Quant75 = quantile(total_physical, probs = (0.75), na.rm =TRUE), N = n()) 

#mean number of physical contacts for age groups 18 - 69 and 70+
df %>% group_by(age) %>% summarise(mean = mean(total_physical), N = n())

#mean number of physical contacts for each four age groups
df %>% group_by(age2) %>% summarise(mean = mean(total_physical), N = n())

#mean number of physical contacts for six age groups
df %>% group_by(age3) %>% summarise(mean = mean(total_physical), SD= sd(total_physical), N = n())

#mean number of physical contacts stratified by sex
df %>% group_by(sex) %>% summarise(mean = mean(total_physical), SD= sd(total_physical), N = n())

#mean number of physical contacts stratified by occupation 
df %>% group_by(occupation) %>% summarise(mean = mean(total_physical), SD= sd(total_physical), N = n())

#how many respondents reported zero physical contacts, count of missing values?
df %>% filter(covid1_9650 > 0) %>% select(covid1_3130) %>% frq()
df %>% filter(covid1_9650 > 0) %>% select(covid1_3131) %>% frq()
df %>% filter(covid1_9650 > 0) %>% select(covid1_3132) %>% frq()
df %>% filter(covid1_9650 > 0) %>% select(covid1_3133) %>% frq()

#how many respondents reported zero physical contacts
df %>% filter(total_physical == 0) %>% tally()
df %>% select(total_physical) %>% frq()

#how many respondents reported at least one physical contact 
#N=900
df %>% filter(total_physical > 0) %>% select(total_physical) %>% tally()

#total physical contacts by G0 and G1 
df %>% group_by(covid1_0005) %>% summarise(mean = mean(total_physical), SD = sd(total_physical), N = n())


#####################
# Total contacts 
#####################

#Calculate total contacts for each participant: sum of f2f and f2f w/ physical: variable = total_contacts 
df %>% select(., total_face_to_face, total_physical) %>% rowSums(na.rm=TRUE) -> df$total_contacts

#Take max value of total face2face and total_physical as total contacts 
df %>% rowwise() %>% mutate(total_contactsLD=max(total_face_to_face,total_physical))->df

#Mean number of total contacts (f2f + f2f w/ physical contact) aged 0-4; 5-17; 18-69; 70+

#mean number of total contacts for age groups 18 - 69 and 70+
df %>% group_by(age) %>% summarise(mean = mean(total_contacts), N = n())

#mean number of total contacts for each four age groups
df %>% group_by(age2) %>% summarise(mean = mean(total_contacts), N = n())

#mean number of total contacts for six age groups
df %>% group_by(age3) %>% summarise(mean = mean(total_contacts), SD = sd(total_contacts), N = n())

#mean total contacts
df %>% summarise(mean = mean(total_contacts), sd = sd(total_contacts), min = min(total_contacts), max = max(total_contacts))
df %>% summarise(mean = mean(total_contacts, na.rm = TRUE), median = median(total_contacts, na.rm = TRUE), sd = sd(total_contacts, na.rm = TRUE), min = min(total_contacts, na.rm = TRUE), max = max(total_contacts, na.rm = TRUE), Quant25 = quantile(total_contacts, probs = (0.25), na.rm =TRUE), Quant75 = quantile(total_contacts, probs = (0.75), na.rm =TRUE), N = n()) 

#mean number of total contacts stratified by sex
df %>% group_by(sex) %>% summarise(mean = mean(total_contacts), SD= sd(total_contacts), N = n())

#mean number of total contacts stratified by occupation 
df %>% group_by(occupation) %>% summarise(mean = mean(total_contacts), SD= sd(total_contacts), N = n())

#how many respondents reported zero face-to-face contacts or face-to-face with physical contact 
df %>% select(total_contacts) %>% frq()

#total contacts by G0 and G1 
df %>% group_by(covid1_0005) %>% summarise(mean = mean(total_contacts), SD = sd(total_contacts), N = n())

#create new variables which are total contacts for each age of contact
#i.e not the total contact for each pp, but sum of the f2f and f2f + physical for each contact group 

#mean 0-4 total 
df %>% select(., covid1_3130, covid1_3100) %>% rowSums(na.rm=TRUE) -> df$total_0to4
tot0_4age3 <- df %>% filter(total_0to4 >= 0) %>% group_by(age.respondent = age3) %>% summarise(mean = mean(total_0to4), median = median(total_0to4), sd = sd(total_0to4), 
                                                                                               IQR = IQR(total_0to4), total.contacts = sum(total_0to4), n= n()) %>% mutate(age.contact = "0 - 4")
#mean 5-17 total
df %>% select(., covid1_3131, covid1_3101) %>% rowSums(na.rm=TRUE) -> df$total_5to17
tot5_17age3 <- df  %>% filter(total_5to17 >= 0) %>% group_by(age.respondent = age3) %>% summarise(mean = mean(total_5to17), median = median(total_5to17), sd = sd(total_5to17),
                                                                                                  IQR = IQR(total_5to17),total.contacts = sum(total_5to17), n= n()) %>% mutate(age.contact = "5 - 17")
#mean 18-69 total
df %>% select(., covid1_3132, covid1_3102) %>% rowSums(na.rm=TRUE) -> df$total_18to69
tot18_69age3 <- df %>% filter(total_18to69 >= 0) %>% group_by(age.respondent = age3) %>% summarise(mean = mean(total_18to69), median = median(total_18to69), sd = sd(total_18to69),
                                                                                                   IQR = IQR(total_18to69),total.contacts = sum(total_18to69), n= n()) %>% mutate(age.contact = "18 - 69")
#mean 70+ total
df %>% select(., covid1_3133, covid1_3103) %>% rowSums(na.rm=TRUE) -> df$total_70over
tot70age3 <- df %>% filter(total_70over >= 0) %>% group_by(age.respondent = age3) %>% summarise(mean = mean(total_70over), median = median(total_70over), sd = sd(total_70over),
                                                                                                IQR = IQR(total_70over),total.contacts = sum(total_70over), n= n()) %>% mutate(age.contact = "70+")


##########################################
#Household size  
##########################################

# Average household size (SD; Max)
# Create new variable Household Size - sum 
# replace values with NA using naniar() replace_with_na
df <- df %>% replace_with_na(replace = list(covid1_5015 = c(-11,-10, -9, -1),
                                            covid1_5016 = c(-11,-10, -9, -1),
                                            covid1_5017 = c(-11,-10, -9, -1),
                                            covid1_5018 = c(-11,-10, -9, -1)))
df %>% dplyr::select(., covid1_5015, covid1_5016, covid1_5017, covid1_5018) %>% rowSums(na.rm=TRUE) -> df$household_size

#add 1 to household size so that pp's living alone show as a household size of 1, those reporting one other in the house show as a household size of 2 etc.
df <- mutate(df, household_size = household_size +1)

#cut household size into: 1,2,3,4 and 5+ and create new variable household_size2
df <- df %>% mutate(household_size2 = cut(household_size, breaks = c(0, 1, 2, 3, 4, Inf), 
                                          labels = c("1", "2", "3", "4", "5+")))

#how many individuals live alone? 
df <- mutate(df, alone = ifelse(household_size == 1, "Live alone", "Multiple Occp"))

#contact patterns in relation to household size
df %>% group_by(household_size2) %>% summarise(mean = mean(total_contacts, na.rm = TRUE), median = median(total_contacts, na.rm = TRUE), sd = sd(total_contacts, na.rm = TRUE), min = min(total_contacts, na.rm = TRUE), max = max(total_contacts, na.rm = TRUE), Quant25 = quantile(total_contacts, probs = (0.25), na.rm =TRUE), Quant75 = quantile(total_contacts, probs = (0.75), na.rm =TRUE), N = n()) 
df %>% group_by(household_size2) %>% summarise(mean = mean(total_physical, na.rm = TRUE), median = median(total_physical, na.rm = TRUE), sd = sd(total_physical, na.rm = TRUE), min = min(total_physical, na.rm = TRUE), max = max(total_physical, na.rm = TRUE), Quant25 = quantile(total_physical, probs = (0.25), na.rm =TRUE), Quant75 = quantile(total_physical, probs = (0.75), na.rm =TRUE), N = n()) 
df %>% group_by(household_size2) %>% summarise(mean = mean(total_face_to_face, na.rm = TRUE), median = median(total_face_to_face, na.rm = TRUE), sd = sd(total_face_to_face, na.rm = TRUE), min = min(total_face_to_face, na.rm = TRUE), max = max(total_face_to_face, na.rm = TRUE), Quant25 = quantile(total_face_to_face, probs = (0.25), na.rm =TRUE), Quant75 = quantile(total_face_to_face, probs = (0.75), na.rm =TRUE), N = n()) 


##########################################
#Lives with children 
##########################################

#total number of children aged 0-17 participant lives with
df %>% dplyr::select(., covid1_5015, covid1_5016) %>% rowSums(na.rm=TRUE) -> df$N_0to17yo

h0to17 <- df %>%
  group_by(age3, N_0to17yo) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

df %>% filter(age3 == "40 - 49") %>% dplyr::select(N_0to17yo) %>% frq()

df <- mutate(df, children_in_house = ifelse(N_0to17yo == 0, "No children", "Children"))

#contact patterns and living with children
df %>%group_by(children_in_house) %>%
  summarise(mean = mean(total_contacts), sd = sd(total_contacts), median = median(total_contacts), Quant25 = quantile(total_contacts, probs = (0.25)), Quant75 = quantile(total_contacts, probs = (0.75)), n = n()) %>%
  mutate(freq = n / sum(n))

df %>%group_by(children_in_house) %>%
  summarise(mean = mean(total_face_to_face), sd = sd(total_face_to_face), median = median(total_face_to_face), Quant25 = quantile(total_face_to_face, probs = (0.25)), Quant75 = quantile(total_face_to_face, probs = (0.75)), n = n()) %>%
  mutate(freq = n / sum(n))

df %>%group_by(children_in_house) %>%
  summarise(mean = mean(total_physical), sd = sd(total_physical), median = median(total_physical), Quant25 = quantile(total_physical, probs = (0.25)), Quant75 = quantile(total_physical, probs = (0.75)), n = n()) %>%
  mutate(freq = n / sum(n))


################################################
#Reported COVID-19 infection and self-isolation
################################################
#what proportion of individuals report having had COVID-19/suspect? 
df %>% select(covid1_2580) %>% frq()
df %>% group_by(covid1_0005) %>% select(covid1_2580) %>% frq()

#what proportion of individuals were self-isolating? 
attributes(df$covid1_3000)
df %>% select(covid1_3000) %>% frq()

#duration of self-isolation?
attributes(df$covid1_3005)
df %>% select(covid1_3005) %>% frq() 

#rational for self-isolation 
#(covid1_3010, covid1_3011, covid1_3012, covid1_3013, covid1_3014, covid1_3015, covid1_3016)
df %>% select(covid1_3010, covid1_3011, covid1_3012, covid1_3013, covid1_3014, covid1_3015, covid1_3016) %>% frq()

#of the participants which have/had COVID-19, how many are self-isolating/did self-isolate?
df %>% filter(covid1_2580 == 1) %>% select(covid1_3000) %>% frq()

#self-isolation = covid1_3000 -> replace missing values with unknown 
df$selfisolation_un <- as_factor(df$covid1_3000, levels = "labels")
attributes(df$selfisolation_un)
df$selfisolation_un <- dplyr::recode(df$selfisolation_un, `-9999` = "UNK", `-11` = "UNK", `-10` = "UNK", 
                                     `-9` = "UNK", `-1` = "UNK", `1` = "No", `2` = "Yes, am now", `3` = "Yes, but have stopped", `9` = "Prefer not to say")
df$selfisolation_un <- as_factor(df$selfisolation_un)
#re-level so no is reference levels
df$selfisolation_un <- factor(df$selfisolation_un, levels = c("No", "Yes, am now","Yes, but have stopped","Prefer not to say", "UNK"))

df %>% group_by(selfisolation_un) %>% tally() #check all missing values coded as UNK for modelling with pln
#levels: 1 = No; 2 = Yes, am now; 3= Yes, but have stopped; 9 = Prefer not to say
#change NA to UNK for modelling 

#contact patterns in relation to self-isolation
df %>% group_by(selfisolation_un) %>% summarise(mean = mean(total_contacts, na.rm = TRUE), median = median(total_contacts, na.rm = TRUE), sd = sd(total_contacts, na.rm = TRUE), min = min(total_contacts, na.rm = TRUE), max = max(total_contacts, na.rm = TRUE), Quant25 = quantile(total_contacts, probs = (0.25), na.rm =TRUE), Quant75 = quantile(total_contacts, probs = (0.75), na.rm =TRUE), N = n()) 
df %>% group_by(selfisolation_un) %>% summarise(mean = mean(total_physical, na.rm = TRUE), median = median(total_physical, na.rm = TRUE), sd = sd(total_physical, na.rm = TRUE), min = min(total_physical, na.rm = TRUE), max = max(total_physical, na.rm = TRUE), Quant25 = quantile(total_physical, probs = (0.25), na.rm =TRUE), Quant75 = quantile(total_physical, probs = (0.75), na.rm =TRUE), N = n()) 
df %>% group_by(selfisolation_un) %>% summarise(mean = mean(total_face_to_face, na.rm = TRUE), median = median(total_face_to_face, na.rm = TRUE), sd = sd(total_face_to_face, na.rm = TRUE), min = min(total_face_to_face, na.rm = TRUE), max = max(total_face_to_face, na.rm = TRUE), Quant25 = quantile(total_face_to_face, probs = (0.25), na.rm =TRUE), Quant75 = quantile(total_face_to_face, probs = (0.75), na.rm =TRUE), N = n()) 


##########################################
#Occupations 
##########################################

#how many respondents classify as both a HCW and keyworker?
df %>% filter(covid1_5035 == 2, covid1_5045 == 1) %>% tally() #631
#how many respondents classify as a HCW but not keyworker
df %>% filter(covid1_5035 == 2, covid1_5045 == 2) %>% tally() #N=52 HCW but not keyworker
df %>% filter(covid1_5045 == 1) %>% tally() #N=2234 keyworker
df %>% filter(covid1_5035 == 2) %>% tally() #N=697 hcw
#how many respondents classify as a keyworker but not HCW, N = 1600
df %>% filter(covid1_5035 == 1 | covid1_5035 == 3 | covid1_5035 == 8 | covid1_5035 == 9, covid1_5045 == 1) %>% tally()
#how many respondents classify as neither a HCW or keyworker
df %>% filter(covid1_5035 == 1 | covid1_5035 == 3 | covid1_5035 == 8 | covid1_5035 == 9, covid1_5045 == 2 | covid1_5045 == 9) %>% tally()

df %>% group_by(covid1_5035) %>% tally()

#create a new variable - occupation - respondents who self-classify as HCW and keyworker are classifed as HCW only
df <- df %>% mutate(occupation = case_when(covid1_5035 == 2 ~ "HCW", #is a HCW
                                           covid1_5045 == 1 ~ "Keyworker", # is a keyworker
                                           !covid1_5035 == 2 & !covid1_5045 == 1 ~ "Other")) # neither HCW or keyworker

##########################################
#SOC2010 occupations 

#de-concatenate SOC2010Title into hierarchical groups using strspilt, first coerce number to character  
df <- df %>% mutate(SOC2010CodeChr = as.character(SOC2010code))
df$SOC2010Major<- substr(df$SOC2010CodeChr, start = 1, stop = 1)
df$SOC2010SubMajor<- substr(df$SOC2010CodeChr, start = 1, stop = 2)
df$SOC2010Minor<- substr(df$SOC2010CodeChr, start = 1, stop = 3)

#Major Groups
df %>% group_by(SOC2010Major) %>% tally()

#SubMajor Groups
df %>% group_by(SOC2010SubMajor) %>% tally()


#match occupational group codes (Major, SubMajor and Minor) to their titles
#import SOC2010 occupations 
soc2010indexversion705june2018 <- read_csv("~/Desktop/SARS-COV-2/Research proposals /ALSPAC Co90 COVID-19 Survey/1st round/Data/Occupations/soc2010indexversion705june2018.csv", 
                                           col_types = cols(SOC2010Major = col_character(), SOC2010SubMajor = col_character(), SOC2010Minor = col_character(), SOC2010code = col_character()))
socdf <- soc2010indexversion705june2018

#contact patterns in relation to HCW and keyworker status 
#HCW
df %>% group_by(covid1_5035) %>% summarise(mean = mean(total_contacts, na.rm = TRUE), median = median(total_contacts, na.rm = TRUE), sd = sd(total_contacts, na.rm = TRUE), min = min(total_contacts, na.rm = TRUE), max = max(total_contacts, na.rm = TRUE), Quant25 = quantile(total_contacts, probs = (0.25), na.rm =TRUE), Quant75 = quantile(total_contacts, probs = (0.75), na.rm =TRUE), N = n()) 
df %>% group_by(covid1_5035) %>% summarise(mean = mean(total_physical, na.rm = TRUE), median = median(total_physical, na.rm = TRUE), sd = sd(total_physical, na.rm = TRUE), min = min(total_physical, na.rm = TRUE), max = max(total_physical, na.rm = TRUE), Quant25 = quantile(total_physical, probs = (0.25), na.rm =TRUE), Quant75 = quantile(total_physical, probs = (0.75), na.rm =TRUE), N = n()) 
df %>% group_by(covid1_5035) %>% summarise(mean = mean(total_face_to_face, na.rm = TRUE), median = median(total_face_to_face, na.rm = TRUE), sd = sd(total_face_to_face, na.rm = TRUE), min = min(total_face_to_face, na.rm = TRUE), max = max(total_face_to_face, na.rm = TRUE), Quant25 = quantile(total_face_to_face, probs = (0.25), na.rm =TRUE), Quant75 = quantile(total_face_to_face, probs = (0.75), na.rm =TRUE), N = n()) 

#keyworker
df %>% group_by(covid1_5045) %>% summarise(mean = mean(total_contacts, na.rm = TRUE), median = median(total_contacts, na.rm = TRUE), sd = sd(total_contacts, na.rm = TRUE), min = min(total_contacts, na.rm = TRUE), max = max(total_contacts, na.rm = TRUE), Quant25 = quantile(total_contacts, probs = (0.25), na.rm =TRUE), Quant75 = quantile(total_contacts, probs = (0.75), na.rm =TRUE), N = n()) 
df %>% group_by(covid1_5045) %>% summarise(mean = mean(total_physical, na.rm = TRUE), median = median(total_physical, na.rm = TRUE), sd = sd(total_physical, na.rm = TRUE), min = min(total_physical, na.rm = TRUE), max = max(total_physical, na.rm = TRUE), Quant25 = quantile(total_physical, probs = (0.25), na.rm =TRUE), Quant75 = quantile(total_physical, probs = (0.75), na.rm =TRUE), N = n()) 
df %>% group_by(covid1_5045) %>% summarise(mean = mean(total_face_to_face, na.rm = TRUE), median = median(total_face_to_face, na.rm = TRUE), sd = sd(total_face_to_face, na.rm = TRUE), min = min(total_face_to_face, na.rm = TRUE), max = max(total_face_to_face, na.rm = TRUE), Quant25 = quantile(total_face_to_face, probs = (0.25), na.rm =TRUE), Quant75 = quantile(total_face_to_face, probs = (0.75), na.rm =TRUE), N = n()) 


##########################################
#Regression 
##########################################

#prepare variables for regression analyses

#variables
#outcome = total_contacts 
#explanatory = age, sex, household size, lives with children, day, occupation (keyworker/healthcare worker/other), self-isolation 
#happy to be guided on whether age and household size should be categorical and not continuous 
#categorical will allow for non-linear trends

#age: factor 
df$age3

#sex: factor
df$sex_un <- as_factor(df$sex, levels = "labels") #male = 1; female = 2
df$sex_un
df <- mutate(df, sex2 = ifelse(sex_un == 1, "Male", "Female"))

df$sex2 <- as_factor(df$sex2)
df$sex2 

#household size: factor
df$household_size2

#lives with children: factor; yes/no
df$children_in_house
df <- mutate(df, Children = ifelse(children_in_house == "Children", "Yes", "No"))
df$Children <- as.factor(df$Children)
df$Children
df %>% group_by(Children) %>% tally()

#number of children in house: continuous 
df$N_0to17yo

#day (weekday/weekend)
df$yesterday2 <- as.factor(df$yesterday2)
df$yesterday2

#occupation: factor 
#create a new variable - occupation - respondents who self-classify as HCW and keyworker are classifed as HCW only
df$occupation
df$occupation2 <- as_factor(df$occupation)
df$occupation2 <- factor(df$occupation2, levels = c("Other", "HCW","Keyworker"))

#self-isolation covid1_3000: factor
df$selfisolation_un <- as_factor(df$covid1_3000, levels = "labels")
df <- df %>% replace_with_na(replace = list(selfisolation_un = c(-11,-10, -9, -1)))
df %>% group_by(selfisolation_un) %>% tally() #check all missing values coded as NA
#levels: 1 = No; 2 = Yes, am now; 3= Yes, but have stopped; 9 = Prefer not to say
attributes(df$selfisolation_un)
df %>% group_by(selfisolation_un) %>% tally()

#self-isolation covid1_3000 - replace missing values with unknown 

df$selfisolation_un <- as_factor(df$covid1_3000, levels = "labels")
attributes(df$selfisolation_un)
df$selfisolation_un <- dplyr::recode(df$selfisolation_un, `-9999` = "UNK", `-11` = "UNK", `-10` = "UNK", 
                                     `-9` = "UNK", `-1` = "UNK", `1` = "No", `2` = "Yes, am now", `3` = "Yes, but have stopped", `9` = "Prefer not to say")
df$selfisolation_un <- as_factor(df$selfisolation_un)
#re-level so no is reference levels
df$selfisolation_un <- factor(df$selfisolation_un, levels = c("No", "Yes, am now","Yes, but have stopped","Prefer not to say", "UNK"))

df %>% group_by(selfisolation_un) %>% tally() #check all missing values coded as UNK for modelling with pln
#levels: 1 = No; 2 = Yes, am now; 3= Yes, but have stopped; 9 = Prefer not to say
#change NA to UNK for modeling 


####################
#Negative binomial 

library(MASS) #for neg. binomial 
library(pscl) #for zero inflated and predicted probabilities 

#negative binomial null model 
summary(negbin<-glm.nb(total_contacts~1, data = df))
negbin0 <- glm.nb(total_contacts~1, data = df)
names(negbin0)
plot(negbin)

#parameters
r <- negbin0$R
mu <- negbin0$coefficients
r/(r+mu)
#1.029553 
1-1.029553
1-0.029553 #p

#plot fit 
nbfit = data.frame(X = seq(1:max(df$total_contacts)))
nbfit$Y = predprob(negbin)[1,1:250]

df %>% 
  # select(total_contacts) %>% 
  ggplot()+
  geom_point(data = nbfit,aes(x=X,y=Y),colour='red')+
  geom_point(aes(x=total_contacts,y=..density..),stat='bin')+
  xlab('Number of contacts, C')+
  ylab('Number of people with C contacts')+
  scale_x_log10()+
  scale_y_log10()+
  theme_minimal_grid()


#check negative binomial assumption
#Variance > mean 
var(df$total_contacts)
mean(df$total_contacts)

#Dispersion parameter and 95% CI 
negbin_multi$theta
negbin_multi$theta + qnorm(c(0.025, 0.975)) * negbin_multi$SE.theta

#proportion of zero's in data contributing to over dispersion not accounted for with neg. bin?
#look at zero inflation index; p0 = proportion of zeros 
mu <- mean(df$total_contacts)
df_p0 <- df %>% group_by(total_contacts) %>% tally()
P0 <- 0.415014 #2825/6807

#zi = zero infaltion index; zi = 1+log(P0)/mu
zi <- 1+log(0.415014)/mu
#0.763

#deviance 
deviance(negbin)

#age as categorical and household size as categorical 
#linear - for creating dataframe later for plotting observed vs predicted 
summary(linear <- glm(total_contacts ~ age3 + sex2 + household_size2 + Children + yesterday2 + occupation2 + selfisolation_un, data = df))

#neg binomial 
summary(negbin_multi <- glm.nb(total_contacts ~ age3 + sex2 + household_size2 + Children + yesterday2 + occupation2 + selfisolation_un, data = df))


#zero inflated neg binomial 
summary(zinb <- zeroinfl(total_contacts ~ age3 + sex2 + household_size2 + Children + yesterday2 + occupation2 + selfisolation_un, data = df, dist = "negbin"))

#Compare AIC between negative binomial and Zero inflated negative binomial 

aic_comparisons <- data.frame(OLS = AIC(linear), 
                              negb = AIC(negbin_multi), zinb = AIC(zinb))
knitr::kable(aic_comparisons, align = "l")

#univariable

summary(negbin_1 <- glm.nb(total_contacts ~ age3 ,data = df))

summary(negbin_2 <- glm.nb(total_contacts ~ sex2 ,data = df))

summary(negbin_3 <- glm.nb(total_contacts ~ household_size2, data = df))

summary(negbin_4 <- glm.nb(total_contacts ~ Children, data = df))

summary(negbin_5 <- glm.nb(total_contacts ~ yesterday2, data = df))

summary(negbin_6 <- glm.nb(total_contacts ~ occupation2, data = df))

summary(negbin_7 <- glm.nb(total_contacts ~ selfisolation_un, data = df))


#IRRs and CIs for negbinomial 

library(sjPlot)
#sjp.glm(negbin1, type = "pred", vars = parent, show.ci = TRUE)

tab_model(negbin_multi, show.aic = TRUE, show.dev = TRUE, show.loglik = TRUE, show.obs = TRUE, show.reflvl = TRUE, show.se = TRUE)
tab_model(negbin_1, show.aic = TRUE, show.dev = TRUE, show.loglik = TRUE, show.obs = TRUE, show.reflvl = TRUE, show.se = TRUE)
tab_model(negbin_2, show.aic = TRUE, show.dev = TRUE, show.loglik = TRUE, show.obs = TRUE, show.reflvl = TRUE, show.se = TRUE)
tab_model(negbin_3, show.aic = TRUE, show.dev = TRUE, show.loglik = TRUE, show.obs = TRUE, show.reflvl = TRUE, show.se = TRUE)
tab_model(negbin_4, show.aic = TRUE, show.dev = TRUE, show.loglik = TRUE, show.obs = TRUE, show.reflvl = TRUE, show.se = TRUE)
tab_model(negbin_5, show.aic = TRUE, show.dev = TRUE, show.loglik = TRUE, show.obs = TRUE, show.reflvl = TRUE, show.se = TRUE)
tab_model(negbin_6, show.aic = TRUE, show.dev = TRUE, show.loglik = TRUE, show.obs = TRUE, show.reflvl = TRUE, show.se = TRUE)
tab_model(negbin_7, show.aic = TRUE, show.dev = TRUE, show.loglik = TRUE, show.obs = TRUE, show.reflvl = TRUE, show.se = TRUE)
tab_model(zinb, show.aic = TRUE, show.dev = TRUE, show.loglik = TRUE, show.obs = TRUE, show.reflvl = TRUE, show.se = TRUE)

#######
#Assess overall significance of each term using LRT - implement in drop1 

drop1(negbin_multi, test = "LRT")
drop1(negbin_multi2, test = "LRT")
drop1(negbin_multi3, test = "LRT")
drop1(negbin_multi4, test = "LRT")
drop1(negbin_multi5, test = "LRT") #model including  interaction between presence of children and household size 

drop1(negbin_1, test = "LRT")
drop1(negbin_2, test = "LRT")
drop1(negbin_3, test = "LRT")
drop1(negbin_4, test = "LRT")
drop1(negbin_5, test = "LRT")
drop1(negbin_6, test = "LRT")
drop1(negbin_7, test = "LRT")

###############################################################################################
#Figures

##########
# Figure 1a - distribution of participants age, coloured by sex 
df %>% group_by(sex2) %>% summarise(mean = mean(covid1_9650)) #male = 1, female = 2
sex_pp <- ggplot(df, aes(x = covid1_9650, fill = sex2)) + geom_histogram(position = "identity", alpha = 0.5) + 
  scale_fill_manual(values=c("darkblue", "#E69F00"), name = "Sex") + 
  scale_x_continuous(name="Age of participant", breaks=seq(20,90,5)) + ylab("Count") + theme_minimal_grid()
sex_pp
#save_plot("Sex_pp_histogram.tiff", sex_pp, base_width = 5.2, dpi = 300)

fig1a <- ggplot(df, aes(x = covid1_9650, fill = sex2)) + geom_histogram(position = "identity", alpha = 0.5) + 
  scale_fill_manual(values=c("#E69F00", "darkblue"), name = "Sex") + 
  scale_x_continuous(name="Age of participant", breaks=seq(20,90,5)) + ylab("Count") + theme_minimal_grid()
fig1a

##########
# Figure 1b
#plot fit of glm.nb 
fig1b <- df %>% group_by(total_contacts) %>%
  summarise(Count=n()) %>%
  ggplot(aes(x=total_contacts,y=Count)) +
  geom_point(color="darkblue") +
  scale_y_continuous(trans='log10') + 
  geom_smooth(method="glm.nb", se=TRUE) + theme_minimal_grid() + xlab("Total contacts")
fig1b
#save_plot("negbin_fit.tiff", plot_negbin, base_width = 5.2, dpi = 300)

#Figure 1 - patch work

library(patchwork)
Figure1 <- fig1a + fig1b + plot_annotation(tag_levels = 'a') + plot_layout(ncol=2)
Figure1

#ggsave("Figure1.tiff", Figure1, scale = 1, width = 7, units = c("in"), dpi = 200)

#ggsave("Figure1.tiff", Figure1,  height = 4, units = c("in"), dpi = 200)

##########
#Figure 2

#bubble plot with smoothed mean for household size by age of respondent
#Figure 2a
df_meanhh_age <- df %>% group_by(covid1_9650) %>% summarise(household_size = mean(household_size), Count = n())

Fig2a <- df %>% group_by(covid1_9650) %>% summarise(household_size = mean(household_size), Count = n()) %>% 
  ggplot(aes(x = as.numeric(covid1_9650), y=household_size, size = Count)) + geom_point(alpha=0.3) + 
  ylab("Mean household size") + xlab("Age of participant") + scale_size(limits = c(1,1658), breaks = c(25, 50, 100, 200, 400, 800, 1200, 1600)) +
  geom_smooth(show.legend = "FALSE") + theme_minimal_grid()
Fig2a

#smooth mean for household size by age of respondent
df %>% group_by(covid1_9650, children_in_house) %>% summarise(household_size = mean(household_size)) %>% 
  ggplot(aes(x = covid1_9650, y=household_size, colour = children_in_house)) + geom_point(alpha=0.3) + 
  ylab("Mean household size") + xlab("Age of participant") + geom_smooth() + theme_minimal_grid()


#box plot: total contacts by household size 
#Figure 2c
hh_mean <- df %>% group_by(household_size) %>% summarise(total_contacts = mean(total_contacts))
Fig2c <- df %>% 
  ggplot(aes(x = as.factor(household_size), y=total_contacts+1))+
  geom_boxplot(varwidth = TRUE, alpha=0.6) +
  geom_point(data= hh_mean, color = "blue", shape = "cross", size = 3) + 
  scale_y_log10() +
  ylab("Total contacts+1") + 
  xlab("Household size") +
  theme_minimal_grid()
Fig2c

#cut y scale at 0.01
Fig2c001 <- df %>% 
  ggplot(aes(x = as.factor(household_size), y=total_contacts))+
  geom_boxplot(varwidth = TRUE, alpha=0.6) +
  geom_point(data= hh_mean, color = "blue", shape = "cross", size = 3) + 
  scale_y_log10(expand = expansion(mult = c(0.1,0.1)), breaks=c(.01,.1,1,10,100,10000)) +
  ylab("Total contacts") + 
  xlab("Household size") +
  theme_minimal_grid()
Fig2c001


#bubble plot with smooth mean for household size by age of respondent
#Figure 2b
Fig2b <- df %>% group_by(covid1_9650, children_in_house) %>% summarise(household_size = mean(household_size), Count = n()) %>% 
  ggplot(aes(x = as.numeric(covid1_9650), y=household_size, size = Count, colour = children_in_house)) + geom_point(alpha=0.3) + 
  ylab("Mean household size") + xlab("Age of participant") + scale_size(limits = c(1,1658), breaks = c(25, 50, 100, 200, 400, 800, 1200, 1600)) +
  geom_smooth(show.legend = "FALSE") + theme_minimal_grid() + scale_color_discrete("Household") 
Fig2b

#box plot number of daily contacts by respondent age, stratified by children in house; mean with cross
ggplot(df, aes(x=age3, y=total_contacts+1)) +
  geom_boxplot(aes(fill=children_in_house), position=position_dodge(.9), alpha=0.3) +
  stat_summary(fun=mean, geom="point", aes(group=children_in_house), position=position_dodge(.9), 
               color="black", size=2, shape = "cross") + scale_y_log10() + theme_minimal_grid() +
  ylab("Total contacts+1") + xlab("Age of participant") + scale_fill_discrete("Household")

#cut scale at 0.01
ggplot(df, aes(x=age3, y=total_contacts)) +
  geom_boxplot(aes(fill=children_in_house), position=position_dodge(.9), alpha=0.3) +
  stat_summary(fun=mean, geom="point", aes(group=children_in_house), position=position_dodge(.9), 
               color="black", size=2, shape = "cross") + scale_y_log10(breaks=c(.01,.1,1,10,100,10000)) + theme_minimal_grid() +
  ylab("Total contacts") + xlab("Age of participant") + scale_fill_discrete("Household")


#box plot number of daily contacts by respondent age, stratified by children in house; mean with cross and number of respondents plotted above each box 
#useful tutorial 
#https://waterdata.usgs.gov/blog/boxplots/

#Figure 2d
n_fun <- function(x){
  return(data.frame(y = 0.09*30,
                    label = length(x)))
}

ggplot(df, aes(x=age3, y=total_contacts+1)) +
  geom_boxplot(aes(fill=children_in_house), position=position_dodge(.9), alpha=0.3) +
  stat_summary(fun=mean, geom="point", aes(group=children_in_house), position=position_dodge(.9), color="black", size=2, shape = "cross") + 
  scale_y_log10(expand = expansion(mult = c(0,0.1))) + theme_minimal_grid() +
  ylab("Total contacts + 1") + xlab("Age of participant") + scale_fill_discrete("Household") + 
  stat_summary(fun.data = n_fun, geom = "text", aes(group=children_in_house), hjust = 0.5, position = position_dodge(1))

#cut y scale at 0.01
Fig2d001 <- ggplot(df, aes(x=age3, y=total_contacts)) +
  geom_boxplot(aes(fill=children_in_house), position=position_dodge(.9), alpha=0.3) +
  stat_summary(fun=mean, geom="point", aes(group=children_in_house), position=position_dodge(.9), color="black", size=2, shape = "cross") + 
  scale_y_log10(expand = expansion(mult = c(0,0.1)), breaks=c(.01,.1,1,10,100,10000)) + theme_minimal_grid() +
  ylab("Total contacts") + xlab("Age of participant") + scale_fill_discrete("Household") + 
  stat_summary(fun.data = n_fun, geom = "text", aes(group=children_in_house), hjust = 0.5, position = position_dodge(1))
Fig2d001

#########
#Final multipanel Figure 2

Figure2 <- Fig2a + Fig2b +  Fig2c001 + Fig2d001 + plot_annotation(tag_levels = 'a') + plot_layout(ncol=2)
Figure2
#ggsave("Figure2.tiff", Figure2, scale = 1, width = 1, height = 7, units = c("in"),
       dpi = 200)

#check number of observations in each group 
boxplot_children <- df %>% group_by(age3, children_in_house) %>% summarise(count = n()) 
#correct 

#Contacts plotted by occupational status: HCW, keyworker, other 
#Figure 3 
occupation_facet3 <- df %>% ggplot(aes(x = total_face_to_face, y=total_physical, colour = occupation)) + geom_count(alpha=0.5)  + scale_size_binned_area(max_size = 8, "Count") +
  ylab("Physical contacts") + xlab("Face-to-face contacts") + theme_minimal_grid() + scale_colour_manual(values = c("#E69F00", "purple", "#009E73"),  "Occupation") +
  guides(colour = guide_legend(override.aes = list(size=4))) + facet_grid(.~occupation2) + scale_y_log10() + scale_x_log10()
occupation_facet3
#save_plot("Figure3.tiff", occupation_facet3, base_width = 7, dpi = 300)

#Table 2
#Top 5 occupational ONS group titles for each occupation: healthcare worker, keyworker and other
library(knitr)
df %>% filter(occupation == "Keyworker") %>% group_by(SOC2010Title) %>% summarise(N=n()) %>% knitr::kable(align = "c")

##########
#Supplementary figures 

#Supplementary Figure 1
ggplot(df, aes(date_ymd, ..count..)) + geom_histogram() + xlab("Date") + scale_x_date(date_minor_breaks = "1 day") + ylab("Count") + theme_minimal_grid()

#plot predicted probabilities
#Supplementary Figure 2
#this was helpful 
#https://francish.netlify.app/post/poisson-and-negative-binomial-regression-using-r/ 

po.nb <- predprob(negbin_multi) %>% colMeans
po.zinb <- predprob(zinb) %>% colMeans

df_SupplF2 <- data.frame(x = 0:max(df$total_contacts), 
                         NegBin = po.nb, Zinb = po.zinb)

obs <- table(df$total_contacts) %>% prop.table() %>% data.frame #Observed
names(obs) <- c("x", 'Observed')

p1 <- predict(linear) %>% round() %>% table %>% prop.table %>% data.frame #for OLS
names(p1) <- c('x', 'OLS')

tmp <- merge(p1, obs, by = 'x', all = T)
tmp$x <- as.numeric(as.character(tmp$x))

comb <- merge(tmp, df_SupplF2, by = 'x', all = T)
comb[is.na(comb)] <- 0
comb2 <- comb[2:52, ] #just for the first 50 results, including zero

mm <- melt(comb2, id.vars = 'x', value.name = 'prob', variable.name = 'Model')
mm <- filter(mm, Model != "OLS") #remove linear model 

mm %>% filter(!Model == "Observed") %>%
  ggplot(aes(x = x, y = prob, group = Model, col = Model)) +
  geom_line(aes(lty = Model), lwd = 2) +
  theme_bw() +
  labs(x = "Number of contacts, C", y = 'Probability') +
  scale_color_manual(values = c('blue','red')) +
  scale_linetype_manual(values = c('dashed', 'dashed')) + theme_minimal_grid()

#Instead of plotting predicted as observed proportion of individuals (freq) with C contacts, plot number of individuals with C contacts
mm <- mm %>% mutate(n = prob*6807)

mm %>% 
  ggplot(aes(x = x, y = n, group = Model, col = Model)) +
  geom_line(aes(lty = Model), lwd = 2) +
  theme_bw() +
  labs(x = "Number of contacts, C", y = 'Number of people with C contacts') +
  scale_color_manual(values = c('black', 'blue','red')) +
  scale_linetype_manual(values = c('solid', 'dashed', 'dashed')) + theme_minimal_grid()

df %>% group_by(total_contacts) %>%
  summarise(Count=n()) %>%
  ggplot(aes(x=total_contacts,y=Count)) +
  geom_bar(stat = "identity") + lims(x = c(-1,50)) +
  labs(x = "Number of contacts, C", y = 'Count') + theme_minimal_grid()

#Combine figures to show observed count of number of contacts and predicted count of number of contacts
SupplFig2_data <- df %>% group_by(total_contacts) %>% summarise(Count=n()) 

SupplFig2_data2 <- mm %>% filter(!Model == "Observed")

SuppFig2_modelfit <- ggplot() + geom_line(data = SupplFig2_data2, aes(x = x, y = n, group = Model, col = Model, lty = Model), lwd = 1) +
  geom_bar(data = SupplFig2_data, aes(x=total_contacts,y=Count), stat = "identity", alpha=0.4) + 
  labs(x = "Number of contacts, C", y = 'Number of people with C contacts') + theme_minimal_grid() + lims(x = c(-1,50))
#save_plot("SuppFig2_modelfit.tiff", SuppFig2_modelfit, base_width = 5.2, dpi = 300)

#zero counts: compare observed (2825) to expected for negative binomial and zero-inflated; similar between the two, possibly marginaly better for the ZINB.
round(c("Obs" = sum(df$total_contacts < 1), "NB" = sum(dnbinom(0, mu = fitted(negbin_multi), size = negbin_multi$theta)),"ZINB" = sum(predict(zinb, type = "prob")[,1])))
