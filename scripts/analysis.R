#Set Options
options(warn=-1)
options(scipen=999)

#Load Libraries
library(reshape)
library(plyr) 
library(dplyr)
library(forcats)
library(lubridate)
library(ggplot2)
library(xlsx)
library(sp)
library(rgdal)
library(viridis)
library(sjPlot)
library(sjmisc)
library(pscl)
library(margins)
library(effects)

#Link to source for cross-tabs
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

# Read in cleaned ODIN data from Pennsylvania [put website]
odin_pa <- read.csv("C:/Users/barboza-salerno.1/Downloads/odin-analysis-master/odin-analysis-master/data/Overdose_Information_Network_Data_CY_January_2018_-_Current_Monthly_County_State_Police-3.csv", na.strings=c("","NA"))

#Do some data cleaning
#Race and Ethnicity -------------------------------------
odin_pa$hisp<-ifelse(odin_pa$Ethnicity.Desc=='Hispanic',1,0)
odin_pa$race_eth[odin_pa$hisp == 0 & odin_pa$Race=="White"]<-"NHWhite"
odin_pa$race_eth[odin_pa$hisp == 0 & odin_pa$Race=="Black"]<-"NHBlack"
odin_pa$race_eth[odin_pa$hisp == 1 ]<-"Hisp"
odin_pa$black<-ifelse(odin_pa$Race=='Black',1,0)
odin_pa$white<-ifelse(odin_pa$Race=='White',1,0)

#Response time-------------------------------------------
odin_pa$Response.Time.Desc[odin_pa$Response.Time.Desc == "DID NOT WORK"]<-NA
odin_pa$Response.Time.Desc <- factor(odin_pa$Response.Time.Desc)

#Date & Time__________________________________

# Strip whitespace and ensure clean strings
date_clean <- trimws(odin_pa$Incident.Date)
time_clean <- trimws(odin_pa$Incident.Time)

datetime_str <- paste(date_clean, time_clean)

odin_pa$DT <- mdy_hms(datetime_str)

sum(is.na(odin_pa$DT))  # How many failed
head(odin_pa$DT)

#Insert hour, month and year as separate variables
odin_pa$hour <- hour(odin_pa$DT) 
odin_pa$month <- month(odin_pa$DT)  
odin_pa$year <- year(odin_pa$DT)  

#Ages into numeric var______________________________
odin_pa <- odin_pa %>%
  mutate(age = fct_recode(Age.Range,
                          "1" = "0 - 9"  ,
                          "1" = "10-14" ,
                          "1"= "15 - 19",
                          "2" = "20 - 24",
                          "2" = "25 - 29",
                          "3"=  "30 - 39" ,
                          "4" = "40 - 49" ,
                          "5" = "50 - 59"  ,
                          "6" = "60 - 69" ,
                          "6" = "70 - 79" ,
                          "6" = "80 - *" 
                          )
         )

#Revival Action ___________________________________         
odin_pa <- odin_pa %>%
  mutate(revived = fct_recode(Revive.Action.Desc,
                          "Arrest" = "ARREST"  ,
                          "Hospital" = "HOSPITAL CONSCIOUS" ,
                          "Hospital"="HOSPITAL UNCONSCIOUS",
                          "Refused" = "REFUSED TRANSPORT",
                          "Other" = "RELEASED",
                          "Referred to treatment"=  "TRANSPORTED TO TREATMENT" ,
                          "Referred to treatment" = "VERBALLY REFERRED TO TREATMENT" ,
                          "Other" = "OTHER"  ,
                          "NA" = "DON'T KNOW" 
                          )
  )

odin_pa <- odin_pa %>%
  mutate(drugs = fct_recode(Susp.OD.Drug.Desc,
                              "Alcohol" = "ALCOHOL"  ,
                              "Heroin" = "HEROIN" ,
                              "Fentanyl"="FENTANYL",
                              "Fentanyl" = "FENTANYL ANALOG/OTHER SYNTHETIC OPIOID",
                              "Pharma" = "PHARMACEUTICAL STIMULANT",
                              "Pharma"=  "PHARMACEUTICAL OTHER" ,
                              "Pharma" = "PHARMACEUTICAL OPIOID" ,
                              "Benzos" = "BENZODIAZEPINES (I.E.VALIUM, XANAX, ATIVAN, ETC)"  ,
                              "Methadone" = "METHADONE", 
                              "Suboxone" = "SUBOXONE",
                              "MJ" = "MARIJUANA",
                              "MJ" = "SYNTHETIC MARIJUANA",
                              "Meth" = "METHAMPHETAMINE",
                              "Carf" = "CARFENTANIL",
                              "Salts" = "BATH SALTS",
                              "unknown" = "unknown"
                        
                        )
  )

odin_pa$heroin<-ifelse(odin_pa$drugs=='Heroin',1,0)
odin_pa$fentanyl<-ifelse(odin_pa$drugs=='Fentanyl',1,0)
odin_pa$pharma<-ifelse(odin_pa$drugs=='Pharma',1,0)

#Before any analysis let's make sure we have the right selections
#In order to know how many unique incidents there were, we need to consider that each incident could have multiple victims. 
#The dataset does not take that into consideration, so we need to apply some filters
#The following dataset provides the count of all drugs involved with the incident using the
#key that identifies a unique record containing the details of a single incident (identified by date, time and location)
unique_incidents <- odin_pa[!duplicated(odin_pa[,c('Incident.ID')]),] # of the 14,400 total cases, 10,290 were unique incidents
duplicated_incidents <- odin_pa[duplicated(odin_pa[,c('Incident.ID')]),] # 4,110 were not unique, there are duplicate victims and/or multiple drugs suspected of causing the OD

#Below we get all incidents where each line represents a unique victim regardless of how many drugs were suspected 
#in causing the OD
#The following dataset contains incidents with unique victim IDs (i.e. each incident may have had more than 1 victim) using the
#key that identifies a unique record for a victim.
#According to the result, there were 10,465 unique incidents with unique victim IDs

unique_incidents_victims <- odin_pa[!duplicated(odin_pa[,c('Incident.ID','Victim.ID')]),]
duplicated_incidents_victims <- odin_pa[duplicated(odin_pa[,c('Incident.ID','Victim.ID')]),]

##Finally, this dataset selects incidents with multiple distinct victims (note, 
#some cases have multiple victimizations of the same person, this code excludes individuals with repeat victimizations 
#as it is not necessary to include them (i.e. their characteristics do not change))
multiple_unique_victims <-unique_incidents_victims %>% 
dplyr::group_by(Incident.ID) %>%
dplyr::add_count(Incident.ID) %>%
dplyr::distinct(Incident.ID, .keep_all = TRUE)

mean(multiple_unique_victims$n) #average number of persons per incident
table(multiple_unique_victims$n) #distribution of number of unique victims of drug OD per incident

aggregate(n ~ race_eth, data=multiple_unique_victims, mean)
aggregate(Dose.Count ~ race_eth, data=multiple_unique_victims, mean)
aggregate(Dose.Unit ~ race_eth, data=multiple_unique_victims, mean)

#incidents and victims with each line representing a different drug suspected of causing the OD
#this dataset is the number of incidents with victims that had multiple drugs suspected of causing the OD using the
#unique identifier for each suspected drug causing the overdose. 
unique_incidents_victims_drugs <- odin_pa[!duplicated(odin_pa[,c('Incident.ID','Victim.ID', 'Victim.OD.Drug.ID')]),]
duplicated_incidents_victims_drugs <- odin_pa[duplicated(odin_pa[,c('Incident.ID','Victim.ID', 'Victim.OD.Drug.ID')]),]

multiple_unique_victims_polydrugs <-unique_incidents_victims_drugs %>% #this dataset selects incidents with multiple distinct victims and counts
  #how many drugs were suspected in causing their overdose
  dplyr::group_by(Incident.ID) %>%
  dplyr::add_count(Victim.ID) %>%
  dplyr::distinct(Victim.ID, .keep_all = TRUE)

multiple_unique_victims_polydrugs$polydrug<-ifelse(multiple_unique_victims_polydrugs$n >1,1,0)
newdata <- multiple_unique_victims_polydrugs[ which(multiple_unique_victims_polydrugs$race_eth=='NHWhite'), ]
newdata$Naloxone_Admin <- ifelse(newdata$Naloxone.Administered == "Y", 1, 0)
crosstab(newdata, row.vars = "Naloxone_Admin", col.vars = "polydrug", type = "c")
chisq.test(newdata$Naloxone_Admin, newdata$polydrug, correct = FALSE)

multiple_unique_victims$missing<-ifelse(is.na(multiple_unique_victims$Survive),1,0)
#same as unique incidents
mean(multiple_unique_victims_polydrugs$n)
table(multiple_unique_victims_polydrugs$n)
aggregate(n ~ race_eth, data=multiple_unique_victims_polydrugs, mean)

#same result using the following code
polydruguse <- aggregate(Victim.OD.Drug.ID ~ Incident.ID+Victim.ID, data = unique_incidents_victims_drugs, FUN = length)
mean(polydruguse$Victim.OD.Drug.ID)
table(polydruguse$Victim.OD.Drug.ID)

##########Descriptive statistics
table(multiple_unique_victims_polydrugs$race_eth)

#Chi-Square of indep vars by race
crosstab(multiple_unique_victims_polydrugs, row.vars = "age", col.vars = "race_eth", type = "c")
chisq.test(multiple_unique_victims_polydrugs$age, multiple_unique_victims_polydrugs$race_eth, correct = FALSE)

#Descriptives on dates
odin_pa %>%
  ggplot(aes(wday(DT, label = TRUE))) +
  geom_bar(fill = "midnightblue", alpha = 0.8) +
  labs(x = NULL,
       y = "Number of events")

race_hour_OD <- unique_incidents_victims %>% dplyr::group_by(race = race_eth, hour = hour(DT)) %>% 
  dplyr::count()  %>% tidyr::spread(race, n) 

race_month_OD <- unique_incidents_victims %>% dplyr::group_by(race = race_eth, month = month(DT)) %>% 
  dplyr::count()  %>% tidyr::spread(race, n) 

#save to excel for better plotting, I did not like any options here
#write.xlsx(data.frame(race_hour_OD), "race_hour_OD_excel.xlsx")
#write.xlsx(data.frame(race_month_OD), "race_month_OD_excel.xlsx")

DRUG_OD <- unique_incidents_victims_drugs %>% dplyr::group_by(ID = Incident.ID, drug = Susp.OD.Drug.Desc) %>% 
  dplyr::count()  %>% tidyr::spread(drug, n) 

d<- unique_incidents_victims %>% #max od in a single day
  select(DT, race_eth) %>%
  dplyr::group_by(race = race_eth, year = year(DT), month = month(DT), day = day(DT)) %>% 
  dplyr::count()  %>% tidyr::spread(race, n) 

d<- as.data.frame(d)
write.xlsx(d, "d.xlsx")

d<- unique_incidents_victims %>%
  select(DT, race_eth) %>%
  dplyr::group_by(race = race_eth, hour = hour(DT)) %>% 
  dplyr::count()  %>% 
  filter(hour>0) %>% tidyr::spread(race, n)

colors <- c("Hisp" = "blue", "NHBlack" = "red", "NHWhite" = "green")

ggplot(d, aes(x=hour)) +
  geom_line(aes(hour, Hisp*50/3, color = "Hisp"), size=.8, linetype="dashed", size=.5) +
  geom_point(aes(hour, Hisp*50/3, color = "Hisp"),size=1) +
  geom_line(aes(hour, NHBlack*50/3, color = "NHBlack"),  linetype="dashed", size=.5) +
  geom_point(aes(hour, NHBlack*50/3,  color = "NHBlack"),size=1) +
  geom_line(aes(hour, NHWhite, color = "NHWhite"), linetype="dashed", size=.5 ) +
  geom_point(aes(hour, NHWhite, color = "NHWhite"),size=1) +
  scale_y_continuous(
    name = "White", 
    sec.axis = sec_axis(~ . *3/50  , name = "Non-White")) + 
  theme(panel.background = element_rect(fill= "white", color = "black", 
     linetype = "solid"), legend.position="top")  + 
  labs(y = "Number of Overdose Responses",
       x = "Hour of Day")  + scale_colour_manual(values = colors)  + scale_color_discrete(name = "Race/Ethnicity", 
      labels=c("Latino", "Non-Hispanic Black", "Non-Hispanic White")) +
  ggtitle("Hourly Distribution of Drug Overdose Responses by Race/Ethnicity") 
  
d<- unique_incidents_victims %>%
  select(DT, race_eth) %>%
  dplyr::group_by(race = race_eth, month = month(DT)) %>% 
  dplyr::count()  %>% 
  tidyr::spread(race, n)

ggplot(d, aes(x=month)) +
  geom_line(aes(month, Hisp*50/3, color = "Hisp"), size=.8, linetype="dashed", size=.5) +
  geom_point(aes(month, Hisp*50/3, color = "Hisp"),size=1) +
  geom_line(aes(month, NHBlack*50/3, color = "NHBlack"),  linetype="dashed", size=.5) +
  geom_point(aes(month, NHBlack*50/3,  color = "NHBlack"),size=1) +
  geom_line(aes(month, NHWhite, color = "NHWhite"), linetype="dashed", size=.5 ) +
  geom_point(aes(month, NHWhite, color = "NHWhite"),size=1) +
  scale_y_continuous(
    name = "White", 
    sec.axis = sec_axis(~ . *3/50  , name = "Non-White")) + 
  theme(panel.background = element_rect(fill= "white", color = "black", 
                                        linetype = "solid"), legend.position="top")  + 
  labs(y = "Number of Overdose Responses",
       x = "Month of Year")  + scale_colour_manual(values = colors)  + scale_color_discrete(name = "Race/Ethnicity", 
  labels=c("Latino", "Non-Hispanic Black", "Non-Hispanic White")) + scale_x_continuous(breaks=c(1:12)) +
  ggtitle("Monthly Distribution of Drug Overdose Responses by Race/Ethnicity") 

round(prop.table(table(unique_incidents_victims$race_eth)),3)
round(prop.table(table(unique_incidents_victims$age)),3)

unique_incidents_victims %>% 
  mutate(wday = wday(DT, label = TRUE)) %>% 
  ggplot(aes(x = wday)) +
  geom_bar()

unique_incidents_victims %>% #This is interesting but useless
  mutate(minute = minute(DT)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_dose = mean(Dose.Unit, na.rm = TRUE),
    n = n()) %>% 
  ggplot(aes(minute, avg_dose)) +
  geom_line()

unique_incidents_victims %>% 
  filter(race_eth=="NHWhite") %>% 
  mutate(month = month(DT, label = TRUE)) %>% 
  ggplot(aes(x = month)) +
  geom_bar()

unique_incidents_victims %>% 
  filter(race_eth=="NHBlack") %>% 
  mutate(month = month(DT, label = TRUE)) %>% 
  ggplot(aes(x = month)) +
  geom_bar()

unique_incidents_victims %>% 
  filter(race_eth=="Hisp") %>% 
  mutate(month = month(DT, label = TRUE)) %>% 
  ggplot(aes(x = month)) +
  geom_bar()

unique_incidents_victims %>% 
  filter(race_eth=="NHWhite") %>% 
  count(week = floor_date(DT, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line() 

unique_incidents_victims %>% 
  filter(race_eth=="NHBlack") %>% 
  count(week = floor_date(DT, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line() 

unique_incidents_victims %>% 
  filter(race_eth=="Hisp") %>% 
  count(week = floor_date(DT, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line() 

unique_incidents_victims %>% 
  filter(race_eth=="NHWhite") %>% 
  mutate(hour = hour(DT)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()

unique_incidents_victims %>% 
  filter(race_eth=="NHBlack") %>% 
  mutate(hour = hour(DT)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()

unique_incidents_victims %>% 
  filter(race_eth=="Hisp") %>% 
  mutate(hour = hour(DT)) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()


unique_incidents_victims$age_num <- as.numeric(as.character(unique_incidents_victims$age))
unique_incidents_victims$Naloxone_Admin <- ifelse(unique_incidents_victims$Naloxone.Administered=="Y", 1, 0) 
unique_incidents_victims$Survived <- ifelse(unique_incidents_victims$Survive=="Y", 1, 0) 

mod1 <- glm(Survived ~ Naloxone_Admin + age_num + hour +  month + year +
               heroin + pharma + fentanyl + Gender.Desc + white + black + black*Naloxone_Admin + white*Naloxone_Admin +
               Incident.County.Latitude+Victim.County.Longitude , family='binomial', data=unique_incidents_victims)
summary(mod1)
confint(mod1)
round(exp(coef(mod1)-1)*100,4)

pR2(mod1)
length(residuals(mod1)) #get number of observations
mod1_margins <- margins(mod1)

cplot(mod1, "age_num", what="predict", draw=TRUE)

allEffects(mod1)

plot(effect("white", mod1))
plot(effect("age_num", mod1))
mod_hisp <- glm(Survived ~ Naloxone_Admin * Gender.Desc * hisp, family='binomial', data=unique_incidents_victims)
no_naloxone_male = data.frame("hisp"=1, "Naloxone_Admin"=0, "Gender.Desc" = "Male")
naloxone_male = data.frame("hisp"=1, "Naloxone_Admin"=1, "Gender.Desc" = "Male")

no_naloxone_female = data.frame("hisp"=1, "Naloxone_Admin"=0, "Gender.Desc" = "Female")
naloxone_female = data.frame("hisp"=1, "Naloxone_Admin"=1, "Gender.Desc" = "Female")

predict(mod_hisp, no_naloxone_male, type = "response")
predict(mod_hisp, naloxone_male, type = "response")
predict(mod_hisp, no_naloxone_female, type = "response")
predict(mod_hisp, naloxone_female, type = "response")

mod_white <- glm(Survived ~  Naloxone_Admin * Gender.Desc * white , family='binomial', data=unique_incidents_victims)
no_naloxone_male = data.frame("white"=1, "Naloxone_Admin"=0, "Gender.Desc" = "Male")
naloxone_male = data.frame("white"=1, "Naloxone_Admin"=1, "Gender.Desc" = "Male")

no_naloxone_female = data.frame("white"=1, "Naloxone_Admin"=0, "Gender.Desc" = "Female")
naloxone_female = data.frame("white"=1, "Naloxone_Admin"=1, "Gender.Desc" = "Female")

predict(mod_white, no_naloxone_male, type = "response")
predict(mod_white, naloxone_male, type = "response")
predict(mod_white, no_naloxone_female, type = "response")
predict(mod_white, naloxone_female, type = "response")

#Note, for plots the order in which variables enter matters
mod_black <- glm(Survived ~ Naloxone_Admin *  Gender.Desc * black , family='binomial', data=unique_incidents_victims)
no_naloxone_male = data.frame("black"=1, "Naloxone_Admin"=0, "Gender.Desc" = "Male")
naloxone_male = data.frame("black"=1, "Naloxone_Admin"=1, "Gender.Desc" = "Male")

no_naloxone_female = data.frame("black"=1, "Naloxone_Admin"=0, "Gender.Desc" = "Female")
naloxone_female = data.frame("black"=1, "Naloxone_Admin"=1, "Gender.Desc" = "Female")

predict(mod_black, no_naloxone_male, type = "response")
predict(mod_black, naloxone_male, type = "response")
predict(mod_black, no_naloxone_female, type = "response")
predict(mod_black, naloxone_female, type = "response")


theme_set(theme_sjplot())
plot_model(mod_white, type = "pred", terms = c("Naloxone_Admin", "white"))
plot_model(mod_black, type = "pred", terms = c("Naloxone_Admin", "black"))
plot_model(mod_hisp, type = "pred", terms = c("Naloxone_Admin", "hisp"))

plot_model(mod_hisp, type = "int", terms = c("hisp", "Naloxone.Administered"))

plot_model(mod_hisp, type = "int")
plot_model(mod_white, type = "int")
plot_model(mod_black, type = "int")
#######################
x<-aggregate(Incident.ID ~ Incident.County.Name, FUN = length, data = unique_incidents_victims) %>%
  rename("nOD_all" = "Incident.ID")

x_black<-aggregate(Incident.ID ~ Incident.County.Name, FUN = length, data = subset(unique_incidents_victims, black==1))%>%
  rename("nOD_black" = "Incident.ID")
x_white<-aggregate(Incident.ID ~ Incident.County.Name, FUN = length, data = subset(unique_incidents_victims, white==1))%>%
  rename("nOD_white" = "Incident.ID")
x_hisp<-aggregate(Incident.ID ~ Incident.County.Name, FUN = length, data = subset(unique_incidents_victims, hisp==1))%>%
  rename("nOD_hisp" = "Incident.ID")


# pop_dat <- read.csv("/Users/gbarboza/Desktop/ODIN_PA/pop.csv")
library(tidycensus)

race_vars <- c(
  total = "B03002_001",          # Total population
  white_nonhisp = "B03002_003",  # White alone, not Hispanic or Latino
  black = "B03002_004",          # Black or African American alone
  hispanic = "B03002_012"        # Hispanic or Latino (of any race)
)

# Download data from 2020 ACS 5-year estimates
pop_dat <- get_acs(
  geography = "county",
  state = "PA",
  variables = race_vars,
  year = 2020,
  survey = "acs5",
  output = "wide"
) %>%
  mutate(
    NAME = stringr::str_remove(NAME, " County, Pennsylvania"),
    pct_white_nonhisp = 100 * white_nonhispE / totalE,
    pct_black = 100 * blackE / totalE,
    pct_hispanic = 100 * hispanicE / totalE
  )

# View result
head(pop_dat)

x <- merge(x_black, x, by = "Incident.County.Name", all.y = TRUE)
x <- merge(x_white, x, by = "Incident.County.Name", all.y = TRUE)
x <- merge(x_hisp, x, by = "Incident.County.Name", all.y = TRUE)



x[is.na(x)] = 0

pop_dat <- pop_dat %>% rename( "Incident.County.Name" = "NAME" )
overdose_rates <- merge(x, pop_dat, by = "Incident.County.Name")

overdose_rates$nOD_all_rate <- ((overdose_rates$nOD_all/overdose_rates$totalE)*100000)/2
overdose_rates$nOD_hisp_rate <- ((overdose_rates$nOD_hisp/overdose_rates$hispanicE)*100000)/2
overdose_rates$nOD_white_rate <- ((overdose_rates$nOD_white/overdose_rates$white_nonhispE)*100000)/2
overdose_rates$nOD_black_rate <- ((overdose_rates$nOD_black/overdose_rates$blackE)*100000)/2

penn <- readxl::read_xlsx("C:/Users/barboza-salerno.1/Downloads/PA_Population_2000_to_2022.xlsx")

penn <- penn %>% 
  mutate(urban = factor(`Rural/Urban Status, 2020`, levels = c(0, 1), labels = c("Rural", "Urban")))%>%
  rename( "Incident.County.Name" = "County" ) 

overdose_rates <- overdose_rates %>%
  inner_join(penn)

gd <- overdose_rates %>% 
  group_by(urban) %>% 
  summarise(nOD_all_rate = mean(nOD_all_rate))

ggplot(overdose_rates, aes(x = urban, y = nOD_all_rate, color = urban, fill = urban)) +
  geom_bar(data = gd, stat = "identity", alpha = .3) +
  ggrepel::geom_text_repel(aes(label = overdose_rates$NAME), color = "black", size = 2.5, segment.color = "grey") +
  geom_point() +
  guides(color = "none", fill = "none") +
  theme_bw() +
  labs(
    title = "Heroin Overdose Response Rate by Urbanicity",
    subtitle = "Source: Pennsylvania Overdose Information Network (2018 - 2019)",
    x = "Region",
    y = "OD RATE"
  )

no_classes <- 5
labels <- c()

#all

quantiles <- quantile(overdose_rates$nOD_all_rate, probs = seq(0, 1, length.out = no_classes + 1))

for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " – ", 
                             round(quantiles[idx + 1], 2)))
}
labels <- labels[1:length(labels)-1]

overdose_rates$OD_all_quantiles <- cut(overdose_rates$nOD_all_rate, 
breaks = quantiles, 
labels = labels, 
include.lowest = T)

pa_counties <- tigris::counties(state = "PA") %>%
  rename("Incident.County.Name" = "NAME") %>%
  dplyr::select(-STATEFP, -COUNTYFP, -COUNTYNS, -GEOID, -LSAD, -ALAND, -AWATER)

pa_counties <- pa_counties %>% left_join(overdose_rates)
p <- ggplot(data = pa_counties) +
  geom_sf(aes(fill = OD_all_quantiles), color = "white", size = 0.2) +
  coord_sf() +
  labs(
    x = NULL,
    y = NULL,
    title = "Spatial Distribution of Overdose Response Rates",
    subtitle = "Pennsylvania Counties",
    caption = "Source: Pennsylvania Overdose Information Network (2018–2019)"
  ) +
  scale_fill_viridis(
    option = "cividis",
    name = "Overdose Response Rate",
    discrete = TRUE,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = TRUE
    )
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),       # remove axis text (like 79W)
    axis.ticks = element_blank(),      # remove axis ticks
    panel.grid = element_blank()       # remove grid lines for a cleaner look
  )

p

# White overdose rate map
no_classes <- 8
quantiles <- quantile(pa_counties$nOD_white_rate, probs = seq(0, 1, length.out = no_classes + 1), na.rm = TRUE)
labels <- c()
for (i in 1:(length(quantiles) - 1)) {
  labels <- c(labels, paste0(round(quantiles[i], 2), " – ", round(quantiles[i + 1], 2)))
}
pa_counties$OD_white_quantiles <- cut(pa_counties$nOD_white_rate, breaks = quantiles, labels = labels, include.lowest = TRUE)

p_white <- ggplot(data = pa_counties) +
  geom_sf(aes(fill = OD_white_quantiles), color = "white", size = 0.2) +
  coord_sf() +
  labs(
    x = NULL, y = NULL,
    title = "Spatial Distribution of Overdose Response Rates [Whites]",
    subtitle = "Pennsylvania Counties",
    caption = "Source: Pennsylvania Overdose Information Network (2018–2019)"
  ) +
  scale_fill_viridis(
    option = "cividis",
    name = "Overdose Response Rate [Whites]",
    discrete = TRUE,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = TRUE
    )
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

p_white

# Black overdose rate map
no_classes <- 10
quantiles <- unique(quantile(pa_counties$nOD_black_rate, probs = seq(0, 1, length.out = no_classes + 1), na.rm = TRUE))
labels <- c()
for (i in 1:(length(quantiles) - 1)) {
  labels <- c(labels, paste0(round(quantiles[i], 2), " – ", round(quantiles[i + 1], 2)))
}
pa_counties$OD_black_quantiles <- cut(pa_counties$nOD_black_rate, breaks = quantiles, labels = labels, include.lowest = TRUE)

p_black <- ggplot(data = pa_counties) +
  geom_sf(aes(fill = OD_black_quantiles), color = "white", size = 0.2) +
  coord_sf() +
  labs(
    x = NULL, y = NULL,
    title = "Spatial Distribution of Overdose Response Rates [Black]",
    subtitle = "Pennsylvania Counties",
    caption = "Source: Pennsylvania Overdose Information Network (2018–2019)"
  ) +
  scale_fill_viridis(
    option = "cividis",
    name = "Overdose Response Rate [Black]",
    discrete = TRUE,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = TRUE
    )
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

p_black

# Latino overdose rate map
no_classes <- 14
quantiles <- unique(quantile(pa_counties$nOD_hisp_rate, probs = seq(0, 1, length.out = no_classes + 1), na.rm = TRUE))
labels <- c()
for (i in 1:(length(quantiles) - 1)) {
  labels <- c(labels, paste0(round(quantiles[i], 2), " – ", round(quantiles[i + 1], 2)))
}
pa_counties$OD_hisp_quantiles <- cut(pa_counties$nOD_hisp_rate, breaks = quantiles, labels = labels, include.lowest = TRUE)

p_hisp <- ggplot(data = pa_counties) +
  geom_sf(aes(fill = OD_hisp_quantiles), color = "white", size = 0.2) +
  coord_sf() +
  labs(
    x = NULL, y = NULL,
    title = "Spatial Distribution of Overdose Response Rates [Latinos]",
    subtitle = "Pennsylvania Counties",
    caption = "Source: Pennsylvania Overdose Information Network (2018–2019)"
  ) +
  scale_fill_viridis(
    option = "cividis",
    name = "Overdose Response Rate [Latinos]",
    discrete = TRUE,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = TRUE
    )
  ) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

p_hisp
