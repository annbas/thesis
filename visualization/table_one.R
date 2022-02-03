setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data/2020 COVID-NET cases")

### CREATING DATASET ###
#read in data
marsep<-read.csv("CDC REDCap_Mar-Sep.csv")
names(marsep)[names(marsep) == 'ï..caseid'] <- 'caseid'
octdec<-read.csv("Yale REDCap_Oct-Dec.csv")
names(octdec)[names(octdec) == 'ï..caseid'] <- 'caseid'

#subset variables to include in table 1
#(sex, age, race,ethnicity, month of collection date)
sub<-c("caseid","sex","dob","race","ethnic","admdate","poststdt1")

marsep <- marsep[sub]
octdec <- octdec[sub]

#convert date variables to dates
library(lubridate)

marsep$dob<-as.Date(marsep$dob, format="%m/%d/%Y")
marsep$admdate<-as.Date(marsep$admdate, format="%m/%d/%Y")
marsep$poststdt1<-as.Date(marsep$poststdt1, format="%m/%d/%Y")

octdec$dob<-as.Date(octdec$dob, format="%m/%d/%Y")
octdec$admdate<-as.Date(octdec$admdate, format="%m/%d/%Y")
octdec$poststdt1<-as.Date(octdec$poststdt1, format="%m/%d/%Y")

#stack the two .csv files to create the main dataset
covidnet<-rbind(marsep,octdec)


### SEX ###
  #convert sex from number to character
  covidnet$sex<-replace(covidnet$sex, covidnet$sex == 1, "Male")
  covidnet$sex<-replace(covidnet$sex, covidnet$sex == 2, "Female")


### AGE ###
  #calculate age by subtracting dob from admdate
  covidnet$age<-trunc((covidnet$dob %--% covidnet$admdate) / years(1))

  #create bins of different age groups
  library(dplyr)
  covidnet <- covidnet %>% mutate(age_group = case_when(age >= 0  & age <= 9  ~ '0-9',
                                                        age >= 10 & age <= 19 ~ '10-19',
                                                        age >= 20 & age <= 29 ~ '20-29',
                                                        age >= 30 & age <= 39 ~ '30-39',
                                                        age >= 40 & age <= 49 ~ '40-49',
                                                        age >= 50 & age <= 59 ~ '50-59',
                                                        age >= 60 & age <= 69 ~ '60-69',
                                                        age >= 70 & age <= 79 ~ '70-79',
                                                        age >= 80 & age <= 89 ~ '80-89',
                                                        age >= 90 & age <= 99 ~ '90-99',

                                                        
                                                                                                                age >= 100 ~ '100+'))
### RACE/ETHNICITY ###
  #one column contains race, while a different column contains ethnicity
  #for everyone who answered "yes" to are you Hispanic/Latino, replace whatever
  #is in the ethnic column for that observation with a 6
  covidnet$race <- replace(covidnet$race, covidnet$ethnic==1, 6)
  
  #convert race from number to character
  covidnet$race<-replace(covidnet$race, covidnet$race == 1, "White")
  covidnet$race<-replace(covidnet$race, covidnet$race == 2, "Black")
  covidnet$race<-replace(covidnet$race, covidnet$race == 3, "Asian/Pacific-Islander")
  covidnet$race<-replace(covidnet$race, covidnet$race == 4, "American Indian or Alaska Native")
  covidnet$race<-replace(covidnet$race, covidnet$race == 5, "Multiracial")
  covidnet$race<-replace(covidnet$race, covidnet$race == 6, "Hispanic/Latino")
  covidnet$race<-replace(covidnet$race, covidnet$race == 9, "Not specified")
  
  
### CONVERT DATE TO NAME OF MONTH ###
  # for admdate and test date
  #data.table has month() too, have to specify lubridate package
  covidnet$admdate<-lubridate::month((covidnet$admdate), label = TRUE, abbr = FALSE)
  covidnet$poststdt1<-lubridate::month((covidnet$poststdt1), label = TRUE, abbr = FALSE)
  
  
  
### CREATE TABLE ###
  covidnetTab<-covidnet
  covidnetTab$caseid<-NULL
  covidnetTab$ethnic<-NULL
  #install.packages("tableone")
  library(tableone)
  CreateTableOne(data = covidnetTab)



### VISUALIZE TRENDS IN AGE ###
  #subset
  age<-subset(covidnet, select = c(admdate,age_group))
  #get counts
  library(data.table)
  age<-data.table(age)
  agecount<-dcast(age,age$admdate + age$age_group~.)
  #re-name columns
  colnames(agecount)<-c("Admit","Age_Group","Case_Count")
  
  library(ggplot2)
  #plot!
  ggplot(data=agecount, aes(x=Admit, y=Case_Count, group=Age_Group)) +
    geom_line()+
    facet_grid(rows = vars(Age_Group))
  
  
### TRYING GIS ###
  library(lubridate)
  library(ggplot2)
  library(ggrepel)
  library(dplyr)
  library(data.table)
  library(tidyverse)
  library(Rcpp)
  library(choroplethr)
  
  get_tract_map(state_name = "Connecticut")
  
  tract_choropleth(
    df,
    state_name,
    title = "",
    legend = "",
    num_colors = 7,
    tract_zoom = NULL,
    county_zoom = NULL,
    reference_map = FALSE
  )
  
