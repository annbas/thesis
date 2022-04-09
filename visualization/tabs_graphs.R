setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data/2020 COVID-NET cases")

library(lubridate)
library(dplyr)
library(tableone)
library(table1)
library(reshape2)
library(MMWRweek)
  

 #### table1 package ####
  
    #geocoded covidnet: need caseid, sex, race, age group, surv.method
      #merging covid-net, geo-coded data
      setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data/2020 COVID-NET cases")
      
      marsep<-read.csv("CDC REDCap_Mar-Sep.csv")
      names(marsep)[1] <- 'caseid'
      
      octdec<-read.csv("Yale REDCap_Oct-Dec.csv")
      names(octdec)[1] <- 'caseid'
      
      setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data")
      geocode<-read.csv("CTCovidFinalMatch_Mar-Dec2020_CDCFormat.csv")
      names(geocode)[names(geocode) == 'CASEID'] <- 'caseid'
    
      #subset necessary variables
      vars<-c("caseid","dob","admdate","sex","race","ethnic")
      
      marsep_sub <- marsep[vars]
      octdec_sub <- octdec[vars]
      
      #rbind
      covidnet <-rbind(marsep_sub ,octdec_sub)
      
      #convert date vars to dates and calculate age
      covidnet$dob     <- as.Date(covidnet$dob, format="%m/%d/%Y")
      covidnet$admdate <- as.Date(covidnet$admdate, format="%m/%d/%Y")
      covidnet$age     <- trunc((covidnet$dob %--% covidnet$admdate) / years(1))
      
      #create bins of different age groups
      covidnet <- covidnet %>% mutate(age_group = case_when(age >= 0  & age <= 9  ~ '0-9',
                                                            age >= 10 & age <= 19 ~ '10-19',
                                                            age >= 20 & age <= 29 ~ '20-29',
                                                            age >= 30 & age <= 39 ~ '30-39',
                                                            age >= 40 & age <= 49 ~ '40-49',
                                                            age >= 50 & age <= 59 ~ '50-59',
                                                            age >= 60 & age <= 69 ~ '60-69',
                                                            age >= 70 & age <= 79 ~ '70-79',
                                                            age >= 80 ~ '80+'))
      covidnet$dob <- NULL
      covidnet$admdate <- NULL
      covidnet$age <- NULL
      
      #convert sex from number to character
      covidnet$sex<-replace(covidnet$sex, covidnet$sex == 1, "Male")
      covidnet$sex<-replace(covidnet$sex, covidnet$sex == 2, "Female")
      
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
      
      covidnet$ethnic<-NULL
      
      #subset geocoded census tract
      CTID <- c("caseid","CTNO2010")
      geocode <- geocode[CTID]
      names(geocode)[names(geocode) == "CTNO2010"] <- "geoid10"
      
      #merge census tract with main dataset
      covidnet<-merge(covidnet, geocode, by = "caseid", all.x = TRUE, all.y = FALSE)
      
      #add COVID-NET indicator
      covidnet$surv.method <- rep("COVID-NET", length(covidnet$caseid))
      
      covidnet$caseid <- NULL
      
    #hospital = Y for dph
      dph<-read.csv("covid_positive_tests_hospitalizations_2022-04-07.csv") 
      hosp<-dph[,c(1:5,32:40)]
      hosp<-subset(hosp,hospital_admission == c("Y"))
      hosp$hospital_admission <- NULL
      
      #create new column with 53 + 2021 week number
      hosp$admit1<-ifelse(hosp$year_of_admission_1 == 2021, hosp$week_of_admission_1+53, hosp$week_of_admission_1)
      hosp$admit2<-ifelse(hosp$year_of_admission_2 == 2021, hosp$week_of_admission_2+53, hosp$week_of_admission_2)
      hosp$admit3<-hosp$week_of_admission_3
      hosp$admit4<-hosp$week_of_admission_4
      
      #remove year and week columns
      rem<-hosp[,grep("^week", names(hosp))]
      rem2<-hosp[,grep("^year", names(hosp))]
      hosp<-hosp[,-which(names(hosp) %in% names(rem))]
      hosp<-hosp[,-which(names(hosp) %in% names(rem2))]
      
      #melt to long
      hosp_long<-melt(hosp, id.vars=c("id", "gender", "hisp_race", "age_group", "geoid10"),
                      na.rm = TRUE,
                      value.name = "admit.week",
                      variable.name = "admit.number")
      
      #remove rows with 2021 admits
      hosp_long <- hosp_long[!(hosp_long$admit.week > 53),]
      #dropped 184 rows
      
      #group by id and calculate lag between hospitalizations
      hosp_diff <- hosp_long %>%
        arrange(id) %>%
        group_by(id) %>%
        mutate(diff = admit.week - lag(admit.week, default = first(admit.week)))
      
      ungroup(hosp_diff)
      
      #convert from tibble to df
      hosp_diff<-as.data.frame(hosp_diff)
      
      #create flag for "true" hospitalizations (i.e. lag between hospitalizations > 2 weeks)
      hosp_diff$flag <- ifelse(hosp_diff$admit.number == c("admit1"), 1,
                               ifelse(hosp_diff$diff > 2, 1, 0))
      
      #subset relevant columns and folks where hosp_diff = 1
      sub2 <- colnames(hosp_diff[1:5])
      ctedss <- subset(hosp_diff, flag == 1, select = sub2)
      
      #convert race from number to character
      ctedss$hisp_race<-replace(ctedss$hisp_race, ctedss$hisp_race == "NH White", "White")
      ctedss$hisp_race<-replace(ctedss$hisp_race, ctedss$hisp_race == "NH Black", "Black")
      ctedss$hisp_race<-replace(ctedss$hisp_race, ctedss$hisp_race == "NH Asian or Pacific Islander", "Asian/Pacific-Islander")
      ctedss$hisp_race<-replace(ctedss$hisp_race, ctedss$hisp_race == "NH American Indian or Alaskan Native", "American Indian or Alaska Native")
      ctedss$hisp_race<-replace(ctedss$hisp_race, ctedss$hisp_race == "NH Multiracial", "Multiracial")
      ctedss$hisp_race<-replace(ctedss$hisp_race, ctedss$hisp_race == "Hispanic", "Hispanic/Latino")
      ctedss$hisp_race<-replace(ctedss$hisp_race, ctedss$hisp_race == "Unknown", "Not specified")
      
      #fix 80+ age in CTEDSS age column
      ctedss$age_group<-replace(ctedss$age_group, ctedss$age_group == ">=80", "80+")
      
      #add ctedss indicator
      ctedss$surv.method <- rep("CTEDSS", length(ctedss$id))
      
      #prep colnames for rbind
      ctedss$id <- NULL
      names(ctedss)[names(ctedss) == "gender"] <- "sex"
      names(ctedss)[names(ctedss) == "hisp_race"] <- "race"
      
      #remove 0 at front of census tract
      ctedss$geoid10<-as.numeric(ctedss$geoid10)
      ctedss$geoid10<-as.character(ctedss$geoid10)
  
  #bind to one        
  all.hosp <- rbind(covidnet,ctedss)
  
      #read in counties and bind to total dataset
      census<-read.csv("DECENNIALPL2010.P1_data_with_overlays_2022-02-11T112106.csv")
      census <- census[-1,] 
      census <- census[c("GEO_ID","NAME")]
      names(census)[names(census) == "GEO_ID"] <- "geoid10"
      census$county <- str_extract(census$NAME, "\\b[^,]+(?= County?)")
      census$NAME <- NULL
      
      all.hosp <-merge(all.hosp, census, by = "geoid10", all.x = T, all.y = F)
      
  #variables we want: county, age, sex, race. so, adjust DPH data and covidnet 
  #data to include these columns, and then rbind
      
      #change column names for table
        names(all.hosp)[names(all.hosp) == "county"] <- "County"
        names(all.hosp)[names(all.hosp) == "age_group"] <- "Age"
        names(all.hosp)[names(all.hosp) == "sex"] <- "Sex"
        names(all.hosp)[names(all.hosp) == "race"] <- "Race"
        
  
      table1(~ County + Age + Sex + Race | surv.method, data=all.hosp, overall=F)
  
  
#### Streamgraph ####
  #date of admit for CTEDSS and COVIDNET
      #covidnet
      marsep.stream <- subset(marsep, select = "admdate")
      octdec.stream <- subset(octdec, select = "admdate")  
      
      covidnet2<-rbind(marsep.stream ,octdec.stream)
      covidnet2$admdate<-as.Date(covidnet2$admdate, format="%m/%d/%Y")
      
  #group by week?
      covidnet.week <- MMWRweek(covidnet2$admdate)
      covidnet.week <- as.data.frame(table(covidnet.week$MMWRweek))
  #merge on week and count diff per week
  #viz in streamgraph
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


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
  
