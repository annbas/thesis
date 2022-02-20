setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data")

dph<-read.csv("covid_positive_tests_hospitalizations.csv")

#create separate datasets for testing and hospitalizations
  test<-dph[,c(1:31)]
  
  hosp<-dph[,c(1:5,32:40)]
  hosp<-subset(hosp,hospital_admission == c("Y"))


### TESTING ###
#melt to long on testing data
  #install.packages("reshape2")
  library(reshape2)
  
  test_long<-melt(test, id.vars=c("id", "geoid10","age_group","gender","hisp_race"),na.rm = TRUE)
  #na.rm = TRUE removes missings from value column
  #e.g. if someone did not have a 3rd, 4th, etc. positive test, then it was removed
  
  #group by id and calculate time lag between positive tests
    library(dplyr)
    
    test_diff<-test_long %>%
                 arrange(id) %>%
                 group_by(id) %>%
                 mutate(diff = value - lag(value, default = first(value)))
    
    ungroup(test_diff)
    
    #convert from tibble to df
    test_diff<-as.data.frame(test_diff)
    
    #create flag where test = "true" infection (i.e. lag between positive tests > 10 weeks)
    test_diff$flag<-ifelse(test_diff$variable == c("week_of_positive_test_1"),1,
                           ifelse(test_diff$diff>9,1,0))
    
    
### HOSPITALIZATIONS ###  
  
  #week number >50 for 2021? Might be something with melt
    yr.fix <- function(week,year) {
      ifelse(week >= 50, 2020, year)
    }  
    
    hosp$year_of_admission_1 <- yr.fix(hosp$week_of_admission_1,hosp$year_of_admission_1)
    hosp$year_of_admission_2 <- yr.fix(hosp$week_of_admission_2,hosp$year_of_admission_2)
    
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
    hosp_long<-melt(hosp, id.vars=c("id", "geoid10","age_group","gender","hisp_race","hospital_admission"),
                                      na.rm = TRUE,
                                      value.name = "admit.week",
                                      variable.name = "admit.number")
    
  #group by id and calculate lag between hospitalizations
  library(dplyr)
    hosp_diff<-hosp_long %>%
      arrange(id) %>%
      group_by(id) %>%
      mutate(diff = admit.week - lag(admit.week, default = first(admit.week)))
    
    ungroup(hosp_diff)
    
  #convert from tibble to df
    hosp_diff<-as.data.frame(hosp_diff)
    
  #create flag for "true" hospitalizations (i.e. lag between hospitalizations > 2 weeks)
    hosp_diff$flag<-ifelse(hosp_diff$admit.number == c("admit1"),1,
                           ifelse(hosp_diff$diff>2,1,0))
    
    
  #hosp_yrfix$week.only<-ifelse(hosp_yrfix$year == 2021, hosp_yrfix$week+53, hosp_yrfix$week)
  
  #hosp_yrfix <- hosp_long2 %>%
  #  mutate(year = ifelse(week >= 50, 2020, year))
    
  #melt to long on hospitalization data
  #hosp_long<-melt(hosp[,c("id", "geoid10","age_group","gender","hisp_race","hospital_admission","year_of_admission_1","year_of_admission_2","year_of_admission_3","year_of_admission_4")], id.vars=c("id", "geoid10","age_group","gender","hisp_race","hospital_admission","year_of_admission_1","year_of_admission_2","year_of_admission_3","year_of_admission_4"),
  #                na.rm = TRUE,
  #                value.name = "week",
  #                variable.name = "week.var")
  
  #hosp$date1 <-as.Date( paste0(hosp$year_of_admission_1,'-',hosp$week_of_admission_1,'-','1'), '%Y-%U-%u' )
  
  #hosp_long <- melt(hosp[,c('date1','date2','date3','date4',"id", "geoid10","age_group","gender","hisp_race","hospital_admission")], id.vars=c("id", "geoid10","age_group","gender","hisp_race","hospital_admission"),
  #                na.rm = TRUE)

  
  #melt again for year
  #hosp_long2<-melt(hosp_long, id.vars=c("id", "geoid10","age_group","gender","hisp_race","hospital_admission","week.var","week"),
  #                 na.rm = TRUE,
  #                 value.name = "year")
  #remove year variable column
  #hosp_long2$variable<-NULL
  
  #remove duplicate rows
  #hosp_long2<-hosp_long2 %>%
  #              distinct(id, geoid10, age_group, gender, hisp_race, hospital_admission, week.var, week,
  #                       .keep_all = TRUE)
  
  
  
    
    
    
  ####MMWRweek package
  
  