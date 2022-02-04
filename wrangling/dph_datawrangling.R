setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data")

dph<-read.csv("covid_positive_tests_hospitalizations.csv")

#create separate datasets for testing and hospitalizations
  test<-dph[,c(1:31)]
  
  hosp<-dph[,c(1:5,32:40)]
  hosp<-subset(hosp,hospital_admission == c("Y"))

#melt to long on testing data
  #install.packages("reshape2")
  library(reshape2)
  
  test_long<-melt(test, id.vars=c("id", "geoid10","age_group","gender","hisp_race"),na.rm = TRUE)
  #na.rm = TRUE removes missings from value column
  #e.g. if someone did not have a 3rd, 4th, etc. positive test, then it was removed
  
  #group by id and determine if someone had >1 "true positive" test/hospitalization
    library(dplyr)
    
    test_diff<-test_long %>%
               arrange(id) %>%
               group_by(id) %>%
               mutate(diff = value - lag(value, default = first(value)))
    
    ungroup(test_diff)
    
    #convert from tibble to df
    test_diff<-as.data.frame(test_diff)
    
    #create flag where test = "true" infection
    test_diff$flag<-ifelse(test_diff$variable == c("week_of_positive_test_1"),1,
                           ifelse(test_diff$diff>9,1,0))
    