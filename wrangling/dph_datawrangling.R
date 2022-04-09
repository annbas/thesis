setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data")

library(reshape2)
library(tidyverse)

dph<-read.csv("covid_positive_tests_hospitalizations_2022-04-07.csv") #delete data for original dataset

#create separate datasets for testing and hospitalizations
  test<-dph[,c(1:2,6:31)]
  
  hosp<-dph[,c(1:2,32:40)]
  hosp<-subset(hosp,hospital_admission == c("Y"))
  
  
#create denominators
  cen_pop<-read.csv("DECENNIALPL2010.P1_data_with_overlays_2022-02-11T112106.csv")
  
  #subset geo_id and total pop per census tract
  cen_pop<-cen_pop[c("GEO_ID","P001001")]
  
  #rename columns
  names(cen_pop)[names(cen_pop) == "P001001"] <- "total_pop"
  
  #delete first row (non-data)
  cen_pop = cen_pop[-1,]


##### TESTING #####
#melt to long on testing data
  #na.rm = TRUE removes missings from value column
  #e.g. if someone did not have a 3rd, 4th, etc. positive test, then it was removed
  
  test_long <- melt(test, id.vars=c("id", "geoid10"),na.rm = TRUE)
  
  
  #group by id and calculate time lag between positive tests
    
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
      
  ###### Calculate dph testing ir ######
    
    dph_test_count<-aggregate(test_diff$flag ~ test_diff$geoid10, FUN=sum)
    #2577 missing, 5481 NULL
    
    #rename columns
      names(dph_test_count)[names(dph_test_count) == "test_diff$geoid10"] <- "GEO_ID"
      names(dph_test_count)[names(dph_test_count) == "test_diff$flag"] <- "cases_per_cen"
      
    #remove 0 at front of census tract
      dph_test_count$GEO_ID<-as.numeric(dph_test_count$GEO_ID)
      dph_test_count$GEO_ID<-as.character(dph_test_count$GEO_ID)
      
    #merge with total pop dataset  
      dph_test_inc<-merge(dph_test_count, cen_pop, by = "GEO_ID", all.x = TRUE, all.y = FALSE)
    
    #calculate incidence 
      dph_test_inc$total_pop<-as.integer(dph_test_inc$total_pop)
      dph_test_inc$incidence<-(dph_test_inc$cases_per_cen/dph_test_inc$total_pop)*100000
      
    #save dataset
    write.csv(dph_test_inc,"dph2020_test_IR_FIPS2.csv",row.names = FALSE)
    
    #189929 before new dataset
    #189795 from new dataset?
    
    
    
    
    
##### HOSPITALIZATIONS #####  
  
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
    hosp_long<-melt(hosp, id.vars=c("id", "geoid10","hospital_admission"),
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
    
    
    
  ###### DPH HOSP IR ######
    

    #create count of cases in each census tract
    dph_hosp_count<-aggregate(hosp_diff$flag ~ hosp_diff$geoid10, FUN=sum)
    #103 missing, 294 NULL
    
    #rename columns
    names(dph_hosp_count)[names(dph_hosp_count) == "hosp_diff$geoid10"] <- "GEO_ID"
    names(dph_hosp_count)[names(dph_hosp_count) == "hosp_diff$flag"] <- "cases_per_cen"
    
    #remove 0 at front of census tract
    dph_hosp_count$GEO_ID<-as.numeric(dph_hosp_count$GEO_ID)
    dph_hosp_count$GEO_ID<-as.character(dph_hosp_count$GEO_ID)
    
    #merge with total pop dataset  
    dph_hosp_inc<-merge(dph_hosp_count, cen_pop, by = "GEO_ID", all.x = TRUE, all.y = FALSE)
    
    #calculate incidence 
    dph_hosp_inc$total_pop<-as.integer(dph_hosp_inc$total_pop)
    dph_hosp_inc$incidence<-(dph_hosp_inc$cases_per_cen/dph_hosp_inc$total_pop)*100000
    
    #save dataset
    write.csv(dph_hosp_inc,"dph2020_IR_FIPS3.csv",row.names = FALSE)
    
    
    #12428 before new dataset
    #13993 after new dataset
  
  