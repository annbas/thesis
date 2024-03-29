setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data")

cen_pop<-read.csv("DECENNIALPL2010.P1_data_with_overlays_2022-02-11T112106.csv")

#subset geo_id and total pop per census tract
cen_pop<-cen_pop[c("GEO_ID","P001001")]

#rename columns
names(cen_pop)[names(cen_pop) == "P001001"] <- "total_pop"

#delete first row (non-data)
cen_pop = cen_pop[-1,]


##### COVID-NET IR #####

#merging covid-net, geo-coded data
setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data/2020 COVID-NET cases")

    marsep<-read.csv("CDC REDCap_Mar-Sep.csv")
    names(marsep)[names(marsep) == '�..caseid'] <- 'caseid'
    
    octdec<-read.csv("Yale REDCap_Oct-Dec.csv")
    names(octdec)[names(octdec) == '�..caseid'] <- 'caseid'
    
    setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data")
    geocode<-read.csv("CTCovidFinalMatch_Mar-Dec2020_CDCFormat.csv")
    names(geocode)[names(geocode) == 'CASEID'] <- 'caseid'
    
#### COVID-NET IR with post admit test ####
    #subset necessary variables
    vars_inc<-c("caseid","admdate")
    
    marsep_inc <- marsep[vars_inc]
    octdec_inc <- octdec[vars_inc]
    
    #rbind
    covidnet_inc<-rbind(marsep_inc,octdec_inc)
    
    #subset geocoded census tract
    CTID_inc<-c("caseid","CTNO2010")
    geocode_inc <- geocode[CTID_inc]
    
    #merge census tract with main dataset
    covidnet_inc<-merge(covidnet_inc, geocode_inc, by = "caseid", all.x = TRUE, all.y = FALSE)
    
    #create count of cases in each census tract and convert to df
    covidnet_inc<-as.data.frame(table(covidnet_inc$CTNO2010))
    #rename columns
    names(covidnet_inc)[names(covidnet_inc) == "Var1"] <- "GEO_ID"
    names(covidnet_inc)[names(covidnet_inc) == "Freq"] <- "cases_per_cen"

    #merge with total pop dataset
    covidnet_inc<-merge(covidnet_inc, cen_pop, by = "GEO_ID", all.x = TRUE, all.y = FALSE)

    #calculate incidence
    covidnet_inc$total_pop<-as.integer(covidnet_inc$total_pop)
    covidnet_inc$incidence<-(covidnet_inc$cases_per_cen/covidnet_inc$total_pop)*100000
    
    #convert GEO-ID to character
    covidnet_inc$GEO_ID<-as.character(covidnet_inc$GEO_ID)
    #save a copy with 9 at beginning of string (full FIPS code)
    write.csv(covidnet_inc,"covidnet2020_IR_FIPS.csv",row.names = FALSE)
    #remove 9 from beginning of string
    covidnet_inc$GEO_ID<-substring(covidnet_inc$GEO_ID,2)

#### export csv #####
    write.csv(covidnet_inc,"covidnet2020_IR.csv",row.names = FALSE)

    
#### COVID-NET IR without post-admit test ####
    
    
    #subset test data
    #melt to long
    #merge to admit date
    #if test date =< 14 days before admit and =< admit date, flag
    #remove entries that are not flagged
    #deduplicate to only include one ID
    
    #qual check - see if it drops ~500
    


    
    
    
    
#IR by age, etc
    #create covidnet dataset with "caseid","admdate","race","ethnic","sex","dob"
        #use marsep, octdec, geocide_inc created above
        sub<-c("caseid","admdate","race","ethnic","sex","dob")
        
        marsep2 <- marsep[sub]
        octdec2 <- octdec[sub]
        
        #convert dates
        marsep2$dob<-as.Date(marsep2$dob, format="%m/%d/%Y")
        marsep2$admdate<-as.Date(marsep2$admdate, format="%m/%d/%Y")
        
        octdec2$dob<-as.Date(octdec2$dob, format="%Y-%m-%d")
        octdec2$admdate<-as.Date(octdec2$admdate, format="%Y-%m-%d")
        
        #stack the two .csv files to create the main dataset
        covidnet<-rbind(marsep2,octdec2)
        
        #merge covidnet with geocoded data 
        covidnet_geo<-merge(covidnet, geocode_inc, by = "caseid", all.x = TRUE, all.y = FALSE)
    
        
   #AGE
    #need lubridate package to use trunc() function
    library(lubridate)
    #calculate age 
    covidnet_geo$age<-trunc((covidnet_geo$dob %--% covidnet_geo$admdate) / years(1))
    
    #make a copy of the main dataset
    covidnet_age<-covidnet_geo
    #create bins of different age groups
    library(dplyr)
    covidnet_age <- covidnet_age %>% 
                        mutate(age_group = case_when(age >= 0  & age <= 9  ~ '0-9',
                                                     age >= 10 & age <= 19 ~ '10-19',
                                                     age >= 20 & age <= 29 ~ '20-29',
                                                     age >= 30 & age <= 39 ~ '30-39',
                                                     age >= 40 & age <= 49 ~ '40-49',
                                                     age >= 50 & age <= 59 ~ '50-59',
                                                     age >= 60 & age <= 69 ~ '60-69',
                                                     age >= 70 & age <= 79 ~ '70-79',
                                                     age >= 80 ~ '80+'))
    library(data.table)
    #convert from data frame to data table
    covidnet_age<-data.table(covidnet_age)
    #create a dataset that has the number of people hospitalized by age group per census tract
    
    
    covidnet_age<-dcast(covidnet_age,CTNO2010+age_group~.)
    #missings
    #1   NA NA    209
    #2   NA 0-9   1
    #3   NA 10-19 2
    #4   NA 20-29 12
    #5   NA 30-39 18
    #6   NA 40-49 22
    #7   NA 50-59 31
    #8   NA 60-69 30
    #9   NA 70-79 33
    #10  NA 80+   36
    
    #change name
    names(covidnet_age)[names(covidnet_age) == "."] <- "count"
    
    #create denominator
    
    #calculate incidence rate 
    