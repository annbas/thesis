setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data")

cen_pop<-read.csv("DECENNIALPL2020.P1_data_with_overlays_2022-02-08T191316.csv")

#subset geo_id and total pop per census tract
cen_pop<-cen_pop[c("GEO_ID","P1_001N")]

#rename columns
names(cen_pop)[names(cen_pop) == "P1_001N"] <- "total_pop"

#delete first row (non-data)
cen_pop = cen_pop[-1,]


##### COVID-NET IR #####

##### merging covid-net, geo-coded data #####
setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data/2020 COVID-NET cases")
    marsep<-read.csv("CDC REDCap_Mar-Sep.csv")
    names(marsep)[names(marsep) == 'ï..caseid'] <- 'caseid'
    octdec<-read.csv("Yale REDCap_Oct-Dec.csv")
    names(octdec)[names(octdec) == 'ï..caseid'] <- 'caseid'
    setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data")
    geocode<-read.csv("CTCovidFinalMatch_Mar-Dec2020_CDCFormat.csv")
    names(geocode)[names(geocode) == 'CASEID'] <- 'caseid'
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

##### merge with total pop dataset #####  
    covidnet_inc<-merge(covidnet_inc, cen_pop, by = "GEO_ID", all.x = TRUE, all.y = FALSE)

##### calculate incidence #####
    covidnet_inc$total_pop<-as.integer(covidnet_inc$total_pop)
    covidnet_inc$incidence<-(covidnet_inc$cases_per_cen/covidnet_inc$total_pop)*100000
    
##### convert GEO-ID to character #####
    covidnet_inc$GEO_ID<-as.character(covidnet_inc$GEO_ID)
    #save a copy with 9 at beginning of string (full FIPS code)
    write.csv(covidnet_inc,"covidnet2020_IR_FIPS.csv",row.names = FALSE)
    #remove 9 from beginning of string
    covidnet_inc$GEO_ID<-substring(covidnet_inc$GEO_ID,2)

#### export csv #####
    write.csv(covidnet_inc,"covidnet2020_IR.csv",row.names = FALSE)
    
    
    
    
    
    
    
###### DPH IR ######
    #dph<-read.csv("covid_positive_tests_hospitalizations.csv")
    
    #create count of cases in each census tract and convert to df
    #dph_inc<-as.data.frame(table(dph$CTNO2010))
    #rename columns
    #names(dph_inc)[names(covidnet_inc) == "Var1"] <- "GEO_ID"
    #names(covidnet_inc)[names(covidnet_inc) == "Freq"] <- "cases_per_cen"
    