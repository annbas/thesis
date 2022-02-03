setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data")

covid_gis<-read.csv("covidnet_svi_nov15.csv")

#library(lubridate)
library(ggplot2)
#library(ggrepel)
#library(dplyr)
#library(data.table)
#library(tidyverse)
library(Rcpp)
library(choroplethr)
library(choroplethrMaps)
library(ggmap)
#install.packages("mapproj")
library(mapproj)

## CASES BY CENSUS TRACT ##
  # group cases by census tract
  case_per_tract<-as.data.frame(table(covid_gis$CTNO2010))

  #change names for chloroplethr
  names(case_per_tract)[names(case_per_tract) == "Var1"] <- "region"
  names(case_per_tract)[names(case_per_tract) == "Freq"] <- "value"
  #convert region and value to numeric for chloroplethr
  case_per_tract$region<-as.numeric(levels(case_per_tract$region))[case_per_tract$region]
  case_per_tract$value<-as.numeric(case_per_tract$value)

  #map cases per census tract
  tract_choropleth(case_per_tract,
                   state_name  = "connecticut",
                   county_zoom = c(09009,09007),
                   title       = "COVID-Related Hospitalizations per Census Tract in New Haven and Middlesex Counties (2020)",
                   legend      = "Case Count")
  

## SVI BY CENSUS TRACT ##
  #read in SVI
  svi<-read.csv("CDC_CensusTract_SVI.csv")
  
  ## SOCIOECONOMIC STATUS ##
    #subset to only include FIPS codes for CT and summary ranking for SES
    svi_ct_ses<-subset(svi, select = c("FIPS","RPL_THEME1"))
    
    #change names for chloroplethr
    names(svi_ct_ses)[names(svi_ct_ses) == "FIPS"] <- "region"
    names(svi_ct_ses)[names(svi_ct_ses) == "RPL_THEME1"] <- "value"
    #convert region and value to numeric for chloroplethr
    #svi_ct_ses$region<-as.numeric(levels(svi_ct_ses$region))[svi_ct_ses$region]  NOTE: FIPS and themes in SVI datset already numeric
    #svi_ct_ses$value<-as.numeric(case_per_tract$value)
    
    #map cases per census tract
    tract_choropleth(svi_ct_ses,
                     state_name  = "connecticut",
                     county_zoom = NULL,
                     title       = "SES Ranking of Census Tracts in Connecticut",
                     legend      = "Rank")
    
    
    
  ## HOUSEHOLD COMPOSITION & DISABILITY ##
    #subset to only include FIPS codes for CT and summary ranking for SES
    svi_ct_hcd<-subset(svi, select = c("FIPS","RPL_THEME2"))
    
    #change names for chloroplethr
    names(svi_ct_hcd)[names(svi_ct_hcd) == "FIPS"] <- "region"
    names(svi_ct_hcd)[names(svi_ct_hcd) == "RPL_THEME2"] <- "value"
    #convert region and value to numeric for chloroplethr
    #svi_ct_ses$region<-as.numeric(levels(svi_ct_ses$region))[svi_ct_ses$region]  NOTE: FIPS and themes in SVI datset already numeric
    #svi_ct_ses$value<-as.numeric(case_per_tract$value)
    
    #map cases per census tract
    tract_choropleth(svi_ct_hcd,
                     state_name  = "connecticut",
                     county_zoom = NULL,
                     title       = "Household Composition & Disability Ranking of Census Tracts in Connecticut",
                     legend      = "Rank")

  ## MINORITY STATUS & LANGUAGE ##
    #subset to only include FIPS codes for CT and summary ranking for SES
    svi_ct_msl<-subset(svi, select = c("FIPS","RPL_THEME3"))
    
    #change names for chloroplethr
    names(svi_ct_msl)[names(svi_ct_msl) == "FIPS"] <- "region"
    names(svi_ct_msl)[names(svi_ct_msl) == "RPL_THEME3"] <- "value"
    
    #map cases per census tract
    tract_choropleth(svi_ct_msl,
                     state_name  = "connecticut",
                     county_zoom = NULL,
                     title       = "Minority Status & Language Ranking of Census Tracts in Connecticut",
                     legend      = "Rank")
    
    
    
  ## HOUSING TYPE & TRANSPORTATION ##
    #subset to only include FIPS codes for CT and summary ranking for SES
    svi_ct_htt<-subset(svi, select = c("FIPS","RPL_THEME4"))
    
    #change names for chloroplethr
    names(svi_ct_htt)[names(svi_ct_htt) == "FIPS"] <- "region"
    names(svi_ct_htt)[names(svi_ct_htt) == "RPL_THEME4"] <- "value"
    
    #map cases per census tract
    tract_choropleth(svi_ct_htt,
                     state_name  = "connecticut",
                     county_zoom = NULL,
                     title       = "Housing Type & Transportation Ranking of Census Tracts in Connecticut",
                     legend      = "Rank")

  ## SUMMARY SVI RANKING ##
    #subset to only include FIPS codes for CT and summary ranking for SES
    svi_ct_sum<-subset(svi, select = c("FIPS","RPL_THEMES"))
    
    #change names for chloroplethr
    names(svi_ct_sum)[names(svi_ct_sum) == "FIPS"] <- "region"
    names(svi_ct_sum)[names(svi_ct_sum) == "RPL_THEMES"] <- "value"
    
    #map cases per census tract
    tract_choropleth(svi_ct_sum,
                     state_name  = "connecticut",
                     county_zoom = NULL,
                     title       = "Summary SVI Ranking of Census Tracts in Connecticut",
                     legend      = "Rank")
    
    
  ## FLAGS FOR CENSUS TRACTS >= 90th PERCENTILE SVI ##
    #create column with flags
    svi$flag<-ifelse(svi$RPL_THEMES >= quantile(svi$RPL_THEMES,0.90),print("Flag"),print("No flag"))
    #subset to only include FIPS codes for CT and flags
    svi_flag<-subset(svi, select = c("FIPS","flag"))
    
    #subset 90th percentile and above
    #flag<-subset(svi,RPL_THEMES >= quantile(svi$RPL_THEMES,0.90))
    
    #change names for chloroplethr
    names(svi_flag)[names(svi_flag) == "FIPS"] <- "region"
    names(svi_flag)[names(svi_flag) == "flag"] <- "value"
    
    #map cases per census tract
    tract_choropleth(svi_flag,
                     state_name  = "connecticut",
                     county_zoom = NULL,
                     title       = "Connecticut Census Tracts Ranking at or above the 90th Percentile on the CDC's Social Vulnerability Index",
                     legend      = NULL) +
    scale_fill_manual(values = c("Flag"    = "#E62C09",
                                 "No flag" = "#FFF4F0"))
  
## FLAGS FOR CENSUS TRACTS WITH IR >= 90th PERCENTILE FOR COVID-NET CASES ##
    ir<-read.csv("covidnet2020_IR_FIPS.csv")
    #create column with flags
    ir$flag<-ifelse(ir$incidence >= quantile(ir$incidence,0.90),print("Flag"),print("No flag"))
    #subset to only include FIPS codes for CT and flags
    ir_flag<-subset(ir, select = c("GEO_ID","flag"))
    
    #change names for chloroplethr
    names(ir_flag)[names(ir_flag) == "GEO_ID"] <- "region"
    names(ir_flag)[names(ir_flag) == "flag"] <- "value"
    #convert region and value to numeric for chloroplethr
    ir_flag$region<-as.numeric(ir_flag$region)
    #ir_flag$value<-as.numeric(ir_flag$value)
    
    #map cases per census tract
    tract_choropleth(ir_flag,
                     state_name  = "connecticut",
                     county_zoom = c(09009,09007),
                     title       = "New Haven and Middlesex Census Tracts with COVID-NET Case Rates at or above the 90th Percentile",
                     legend      = NULL) +
    scale_fill_manual(values = c("Flag"    = "#E62C09",
                                 "No flag" = "#FFF4F0"))

    
## FLAGS FOR *COVID-NET* CENSUS TRACTS >= 90th PERCENTILE SVI ##
    #subset COVID-NET catchment area and find 90th percentile value
    covidnet_svi<-subset(svi, COUNTY == c("New Haven","Middlesex"))
    #duplicate svi dataset and create column with flags
    svi_flag_covidnet<-svi
    svi_flag_covidnet$flag<-ifelse(svi_flag_covidnet$RPL_THEMES >= quantile(covidnet_svi$RPL_THEMES,0.90),print("Flag"),print("No flag"))
    #subset to only include FIPS codes for CT and flags
    svi_flag_covidnet<-subset(svi_flag_covidnet, select = c("FIPS","flag"))
    
    #subset 90th percentile and above
    #flag<-subset(svi,RPL_THEMES >= quantile(svi$RPL_THEMES,0.90))
    
    #change names for chloroplethr
    names(svi_flag_covidnet)[names(svi_flag_covidnet) == "FIPS"] <- "region"
    names(svi_flag_covidnet)[names(svi_flag_covidnet) == "flag"] <- "value"
    
    #map cases per census tract
    tract_choropleth(svi_flag_covidnet,
                     state_name  = "connecticut",
                     county_zoom = c(09009,09007),
                     title       = "Census Tracts within CT COVID-NET's Catchment Area Ranking at or above the 90th Percentile on the CDC's Social Vulnerability Index",
                     legend      = NULL) +
    scale_fill_manual(values = c("Flag"    = "#E62C09",
                                 "No flag" = "#FFF4F0"))
    
    
#CHOROLEPATH OF DPH DATA
    dph<-read.csv("covid_positive_tests_hospitalizations.csv")
    # group cases by census tract
    ct_case_per_tract<-as.data.frame(table(dph$geoid10,useNA = c("no")))
    
    #change names for chloroplethr
    names(ct_case_per_tract)[names(ct_case_per_tract) == "Var1"] <- "region"
    names(ct_case_per_tract)[names(ct_case_per_tract) == "Freq"] <- "value"
    #convert region and value to numeric for chloroplethr
    ct_case_per_tract$region<-as.numeric(levels(ct_case_per_tract$region))[ct_case_per_tract$region]
    ct_case_per_tract$value<-as.numeric(ct_case_per_tract$value)
    ct_case_per_tract<-na.omit(ct_case_per_tract)
    
    #map cases per census tract
    tract_choropleth(ct_case_per_tract,
                     state_name  = "connecticut",
                     #county_zoom = c(09009,09007),
                     title       = "Cases of COVID-19 per Census Tract in Connecticut (2020)",
                     legend      = "Case Count")
    
   
    