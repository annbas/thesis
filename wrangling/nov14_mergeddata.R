setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data")

marsep<-read.csv("CDC REDCap_Mar-Sep.csv")
names(marsep)[names(marsep) == 'ï..caseid'] <- 'caseid'
octdec<-read.csv("Yale REDCap_Oct-Dec.csv")
names(octdec)[names(octdec) == 'ï..caseid'] <- 'caseid'
svi<-read.csv("CDC_CensusTract_SVI.csv")
geocode<-read.csv("CTCovidFinalMatch_Mar-Dec2020_CDCFormat.csv")
names(geocode)[names(geocode) == 'CASEID'] <- 'caseid'
agesex<-read.csv("NH_MS_age_sex_census.csv")


#########COVID-NET DATA#########

#subset necessary variables
vars<-c("caseid","admdate","race","sex","dob")

marsep <- marsep[vars]
octdec <- octdec[vars]

#rbind
covidnet2<-rbind(marsep,octdec)

#convert dates to dates
covidnet2$dob<-as.Date(covidnet2$dob, format="%m/%d/%Y")
covidnet2$admdate<-as.Date(covidnet2$admdate, format="%m/%d/%Y")

#convert to birth date to age
library(lubridate)
covidnet2$age<-trunc((covidnet2$dob %--% covidnet2$admdate) / years(1))

#create bins
labs <- c(paste(seq(0, 80, by = 10), seq(0 + 10 - 1, 90 - 1, by = 10),
                sep = "-"), paste(90, "+", sep = ""))
covidnet2$age.group <- cut(covidnet2$age, breaks = c(seq(0, 90, by = 10), Inf), labels = labs, right = FALSE)

#subset geocoded census tract
CTID<-c("caseid","CTNO2010")
geocode <- geocode[CTID]

#merge census tract with main dataset
covidnet2<-merge(covidnet2, geocode, by = "caseid", all.x = TRUE, all.y = FALSE)


#########DEMO#########

vars_demo<-c("GEO_ID",
             "S0101_C01_002E","S0101_C01_003E","S0101_C01_004E","S0101_C01_005E",
             "S0101_C01_006E","S0101_C01_007E","S0101_C01_008E","S0101_C01_009E",
             "S0101_C01_010E","S0101_C01_011E","S0101_C01_012E","S0101_C01_013E",
             "S0101_C01_014E","S0101_C01_015E","S0101_C01_016E","S0101_C01_017E",
             "S0101_C01_018E","S0101_C01_019E")
agesex<-agesex[vars_demo]

#rename
setnames(agesex, old = c("S0101_C01_002E","S0101_C01_003E","S0101_C01_004E","S0101_C01_005E",
                    "S0101_C01_006E","S0101_C01_007E","S0101_C01_008E","S0101_C01_009E",
                    "S0101_C01_010E","S0101_C01_011E","S0101_C01_012E","S0101_C01_013E",
                    "S0101_C01_014E","S0101_C01_015E","S0101_C01_016E","S0101_C01_017E",
                    "S0101_C01_018E","S0101_C01_019E"), 
            new = c("Under 5","5 to 9","10 to 14","15 to 19","20 to 24","25 to 29","30 to 34",
                    "35 to 39","40 to 44","45 to 49","50 to 54","55 to 59","60 to 64","65 to 69",
                    "70 to 74","75 to 79","80 to 84","Over 85"))

#subset SVI
svi<-svi[, !names(svi) %in% c("ï..ST", "STATE","ST_ABBR","STCNTY","COUNTY","LOCATION","AREA_SQMI")] 

#merge SVI to main dataset
covidnet2<-merge(covidnet2, svi, by.x = "CTNO2010", by.y = "FIPS", all.x = TRUE, all.y = FALSE)

#export
write.csv(covidnet2,"covidnet_svi_nov15.csv",row.names = FALSE)

