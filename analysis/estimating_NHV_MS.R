setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data")

#merge covidnet ir, +test ir, svi variables
hosp_ir<-read.csv("covidnet2020_IR_FIPS.csv")
test_ir<-read.csv("dph2020_test_IR_FIPS.csv")
svi<-read.csv("CDC_CensusTract_SVI.csv")
town<-read.csv("tract2town-2010.csv")


#create dataset with outcome and covariates
  #subset GEO_ID and incidence
  hosp_ir<-subset(hosp_ir, select = c("GEO_ID","incidence"))
  test_ir<-subset(test_ir, select = c("GEO_ID","incidence"))
  
  #merge on FIPS
  test_hosp_ir<-merge(hosp_ir, test_ir, by = "GEO_ID", all.x = TRUE, all.y = FALSE)
  
  names(test_hosp_ir)[names(test_hosp_ir) == "incidence.x"] <- "covidnet.ir"
  names(test_hosp_ir)[names(test_hosp_ir) == "incidence.y"] <- "test.ir"
  
  #subset to include estimates from svi data
  e_sub<-svi[,grep("^EP_", colnames(svi))]
  e_sub<-colnames(e_sub)
  sub<-c("FIPS",e_sub)
  svi<-subset(svi,select = sub)
  
  #merge ir with svi
  ir_svi<-merge(test_hosp_ir, svi, by.x = "GEO_ID", by.y = "FIPS", all.x = TRUE, all.y = FALSE) #dataset with IR and SVI variables
  
  #rename tract column for town dataset
  names(town)[names(town) == "ï..tract_fips"] <- "GEO_ID"
  
  #merge tract to town with ir/svi data
  ir_svi<-merge(ir_svi, town, by = "GEO_ID", all.x = TRUE, all.y = FALSE)

  
#group by town and create datasets with 10% missing data by town
  #group by town and calculate time lag between positive tests
  #install.packages("missMethods")
  library(missMethods)
  library(dplyr)
  
  ir_svi_miss<-ir_svi %>%
                group_by(town) %>%
                mutate(miss1 = replace(covidnet.ir, sample(row_number(),  
                                       size = ceiling(0.1 * n()), replace = FALSE), NA))
              
  ungroup(ir_svi_miss)

  
#try to create loop to make 10 new datasets and/or 10 new column with missings
  data<-ir_svi_miss
  for(i in 1:10) {                                   # Head of for-loop
    miss <- data %>%
              group_by(town) %>%
              mutate(miss1 = replace(covidnet.ir, sample(row_number(),  
                                     size = ceiling(0.1 * n()), replace = FALSE), NA))                       # Create new column
    data[ , ncol(data) + 1] <- miss                  # Append new column
    colnames(data)[ncol(data)] <- paste0("miss", i)  # Rename column name
  } 

  