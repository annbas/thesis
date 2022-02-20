setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data")

#merge covidnet ir, +test ir, svi variables
hosp_ir<-read.csv("covidnet2020_IR_FIPS.csv")
test_ir<-read.csv("dph2020_test_IR_FIPS.csv")
svi<-read.csv("CDC_CensusTract_SVI.csv")
town<-read.csv("tract2town-2010.csv") #from CT Data Collborative: https://github.com/CT-Data-Collaborative/ct-census-tract-to-town


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


#######################################################################################  
  
#try to create loop to make 10 new datasets with missings
  #created 10 datasets but missing in same location
  #too loss-y?
  library(dplyr)
  
  miss<-function(){
    ir_svi %>%
      group_by(town) %>%
      mutate(covidnet.ir = replace(covidnet.ir, sample(row_number(),  
                                                       size = ceiling(0.1 * n()), 
                                                       replace = FALSE), NA))
    ungroup(ir_svi_miss)
  }
  
  result <- replicate(10, miss(),simplify = FALSE)

  
######################################################################################
  
  maybe<-ir_svi %>% 
          group_by(town) %>% 
          sample_frac(.1) 
  
  
miss1<-ir_svi %>%
        group_by(town) %>%
        mutate(covidnet.ir = replace(covidnet.ir, sample(row_number(),  
                                                         size = ceiling(0.1 * n()), 
                                                         replace = FALSE), NA))
  ungroup(miss1)  
  

#keep track of missing variables
  miss_ind <- which(is.na(miss1$covidnet.ir))
  
#remove rows where miss1$covidnet.ir == NA
  library(tidyr)
  miss1_train <- miss1 %>% 
                  drop_na(covidnet.ir)
  
#train on data with missings
  #univariate? glm????
  lm_miss <- lm(covidnet.ir ~ "??")
  
  
  
  
 
  
  
  
  
  
  


  