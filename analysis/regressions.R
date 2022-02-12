setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data")


### univariate analysis ###












############## fix everything below this point ##############
#2020 IR per census tract and merge with SVI dataset
ir<-read.csv("covidnet2020_IR_FIPS.csv")
svi<-read.csv("CDC_CensusTract_SVI.csv")

#merge on FIPS
ir_svi<-merge(ir, svi, by.x = "GEO_ID", by.y = "FIPS", all.x = TRUE, all.y = FALSE) #dataset with IR and SVI variables


#read in dataset with all 2020 COVID-NET cases merged with SVI
cases_svi<-read.csv("covidnet_svi_nov15.csv")

## CREATE REGRESSION USING ESTIMATE VALUES
  
  #subset to include estimates from svi data
  e_sub<-ir_svi[,grep("^EP_", colnames(ir_svi))]
  e_sub<-colnames(e_sub)
  
  #histogram of all estimates
  
  
  #subset to include incidence and svi
  sub<-c("incidence",e_sub)
  lm.sub<-subset(ir_svi,select = sub)

  #use scale() to normalize data
  #lm.norm<-data.frame(scale(lm.sub))    #only scale covariates; apply scale() over columns
  
  #run regression
  mod1<-glm(incidence~EP_POV+EP_UNEMP+EP_PCI+EP_NOHSDP+EP_AGE65+EP_AGE17+
                         EP_DISABL+EP_SNGPNT+EP_MINRTY+EP_LIMENG+EP_MUNIT+
                         EP_MOBILE+EP_CROWD+EP_NOVEH+EP_GROUPQ+EP_UNINSUR,
            family = 'poisson',
            offset=log(pop/100000),
            data=lm.norm)
  summary(linmod)
  
  #find optimal model using AIC
  #install.packages("MASS")
  library(MASS)
  optimal_model<-stepAIC(linmod)
  summary(optimal_model)
  
  
  
  #TRY AGAIN WITH MIN-MAX NORMALIZATION
    #define function
    #min_max_norm <- function(x) {
    #  (x - min(x)) / (max(x) - min(x))
    #}
    
    #apply Min-Max normalization to data
    #lm.norm.mm <- as.data.frame(lapply(lm.sub, min_max_norm))    
    #summary(lm.norm.mm)
    
    #run regression
    #linmod.mm<-lm(incidence~EP_POV+EP_UNEMP+EP_PCI+EP_NOHSDP+EP_AGE65+EP_AGE17+
    #                        EP_DISABL+EP_SNGPNT+EP_MINRTY+EP_LIMENG+EP_MUNIT+
    #                        EP_MOBILE+EP_CROWD+EP_NOVEH+EP_GROUPQ+EP_UNINSUR,
    #              data=lm.norm.mm)
    #summary(linmod.mm)

    #AIC
    #optimal_model.mm<-stepAIC(linmod.mm)
    #summary(optimal_model.mm)
    #incidence ~ EP_POV + EP_UNEMP + EP_AGE65 + EP_DISABL + EP_MINRTY +  EP_MUNIT + EP_UNINSUR
    
    
    
 
  