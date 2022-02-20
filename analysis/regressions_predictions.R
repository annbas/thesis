setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data")

library(tidyr)
library(dplyr)
#install.packages("Hmisc")
library(Hmisc)
library(ggplot2)
#install.packages("corrplot")
library(corrplot)
#install.packages("e1071")
library(e1071)
#install.packages("lmtest")
library(lmtest)
library(MASS)


#2020 IR per census tract and merge with SVI dataset
ir<-read.csv("covidnet2020_IR_FIPS.csv")
svi<-read.csv("CDC_CensusTract_SVI.csv") 
test<-read.csv("dph2020_test_IR_FIPS.csv")


##replace -999 with NA?
  svi[svi == -999] <- NA
  #count NAs in each column
  apply(is.na(svi), MARGIN = 2, FUN = sum)

#sub FIPS and test in test
test<-subset(test, select = c("GEO_ID","incidence"))
#change name
names(test)[names(test) == "incidence"] <- "test.incidence"

#merge test and hosp 
ir_svi<-merge(ir, test, by = "GEO_ID", all.x = TRUE, all.y = FALSE)
names(ir_svi)[names(ir_svi) == "incidence"] <- "hosp.incidence"

#merge SVI to incidence on FIPS
ir_svi<-merge(ir_svi, svi, by.x = "GEO_ID", by.y = "FIPS", all.x = TRUE, all.y = FALSE) #dataset with IR and SVI variables


  
  #subset to include estimates from svi data
  e_name<-ir_svi[,grep("^EPL_", colnames(ir_svi))]
  e_name<-colnames(e_name)
  e_name_unin<-c(e_name,"EP_UNINSUR") #add uninsured even though it doesn't have an EPL value?
  e_sub<-subset(ir_svi,select=e_name)
  e_sub_unin<-subset(ir_svi,select=e_name_unin)
  
  #subset including hosp rate, pos test rate
  sub<-c("hosp.incidence","test.incidence",e_name_unin)
  mod_sub<-subset(ir_svi,select = sub)
  
  
  #summarize all vars
  summary(mod_sub)
  
  
  
  #histogram of all estimates
  plot_long<-gather(e_sub, key = "name", value = "value")
  ggplot(plot_long) +
    geom_histogram(aes(value)) +
    facet_wrap(~name, ncol = 4)
  
  hist(e_sub_unin$EP_UNINSUR)
  hist(mod_sub$test.incidence)
  
  
  #variance ?????
  bptest(test.incidence~hosp.incidence, data=mod_sub)
  
  
  #histogram of hosp rate
  hist(mod_sub$hosp.incidence)
  mean(mod_sub$hosp.incidence)
  var(mod_sub$hosp.incidence)
  
  #scale ???
  mod_sc<-cbind(mod_sub[1],apply(mod_sub[2:18],2, scale))
  
#is pop in mod1 the total pop of new haven and middlesex?
pop<-sum(ir$total_pop) 
  
#pois regression
  mod1<-glm(hosp.incidence~test.incidence+EPL_POV+EPL_UNEMP+EPL_PCI+EPL_NOHSDP+EPL_AGE65+
                         EPL_AGE17+EPL_DISABL+EPL_SNGPNT+EPL_MINRTY+EPL_LIMENG+EPL_MUNIT+
                         EPL_MOBILE+EPL_CROWD+EPL_NOVEH+EPL_GROUPQ+EP_UNINSUR,
            family = 'poisson',
            #offset=log(pop/100000), #Error: variable lengths differ (found for '(offset)')
            data=mod_sc)
  summary(mod1)
  #warnings that outcome is non-integer--use hosp count instead of rate?
  #Residual deviance:  9683.1  on 206  degrees of freedom; use negative binomial
  
#using hosp count instead
  mod_count<-cbind(ir[2],mod_sc[2:18])
  
  mod2<-glm(cases_per_cen~test.incidence+EPL_POV+EPL_UNEMP+EPL_PCI+EPL_NOHSDP+EPL_AGE65+
              EPL_AGE17+EPL_DISABL+EPL_SNGPNT+EPL_MINRTY+EPL_LIMENG+EPL_MUNIT+
              EPL_MOBILE+EPL_CROWD+EPL_NOVEH+EPL_GROUPQ+EP_UNINSUR,
            family = 'poisson',
            #offset=log(pop/100000), #Error: variable lengths differ (found for '(offset)')
            data=mod_count)
  summary(mod2)
  #still Residual deviance:  9683.1  on 206  degrees of freedom; use negative binomial
  
#neg binom regression
  #still using count data?
  mod3<-glm.nb(cases_per_cen~test.incidence+EPL_POV+EPL_UNEMP+EPL_PCI+EPL_NOHSDP+EPL_AGE65+
                            EPL_AGE17+EPL_DISABL+EPL_SNGPNT+EPL_MINRTY+EPL_LIMENG+EPL_MUNIT+
                            EPL_MOBILE+EPL_CROWD+EPL_NOVEH+EPL_GROUPQ+EP_UNINSUR,
               data=mod_count)
  summary(mod3)
  #Residual deviance: 232.08  on 206  degrees of freedom ?????
  
#try again with missing values removed before putting into model?
  #should missing value be replaced with 0?
  no_na_mod<-na.omit(mod_count)
  mod4<-glm.nb(cases_per_cen~test.incidence+EPL_POV+EPL_UNEMP+EPL_PCI+EPL_NOHSDP+EPL_AGE65+
                 EPL_AGE17+EPL_DISABL+EPL_SNGPNT+EPL_MINRTY+EPL_LIMENG+EPL_MUNIT+
                 EPL_MOBILE+EPL_CROWD+EPL_NOVEH+EPL_GROUPQ+EP_UNINSUR,
               data=no_na_mod)
  summary(mod4)
  
  
#find optimal model using AIC
  #try 
  optimal_model<-stepAIC(mod4)
  summary(optimal_model)
  
  # stepAIC output
  #
  # Step:  AIC=1676.25
  # cases_per_cen ~ test.incidence + EPL_POV + EPL_UNEMP + EPL_AGE65 + 
  #   EPL_SNGPNT + EPL_MINRTY + EPL_LIMENG + EPL_MUNIT + EPL_MOBILE + 
  #   EPL_CROWD + EPL_NOVEH
  #
  #
  # Step:  AIC=1675.43
  # cases_per_cen ~ test.incidence + EPL_POV + EPL_UNEMP + EPL_AGE65 + 
  #   EPL_SNGPNT + EPL_MINRTY + EPL_MUNIT + EPL_MOBILE + EPL_CROWD + 
  #   EPL_NOVEH
  #
  #
  # Step:  AIC=1675.22
  # cases_per_cen ~ test.incidence + EPL_POV + EPL_UNEMP + EPL_AGE65 + 
  #   EPL_SNGPNT + EPL_MINRTY + EPL_MUNIT + EPL_MOBILE + EPL_NOVEH

  
  
  

  
#estimate new haven and middlesex
  geoid_sub<-cbind(ir[1:2],mod_sc[2:18])
  
  #leave out sample
  rem<-geoid_sub %>% 
          sample_frac(.1)
  
  #subset w/o leave out sample
  train<-geoid_sub[!(geoid_sub$GEO_ID %in% rem$GEO_ID),]
  train<-train[2:19]

################################################################################
 
  #function that creates a dataset with 10% missing data
  miss<-function(){
    
    #randomly sample 10% of rows
    x1<-geoid_sub %>% 
         sample_frac(.1)
    
    ungroup(x1)
    
    #exclude sampled rows from datset
    x2<-geoid_sub[!(geoid_sub$GEO_ID %in% x1$GEO_ID),]
    x2<-x2[2:19]
    
    #create list of 
    list(x1,x2)
    
  }
  
################################################################################
  
#create list of 10 datasets of in sample and out of sample
  result <- replicate(10, miss(),simplify = FALSE)
  
  #create list of out of sample and in samples
  listOut <- lapply(result, "[[", 1)
  listIn <- lapply(result, "[[", 2)
  
  #train
  mod5<-glm.nb(cases_per_cen ~ test.incidence + EPL_POV + EPL_UNEMP + EPL_AGE65 + 
                 EPL_SNGPNT + EPL_MINRTY + EPL_MUNIT + EPL_MOBILE + EPL_NOVEH,
               data=train)
  summary(mod5)  

  #predict
  predict_miss<-predict(mod5, newdata = rem, type = "response")
  cbind(predict_miss,rem$cases_per_cen)
  