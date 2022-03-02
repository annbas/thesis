setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data")

library(tidyr)
library(dplyr)
library(purrr)
#install.packages("Hmisc")
library(Hmisc)
library(ggplot2)
library(corrplot)
#install.packages("e1071")
library(e1071)
library(lmtest)
library(MASS)
#install.packages("plyr")
library(plyr)
library(Metrics)


#read in data
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
  
  #corrplot of variables
  mod_sub2<-na.omit(mod_sub)
  cov_cor<-cor(mod_sub2)
  #corrplot(cov_cor,method = "number")
  corrplot(cov_cor, order = 'AOE')
  
  #histogram of all estimates
  plot_long<-gather(e_sub, key = "name", value = "value")
  ggplot(plot_long) +
    geom_histogram(aes(value)) +
    facet_wrap(~name, ncol = 4)
  
  hist(e_sub_unin$EP_UNINSUR)
  hist(mod_sub$test.incidence)
  
  
  #histogram of hosp rate
  hist(mod_sub$hosp.incidence)
  mean(mod_sub$hosp.incidence)
  var(mod_sub$hosp.incidence)
  
  #scale
  mod_sc<-cbind(mod_sub[1],ir[3],apply(mod_sub[2:18],2, scale))
  
  #corplot
  mod_sc_cor<-na.omit(mod_sc)
  cov_cor<-cor(mod_sc_cor)
  corrplot(cov_cor, order = 'AOE')
  
  
####### regressions #####
   
#using hosp count instead?
  mod_count<-cbind(ir[2],mod_sc[2:19])
  
  mod2<-glm(cases_per_cen~test.incidence+EPL_POV+EPL_UNEMP+EPL_PCI+EPL_NOHSDP+EPL_AGE65+
              EPL_AGE17+EPL_DISABL+EPL_SNGPNT+EPL_MINRTY+EPL_LIMENG+EPL_MUNIT+
              EPL_MOBILE+EPL_CROWD+EPL_NOVEH+EPL_GROUPQ+EP_UNINSUR,
            family = 'poisson',
            offset=log(total_pop/100000),
            data=mod_count)
  summary(mod2)
  plot(mod2$residuals)
  #Residual deviance:  779.46  on 207  degrees of freedom
  
#neg binom regression
  #count data
  mod3<-glm.nb(cases_per_cen~test.incidence+EPL_POV+EPL_UNEMP+EPL_PCI+EPL_NOHSDP+EPL_AGE65+
                            EPL_AGE17+EPL_DISABL+EPL_SNGPNT+EPL_MINRTY+EPL_LIMENG+EPL_MUNIT+
                            EPL_MOBILE+EPL_CROWD+EPL_NOVEH+EPL_GROUPQ+EP_UNINSUR+
                 offset(log(total_pop/100000)),
               data=mod_count)
  summary(mod3)
  plot(mod3$residuals)
  #Residual deviance: 232.08  on 206  degrees of freedom
  
  #null model
  null_mod<-glm.nb(cases_per_cen~total_pop+test.incidence+
                     offset(log(total_pop/100000)),
                   data=mod_count)
  summary(null_mod)
  
#poisson with log transformed data
  mod4<-glm(log(cases_per_cen)~test.incidence+EPL_POV+EPL_UNEMP+EPL_PCI+EPL_NOHSDP+EPL_AGE65+
              EPL_AGE17+EPL_DISABL+EPL_SNGPNT+EPL_MINRTY+EPL_LIMENG+EPL_MUNIT+
              EPL_MOBILE+EPL_CROWD+EPL_NOVEH+EPL_GROUPQ+EP_UNINSUR,
            family = 'poisson',
            offset=log(total_pop/100000),
            data=mod_count)
  summary(mod4)
  plot(mod4$residuals)
  
#find optimal model using AIC
  optimal_model<-stepAIC(mod3)
  summary(optimal_model)
  #AIC of null mod vs. optimal mod
  AIC(null_mod)
  AIC(optimal_model)

  

################################################################################
 
  #function that creates a dataset with 10% missing data
  miss<-function(data){
    
    #add back census id
    data<-cbind(ir[1],data)
    
    #set.seed(123) ??
    
    #randomly sample 10% of rows
    x1<-data %>% 
         sample_frac(.1)
    
    ungroup(x1)
    
    #exclude sampled rows from datset
    x2<-data[!(data$GEO_ID %in% x1$GEO_ID),]
    x2<-x2[2:20]
    
    #create list of 
    list(x1,x2)
    
  }
  
################################################################################
  
#create list of 10 datasets of in sample and out of sample
  result <- replicate(10, miss(mod_count),simplify = FALSE)
  
  #create list of out of sample and in samples
  listOut <- lapply(result, "[[", 1)
  listIn <- lapply(result, "[[", 2)
  
  #AIC of hold out
    x<-lapply(listOut, function(x){
                 mod<-glm.nb(cases_per_cen~test.incidence+EPL_POV+EPL_UNEMP+EPL_PCI+EPL_NOHSDP+EPL_AGE65+
                                EPL_AGE17+EPL_DISABL+EPL_SNGPNT+EPL_MINRTY+EPL_LIMENG+EPL_MUNIT+
                                EPL_MOBILE+EPL_CROWD+EPL_NOVEH+EPL_GROUPQ+EP_UNINSUR+
                              offset(log(total_pop/100000)),
                              data=x)
                y<-stepAIC(mod)
                z<-list(y$call,y$aic)
              }
            )
  
  #try predicting with model in lapply 
  predict_miss<-lapply(result, function(x){
        mod6<-glm.nb(cases_per_cen ~ test.incidence + EPL_UNEMP + EPL_AGE65 + 
                       EPL_SNGPNT + EPL_MINRTY + EPL_MUNIT + EPL_GROUPQ + EP_UNINSUR + 
                     offset(log(total_pop/1e+05)),
                  data=x[[2]])
        predict(mod6, newdata = x[[1]], type = "response")
      }
    ) 
  
#create dataset to compare predicted and out of sample values
  hm<-as.data.frame(unlist(lapply(listOut,function(x) x[,1])))
  names(hm)[names(hm) == "unlist(lapply(listOut, function(x) x[, 1]))"] <- "GEO_ID"
  hm$mod<-unlist(predict_miss)
  hm$out.samp<-unlist(lapply(listOut,function(x) x[,2]))
 
  
  #mean absolute error for each dataset
  maeSub<-lapply(listOut, "[", , "cases_per_cen")
  pred_vec<-lapply(predict_miss,as.vector)
  
  #mean absolute error
  maeres<-NULL
  for(i in 1:10){
    x2<-mae(maeSub[[i]],pred_vec[[i]])
    maeres<-c(maeres,x2)
  }
   
#plot modeled vs. actual
  ggplot(hm, aes(x=mod, y=out.samp)) + 
    geom_point() +
    geom_abline(slope = 1, color = "red") +
    xlab("predicted") +
    ylab("observed") +
    labs(caption = "not log-transformed") +
    theme_classic()
  
#correlogram
  m<-na.omit(hm)
  m<-cor(m[,2:3])
  corrplot(m,method = "number")

  
                        
  
#try with log transformed data
  predict_miss_log<-lapply(result, function(x){
    mod6<-glm(log(cases_per_cen) ~ test.incidence + EPL_POV + 
                EPL_UNEMP + EPL_PCI + EPL_NOHSDP + EPL_AGE65 + EPL_AGE17 + 
                EPL_DISABL + EPL_SNGPNT + EPL_MINRTY + EPL_LIMENG + EPL_MUNIT + 
                EPL_MOBILE + EPL_CROWD + EPL_NOVEH + EPL_GROUPQ + EP_UNINSUR,
              data=x[[2]])
    pred<-exp(predict(mod6, newdata = x[[1]], type = "response"))
    }
  ) 

#create dataset to compare predicted and out of sample values
  log_hm<-hm
  log_hm$mod<-NULL
  log_hm$mod<-unlist(predict_miss_log)
    
  #plot modeled vs. actual
  ggplot(log_hm, aes(x=mod, y=out.samp)) + 
    geom_point() +
    geom_abline(slope = 1, color = "red") +
    labs(caption = "log-transformed") +
    theme_classic()
  
  #correlogram
  m<-na.omit(log_hm)
  m<-cor(m[,2:3])
  corrplot(m,method = "number")
  
  
  
  

  
  
  
  
  
  
  
