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
  null_mod<-glm.nb(cases_per_cen~log(total_pop),
                   data=mod_count)
  summary(null_mod)
  
#poisson with log transformed data
  mod4<-glm.nb(log(cases_per_cen)~test.incidence+EPL_POV+EPL_UNEMP+EPL_PCI+EPL_NOHSDP+EPL_AGE65+
                  EPL_AGE17+EPL_DISABL+EPL_SNGPNT+EPL_MINRTY+EPL_LIMENG+EPL_MUNIT+
                  EPL_MOBILE+EPL_CROWD+EPL_NOVEH+EPL_GROUPQ+EP_UNINSUR+
                offset(log(total_pop/100000)),
                data=mod_count)
  summary(mod4)
  plot(mod4$residuals)
  
#find optimal model using AIC
  optimal_model<-stepAIC(mod4)
  summary(optimal_model)
  #AIC of null mod vs. optimal mod
  AIC(null_mod)
  AIC(optimal_model)

  

################################################################################
 
  #function that creates a dataset with 10% missing data
  mod_estim<-function(data){
    
    #add back census id
    data<-cbind(ir[1],data)
    
    #set.seed(123) ??
    
    #randomly sample 10% of rows
    dfOut<-data %>% 
         sample_frac(.1)
    
    ungroup(dfOut)
    
    #exclude sampled rows from dataset
    dfIn<-data[!(data$GEO_ID %in% dfOut$GEO_ID),]
    #dfIn<-dfIn[2:20]
    
    #AIC of hold out (?) switched to poisson due to warnings
    mod<-glm(cases_per_cen~test.incidence+EPL_POV+EPL_UNEMP+EPL_PCI+EPL_NOHSDP+EPL_AGE65+
                  EPL_AGE17+EPL_DISABL+EPL_SNGPNT+EPL_MINRTY+EPL_LIMENG+EPL_MUNIT+
                  EPL_MOBILE+EPL_CROWD+EPL_NOVEH+EPL_GROUPQ+EP_UNINSUR+
                  offset(log(total_pop/100000)),
                family = 'poisson',
                data=dfOut)
   
    #find model using stepAIC
    opt_mod<-stepAIC(mod)
   
    #generate predictions and bind to leave out
    dfIn$pred<-predict(opt_mod, newdata = dfIn, type = "response")
    #drop covariates
    dfIn<-subset(dfIn,select = c("GEO_ID","cases_per_cen","pred"))
    
    #extract coefficients
    coef1<-coef(opt_mod)
    
    #return list
    out.list<-list("coef"=coef1,"pred.df"=dfIn)
    

    
    #stepAIC for each hold out
    #generate predictions
    #merge with original dataset
    #drop rows from hold out
    #extract coefficients (coef())
    
  }  
  
  
################################################################################
  
#create list of predicted/observed values and coefficients
  result <- replicate(10, mod_estim(mod_count),simplify = FALSE)
  
  
  
  coefs<-sapply(result,"[[","coef")
  coefs<-bind_rows(coefs)
  ungroup(coefs)
 
  #count NAs across columns for ?
  sort(apply(coefs,2,function(x){
    sum(is.na(x))
      }
    ))
  
  #pull list of predicted and observed values
  pred.df<-lapply(result,"[[","pred.df")

  
  
  #flatten pred.df into one dataset and add new column that designates index in list
  pred.df<-bind_rows(pred.df)
  pred.df$index<-rep(1:10, each = 203)
  
  plot<-pred.df
  #grid of ten different predicted vs. observed
  ggplot(plot,aes(cases_per_cen, pred)) +
    geom_point() +
    geom_abline(slope = 1, color = "red") +
    facet_wrap(~index)
  
  #only look at 1, 4, 5, 7, 9
  plot2<-plot %>%
          filter(index == 1 | index == 4 | index == 5 |
                 index == 7 | index == 9)
  
  ggplot(plot2,aes(cases_per_cen, pred)) +
    geom_point() +
    geom_abline(slope = 1, color = "red") +
    facet_wrap(~index)
  #funnel-shaped??
  
  #mean absolute error
  library(data.table)
  pred.dt<-data.table(pred.df)
  mae1<-as.data.frame(pred.dt[, mae(cases_per_cen, pred), by = index])
  
#correlation
  pred.dt[, cor(x=cases_per_cen, y=pred), by = index]


  
                        
  
