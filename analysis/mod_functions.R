##### fit to hold out (10%), poisson #####

#function that creates a dataset with 10% missing data
fitout_pois<-function(data){
  
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
             EPL_MOBILE+EPL_CROWD+EPL_NOVEH+EPL_GROUPQ+EP_UNINSUR,
           family = 'poisson',
           offset=log(total_pop/100000),
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







##### fit to hold out (10%), neg binom #####

#create a dataset with 10% missing data
  fitout_nb<-function(data){
    
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
    
    #AIC of hold out (?)
    mod<-glm.nb(cases_per_cen~test.incidence+EPL_POV+EPL_UNEMP+EPL_PCI+EPL_NOHSDP+EPL_AGE65+
                   EPL_AGE17+EPL_DISABL+EPL_SNGPNT+EPL_MINRTY+EPL_LIMENG+EPL_MUNIT+
                   EPL_MOBILE+EPL_CROWD+EPL_NOVEH+EPL_GROUPQ+EP_UNINSUR+
                   offset(log(total_pop/100000)),
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
  

  
  
 
  
   
##### fit to leave in (90%), neg binom ##### 
  
  #create a dataset with 10% missing data
  fitin_nb<-function(data){
    
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
    
    #AIC of in-sample (?)
    mod<-glm.nb(cases_per_cen~test.incidence+EPL_POV+EPL_UNEMP+EPL_PCI+EPL_NOHSDP+EPL_AGE65+
                  EPL_AGE17+EPL_DISABL+EPL_SNGPNT+EPL_MINRTY+EPL_LIMENG+EPL_MUNIT+
                  EPL_MOBILE+EPL_CROWD+EPL_NOVEH+EPL_GROUPQ+EP_UNINSUR+
                  offset(log(total_pop/100000)),
                data=dfIn)
    
    #find model using stepAIC
    opt_mod<-stepAIC(mod)
    
    #generate predictions and bind to leave out
    dfOut$pred<-predict(opt_mod, newdata = dfOut, type = "response")
    #drop covariates
    dfOut<-subset(dfOut,select = c("GEO_ID","cases_per_cen","pred"))
    
    #extract coefficients
    coef1<-coef(opt_mod)
    
    #return list
    out.list<-list("coef"=coef1,"pred.df"=dfOut)
    
  }
  

  
  
  
  
  

##### fit to leave in (90%), poisson #####

#create a dataset with 10% missing data
fitin_pois<-function(data){
  
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
  
  #AIC of in-sample (?)
  mod<-glm(cases_per_cen~test.incidence+EPL_POV+EPL_UNEMP+EPL_PCI+EPL_NOHSDP+EPL_AGE65+
             EPL_AGE17+EPL_DISABL+EPL_SNGPNT+EPL_MINRTY+EPL_LIMENG+EPL_MUNIT+
             EPL_MOBILE+EPL_CROWD+EPL_NOVEH+EPL_GROUPQ+EP_UNINSUR,
           family = 'poisson',
           offset=log(total_pop/100000),
           data=dfIn)
  
  #find model using stepAIC
  opt_mod<-stepAIC(mod)
  
  #generate predictions and bind to leave out
  dfOut$pred<-predict(opt_mod, newdata = dfOut, type = "response")
  #drop covariates
  dfOut<-subset(dfOut,select = c("GEO_ID","cases_per_cen","pred"))
  
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






##### fit to leave in (90%), neg binom, use best mod compared to null ##### 

#create a dataset with 10% missing data
fit_optmod<-function(data){
  
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
  
  #fit the model
  mod<-glm.nb(cases_per_cen~EPL_MINRTY+EPL_AGE65+test.incidence+EPL_SNGPNT+
                EPL_GROUPQ+EP_UNINSUR+EPL_MUNIT+EPL_UNEMP+
                offset(log(total_pop/100000)),
              data=dfIn)
  
  #generate predictions and bind to leave out
  dfOut$pred<-predict(mod, newdata = dfOut, type = "response")
  #drop covariates
  dfOut<-subset(dfOut,select = c("GEO_ID","cases_per_cen","pred"))
  
  #extract coefficients
  coef1<-coef(opt_mod)
  
  #return list
  out.list<-list("coef"=coef1,"pred.df"=dfOut)
  
}


