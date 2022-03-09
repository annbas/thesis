setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data")

library(tidyverse)
#install.packages("Hmisc")
library(Hmisc)
library(corrplot)
#install.packages("e1071")
library(e1071)
library(lmtest)
library(MASS)
#install.packages("plyr")
library(plyr)
library(Metrics)
#install.packages("BMA")
library(BMA)
source("Z:/FluSurv-NET/COVID-19/Ann/Thesis/thesis/analysis/mod_functions.R")


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
  
  
##### regressions #####
   
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
  
  
#poisson with log transformed data
  mod4<-glm.nb(log(cases_per_cen)~test.incidence+EPL_POV+EPL_UNEMP+EPL_PCI+EPL_NOHSDP+EPL_AGE65+
                  EPL_AGE17+EPL_DISABL+EPL_SNGPNT+EPL_MINRTY+EPL_LIMENG+EPL_MUNIT+
                  EPL_MOBILE+EPL_CROWD+EPL_NOVEH+EPL_GROUPQ+EP_UNINSUR+
                offset(log(total_pop/100000)),
                data=mod_count)
  summary(mod4)
  plot(mod4$residuals)
  
#find optimal model using AIC
  optimal_model<-stepAIC(mod3)
  summary(optimal_model)
  AIC(optimal_model)

  
 
##### AIC of each dataset and analysis #####
#create list of predicted/observed values and coefficients
  #out_pois <- replicate(10, fitout_pois(mod_count),simplify = FALSE)
  #out_nb <- replicate(10, fitout_nb(mod_count),simplify = FALSE)
  in_nb <- replicate(10, fitin_nb(mod_count),simplify = FALSE)
  #in_pois <- replicate(10, fitin_pois(mod_count),simplify = FALSE)
  
  
  
  coefs<-sapply(in_nb,"[[","coef")
  coefs<-bind_rows(coefs)
  ungroup(coefs)
  
 
  #count how many times coefficient appeared in each dataset
  sort(apply(coefs,2,function(x){
    10-sum(is.na(x))
      }
    ), decreasing = TRUE)
  
  #get mean of each column
  sort(apply(coefs,2,mean,na.rm=T))
  
  #pull list of predicted and observed values
  pred.df<-lapply(in_nb,"[[","pred.df")
  
  #flatten pred.df into one dataset and add new column that designates index in list
  pred.df<-bind_rows(pred.df)
  pred.df$index<-rep(1:10, each = 22)
  
  #grid of ten different predicted vs. observed
  p1 <- ggplot(pred.df,aes(cases_per_cen, pred)) +
          geom_point() +
          geom_abline(slope = 1, color = "red") +
          facet_wrap(~index)
  
  #look at 4 & 10
  plot2<-plot %>%
          filter(index == 4 | index == 10)
  
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
  
  
##### adding to null model #####
  #null model
    null_mod<-glm.nb(cases_per_cen~log(total_pop),
                     data=mod_count)
    AIC(null_mod)
    #1800.888
    
  #add in EPL_AGE65 (next best)
    modA<-glm.nb(cases_per_cen~EPL_AGE65+
                   offset(log(total_pop/100000)),
                     data=mod_count)
    AIC(modA)
    #1800.4
    
  #take out EPL_AGE65, add in EPL_MINRTY
    modB<-glm.nb(cases_per_cen~EPL_MINRTY+
                   offset(log(total_pop/100000)),
                 data=mod_count)
    AIC(modB)
    #1728.461
    
  #Add EPL_AGE65 back in
    modC<-glm.nb(cases_per_cen~EPL_MINRTY+EPL_AGE65+
                   offset(log(total_pop/100000)),
                 data=mod_count)
    AIC(modC)
    #1678.894
    
  #Add testing
    modD<-glm.nb(cases_per_cen~EPL_MINRTY+EPL_AGE65+test.incidence+
                   offset(log(total_pop/100000)),
                 data=mod_count)
    AIC(modD) 
    #1637.849
    
  #Add EPL_SNGPNT
    modE<-glm.nb(cases_per_cen~EPL_MINRTY+EPL_AGE65+
                   test.incidence+EPL_SNGPNT+
                   offset(log(total_pop/100000)),
                 data=mod_count)
    AIC(modE) 
    #1630.707
    
  #Add EPL_GROUPQ
    modF<-glm.nb(cases_per_cen~EPL_MINRTY+EPL_AGE65+
                   test.incidence+EPL_SNGPNT+EPL_GROUPQ+
                   offset(log(total_pop/100000)),
                 data=mod_count)
    AIC(modF)
    #1616.643
    
  #Add EP_UNINSUR
    modG<-glm.nb(cases_per_cen~EPL_MINRTY+EPL_AGE65+test.incidence+
                   EPL_SNGPNT+EPL_GROUPQ+EP_UNINSUR+
                   offset(log(total_pop/100000)),
                 data=mod_count)
    AIC(modG)
    #1611.601
    
  #Add EPL_MUNIT
    modH<-glm.nb(cases_per_cen~EPL_MINRTY+EPL_AGE65+
                   test.incidence+EPL_SNGPNT+EPL_GROUPQ+EP_UNINSUR+
                   EPL_MUNIT+offset(log(total_pop/100000)),
                 data=mod_count)
    AIC(modH)
    #1608.484
    
  #Add EPL_UNEMP
    modI<-glm.nb(cases_per_cen~EPL_MINRTY+EPL_AGE65+
                   test.incidence+EPL_SNGPNT+EPL_GROUPQ+EP_UNINSUR+
                   EPL_MUNIT+EPL_UNEMP+
                   offset(log(total_pop/100000)),
                 data=mod_count)
    AIC(modI)
    #1605.432

    
##### look at outlier data #####
    #add back census id
    full_dat<-cbind(ir[1],mod_count)
    
    #pull five highest values
    badfit <- pred.df.opt %>%                                      
      arrange(desc(cases_per_cen)) %>% 
      slice(1:5)
    
    #extract geo id
    geo<-badfit$GEO_ID
    
    #filter
    outlier<-filter(full_dat, GEO_ID %in% geo)
    
    #see what towns they're in
    town<-read.csv("tract2town-2010.csv")
    filter(town, ï..tract_fips %in% geo)
    
    #9009350100 (Waterbury) being overestimated a ton
    
    
    
    
###### convert counts to rates and re-plot #####
    
    #merge total pop of each census tract to pred.df
    tot <- subset(ir, select = c("GEO_ID","total_pop"))
    pred.df.rate <- merge(pred.df, tot, by = "GEO_ID", all.x = TRUE, all.y = FALSE)
    pred.df.rate$observed.rate <- (pred.df.rate$cases_per_cen/pred.df.rate$total_pop)*100000
    pred.df.rate$predicted.rate <- (pred.df.rate$pred/pred.df.rate$total_pop)*100000
    
    #grid of ten different predicted vs. observed
    p2 <- ggplot(pred.df.rate,aes(observed.rate, predicted.rate)) +
            geom_point() +
            geom_abline(slope = 1, color = "red") +
            facet_wrap(~index)
    
    #look at rate and count side by side
    gridExtra::grid.arrange(p1,p2,ncol=2)
    
    #correlation
    pred.dt.rate <- data.table(pred.df.rate)
    pred.dt.rate[, cor(x=cases_per_cen, y=pred), by = index]
    
    
##### matrix multiplication to find optimal model #####
    
    #convert NAs to 0
    coefs[is.na(coefs)] <- 0
    
    #create a model matrix and do matrix multiplication by coef
    mod.mat <- model.matrix(~test.incidence + EPL_AGE65 + EPL_DISABL + 
                              EPL_SNGPNT + EPL_MINRTY + EPL_MUNIT + EPL_MOBILE +
                              EPL_GROUPQ + EP_UNINSUR + EPL_LIMENG + EPL_UNEMP,
                            data = mod_count)
    
    coefs.mat <- as.matrix(coefs)
    
    data<-data.frame(NA_col = rep(NA, 225))
    #for loop to create matrix 
    for(i in 1:10){
      new<-mod.mat %*% coefs.mat[i,] + log(mod_count$total_pop/100000) #is my intercept term the offset or is it something else
      data[ , i] <- new                     
      colnames(data)[i] <- paste0("V", i) 
    }

    
    
    
    
    
    
    
    
    
    
    
    
##### predict for state ##### 
    
    #create dph dataset for model
      #read in state hosp data
      dph_hosp <- read.csv("dph2020_IR_FIPS.csv")
      names(dph_hosp)[names(dph_hosp) == "cases_per_cen"] <- "dph_hosp_count"
      
      #merge with test
      dph_hosp <- merge(dph_hosp, test, by = "GEO_ID", all.x = FALSE, all.y = TRUE)
      
      #subset svi and merge
      svi.sub <- subset(svi,select=c("FIPS", e_name_unin))
      dph_hosp <- merge(dph_hosp, svi.sub, by.x = "GEO_ID", by.y = "FIPS", all.x = TRUE, all.y = FALSE)
      
      
    #scale
    dph_hosp <- cbind(dph_hosp[1:4],apply(dph_hosp[5:21],2, scale))
    
    #fit to covid-net data
    mod.fit <- glm.nb(cases_per_cen~EPL_MINRTY+EPL_AGE65+
                       test.incidence+EPL_SNGPNT+EPL_GROUPQ+EP_UNINSUR+
                       EPL_MUNIT+EPL_UNEMP+
                       offset(log(total_pop/100000)),
                     data=mod_count)              #mod_count is leave in sample
    
    #drop covid-net census tracts
    outsamp <- dph_hosp[!(dph_hosp$GEO_ID %in% ir$GEO_ID),]
    
    #predict and bind to outsamp
    outsamp$pred <- predict(mod.fit, newdata = outsamp, type = "response")
    
    #drop covariates
    dph.pred<-subset(outsamp,select = c("GEO_ID","dph_hosp_count","pred","total_pop"))
    
    #first check of sums
    sum(dph.pred$pred, na.rm = T) - sum(dph.pred$dph_hosp_count,na.rm = T)
    
    #convert to rates
    dph.pred$observed.rate <- (dph.pred$dph_hosp_count/dph.pred$total_pop)*100000
    dph.pred$predicted.rate <- (dph.pred$pred/dph.pred$total_pop)*100000
    
    #linmod to check
    linmod<-lm(observed.rate~predicted.rate, data = dph.pred)
    summary(linmod)
    #intercept = -2.76
    
    #look at plot
    ggplot(dph.pred,aes(observed.rate, predicted.rate)) +
      geom_point() +
      geom_abline(slope = 1, color = "red") 
    
    
    sum(is.na(dph.pred$dph_hosp_count))
    sum(is.na(dph.pred$pred))
    