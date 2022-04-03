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
#test <- subset(test, select = c("GEO_ID","incidence"))
#change name
names(test)[names(test) == "incidence"] <- "test.incidence"

#merge test and SVI
test_svi <- merge(test, svi, by = "GEO_ID", by.y = "FIPS", all.x = TRUE, all.y = FALSE)


#sub EP values
e_name<-names(test_svi[,grep("^EP_", colnames(test_svi))])

#subset including hosp rate, pos test rate
sub1<-names(test[-2])
sub2<-c(sub1,e_name)
mod_sub<-subset(test_svi,select = sub2)

  
  
  #summarize all vars
  summary(mod_sub)
  
  #corrplot of variables
  mod_sub2<-na.omit(mod_sub)
  cov_cor<-cor(mod_sub2)
  #corrplot(cov_cor,method = "number")
  corrplot(cov_cor, order = 'AOE')
  
  #scale covariates for all census tracts in CT, then subset COVID-NET catchment
  scaled <- cbind(mod_sub[1:2], apply(mod_sub[3:19], 2, scale))
  
  #histogram of all estimates
  plot_long<-gather(scaled[3:19], key = "name", value = "value")
  #remove PCI
  plot_long <- plot_long[!(plot_long$name == "EP_PCI"),]
  
  ggplot(plot_long) +
    geom_histogram(aes(value)) +
    facet_wrap(~name, ncol = 4)
  
  hist(scaled$EP_PCI)
  hist(scaled$test.incidence)
  
  #merge to COVID-NET data
  covidnet <- ir[1:2]
  names(covidnet)[names(covidnet) == "cases_per_cen"] <- "covidnet.cases" 
  mod_count <- merge(covidnet, scaled, by = "GEO_ID", all.x = TRUE, all.y = FALSE)
  
  #histogram of hosp rate
  hist(mod_count$covidnet.cases)
  mean(mod_count$covidnet.cases)
  var(mod_count$covidnet.cases)
  
##### regressions #####
  
  mod2<-glm(covidnet.cases~test.incidence+EP_POV+EP_UNEMP+EP_PCI+EP_NOHSDP+EP_AGE65+
              EP_AGE17+EP_DISABL+EP_SNGPNT+EP_MINRTY+EP_LIMENG+EP_MUNIT+
              EP_MOBILE+EP_CROWD+EP_NOVEH+EP_GROUPQ+EP_UNINSUR,
            family = 'poisson',
            offset=log(total_pop/100000),
            data=mod_count)
  summary(mod2)
  plot(mod2$residuals)
  #Residual deviance:  817.47  on 207  degrees of freedom
  
#neg binom regression
  #count data
  mod3<-glm.nb(covidnet.cases~test.incidence+EP_POV+EP_UNEMP+EP_PCI+EP_NOHSDP+EP_AGE65+
                 EP_AGE17+EP_DISABL+EP_SNGPNT+EP_MINRTY+EP_LIMENG+EP_MUNIT+
                 EP_MOBILE+EP_CROWD+EP_NOVEH+EP_GROUPQ+EP_UNINSUR+
                 offset(log(total_pop/100000)),
               data=mod_count)
  summary(mod3)
  plot(mod3$residuals)
  #Residual deviance: 229.72  on 207  degrees of freedom
  
  
#poisson with log transformed data
  mod4<-glm.nb(log(covidnet.cases)~test.incidence+EP_POV+EP_UNEMP+EP_PCI+EP_NOHSDP+EP_AGE65+
                 EP_AGE17+EP_DISABL+EP_SNGPNT+EP_MINRTY+EP_LIMENG+EP_MUNIT+
                 EP_MOBILE+EP_CROWD+EP_NOVEH+EP_GROUPQ+EP_UNINSUR+
                offset(log(total_pop/100000)),
                data=mod_count)
  summary(mod4)
  plot(mod4$residuals)

  
 
##### AIC of each dataset and analysis #####
#create list of predicted/observed values and coefficients
  set.seed(8008135)
  in_nb <- replicate(10, fitin_nb2(mod_count),simplify = FALSE)
  
  #pull out coefficients
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
  
  
  #read in census
  census<-read.csv("DECENNIALPL2010.P1_data_with_overlays_2022-02-11T112106.csv")
  census <- census[-1,] 
  census <- census[c("GEO_ID","NAME")]
  census$county <- sub("^([^,]+),\\s*([^,]+),.*", "\\2", census$NAME)
  
  
  pred.df2<-merge(pred.df,census,all.x = TRUE, all.y = FALSE)
  
  #grid of predicted vs. observed by NHV and MS
  p1 <- ggplot(pred.df2,aes(covidnet.cases, pred, color = county)) +
          geom_point() +
          geom_abline(slope = 1, color = "red") +
          theme_bw() +
          facet_wrap(~county)
  
  
  #mean absolute error
  library(data.table)
  pred.dt<-data.table(pred.df)
  mae1<-as.data.frame(pred.dt[, mae(covidnet.cases, pred), by = index])
  
#correlation
  pred.dt[, cor(x=covidnet.cases, y=pred), by = index]
    
    
    
    
###### convert counts to rates and re-plot #####
    
    #merge total pop of each census tract to pred.df
    tot <- subset(mod_count, select = c("GEO_ID","total_pop"))
    pred.df.rate <- merge(pred.df2, tot, by = "GEO_ID", all.x = TRUE, all.y = FALSE)
    pred.df.rate$observed.rate <- (pred.df.rate$covidnet.cases/pred.df.rate$total_pop)*100000
    pred.df.rate$predicted.rate <- (pred.df.rate$pred/pred.df.rate$total_pop)*100000
    
    #grid of ten different predicted vs. observed
    p2 <- ggplot(pred.df.rate,aes(observed.rate, predicted.rate, color = county)) +
            geom_point() +
            geom_abline(slope = 1, color = "red") +
            theme_bw() +
            facet_wrap(~county)
    
    #look at rate and count side by side
    gridExtra::grid.arrange(p1,p2,ncol=2)
    
    #correlation
    pred.dt.rate <- data.table(pred.df.rate)
    pred.dt.rate[, cor(x=cases_per_cen, y=pred), by = index]
    
    
    
##### predict for state ##### 
    
    #create dph dataset for model
      #read in state hosp data
      dph_hosp <- read.csv("dph2020_IR_FIPS2.csv")
      names(dph_hosp)[names(dph_hosp) == "cases_per_cen"] <- "dph_hosp_count"
      
      #merge with scaled
      dph_hosp <- merge(dph_hosp, scaled, by = "GEO_ID", all.x = TRUE, all.y = FALSE)
      
      
    
    #predict based on coef dataset
    all.covars <- paste(names(coefs)[-1], collapse='+')
    
    form1 <- as.formula(paste0('~',all.covars))
    
    dph_hosp2 <- na.omit(dph_hosp)
    coefs[is.na(coefs)] <- 0
    
    mod.mat1 <- model.matrix(form1, data=dph_hosp2)
    
    ave.coefs <- apply(coefs,2,mean)
    
    dph_hosp2$preds <- as.vector(exp(mod.mat1 %*% ave.coefs + log(dph_hosp2$total_pop.x/100000)))
    
    #drop covariates
    dph.pred<-subset(dph_hosp2,select = c("GEO_ID","dph_hosp_count","preds","total_pop.x"))
    
    
    #first check of sums
    sum(dph.pred$preds) - sum(dph.pred$dph_hosp_count)
    
    #convert to rates
    dph.pred$observed.rate <- (dph.pred$dph_hosp_count/dph.pred$total_pop.x)*100000
    dph.pred$predicted.rate <- (dph.pred$preds/dph.pred$total_pop.x)*100000
    
    dph.pred2<-merge(dph.pred,census,all.x = TRUE, all.y = FALSE)
    
    #sum of predicted and observed values per county
    county <- cbind(dph.pred2[8],dph.pred2[2:5]) 
    county <- aggregate(. ~ county, data = as.data.frame(county), FUN = sum)
    names(county)[names(county) == "dph_hosp_count"] <- "obs"
    #rates by county
    county$obs.rate <- (county$obs/county$total_pop)*100000
    county$pred.rate <- (county$pred/county$total_pop)*100000
    #difference in rates by county
    county$diff <- county$pred.rate - county$obs.rate
    
    #linmod for all census tracts using transformed data
    linmod1<-lm(observed.rate~predicted.rate, data = dph.pred)
    summary(linmod1)
    #intercept 3.77310
    plot(linmod1$residuals)
    
    ggplot(dph.pred, aes(x = predicted.rate, y = observed.rate)) + 
      geom_point(alpha = 0.3) +
      stat_smooth(method = "lm", col = "aquamarine3") +
      geom_abline(slope = 1, linetype = "dotted", size = 1, col = "red")+
      theme_bw()

    
    hist(log(dph.pred$predicted.rate))
    hist(sqrt(dph.pred$observed.rate))
    
    #linmod for all census tracts using transformed data
    linmod2<-lm(sqrt(observed.rate)~log(predicted.rate), data = dph.pred)
    summary(linmod2)
    #intercept 3.77310
    plot(linmod2$residuals)
    
    ggplot(dph.pred, aes(x = log(predicted.rate), y = sqrt(observed.rate))) + 
      geom_point(alpha = 0.3) +
      stat_smooth(method = "lm", col = "aquamarine3") +
      geom_abline(slope = 1, linetype = "dotted", size = 1, col = "red")+
      theme_bw()
    
    
    #pull out NHV and MS
    noNHVMS<-dph.pred[!(dph.pred$GEO_ID %in% covidnet$GEO_ID),]
    NHVMS <- dph.pred[(dph.pred$GEO_ID %in% covidnet$GEO_ID),]
    
    #linmod to check (no NHV and MS)
    linmod3<-lm(observed.rate ~ predicted.rate, data = noNHVMS)
    summary(linmod2)
    #intercept = -1.7217
    
    ggplot(noNHVMS, aes(x = predicted.rate, y = observed.rate)) + 
      geom_point(alpha = 0.3) +
      stat_smooth(method = "lm", col = "red") +
      theme_bw()
    
    
    #corrplot
    dph.pred.cor<-subset(NHVMS,select = c("observed.rate","predicted.rate"))
    dph.pred.cor<-cor(dph.pred.cor)
    #corrplot(cov_cor,method = "number")
    corrplot(dph.pred.cor, method = 'number')
    
    
    #look at plots
    p1<-ggplot(noNHVMS,aes(observed.rate, predicted.rate)) +
          geom_point() +
          geom_abline(slope = 1, color = "red") +
          ggtitle("Observed vs. Predicted Rates", subtitle = "for census tracts outside of the COVID-NET catchment area")
    
    p2<-ggplot(NHVMS,aes(observed.rate, predicted.rate)) +
          geom_point() +
          geom_abline(slope = 1, color = "red") +
          ggtitle("Observed vs. Predicted Rates", subtitle = "for census tracts in the COVID-NET catchment area")
    
    p3<-gridExtra::grid.arrange(p1,p2,ncol=2)
    
    #try one plot with different colors
    plot<-dph.pred
    catch<-ir$GEO_ID
    plot$legend<-ifelse(dph.pred$GEO_ID %in% catch,c("in catchment"),c("out of catchment"))
    
    p4<-ggplot(plot,aes(observed.rate, predicted.rate,color=legend)) +
          geom_point(alpha=0.5) +
          geom_abline(slope = 1, color = "red") +
          theme_bw() +
          xlab("observed rate") + ylab("predicted rate") +
          scale_color_manual(values = c("#2C7096","#E69833")) +
          facet_wrap(~legend)
    
    p5<-ggplot(dph.pred2,aes(observed.rate, predicted.rate,color=county)) +
          geom_point(alpha=0.6) +
          geom_abline(slope = 1, color = "red") +
          theme_bw() +
          xlab("Observed Hospitalization Rate") + ylab("Estimated Hospitalization Rate")+
          facet_wrap(~county)
          #scale_color_manual(values = c("#2C7096","#E69833"))
    
    
  
    
    
  
    
   
    
    