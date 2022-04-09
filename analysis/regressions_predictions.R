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
  set.seed(123)
  in_nb <- replicate(10, fitin_nb(mod_count),simplify = FALSE)
  
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
  
  
  pred.df2<-merge(pred.df,census,all.x = TRUE, all.y = FALSE)
  
  #grid of ten different predicted vs. observed
  p1 <- ggplot(pred.df2,aes(cases_per_cen, pred, color = county)) +
          geom_point() +
          geom_abline(slope = 1, color = "red") +
          theme_bw() +
          facet_wrap(~county)
  
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
    pred.df.rate <- merge(pred.df2, tot, by = "GEO_ID", all.x = TRUE, all.y = FALSE)
    pred.df.rate$observed.rate <- (pred.df.rate$cases_per_cen/pred.df.rate$total_pop)*100000
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
      
      #merge with test
      dph_hosp <- merge(dph_hosp, test, by = "GEO_ID", all.x = FALSE, all.y = TRUE)
      
      #subset svi and merge
      svi.sub <- subset(svi,select=c("FIPS", e_name_unin))
      dph_hosp <- merge(dph_hosp, svi.sub, by.x = "GEO_ID", by.y = "FIPS", all.x = TRUE, all.y = FALSE)
      
      
    #scale (?)
    dph_hosp <- cbind(dph_hosp[1:4],apply(dph_hosp[5:21],2, scale))
    
    #predict based on coef dataset
    all.covars <- paste(names(coefs)[-1], collapse='+')
    
    form1 <- as.formula(paste0('~',all.covars))
    
    dph_hosp2 <- na.omit(dph_hosp)
    coefs[is.na(coefs)] <- 0
    
    mod.mat1 <- model.matrix(form1, data=dph_hosp2)
    
    ave.coefs <- apply(coefs,2,mean)
    
    dph_hosp2$preds <- as.vector(exp(mod.mat1 %*% ave.coefs + log(dph_hosp2$total_pop/100000)))
    
    #drop covariates
    dph.pred<-subset(dph_hosp2,select = c("GEO_ID","dph_hosp_count","preds","total_pop"))
    
    
    #first check of sums
    sum(dph.pred$preds) - sum(dph.pred$dph_hosp_count)
    
    #add county column
    census<-read.csv("DECENNIALPL2010.P1_data_with_overlays_2022-02-11T112106.csv")
    census <- census[-1,] 
    census <- census[c("GEO_ID","NAME")]
    census$county <- sub("^([^,]+),\\s*([^,]+),.*", "\\2", census$NAME)
    dph.pred2<-merge(dph.pred,census,all.x = TRUE, all.y = FALSE)
    
    #sum of predicted and observed values per county
    county <- cbind(dph.pred2[6],dph.pred2[2:3],dph.pred2[4]) 
    county <- aggregate(. ~ county, data = as.data.frame(county), FUN = sum)
    names(county)[names(county) == "dph_hosp_count"] <- "obs"
    #rates by county
    county$obs.rate <- (county$obs/county$total_pop)*100000
    county$pred.rate <- (county$pred/county$total_pop)*100000
    #difference in rates by county
    county$diff <- county$pred.rate - county$obs.rate
    
    #convert to rates
    dph.pred$observed.rate <- (dph.pred$dph_hosp_count/dph.pred$total_pop)*100000
    dph.pred$predicted.rate <- (dph.pred$preds/dph.pred$total_pop)*100000
    
    
    #pull out NHV and MS
    noNHVMS<-dph.pred[!(dph.pred$GEO_ID %in% ir$GEO_ID),]
    NHVMS <- dph.pred[(dph.pred$GEO_ID %in% ir$GEO_ID),]
    
    #linmod for all census tracts
    linmod1<-lm(observed.rate~predicted.rate, data = dph.pred)
    summary(linmod1)
    #intercept 3.77310
    
    #linmod to check (no NHV and MS)
    linmod2<-lm(observed.rate~predicted.rate, data = noNHVMS)
    summary(linmod2)
    #intercept = -1.7217
    
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
    
    
    
    
    #%>%
     # mutate(diff = value - lag(value, default = first(value)))
    
    
##### check diff between dph hosp and covidnet hosp #####
    covidnet<-cbind(ir[1],mod_count[2])
    dph.covidnet<-merge(covidnet,dph_hosp,all.x = T, all.y = F)
    dph.covidnet<-na.omit(dph.covidnet)
    
    sum(dph.covidnet$covidnet.cases) - sum(dph.covidnet$dph_hosp_count)
    #1000 MORE HOSPITALIZATIONS FOR COVIDNET???
    
    #check fit when using COVID-NET as observed for NHV and MS
    #subset covidnet geo id and hosp rate
    covidnet <- cbind(ir[1], ir[4])
    
    covidnet.preds <- merge(covidnet, dph.pred2, all.x = T, all.y = F) 
    covidnet.preds <- na.omit(covidnet.preds)
    
    p6<-ggplot(covidnet.preds,aes(incidence, predicted.rate,color=county)) +
      geom_point(alpha=0.6) +
      geom_abline(slope = 1, color = "red") +
      theme_bw() +
      xlab("Observed Hospitalization Rate") + ylab("Estimated Hospitalization Rate")+
      ggtitle("COVID-NET hospitalization rate used as observed") +
      facet_wrap(~county)
    
    p7<-ggplot(covidnet.preds,aes(observed.rate, predicted.rate,color=county)) +
      geom_point(alpha=0.6) +
      geom_abline(slope = 1, color = "red") +
      theme_bw() +
      xlab("Observed Hospitalization Rate") + ylab("Estimated Hospitalization Rate")+
      ggtitle("DPH hospitalization rate used as observed") +
      facet_wrap(~county)
    
    gridExtra::grid.arrange(p6,p7,ncol=1)
    
    