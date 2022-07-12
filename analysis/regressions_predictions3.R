setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data")

library(plyr)
library(tidyverse)
library(Hmisc)
library(corrplot)
library(e1071)
library(lmtest)
library(MASS)
library(Metrics)
library(BMA)
library(ggpubr)
library(Rcpp)
library(geojsonio)
library(mctest)
library(MetBrewer)
library(png)
source("Z:/FluSurv-NET/COVID-19/Ann/Thesis/thesis/analysis/mod_functions.R")
source('./excess_deaths_functions/runIfExpired.R')

#read in data
ir<-read.csv("covidnet2020_IR_FIPS.csv")     #COVID-NET data
svi<-read.csv("CDC_CensusTract_SVI.csv")     #CDC SVI data
test<-read.csv("dph2020_test_IR_FIPS2.csv")  #state testing data
dph.hosp.rate <- read.csv("dph2020_IR_FIPS3.csv") #state hosp data
names(dph_hosp)[names(dph_hosp) == "cases_per_cen"] <- "dph_hosp_count"

#read in census
census<-read.csv("DECENNIALPL2010.P1_data_with_overlays_2022-02-11T112106.csv")
census <- census[-1,] 
census <- census[c("GEO_ID","NAME")]
census$county <- sub("^([^,]+),\\s*([^,]+),.*", "\\2", census$NAME)

#create df of total pop by county
tot.pop <- c(916829, 894014, 189927, 165676, 862477, 274055, 152691, 118428)
county <- unique(census$county)
county <- data.frame(county, tot.pop)

#create df with observed rates by county using CTEDSS data
ctedss.county.count <- merge(dph.hosp.rate[ ,c("GEO_ID", "cases_per_cen")], census[ ,c("GEO_ID", "county")],all.x = TRUE, all.y = FALSE)
ctedss.county.count <- ctedss.county.rate %>%
                        group_by(county) %>%
                        filter(!is.na(county)) %>%
                        dplyr::summarize('hosp.count' = sum(cases_per_cen, na.rm = T))

ctedss.county.rate <- merge(ctedss.county.count,county)                 
ctedss.county.rate <- ctedss.county.rate %>%
                        mutate('hosp.rate' = (hosp.count/tot.pop)*100000) 
ctedss.county.rate <- subset(ctedss.county.rate, select = -c(tot.pop) )

##replace -999 with NA?
svi[svi == -999] <- NA
#count NAs in each column
apply(is.na(svi), MARGIN = 2, FUN = sum)

#change name in test df
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
  corrplot(cov_cor,method = "number")
  corrplot(cov_cor, order = 'AOE')
  
  #scale covariates for all census tracts in CT, then subset COVID-NET catchment
  scaled <- cbind(mod_sub[1:2], apply(mod_sub[3:19], 2, scale))
  
  #histogram of all estimates
  plot_long<-gather(scaled[3:19], key = "name", value = "value")
  #remove PCI
  plot_long <- plot_long[!(plot_long$name == "EP_PCI"),]
  
  ggplot(plot_long) +
    geom_histogram(aes(value), bins = 50) +
    facet_wrap(~name, ncol = 4)
  
  hist(scaled$EP_PCI)
  hist(scaled$test.incidence)
  
  #merge to COVID-NET data
  covidnet <- ir[1:2]
  names(covidnet)[names(covidnet) == "cases_per_cen"] <- "covidnet.cases" 
  mod_count <- merge(covidnet, scaled, by = "GEO_ID", all.x = TRUE, all.y = FALSE)
  
  #corrplot of variables
  mod_count2<-na.omit(mod_count)
  cov_cor<-cor(mod_count2)
  corrplot(cov_cor, method = "number", number.cex = 0.5)
  
  
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
  
  #check model for collinearity
  imcdiag(mod3, method = "VIF", all = T)

  
 
##### AIC of each dataset and analysis #####
#create list of predicted/observed values and coefficients
  set.seed(123)
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
  
  
  ## LEAVE IN (checking fit)
    #pull out list of predicted and observed for leave in
    pred.in.df <- lapply(in_nb,"[[","pred.in.df")
    
    #flatten pred.df into one dataset and add new column that designates index in list
    pred.in.df<-bind_rows(pred.in.df)
    pred.in.df$index<-rep(1:10, each = 203)
  
    #check fit
    fit.check <- merge(pred.in.df,census,all.x = TRUE, all.y = FALSE)
    
      #convert to rate
      tot <- subset(mod_count, select = c("GEO_ID","total_pop"))
      fit.check <- merge(fit.check, tot, by = "GEO_ID", all.x = T, all.y = F)
      fit.check$observed.rate <- (fit.check$covidnet.cases/fit.check$total_pop)*100000
      fit.check$predicted.rate <- (fit.check$pred/fit.check$total_pop)*100000
      
      p.in <- ggplot(fit.check, aes(observed.rate, predicted.rate,color = county)) +
                geom_point(alpha = 0.4) +
                geom_abline(slope = 1, color = "red") +
                theme_bw() +
                scale_color_manual(values = c("#00BA38","#00BFC4")) +
                ggtitle("Leave-In Sample") +
                xlab("Observed Hospitalization Rate") + ylab("Estimated Hospitalization Rate")+
                ylim(0,3000) +
                facet_wrap(~county) +
                theme(legend.position="none",
                      text = element_text(size = 10))
    
    
  ## LEAVE OUT
    #pull list of predicted and observed values for leave out
    pred.df<-lapply(in_nb,"[[","pred.df")
    
    #flatten pred.df into one dataset and add new column that designates index in list
    pred.df<-bind_rows(pred.df)
    pred.df$index<-rep(1:10, each = 22)
  
  #check fit for actual leave out/predicted values
  pred.df2<-merge(pred.df,census,all.x = TRUE, all.y = FALSE)
  
  #grid of predicted vs. observed by NHV and MS
  p1 <- ggplot(pred.df2,aes(pred, covidnet.cases, color = county)) +
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
    pred.df.rate <- merge(pred.df2, tot, by = "GEO_ID", all.x = TRUE, all.y = FALSE)
    pred.df.rate$observed.rate <- (pred.df.rate$covidnet.cases/pred.df.rate$total_pop)*100000
    pred.df.rate$predicted.rate <- (pred.df.rate$pred/pred.df.rate$total_pop)*100000
    
    #grid of ten different predicted vs. observed
    p.out <- ggplot(pred.df.rate,aes(observed.rate, predicted.rate, color = county)) +
                  geom_point(alpha = 0.4) +
                  geom_abline(slope = 1, color = "red") +
                  theme_bw() +
                  scale_color_manual(values = c("#00BA38","#00BFC4")) +
                  ggtitle("Hold-Out Sample") +
                  xlab("Observed Hospitalization Rate") + ylab("Estimated Hospitalization Rate")+
                  ylim(0,3000) +
                  facet_wrap(~county) +
                  theme(legend.position="none",
                        text = element_text(size = 10))
    
    #look at in and out side by side
    ggarrange(p.in,p.out,
              labels = c("A","B"),
              ncol=1)
    
    #correlation
    pred.dt.rate <- data.table(pred.df.rate)
    pred.dt.rate[, cor(x=cases_per_cen, y=pred), by = index]
    
    
    
##### predict for state ##### 
    
    #create dph dataset for model
      
      #merge dph hospital counts with scaled
      dph_hosp <- merge(dph.hosp.rate, scaled, by = "GEO_ID", all.x = TRUE, all.y = FALSE)
      
      
    
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
    

    
##### prediction intervals #####
    #create dataset of just predictions and observed
    dph.pred.pi<-subset(dph.pred,select = c("GEO_ID","dph_hosp_count","preds"))
    
    #generating samples based on parameter uncertainty
      set.seed(1234)
      
      #coefs.mod1 <- coef(mod1) #Mean estimate of the regression coefficients
      #NOTE: this was created when generating predictions for the state
      #var is called ave.coefs
      
      #make sure that this is correct formula/data/etc.
      mod1 <- glm.nb(dph_hosp_count~test.incidence + EP_AGE65 + EP_DISABL + EP_SNGPNT + EP_MINRTY + 
                       EP_LIMENG + EP_MUNIT + EP_NOVEH + EP_UNINSUR + EP_PCI + EP_AGE17 + 
                       EP_MOBILE + EP_UNEMP + EP_POV + EP_NOHSDP +
                       offset(log(dph_hosp2$total_pop.x/100000)), data = dph_hosp2)
      
      v.cov.mat <- vcov(mod1) 
      
      pred.coefs.reg.mean <-
        MASS::mvrnorm(n = 1000,
                      mu = ave.coefs,
                      Sigma = v.cov.mat) 
      
      str(pred.coefs.reg.mean) #1000 estimates of the regression coefficient
      
    #generate 1000 versions of predictions based on these 1000 regression coefficients
      log.preds.stage1.regmean <- mod.mat1 %*% t(pred.coefs.reg.mean)
      
      
      log.preds.stage1.regmean <- apply(log.preds.stage1.regmean, 2,
                                        function(x) x + log(dph_hosp2$total_pop.x/100000))
    #adding observation noise
    
      preds.stage2 <- rnbinom(n = length(log.preds.stage1.regmean) ,
                              size = mod1$theta, mu = exp(log.preds.stage1.regmean))
      
      preds.stage2 <- matrix(preds.stage2, ncol=ncol(log.preds.stage1.regmean))
      
      #bind to and melt by census tract? 
      preds.stage2.m <- reshape2::melt(cbind.data.frame('GEO_ID'=dph_hosp2$GEO_ID,preds.stage2), id.vars='GEO_ID')
      
      
      
    #OVERALL EXCESS
      excess1 <- merge(preds.stage2.m, dph_hosp2[,c('GEO_ID','dph_hosp_count')], 
                       by='GEO_ID')
      
      #Generate 1000 estimates of total excess 
      excess1 <- excess1 %>%
        mutate('excess'= value - dph_hosp_count) %>%
        group_by(variable) %>%
        dplyr::summarize('excessN' = sum(excess))
      
      excess2 <- quantile(excess1$excessN, probs=c(0.5, 0.025, 0.975))
      
      round(excess2, -2)
      
    #PREDICTION INTERVALS BY CENSUS TRACT
      preds.ci <- as.data.frame(t(apply(preds.stage2, 1, quantile, probs=c(0.5, 0.025, 0.975))))
      
      names(preds.ci) <- c('pred.mc','lcl.mc','ucl.mc')
      
      preds.ci <- cbind.data.frame(dph_hosp2, preds.ci)
      
    #PREDICTION INTERVALS BY COUNTY
      #bind county
      preds.stage2.m.county <- merge(cbind.data.frame('GEO_ID'=dph_hosp2$GEO_ID, preds.stage2), 
                                     census[,c('GEO_ID','county')], 
                                     by='GEO_ID', all.x = T, all.y = F)
      #remove GEO_ID
      preds.stage2.m.county[1] <- NULL
      
      #melt
      preds.stage2.m.county <- reshape2::melt(preds.stage2.m.county, id.vars = 'county')
      
      #find quantiles for each county
      preds.ci.county <- preds.stage2.m.county %>%
                            group_by(variable, county) %>%
                            dplyr::summarize(N_pred_county= sum(value)) %>%
                            ungroup() %>%
                            group_by(county) %>%
                            mutate(median=median(N_pred_county), 
                                   lcl= quantile(N_pred_county, probs=0.025),  
                                   ucl= quantile(N_pred_county, probs=0.975)) %>%
                            ungroup() %>%
                            dplyr::select(-variable, -N_pred_county) %>%
                            unique()
      
      #merge CTEDSS hosp counts to preds.ci data
      preds.ci.county.obs <- merge(preds.ci.county, ctedss.county.count[ ,c("county","hosp.count")])
      
      #subtract ub, lb, and pt est from observed
      preds.ci.county.dev <- preds.ci.county.obs %>%
                              dplyr::summarize('county' = county,
                                               'median.dev' = median - hosp.count,
                                               'lcl.dev'  = lcl - hosp.count,
                                               'ucl.dev'  = ucl - hosp.count)
      
      #merge county ds to tot pop data
      preds.ci.county.rate <- merge(preds.ci.county.dev, county, all.x = TRUE, all.y = FALSE)
      
      #calculate rates
      preds.ci.county.rate <- preds.ci.county.rate %>%
                              mutate('median.rate' = (median.dev/tot.pop)*100000,
                                     'lcl.rate'  = (lcl.dev/tot.pop)*100000,
                                     'ucl.rate'  = (ucl.dev/tot.pop)*100000)
      
      #create plot 
      library(hrbrthemes)
      
      ggplot(preds.ci.county.rate, aes(median.rate, county)) + 
        geom_errorbarh(aes(xmin = lcl.rate, xmax = ucl.rate), height = .2) +
        geom_point(color = "#2C8BFF", alpha = 1, size = 4) + 
        geom_vline(xintercept=0, linetype = "dashed") +
        theme_pubclean() +
        theme(
          legend.position="none"
        ) +
        scale_y_discrete(limits=rev) +
        ylab("") +
        xlab("Estimated excess hospitalizations per 100,000")
    
      ggsave(filename = "county_pi2.png",width = 7, height = 7, 
             units = "in", device='png', dpi=300)
      
      
    
##### AGGREGATE BY COUNTY #####    
    #sum of predicted and observed values per county
    county <- cbind(dph.pred2[8],dph.pred2[2:5]) 
    county <- aggregate(. ~ county, data = as.data.frame(county), FUN = sum)
    names(county)[names(county) == "dph_hosp_count"] <- "obs"
    #rates by county
    county$obs.rate <- (county$obs/county$total_pop)*100000
    county$pred.rate <- (county$pred/county$total_pop)*100000
    #difference in rates by county
    county$diff <- county$pred.rate - county$obs.rate
    
    #linmod for all census tracts using non-transformed data
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
      #geom_abline(slope = 1, linetype = "dotted", size = 1, col = "red")+
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
    
    p5<-ggplot(dph.pred2,aes(observed.rate, predicted.rate, color=county)) +
          geom_point(alpha=0.4) +
          geom_abline(slope = 1, color = "red") +
          theme_bw() +
          xlab("Observed Hospitalization Rate") + ylab("Estimated Hospitalization Rate")+
          facet_wrap(~county) +
          theme(legend.position="none",
                text = element_text(size = 10)) 
          #scale_color_manual(values = met.brewer("Egypt", n=8, type = "continuous"))
          
    #export
    ggsave(filename = "highres_xyplot.png",width = 8, height = 5, units = "in", device='png', dpi=300)
    
  #SVI by difference in rate? at census tract?
    #merge svi subbed to e_name and dph.pred
    e_name2 <- c("FIPS", e_name)
    svi.sub <- subset(svi, select = e_name2)
    svi.sub[svi.sub == -999] <- NA
    
    dph.pred$diff.rate <- dph.pred$predicted.rate - dph.pred$observed.rate
    
    pred.svi <- merge(dph.pred, svi.sub, by.x = "GEO_ID", by.y = "FIPS", all.x = TRUE, all.y = FALSE)
    pred.svi <- cbind(pred.svi[1:7], apply(pred.svi[8:23], 2, scale))
    
    names <- names(pred.svi[1:7])
    pred.svi.long <- melt(pred.svi, id.vars=names,
                      na.rm = TRUE)
    
    ggplot(pred.svi.long,aes(diff.rate, value, color = variable)) +
      geom_point() +
      #geom_abline(slope = 1, color = "red") +
      theme_bw() +
      facet_wrap(~variable)
    
    #corrplot of variables
    svi.cor <- pred.svi[7:23]
    svi.cor <- na.omit(svi.cor)
    svi.cor <- cor(svi.cor)
    corrplot(svi.cor,method = "number")
         
    
    
  
    