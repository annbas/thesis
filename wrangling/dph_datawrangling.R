setwd("Z:/FluSurv-NET/COVID-19/Ann/Thesis/data")

dph<-read.csv("covid_positive_tests_hospitalizations.csv")

#melt to long on testing data
  #install.packages("reshape2")
  library(reshape2)
  
  dphtest_long<-melt(dph, id.vars=c("id", "geoid10","age_group","gender","hisp_race","hospital_admission"),na.rm = TRUE)
  #na.rm = TRUE removes missings from value column
  #e.g. if someone did not have a 3rd, 4th, etc. positive test, then it was removed
  
  #group by id and determine if someone had >1 "true positive" tests/hospitalizations
  library(dplyr)
  