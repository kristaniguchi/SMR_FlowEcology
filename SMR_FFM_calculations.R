#Santa Margarita Flow Ecology Study
  #Calculate functional flow metrics for Old Hospital (POD) and Gorge locations under reference, baseline, and future scenarios
  #Various data sources and years used for each scenario
  #Summarize percentiles for each scenario

#################################################################################################################
#load libraries
#for functional flow calculator:
#install.packages("devtools")
library("devtools")
#devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
library("ffcAPIClient")

#to uninstall package and reinstall (if updates to R package were made)
#remove.packages("ffcAPIClient") #uninstall then restart R session
#library("devtools")
#devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
#install.packages("ffcAPIClient")
#library("ffcAPIClient")

#load other packages
library("ggplot2")
library("scales")
library("purrr")
library("plyr")
library("tidyverse")
library(lubridate);library(dplyr);library(dataRetrieval);library(stringr);library(tidyr);library(purrr);library(Cairo)

#################################################################################################################

#directories with flow data
dir <- "C:/Users/KristineT/SCCWRP/Santa Margarita River Climate Change Analyses - FlowEcology/FlowData/"
#scenario directories
ref.dir <- paste0(dir, "Reference/")
baseline.dir <- paste0(dir, "Baseline/")
future.CWRMA.dir <- paste0(dir, "Future_CWRMA/")
future.NoCWRMA.dir <- paste0(dir, "Future_NoCWRMA/")
#list files in scenario directories, full names
ref.files <- list.files(ref.dir, full.names=TRUE, pattern=".csv")
baseline.files <- list.files(baseline.dir, full.names=TRUE, pattern=".csv")
future.CWRMA.files <- list.files(future.CWRMA.dir, full.names=TRUE, pattern=".csv")
future.NoCWRMA.files <- list.files(future.NoCWRMA.dir, full.names=TRUE, pattern=".csv")
#short.names of flow files
ref.files.short <- list.files(ref.dir, pattern=".csv")
baseline.files.short <- list.files(baseline.dir, pattern=".csv")
future.CWRMA.files.short <- list.files(future.CWRMA.dir, pattern=".csv")
future.NoCWRMA.files.short <- list.files(future.NoCWRMA.dir, pattern=".csv")

#set output directory
out.dir <- paste0(dir, "FFM/")
dir.create(out.dir)

#sites to calc FFM
sites <- c("Gorge", "OldHospital")

#my token for FFC API Client
mytoken <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJLcmlzIiwibGFzdE5hbWUiOiJUYW5pZ3VjaGkgUXVhbiIsImVtYWlsIjoia3Jpc3RpbmV0cUBzY2N3cnAub3JnIiwicm9sZSI6IlVTRVIiLCJpYXQiOjE1NzM4NjgwODN9.UJhTioLNNJOxvY_PYb_GIbcMRI_qewjkfYx-usC_7ZA"
#set token
set_token(mytoken)

#find COMID for Gorge USGS gage site
gorge.COMID <- get_comid_for_lon_lat(-117.141389, 33.473889, online = TRUE)
#OldHospital POD site
oldhospital.COMID <- get_comid_for_lon_lat(-117.332035, 33.341111, online = TRUE)
COMIDs <- c(gorge.COMID, oldhospital.COMID)
COMIDs.all <- data.frame(cbind(sites, COMIDs))


#Functional flow metric names and labels for plots
filename <- ("L:/CA  E-flows framework_ES/Misc/Functional Flows metrics/functional_flow_metric_modeling/all_metric_def_list_FFMs_v2.csv")
ffm.labels <- read.csv(filename)
ffm.labels$metric <- ffm.labels$flow_metric

#output dataframe
#create empty percentiles df with all summary values
percentiles.all <- data.frame(matrix(NA,1,11))
names(percentiles.all) <- c("p10","p25","p50","p75","p90","metric","comid","result_type", "source2","Site", "Scenario")
#create empty df with all annual results from each scenario
results.all <- data.frame(matrix(NA, 1, 31))
names(results.all) <- c("Year","DS_Dur_WS","DS_Tim","DS_Mag_50","DS_Mag_90","FA_Dur","FA_Mag","FA_Tim","SP_ROC","SP_Dur","SP_Mag","SP_Tim","Wet_BFL_Dur",
                        "Wet_BFL_Mag_10","Wet_BFL_Mag_50","Wet_Tim","Peak_Tim_10","Peak_Tim_2","Peak_Tim_5","Peak_Dur_10","Peak_Dur_2","Peak_Dur_5","Peak_10","Peak_2",       
                        "Peak_5","Peak_Fre_10","Peak_Fre_2","Peak_Fre_5","Site","Scenario", "Q99")

#loop through each site and calculate annual FFM and percentiles
for(i in 1:length(sites)){
  #comid for site i
  COMID.i <- as.numeric(COMIDs.all$COMIDs[COMIDs.all$sites == sites[i]])
  
  #########################################
  #reference FFMs for site i - Ref timeperiod of 1931-1945
  #if Gorge, use USGS site info, else use reconstructed reference timeseries
  if(sites[i] == "Gorge"){
    #use USGS 11044000, get data, subset to 31-45, select date and flow columns
    ref.sub <- get_usgs_gage_data(11044000) %>% 
      filter(waterYear >=1931 & waterYear <=1945) %>% 
      select(date, flow)
    #set source USGS
    source <- "USGS 11044000"
    result.type <- "observed"
    
  }else{
    #else if OldHospital, read in reconstructed timeseries and use unimpaired period 1931-1945
    #find index of ref file for 
    ref.ind <- grep(sites[i], ref.files.short)
    
    #read in ref flow data
    ref <- read.csv(ref.files[ref.ind])
    #remove commas in flow column and save as numeric
    ref$flow <- gsub(",","", ref$flow)
    ref$flow <- as.numeric(ref$flow)
    
    #format date
    #add leading zero to hour
    date <- as.POSIXct(ref$date, format = "%m/%d/%Y")
    date2 <- format(date, format = "%m/%d/%Y")
    #substitute 00 to 19 in year of date
    date2 <- gsub("00", "19", date2)
    #save newly formatted date back into date col
    ref$date <- as.character(date2)
    
    #subset to refernce time period (1931-1945)
    ind.start <- grep("10/01/1930", date2)
    ind.end <- grep("09/30/1945", date2)
    ref.sub <- ref[ind.start:ind.end,]
    
    #source
    source.ref <- "Reconstructed Unimpaired"
    result.type <- "reconstructed water balance"
  }
  
  
  #run ffc.ref for ref
  #new FFC api set up
  ffc.ref <- FFCProcessor$new()  # make a new object we can use to run the commands
  #allow ffc.ref to run with min of 1 years
  ffc.ref$fail_years_data <- 1
  #setup
  ffc.ref$set_up(timeseries=ref.sub,
             token=mytoken,
             comid = COMID.i)
  #then run
  ffc.ref$run()
  
  #get annual flow metrics
  ref.results.all <- ffc.ref$ffc_results
  #calculate Q99 and add to dataframe
  #Add water year column
  dat<-ref.sub %>% 
    mutate(date=mdy(date),
           month=month(date), 
           year=year(date),
           Water.year=ifelse(month > 9, year+1,year))
  #calculate Q99 for every water year
  # Create summary data frame (dat3) for each water month
  dat3<-dat%>%
    group_by(Water.year)%>% 
    summarise(Q99=quantile(flow, 0.99)) %>% 
    ungroup() %>% 
    data.frame()
  #add Q99 to results
  ref.results.all$Q99 <- dat3$Q99
  #add in additional info
  ref.results.all$Site <- sites[i]
  ref.results.all$Scenario <- "Reference"
  #save into overall df
  results.all <- data.frame(rbind(results.all, ref.results.all))
  
  #get percentiles
  ref.percentiles <- ffc.ref$ffc_percentiles
  #add percentiles for Q99
  ref.percentiles[length(ref.percentiles$p10)+1,] <- ref.percentiles[1,]
  Q99.percentiles <- as.numeric(quantile(ref.results.all$Q99, c(0.10, 0.25, 0.50, 0.75, 0.9)))
  #add to percentiles
  ref.percentiles[(length(ref.percentiles$p10)),1:5] <- Q99.percentiles
  #save metric name Q99
  ref.percentiles[(length(ref.percentiles$p10)),6] <- "Q99"
  #add in additional fields
  ref.percentiles$source2 <- source
  ref.percentiles$Site <- sites[i]
  ref.percentiles$Scenario <- "Reference"
  ref.percentiles$result_type <- result.type
  #save into overall df
  percentiles.all <- data.frame(rbind(percentiles.all, ref.percentiles))
  
  #########################################
  #baseline FFMs from HSPF
  
  #find index of baseline site i
  baseline.ind <- grep(sites[i], baseline.files.short)
  
  #read in baseline flow data
  baseline <- read.csv(baseline.files[baseline.ind])
  #remove commas in flow column and save as numeric
  baseline$flow <- gsub(",","", baseline$flow)
  baseline$flow <- as.numeric(baseline$flow)
  
  #format date
  #add leading zero to hour
  date <- as.POSIXct(baseline$date, format = "%m/%d/%Y")
  date2 <- format(date, format = "%m/%d/%Y")
  year <- as.numeric(format(date, format="%Y"))
  #save newly formatted date back into date col
  baseline$date <- as.character(date2)
  
  #subset to current time period (1995-2016) and calculate percentiles
  ind.start <- grep("10/01/1994", date2)
  ind.end <- grep("09/30/2016", date2)
  baseline.sub <- baseline[ind.start:ind.end,]
  
  #run ffc for baseline
  #new FFC api set up
  ffc <- FFCProcessor$new()  # make a new object we can use to run the commands
  #allow ffc to run with min of 1 years
  ffc$fail_years_data <- 1
  #setup
  ffc$set_up(timeseries=baseline.sub,
             token=mytoken,
             comid = COMID.i)
  #then run
  ffc$run()
  
  #get annual flow metrics
  baseline.results.all <- ffc$ffc_results
  #Add water year column
  dat<-baseline.sub %>% 
    mutate(date=mdy(date),
           month=month(date), 
           year=year(date),
           Water.year=ifelse(month > 9, year+1,year))
  #calculate Q99 for every water year
  # Create summary data frame (dat3) for each water month
  dat3<-dat%>%
    group_by(Water.year)%>% 
    summarise(Q99=quantile(flow, 0.99)) %>% 
    ungroup() %>% 
    data.frame()
  #add Q99 to results
  baseline.results.all$Q99 <- dat3$Q99
  #add in additional info
  baseline.results.all$Site <- sites[i]
  baseline.results.all$Scenario <- "Current Condition HSPF Calibration"
  #save into overall df
  results.all <- data.frame(rbind(results.all, baseline.results.all))
  
  #get percentiles
  baseline.percentiles <- ffc$ffc_percentiles
  #add percentiles for Q99
  baseline.percentiles[length(baseline.percentiles$p10)+1,] <- baseline.percentiles[1,]
  Q99.percentiles <- as.numeric(quantile(baseline.results.all$Q99, c(0.10, 0.25, 0.50, 0.75, 0.9)))
  #add to percentiles
  baseline.percentiles[(length(baseline.percentiles$p10)),1:5] <- Q99.percentiles
  #save metric name Q99
  baseline.percentiles[(length(baseline.percentiles$p10)),6] <- "Q99"
  #add in additional fields
  baseline.percentiles$source2 <- "HSPF Calibration"
  baseline.percentiles$Site <- sites[i]
  baseline.percentiles$Scenario <- "Current Condition HSPF Calibration"
  baseline.percentiles$result_type <- "modeled"
  #save into overall df
  percentiles.all <- data.frame(rbind(percentiles.all, baseline.percentiles))
  
  #########################################
  #Future scenarios with CWRMA - loop through various GCM outputs with CWRMA flows and during current and future time periods
  
  #######With CWRMA
  #find index of baseline site i
  future.CWRMA.ind <- grep(sites[i], future.CWRMA.files.short)
  #subset files
  future.CWRMA.files.sub <- future.CWRMA.files[future.CWRMA.ind]
  
  #GCMs to loop through
  gcms <- c("CNRM-CM5", "HadGEM2-ES365", "MIROC5")
  
  for(j in 1:3){
    #subset to gcm file j
    ind.gcm <- grep(gcms[j], future.CWRMA.files.sub)
    #read in future.CWRMA flow data
    future.CWRMA <- read.csv(future.CWRMA.files.sub[ind.gcm])
    #remove commas in flow column and save as numeric
    future.CWRMA$flow <- gsub(",","", future.CWRMA$flow)
    future.CWRMA$flow <- as.numeric(future.CWRMA$flow)
    
    #format date
    #add leading zero to hour
    date <- as.POSIXct(future.CWRMA$date, format = "%m/%d/%Y")
    date2 <- format(date, format = "%m/%d/%Y")
    #save newly formatted date back into date col
    future.CWRMA$date <- as.character(date2)
    
    #####current GCM time period
    #subset to current time period (1995-2016) and calculate percentiles
    ind.start <- grep("10/01/1994", date2)
    ind.end <- grep("09/30/2016", date2)
    future.CWRMA.current.sub <- future.CWRMA[ind.start:ind.end,]
    
    #run ffc for current.CWRMA
    #new FFC api set up
    ffc.current.CWRMA <- FFCProcessor$new()  # make a new object we can use to run the commands
    #allow ffc to run with min of 1 years
    ffc.current.CWRMA$fail_years_data <- 1
    #setup
    ffc.current.CWRMA$set_up(timeseries=future.CWRMA.current.sub,
               token=mytoken,
               comid = COMID.i)
    #then run
    ffc.current.CWRMA$run()
    
    #get annual flow metrics
    current.CWRMA.results.all <- ffc.current.CWRMA$ffc_results
    #Add water year column
    dat<-future.CWRMA.current.sub %>% 
      mutate(date=mdy(date),
             month=month(date), 
             year=year(date),
             Water.year=ifelse(month > 9, year+1,year))
    #calculate Q99 for every water year
    # Create summary data frame (dat3) for each water month
    dat3<-dat%>%
      group_by(Water.year)%>% 
      summarise(Q99=quantile(flow, 0.99)) %>% 
      ungroup() %>% 
      data.frame()
    #add Q99 to results
    current.CWRMA.results.all$Q99 <- dat3$Q99
    #add in additional info
    current.CWRMA.results.all$Site <- sites[i]
    current.CWRMA.results.all$Scenario <- paste0("Current Condition: ", gcms[j])
    #save into overall df
    results.all <- data.frame(rbind(results.all, current.CWRMA.results.all))
    
    #get percentiles
    current.CWRMA.percentiles <- ffc.current.CWRMA$ffc_percentiles
    #add percentiles for Q99
    current.CWRMA.percentiles[length(current.CWRMA.percentiles$p10)+1,] <- current.CWRMA.percentiles[1,]
    Q99.percentiles <- as.numeric(quantile(current.CWRMA.results.all$Q99, c(0.10, 0.25, 0.50, 0.75, 0.9)))
    #add to percentiles
    current.CWRMA.percentiles[(length(current.CWRMA.percentiles$p10)),1:5] <- Q99.percentiles
    #save metric name Q99
    current.CWRMA.percentiles[(length(current.CWRMA.percentiles$p10)),6] <- "Q99"
    #add in additional fields
    current.CWRMA.percentiles$source2 <- paste0("HSPF ", gcms[j])
    current.CWRMA.percentiles$Site <- sites[i]
    current.CWRMA.percentiles$Scenario <- paste0("Current Condition: ", gcms[j])
    current.CWRMA.percentiles$result_type <- "modeled"
    #save into overall df
    percentiles.all <- data.frame(rbind(percentiles.all, current.CWRMA.percentiles))
    
    #####future GCM time period
    #subset to future time period (2050-2099) and calculate percentiles
    ind.start <- grep("10/01/2049", date2)
    ind.end <- grep("09/30/2099", date2)
    future.CWRMA.future.sub <- future.CWRMA[ind.start:ind.end,]
    
    #run ffc for future.CWRMA
    #new FFC api set up
    ffc.future.CWRMA <- FFCProcessor$new()  # make a new object we can use to run the commands
    #allow ffc to run with min of 1 years
    ffc.future.CWRMA$fail_years_data <- 1
    #setup
    ffc.future.CWRMA$set_up(timeseries=future.CWRMA.future.sub,
                             token=mytoken,
                             comid = COMID.i)
    #then run
    ffc.future.CWRMA$run()
    
    #get annual flow metrics
    future.CWRMA.results.all <- ffc.future.CWRMA$ffc_results
    #Add water year column
    dat<-future.CWRMA.future.sub %>% 
      mutate(date=mdy(date),
             month=month(date), 
             year=year(date),
             Water.year=ifelse(month > 9, year+1,year))
    #calculate Q99 for every water year
    # Create summary data frame (dat3) for each water month
    dat3<-dat%>%
      group_by(Water.year)%>% 
      summarise(Q99=quantile(flow, 0.99)) %>% 
      ungroup() %>% 
      data.frame()
    #add Q99 to results
    future.CWRMA.results.all$Q99 <- dat3$Q99
    #add in additional info
    future.CWRMA.results.all$Site <- sites[i]
    future.CWRMA.results.all$Scenario <- paste0("Future Condition: ", gcms[j])
    #save into overall df
    results.all <- data.frame(rbind(results.all, future.CWRMA.results.all))
    
    #get percentiles
    future.CWRMA.percentiles <- ffc.future.CWRMA$ffc_percentiles
    #add percentiles for Q99
    future.CWRMA.percentiles[length(future.CWRMA.percentiles$p10)+1,] <- future.CWRMA.percentiles[1,]
    Q99.percentiles <- as.numeric(quantile(future.CWRMA.results.all$Q99, c(0.10, 0.25, 0.50, 0.75, 0.9)))
    #add to percentiles
    future.CWRMA.percentiles[(length(future.CWRMA.percentiles$p10)),1:5] <- Q99.percentiles
    #save metric name Q99
    future.CWRMA.percentiles[(length(future.CWRMA.percentiles$p10)),6] <- "Q99"
    #add in additional fields
    future.CWRMA.percentiles$source2 <- paste0("HSPF ", gcms[j])
    future.CWRMA.percentiles$Site <- sites[i]
    future.CWRMA.percentiles$Scenario <- paste0("Future Condition: ", gcms[j])
    future.CWRMA.percentiles$result_type <- "modeled"
    #save into overall df
    percentiles.all <- data.frame(rbind(percentiles.all, future.CWRMA.percentiles))
  }
  
  #########################################
  #Future scenarios NO CWRMA - loop through various GCM outputs with CWRMA flows and during current and future time periods
  
  #######With NoCWRMA
  #find index of baseline site i
  future.NoCWRMA.ind <- grep(sites[i], future.NoCWRMA.files.short)
  #subset files
  future.NoCWRMA.files.sub <- future.NoCWRMA.files[future.NoCWRMA.ind]
  
  #GCMs to loop through
  gcms <- c("CNRM-CM5", "HadGEM2-ES365", "MIROC5")
  
  for(j in 1:3){
    #subset to gcm file j
    ind.gcm <- grep(gcms[j], future.NoCWRMA.files.sub)
    #read in future.NoCWRMA flow data
    future.NoCWRMA <- read.csv(future.NoCWRMA.files.sub[ind.gcm]) %>% 
      select(date, flow)
    #remove commas in flow column and save as numeric
    future.NoCWRMA$flow <- gsub(",","", future.NoCWRMA$flow)
    future.NoCWRMA$flow <- as.numeric(future.NoCWRMA$flow)
    
    #format date
    #add leading zero to hour
    date <- as.POSIXct(future.NoCWRMA$date, format = "%m/%d/%Y")
    date2 <- format(date, format = "%m/%d/%Y")
    #save newly formatted date back into date col
    future.NoCWRMA$date <- as.character(date2)
    
    #####current GCM time period
    #subset to current time period (1995-2016) and calculate percentiles
    ind.start <- grep("10/01/1994", date2)
    ind.end <- grep("09/30/2016", date2)
    future.NoCWRMA.current.sub <- future.NoCWRMA[ind.start:ind.end,]
    
    #run ffc for current.NoCWRMA
    #new FFC api set up
    ffc.current.NoCWRMA <- FFCProcessor$new()  # make a new object we can use to run the commands
    #allow ffc to run with min of 1 years
    ffc.current.NoCWRMA$fail_years_data <- 1
    #setup
    ffc.current.NoCWRMA$set_up(timeseries=future.NoCWRMA.current.sub,
                             token=mytoken,
                             comid = COMID.i)
    #then run
    ffc.current.NoCWRMA$run()
    
    #get annual flow metrics
    current.NoCWRMA.results.all <- ffc.current.NoCWRMA$ffc_results
    #Add water year column
    dat<-future.NoCWRMA.current.sub %>% 
      mutate(date=mdy(date),
             month=month(date), 
             year=year(date),
             Water.year=ifelse(month > 9, year+1,year))
    #calculate Q99 for every water year
    # Create summary data frame (dat3) for each water month
    dat3<-dat%>%
      group_by(Water.year)%>% 
      summarise(Q99=quantile(flow, 0.99)) %>% 
      ungroup() %>% 
      data.frame()
    #add Q99 to results
    current.NoCWRMA.results.all$Q99 <- dat3$Q99
    #add in additional info
    current.NoCWRMA.results.all$Site <- sites[i]
    current.NoCWRMA.results.all$Scenario <- paste0("Current No CWRMA: ", gcms[j])
    #save into overall df
    results.all <- data.frame(rbind(results.all, current.NoCWRMA.results.all))
    
    #get percentiles
    current.NoCWRMA.percentiles <- ffc.current.NoCWRMA$ffc_percentiles
    #add percentiles for Q99
    current.NoCWRMA.percentiles[length(current.NoCWRMA.percentiles$p10)+1,] <- current.NoCWRMA.percentiles[1,]
    Q99.percentiles <- as.numeric(quantile(current.NoCWRMA.results.all$Q99, c(0.10, 0.25, 0.50, 0.75, 0.9)))
    #add to percentiles
    current.NoCWRMA.percentiles[(length(current.NoCWRMA.percentiles$p10)),1:5] <- Q99.percentiles
    #save metric name Q99
    current.NoCWRMA.percentiles[(length(current.NoCWRMA.percentiles$p10)),6] <- "Q99"
    #add in additional fields
    current.NoCWRMA.percentiles$source2 <- paste0("HSPF ", gcms[j])
    current.NoCWRMA.percentiles$Site <- sites[i]
    current.NoCWRMA.percentiles$Scenario <- paste0("Current No CWRMA: ", gcms[j])
    current.NoCWRMA.percentiles$result_type <- "modeled"
    #save into overall df
    percentiles.all <- data.frame(rbind(percentiles.all, current.NoCWRMA.percentiles))
    
    #####future GCM time period
    #subset to future time period (2050-2099) and calculate percentiles
    ind.start <- grep("10/01/2049", date2)
    ind.end <- grep("09/30/2099", date2)
    future.NoCWRMA.future.sub <- future.NoCWRMA[ind.start:ind.end,]
    
    #run ffc for future.NoCWRMA
    #new FFC api set up
    ffc.future.NoCWRMA <- FFCProcessor$new()  # make a new object we can use to run the commands
    #allow ffc to run with min of 1 years
    ffc.future.NoCWRMA$fail_years_data <- 1
    #setup
    ffc.future.NoCWRMA$set_up(timeseries=future.NoCWRMA.future.sub,
                            token=mytoken,
                            comid = COMID.i)
    #then run
    ffc.future.NoCWRMA$run()
    
    #get annual flow metrics
    future.NoCWRMA.results.all <- ffc.future.NoCWRMA$ffc_results
    #Add water year column
    dat<-future.NoCWRMA.future.sub %>% 
      mutate(date=mdy(date),
             month=month(date), 
             year=year(date),
             Water.year=ifelse(month > 9, year+1,year))
    #calculate Q99 for every water year
    # Create summary data frame (dat3) for each water month
    dat3<-dat%>%
      group_by(Water.year)%>% 
      summarise(Q99=quantile(flow, 0.99)) %>% 
      ungroup() %>% 
      data.frame()
    #add Q99 to results
    future.NoCWRMA.results.all$Q99 <- dat3$Q99
    #add in additional info
    future.NoCWRMA.results.all$Site <- sites[i]
    future.NoCWRMA.results.all$Scenario <- paste0("Future No CWRMA: ", gcms[j])
    #save into overall df
    results.all <- data.frame(rbind(results.all, future.NoCWRMA.results.all))
    
    #get percentiles
    future.NoCWRMA.percentiles <- ffc.future.NoCWRMA$ffc_percentiles
    #add percentiles for Q99
    future.NoCWRMA.percentiles[length(future.NoCWRMA.percentiles$p10)+1,] <- future.NoCWRMA.percentiles[1,]
    Q99.percentiles <- as.numeric(quantile(future.NoCWRMA.results.all$Q99, c(0.10, 0.25, 0.50, 0.75, 0.9)))
    #add to percentiles
    future.NoCWRMA.percentiles[(length(future.NoCWRMA.percentiles$p10)),1:5] <- Q99.percentiles
    #save metric name Q99
    future.NoCWRMA.percentiles[(length(future.NoCWRMA.percentiles$p10)),6] <- "Q99"
    #add in additional fields
    future.NoCWRMA.percentiles$source2 <- paste0("HSPF ", gcms[j])
    future.NoCWRMA.percentiles$Site <- sites[i]
    future.NoCWRMA.percentiles$Scenario <- paste0("Future No CWRMA: ", gcms[j])
    future.NoCWRMA.percentiles$result_type <- "modeled"
    #save into overall df
    percentiles.all <- data.frame(rbind(percentiles.all, future.NoCWRMA.percentiles))
  }
  
}

#delete first NA row before writing csv
results.all2 <- results.all[2:length(results.all$Year),]
percentiles.all2 <- percentiles.all[2:length(percentiles.all$p10),]

#save csv
filename1 <- paste0(out.dir, "FFM_results_all_scenarios.csv")
write.csv(results.all2, file=filename1)
filename2 <- paste0(out.dir, "FFM_percentiles_all_scenarios.csv")
write.csv(percentiles.all2, file=filename2)



