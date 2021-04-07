#load other packages
library("ggplot2")
library("scales")
library("purrr")
library("plyr")
library("tidyverse")
library(lubridate);library(dplyr);library(dataRetrieval);library(stringr);library(tidyr);library(purrr);library(Cairo)
library(ffcAPIClient);library(ggplot2);library(lubridate);library(dplyr);library(dataRetrieval);library(stringr);library(tidyr);library(purrr);library(Cairo)

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
out.dir <- paste0(dir, "FFM/Current_2002_2020/hydrographs/")
dir.create(out.dir)

#sites to calc FFM
sites <- c("Gorge", "OldHospital")

#loop through each site and create hydrograph plots of the time periods
for(i in 1:2){
  
  #########################################
  #reference flows for site i - Ref timeperiod of 1931-1945
  #if Gorge, use USGS site info, else use reconstructed reference timeseries
  if(sites[i] == "Gorge"){
    #use USGS 11044000, get data, subset to 31-45, select date and flow columns
    ref.sub <- get_usgs_gage_data(11044000) %>% 
      filter(waterYear >=1931 & waterYear <=1945) %>% 
      select(date, flow)
    #Omit NAs from data
    dat<-na.omit(gage)
    
    #Add water year column
    dat<-dat %>% 
      mutate(date=ymd(date),
             month=month(date), 
             year=year(date),
             Water.year=ifelse(month > 9, year+1,year))
    
    #Specify water years that have more than 360 discharge values
    keep <- levels(as.factor(dat$Water.year))[table(as.factor(dat$Water.year)) >= 358]
    dat <- dat[as.factor(dat$Water.year) %in% keep, ]
    
    
    # Create summary data frame (dat3) for each water month
    dat3<-dat%>%
      group_by(Water.year)%>% 
      summarise(Mean=mean(flow))
    
    # Rank yearly discharge values from largest to smallest for entire period of record
    dat3$Rank.wy<-rank(-dat3$Mean, ties.method="first")
    
    dat3<-dat3[order(dat3$Rank.wy),]# Sort rows by the "Rank.wyt" column
    
    dat3<- dat3%>% 
      mutate(Exceedance.wy=(Rank.wy/length(Mean+1))*100,
             wyt=case_when(Exceedance.wy<=30 ~ "wet",
                           Exceedance.wy<=70 ~ "moderate",
                           TRUE ~ "dry"))%>%
      select(Water.year,wyt, Exceedance.wy)
    
    # Assign water year types from dat3 to dat and add water year day
    dat<-dat %>% 
      left_join(dat3)%>%
      filter(!is.na(wyt)) %>% 
      group_by(Water.year)%>% 
      arrange(date, .by_group= TRUE) %>% # in case the entries are not in order
      mutate(Day = 1:n()) #assign water year day to each flow
    
    #find median year within each wyt based on exceedance
    med.ref<-dat %>%
      group_by(wyt) %>% 
      filter(Exceedance.wy==quantile(Exceedance.wy,p=.99, type=1)) %>% 
      mutate(Scenario = "Reference")
    
    #current.obs based on gage data
    #use USGS 11044000, get data, subset to 2002-2020, select date and flow columns
    current.obs.sub <- get_usgs_gage_data(11044000) %>% 
      filter(waterYear >=2002 & waterYear <=2020) %>% 
      select(date, flow)
    #Omit NAs from data
    dat<-na.omit(gage)
    
    #Add water year column
    dat<-dat %>% 
      mutate(date=ymd(date),
             month=month(date), 
             year=year(date),
             Water.year=ifelse(month > 9, year+1,year))
    
    #Specify water years that have more than 360 discharge values
    keep <- levels(as.factor(dat$Water.year))[table(as.factor(dat$Water.year)) >= 358]
    dat <- dat[as.factor(dat$Water.year) %in% keep, ]
    
    
    # Create summary data frame (dat3) for each water month
    dat3<-dat%>%
      group_by(Water.year)%>% 
      summarise(Mean=mean(flow))
    
    # Rank yearly discharge values from largest to smallest for entire period of record
    dat3$Rank.wy<-rank(-dat3$Mean, ties.method="first")
    
    dat3<-dat3[order(dat3$Rank.wy),]# Sort rows by the "Rank.wyt" column
    
    dat3<- dat3%>% 
      mutate(Exceedance.wy=(Rank.wy/length(Mean+1))*100,
             wyt=case_when(Exceedance.wy<=30 ~ "wet",
                           Exceedance.wy<=70 ~ "moderate",
                           TRUE ~ "dry"))%>%
      select(Water.year,wyt, Exceedance.wy)
    
    # Assign water year types from dat3 to dat and add water year day
    dat<-dat %>% 
      left_join(dat3)%>%
      filter(!is.na(wyt)) %>% 
      group_by(Water.year)%>% 
      arrange(date, .by_group= TRUE) %>% # in case the entries are not in order
      mutate(Day = 1:n()) #assign water year day to each flow
    
    #find median year within each wyt based on exceedance
    med.current.obs<-dat %>%
      group_by(wyt) %>% 
      filter(Exceedance.wy==quantile(Exceedance.wy,p=.99, type=1)) %>% 
      mutate(Scenario = "current.obserence")
    
    #combine with med.ref.current
    med.ref.current.obs <- bind_rows(med.ref, med.current.obs)
    
    
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
    #format month/date column for plot
    ref$month.day <- format(date, format = "%m/%d")
    ref$month <- as.numeric(format(date, format = "%m"))
    ref$year <- as.numeric(format(date, format = "%Y"))
    
    #subset to refernce time period (1931-1945)
    ind.start <- grep("10/01/1930", date2)
    ind.end <- grep("09/30/1945", date2)
    ref.sub <- ref[ind.start:ind.end,]
    
    #Add water year column
    dat<-ref.sub %>% 
      mutate(Water.year=ifelse(month > 9, year+1,year))
    
    #Specify water years that have more than 360 discharge values
    keep <- levels(as.factor(dat$Water.year))[table(as.factor(dat$Water.year)) >= 358]
    dat <- dat[as.factor(dat$Water.year) %in% keep, ]
    
    # Create summary data frame (dat3) for each water month
    dat3<-dat%>%
      group_by(Water.year)%>% 
      summarise(Mean=mean(flow))
    
    # Rank yearly discharge values from largest to smallest for entire period of record
    dat3$Rank.wy<-rank(-dat3$Mean, ties.method="first")
    
    dat3<-dat3[order(dat3$Rank.wy),]# Sort rows by the "Rank.wyt" column
    
    dat3<- dat3%>% 
      mutate(Exceedance.wy=(Rank.wy/length(Mean+1))*100,
             wyt=case_when(Exceedance.wy<=30 ~ "wet",
                           Exceedance.wy<=70 ~ "moderate",
                           TRUE ~ "dry"))%>%
      select(Water.year,wyt, Exceedance.wy)
    
    # Assign water year types from dat3 to dat and add water year day
    dat<-dat %>% 
      left_join(dat3)%>%
      filter(!is.na(wyt)) %>% 
      group_by(Water.year)%>% 
      arrange(date, .by_group= TRUE) %>% # in case the entries are not in order
      mutate(Day = 1:n()) #assign water year day to each flow
    
    #find median year within each wyt based on exceedance
     med.ref<-dat %>%
        group_by(wyt) %>% 
        filter(Exceedance.wy==quantile(Exceedance.wy,p=.5, type=1)) %>% 
       mutate(Scenario = "Reference")
  }
  #plot reference
  ref.plot <- ggplot()+theme_classic()+
    #hydrographs
    #geom_line(data=dat, aes(x=Day, y=(flow)),color="grey30",alpha=.2,size=.2)+  
    geom_line(data=med.ref, aes(x=Day, y=(flow)),color="black",size=.2)+
    facet_wrap(~wyt, ncol=1)
  
  #ref current plot
  ref.current.plot <- ggplot()+theme_classic()+
    #hydrographs
    #geom_line(data=dat, aes(x=Day, y=(flow)),color="grey30",alpha=.2,size=.2)+  
    geom_line(data=med.ref.current.obs, aes(x=Day, y=(flow), color=Scenario),size=.2) +
    geom_line(data=med.current.obs, aes(x=Day, y=(flow), color=Scenario),size=.2)
  print(ref.current.plot )
  
  #########################################
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
    future.CWRMA$date <- date2
    #format month/date column for plot
    future.CWRMA$month.day <- format(date, format = "%m/%d")
    future.CWRMA$month <- as.numeric(format(date, format = "%m"))
    future.CWRMA$year <- as.numeric(format(date, format = "%Y"))
    
    
    #####current GCM time period
    #subset to current time period (1995-2016) and calculate percentiles
    ind.start <- grep("10/01/1994", date2)
    ind.end <- grep("09/30/2016", date2)
    future.CWRMA.current.sub <- future.CWRMA[ind.start:ind.end,]

    #Add water year column
    dat<-future.CWRMA.current.sub %>% 
      mutate(Water.year=ifelse(month > 9, year+1,year))
    
    #Specify water years that have more than 360 discharge values
    keep <- levels(as.factor(dat$Water.year))[table(as.factor(dat$Water.year)) >= 358]
    dat <- dat[as.factor(dat$Water.year) %in% keep, ]
    
    # Create summary data frame (dat3) for each water month
    dat3<-dat%>%
      group_by(Water.year)%>% 
      summarise(Mean=mean(flow))
    
    # Rank yearly discharge values from largest to smallest for entire period of record
    dat3$Rank.wy<-rank(-dat3$Mean, ties.method="first")
    
    dat3<-dat3[order(dat3$Rank.wy),]# Sort rows by the "Rank.wyt" column
    
    dat3<- dat3%>% 
      mutate(Exceedance.wy=(Rank.wy/length(Mean+1))*100,
             wyt=case_when(Exceedance.wy<=30 ~ "wet",
                           Exceedance.wy<=70 ~ "moderate",
                           TRUE ~ "dry"))%>%
      select(Water.year,wyt, Exceedance.wy)
    
    # Assign water year types from dat3 to dat and add water year day
    dat<-dat %>% 
      left_join(dat3)%>%
      filter(!is.na(wyt)) %>% 
      group_by(Water.year)%>% 
      arrange(date, .by_group= TRUE) %>% # in case the entries are not in order
      mutate(Day = 1:n()) #assign water year day to each flow
    
    #find median year within each wyt based on exceedance
    med.current<-dat %>%
      group_by(wyt) %>% 
      filter(Exceedance.wy==quantile(Exceedance.wy,p=.5, type=1)) %>% 
      mutate(Scenario = "Current")
    
    #add to ref dataframe
    #if class of med.ref$date is "Date", change to as.character, need to be same to combine
    if(class(med.ref$date) == "Date"){
      med.ref$date <- as.character(med.ref$date)
      }
    ref.current.med.data <- data.frame(bind_rows(med.ref, med.current))
    #plot reference
    current.plot <- ggplot()+theme_classic()+
      #hydrographs
      geom_line(data=dat, aes(x=Day, y=(flow)),color="grey30",alpha=.2,size=.2)+  
      geom_line(data=med.current, aes(x=Day, y=(flow), color=Scenario),size=.2)+
      facet_wrap(~wyt, ncol=1)
    
    
  }
}
