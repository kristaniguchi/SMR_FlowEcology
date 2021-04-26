#create hydrograph plots of change from latecentury to future for the 3 GCMs - plot annual hydrographs of mean daily flow across 3 time periods for each GCM
  #Focus on future with CWRMA



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

#gcm lookup labels
GCM.name <- c("HadGEM2-ES365", "CNRM-CM5", "MIROC5")
GCM.labels <- c("HadGEM2-ES365 (Drier/Warmer)", "CNRM-CM5 (Wetter/Cooler)", "MIROC5 (Other)")
gcm.lookup <- data.frame(cbind(GCM.name, GCM.labels))

#sites to calc FFM
sites <- c("Gorge", "OldHospital")

#loop through each site and create hydrograph plots of the time periods
for(i in 1:2){

  #########################################
  #Future scenarios with CWRMA - loop through various GCM outputs with CWRMA flows and during midcentury and future time periods
  
  #######With CWRMA
  #find index of baseline site i
  future.CWRMA.ind <- grep(sites[i], future.CWRMA.files.short)
  #subset files
  future.CWRMA.files.sub <- future.CWRMA.files[future.CWRMA.ind]
  
  #GCMs to loop through
  gcms <- c("CNRM-CM5", "HadGEM2-ES365", "MIROC5")
  
  #create empty df of pivot longer for all gcms
  output.pivot.longer <- data.frame(matrix(NA, 1, 5))
  names(output.pivot.longer) <- c("Day", "summary", "flow", "scenario", "GCM")
  
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
    
    ######################################################################
    #####current GCM time period
    #subset to current time period (2002-2020) and calculate percentiles
    ind.start <- grep("10/01/2001", date2)
    ind.end <- grep("09/30/2020", date2)
    future.CWRMA.current.sub <- future.CWRMA[ind.start:ind.end,]

    #Add water year column
    dat<-future.CWRMA.current.sub %>% 
      mutate(Water.year=ifelse(month > 9, year+1,year))
    
    #Specify water years that have more than 360 discharge values
    keep <- levels(as.factor(dat$Water.year))[table(as.factor(dat$Water.year)) >= 358]
    dat <- dat[as.factor(dat$Water.year) %in% keep, ]
    
    # Create summary data frame (dat3) for each water month
    #UPDATE group by water year day, summarize mean, 10th, 50th, 90th 
    dat3<-dat%>%
      group_by(Water.year)%>% 
      mutate(Day = 1:n()) #assign water year day to each flow
    
    # summarize the 10th, 50th, and 90th, mean of daily flow for WY
    current.flow <- dat3 %>% 
      group_by(Day)%>% 
      summarize(mean= mean(flow), p10 = quantile(flow, p=.1), 
                p50 = quantile(flow, p=.5),
                p90 = quantile(flow, p=.9)) 
    
    # pivot longer
    current_longer <- pivot_longer(current.flow, names_to = "summary", values_to="flow", cols=c("mean","p10","p50","p90")) %>% 
      mutate(scenario = "Current")
    
    #plot current
    current.plot <- ggplot() + theme_classic() +
      geom_line(data=dat4_longer, aes(x=Day, y=(flow), color=summary), size=.2)  
    #plot(current.plot)
    
    ######################################################################
    #####midcentury GCM time period
    #subset to mid-century time period (2030-2065) and calculate percentiles
    ind.start <- grep("10/01/2029", date2)
    ind.end <- grep("09/30/2065", date2)
    midcentury.CWRMA.midcentury.sub <- future.CWRMA[ind.start:ind.end,]
    
    #Add water year column
    dat<-midcentury.CWRMA.midcentury.sub %>% 
      mutate(Water.year=ifelse(month > 9, year+1,year))
    
    #Specify water years that have more than 360 discharge values
    keep <- levels(as.factor(dat$Water.year))[table(as.factor(dat$Water.year)) >= 358]
    dat <- dat[as.factor(dat$Water.year) %in% keep, ]
    
    # Create summary data frame (dat3) for each water month
    #UPDATE group by water year day, summarize mean, 10th, 50th, 90th 
    dat3<-dat%>%
      group_by(Water.year)%>% 
      mutate(Day = 1:n()) #assign water year day to each flow
    
    # summarize the 10th, 50th, and 90th, mean of daily flow for WY
    midcentury.flow <- dat3 %>% 
      group_by(Day)%>% 
      summarize(mean= mean(flow), p10 = quantile(flow, p=.1), 
                p50 = quantile(flow, p=.5),
                p90 = quantile(flow, p=.9)) 
    
    # pivot longer
    midcentury_longer <- pivot_longer(midcentury.flow, names_to = "summary", values_to="flow", cols=c("mean","p10","p50","p90")) %>% 
      mutate(scenario = "Mid-Century")
    
    #plot midcentury
    midcentury.plot <- ggplot() + theme_classic() +
      geom_line(data=midcentury_longer, aes(x=Day, y=(flow), color=summary), size=.2)    +
      ylab("Discharge (cfs)") + 
      theme(axis.title.x=element_blank())+
      scale_x_continuous(breaks = c(1,30,60,90,120,150,180,210,240,271,302,333,366),
                         labels = paste0(c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct")),"Month")
    #print(midcentury.plot)
    
    ######################################################################
    #####latecentury GCM time period
    #subset to latecentury time period (2065-2099) and calculate percentiles
    ind.start <- grep("10/01/2064", date2)
    ind.end <- grep("09/30/2099", date2)
    latecentury.CWRMA.latecentury.sub <- future.CWRMA[ind.start:ind.end,]
    
    #Add water year column
    dat<-latecentury.CWRMA.latecentury.sub %>% 
      mutate(Water.year=ifelse(month > 9, year+1,year))
    
    #Specify water years that have more than 360 discharge values
    keep <- levels(as.factor(dat$Water.year))[table(as.factor(dat$Water.year)) >= 358]
    dat <- dat[as.factor(dat$Water.year) %in% keep, ]
    
    # Create summary data frame (dat3) for each water month
    #UPDATE group by water year day, summarize mean, 10th, 50th, 90th 
    dat3<-dat%>%
      group_by(Water.year)%>% 
      mutate(Day = 1:n()) #assign water year day to each flow
    
    # summarize the 10th, 50th, and 90th, mean of daily flow for WY
    latecentury.flow <- dat3 %>% 
      group_by(Day)%>% 
      summarize(mean= mean(flow), p10 = quantile(flow, p=.1), 
                p50 = quantile(flow, p=.5),
                p90 = quantile(flow, p=.9)) 
    
    # pivot longer
    latecentury_longer <- pivot_longer(latecentury.flow, names_to = "summary", values_to="flow", cols=c("mean","p10","p50","p90")) %>% 
      mutate(scenario = "Late-Century") 
    
    #plot latecentury
    latecentury.plot <- ggplot() + theme_classic() +
      geom_line(data=latecentury_longer, aes(x=Day, y=(flow), color=summary), size=.2)  +
      ylab("Discharge (cfs)") + 
      theme(axis.title.x=element_blank())+
      scale_x_continuous(breaks = c(1,30,60,90,120,150,180,210,240,271,302,333,366),
                         labels = paste0(c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct")),"Month")
    
    #print(latecentury.plot)
    
    #plot mean daily flow for current and mid century
    min.y <- min(c(latecentury.flow$mean, current.flow$mean))
    
    current.mid.latecentury.plot.mean <- ggplot() + theme_classic() +
      #geom_line(data=current.flow, aes(x=Day, y=(mean)), color="grey", size=.75)  +
      #geom_line(data=midcentury.flow, aes(x=Day, y=(mean)), color = "blue", lty="dashed") +
      #geom_line(data=latecentury.flow, aes(x=Day, y=(mean)), color = "red",) +
      geom_area(data=latecentury.flow, aes(x=Day, y=(mean)), fill = "cyan1") +
      geom_area(data=current.flow, aes(x=Day, y=(mean)), fill="dodgerblue4", alpha=0.45) +
      ylab("Discharge (cfs)") + 
      theme(axis.title.x=element_blank())+
      scale_x_continuous(breaks = c(1,30,60,90,120,150,180,210,240,271,302,333,366),
                         labels = paste0(c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct")),"Month") +
      labs(title = paste0(sites[i],": ",gcms[j]), subtitle="Mean daily flow across time period")  +
      #scale_y_log10() +
      coord_cartesian(ylim=c(min.y,NA))
      
    #plot  
    print(current.mid.latecentury.plot.mean)
    
    #zoom of dry-season
    max.dry.season <- max(latecentury.flow$mean[latecentury.flow$Day>=210], current.flow$mean[current.flow$Day>=210])
    mean.dry.season.zoom <- current.mid.latecentury.plot.mean +
      coord_cartesian(ylim=c(min.y,max.dry.season), xlim=c(210, 366))
    #plot  
    print(mean.dry.season.zoom)
    
    #combine all pivot longers from current and late-century, plot and facet by scenario
    gcm.label1 <- gcm.lookup$GCM.labels[gcm.lookup == gcms[j]]
    current.latecentury.pivot.longer <- bind_rows(current_longer,latecentury_longer) %>% 
      filter(summary == "mean") %>% 
      mutate(GCM = gcm.label1) %>% 
      data.frame()
    
    #add it to entire df
    output.pivot.longer <- output.pivot.longer %>% 
      bind_rows(current.latecentury.pivot.longer) %>% 
      data.frame()
  }
  #write.csv(output.pivot.longer, file="C:/Users/KristineT/SCCWRP/Santa Margarita River Climate Change Analyses - FlowEcology/FlowData/FFM/Current_2002_2020/hydrographs/pivotlonger.test.csv", row.names=FALSE)
  #write.csv(latecentury.flow,  file="C:/Users/KristineT/SCCWRP/Santa Margarita River Climate Change Analyses - FlowEcology/FlowData/FFM/Current_2002_2020/hydrographs/pivotlonger.test.latecentury.csv", row.names=FALSE)
  #write.csv(current.flow,  file="C:/Users/KristineT/SCCWRP/Santa Margarita River Climate Change Analyses - FlowEcology/FlowData/FFM/Current_2002_2020/hydrographs/pivotlonger.test.current.csv", row.names=FALSE)
  
  #if finished with last scenario, create facet plot of mean daily flow by gcm
  if(j == 3){
    #remove first NA in output.pivot.longer
    output.pivot.longer <- output.pivot.longer[2:length(output.pivot.longer$Day),]
    #save scenario levels
    output.pivot.longer$scenario <- factor(output.pivot.longer$scenario, levels = c("Late-Century", "Current"))
    #facet plots
    colors <- c("Late-Century" = "cyan1", "Current" = "dodgerblue4")
    alphas <- c("Late-Century" = 1, "Current" = 0.45)
    
    facet.plot <- ggplot(data=output.pivot.longer) + theme_classic() +
      facet_wrap(~GCM,  ncol=1, nrow=3, scales ='free') +
      geom_area(aes(x=Day, y=(flow), fill=scenario, alpha=scenario), position = 'identity') +
      scale_fill_manual(values = colors) +
      scale_alpha_manual(values = alphas) +
      theme(legend.position="bottom") + 
      ylab("Mean Daily Discharge (cfs)") + 
      theme(axis.title.x=element_blank())+
      #scale_y_log10() +
      scale_x_continuous(breaks = c(1,30,60,90,120,150,180,210,240,271,302,333,366),
                         labels = paste0(c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct")),"Month")
    
    print(facet.plot)
    #save
    file.name2 <- paste0(out.dir, sites[i], "_hydrograph_current_latecentury.jpg")
    ggsave(facet.plot, filename=file.name2, dpi=300, height=6, width=8)
    
    #zoom of dry-season
    max.dry.season <- max(output.pivot.longer$flow[output.pivot.longer$Day>=210], output.pivot.longer$flow[output.pivot.longer$Day>=210])
    mean.dry.season.zoom <- facet.plot +
      coord_cartesian(ylim=c(min.y,max.dry.season), xlim=c(210, 366))
    #plot  
    print(mean.dry.season.zoom)
    #save
    file.name2 <- paste0(out.dir, sites[i], "_hydrograph_current_latecentury_dryseason.jpg")
    ggsave(mean.dry.season.zoom, filename=file.name2, dpi=300, height=6, width=4)
    
    
  }
  
}


