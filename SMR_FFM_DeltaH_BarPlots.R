#Bar plots of the deltaH for each metrics with the thresholds indicated as "likely altered" "likely unaltered"
library(tidyverse)

#set output directories
#current condition time period WY 1995-2016 (calibration time period)
#out.dir <- "C:/Users/KristineT/SCCWRP/Santa Margarita River Climate Change Analyses - FlowEcology/FlowData/FFM/DeltaHBarplots/"
#current condition time period WY 2002-2020
out.dir <- "C:/Users/KristineT/SCCWRP/Santa Margarita River Climate Change Analyses - FlowEcology/FlowData/FFM/Current_2002_2020/DeltaHBarplots/Current_Mid_Late_Century/"
dir.create(out.dir)


#read in to summarize the number of altered metric per scenario per CSCI and ASCI
results <- read.csv("C:/Users/KristineT/SCCWRP/Santa Margarita River Climate Change Analyses - FlowEcology/FlowData/FFM/Current_2002_2020/FFM_DeltaH_all_scenarios_CSCIASCI_alteration_mid_late_century.csv")
#omit hspf calibration scenarios from the analysis
ind.hspf.cal <- grep("HSPF Calibration", results$Scenario)
results <- results[-ind.hspf.cal,]


#add columns with GCM and Scenario type
#find indices of current scenarios
ind.current <- grep("Current", results$Scenario)
#add scenario type column, set all to future, change current to current
results$ScenarioType <- "Late-Century"
results$ScenarioType[ind.current] <- "Current"
#mid-century
ind.midcentury <- grep("Mid-Century", results$Scenario)
results$ScenarioType[ind.midcentury] <- "Mid-Century"


#find indices of no cwrma, set CWRMA
ind.nocwrma <- grep("No CWRMA", results$Scenario)
#add scenario type column, set all to CWRMA, change no CWRMA to no CWRMA
results$CWRMA <- "CWRMA"
results$CWRMA[ind.nocwrma] <- "No CWRMA"
#set the GCM column, string split by : and take second element
results$GCM <- sapply(strsplit( results$Scenario, ": " ), "[", 2 )
#create a column header col
results$colheader <- paste0(results$ScenarioType, "_", results$CWRMA)
#create a new column with scenario type and GCM for bar labels
results$bar.labels <- paste0(results$GCM, "\n",results$CWRMA)
results$bar.labels1 <- paste0(results$ScenarioType, "\n",results$CWRMA)
#set levels for bar labels current, mid, late
results$bar.labels1 <- factor(results$bar.labels1, levels=unique(results$bar.labels1))
#create new column with GCM wet/cool etc.
results$facet.gcm.name <- results$GCM
results$facet.gcm.name[results$facet.gcm.name == "HadGEM2-ES365"] <- "HadGEM2-ES365 (Drier/Warmer)"
results$facet.gcm.name[results$facet.gcm.name == "CNRM-CM5"] <- "CNRM-CM5 (Wetter/Cooler)"
results$facet.gcm.name[results$facet.gcm.name == "MIROC5"] <- "MIROC5 (Other)"

#read in deltaH thresholds for CSCI and ASCI
#bio.thresholds <- read.csv("C:/Users/KristineT/SCCWRP/SOC WQIP - Flow Ecology Study - General/Tier2_analysis/08_all_delta_thresholds.csv")
#scaled bio thresholds 0 to 1
bio.thresholds <- read.csv("C:/Users/KristineT/SCCWRP/SOC WQIP - Flow Ecology Study - General/Tier2_analysis/08_all_delta_thresholds_scaled.csv")



#Bar plot of median Delta H for each metric for each site

sites <- unique(results$Site)
metrics <- unique(results$metric)
metric.names <- c("Dry Season Baseflow Magnitude (cfs)", "Spring Recession Duration (days)", "Wet Season Baseflow Duration (days)", "Wet Season Baseflow Magnitude (cfs)", "Magnitude of Largest Annual Storm (cfs)")

for(i in 1:length(sites)){
  #subset to site i
  results.sub <- results %>% 
    filter(Site == sites[i])
  
  #make plots for each metric
  for(j in 1:length(metrics)){
    results.sub.metric <- results.sub %>% 
      filter(metric == metrics[j])
    
    #y-axis label
    ylab.name <- paste0("Change in ", metric.names[j])
    
    #find the thresholds for CSCI and/or ASCI for metric j
    bio.thresholds.sub <- bio.thresholds %>% 
      filter(metric == metrics[j])
    
    #make faceted bar plots by ScenarioType (Current, Future) and GCM, color = CRWMA and no CWRMA
    barplot <- ggplot(results.sub.metric, aes(x=bar.labels1, y=p50, fill=CWRMA)) +
      facet_wrap(~facet.gcm.name,  ncol=3) +
      geom_bar(stat="identity") + 
      labs(title = paste0(sites[i]), subtitle=metric.names[j],
                                       color = "Legend") + ylab(ylab.name) + xlab("") +
      
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
      
      theme_bw() + theme(legend.position="bottom") +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      geom_hline(yintercept = bio.thresholds.sub$Threshold50[1], lty = "dashed") +
      geom_hline(yintercept = bio.thresholds.sub$Threshold50[2], lty = "dashed") +
      geom_hline(yintercept = 0) 
      
    
    #if Q99, add in thresholds for CSCI and ASCI
    if(metrics[j] == "Q99"){
      #make faceted bar plots by ScenarioType (Current, Future) and GCM, color = CRWMA and no CWRMA
      barplot <- ggplot(results.sub.metric, aes(x=bar.labels1, y=p50, fill=CWRMA)) +
        facet_wrap(~facet.gcm.name,  ncol=3) +
        geom_bar(stat="identity") + 
        labs(title = paste0(sites[i]), subtitle=metric.names[j],
             color = "Legend") + ylab(ylab.name) + xlab("") +
        
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        theme_bw() + theme(legend.position="bottom") +
        theme(axis.text.x = element_text(angle = 45, hjust=1)) +
        geom_hline(yintercept = bio.thresholds.sub$Threshold50[1], lty = "dashed") +
        geom_hline(yintercept = bio.thresholds.sub$Threshold50[2], lty = "dashed") +
        geom_hline(yintercept = 0) +
        geom_hline(yintercept = bio.thresholds.sub$Threshold50[3], lty = "dashed", color= "green") +
        geom_hline(yintercept = bio.thresholds.sub$Threshold50[4], lty = "dashed", color= "green") 
      
      
    }
    
    print(barplot)
    
    #save
    file.name2 <- paste0(out.dir, sites[i], "_", metrics[j], "_DeltaH_barplots.jpg")
    ggsave(barplot, filename=file.name2, dpi=300, height=5, width=10)
    
    
  }
}



###########################################################
#Bar plot of p10 Delta H for each metric for each site

sites <- unique(results$Site)
metrics <- unique(results$metric)
metric.names <- c("Dry Season Baseflow Magnitude (cfs)", "Spring Recession Duration (days)", "Wet Season Baseflow Duration (days)", "Wet Season Baseflow Magnitude (cfs)", "Magnitude of Largest Annual Storm (cfs)")

#output director for p10 deltaH barplots
out.dir2 <- paste0(out.dir, "p10_barplots/")
dir.create(out.dir2)


for(i in 1:length(sites)){
  #subset to site i
  results.sub <- results %>% 
    filter(Site == sites[i])
  
  #make plots for each metric
  for(j in 1:length(metrics)){
    results.sub.metric <- results.sub %>% 
      filter(metric == metrics[j])
    
    #y-axis label
    ylab.name <- paste0("Change in ", metric.names[j])
    
    #find the thresholds for CSCI and/or ASCI for metric j
    bio.thresholds.sub <- bio.thresholds %>% 
      filter(metric == metrics[j])
    
    #make faceted bar plots by ScenarioType (Current, Future) and GCM, color = CRWMA and no CWRMA
    barplot <- ggplot(results.sub.metric, aes(x=bar.labels1, y=p10, fill=CWRMA)) +
      facet_wrap(~facet.gcm.name,  ncol=3) +
      geom_bar(stat="identity") + 
      labs(title = paste0(sites[i]), subtitle=metric.names[j],
           color = "Legend") + ylab(ylab.name) + xlab("") +
      
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
      
      theme_bw() + theme(legend.position="bottom") +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      geom_hline(yintercept = bio.thresholds.sub$Threshold50[1], lty = "dashed") +
      geom_hline(yintercept = bio.thresholds.sub$Threshold50[2], lty = "dashed") +
      geom_hline(yintercept = 0) 
    
    
    #if Q99, add in thresholds for CSCI and ASCI
    if(metrics[j] == "Q99"){
      #make faceted bar plots by ScenarioType (Current, Future) and GCM, color = CRWMA and no CWRMA
      barplot <- ggplot(results.sub.metric, aes(x=bar.labels1, y=p10, fill=CWRMA)) +
        facet_wrap(~facet.gcm.name,  ncol=3) +
        geom_bar(stat="identity") + 
        labs(title = paste0(sites[i]), subtitle=metric.names[j],
             color = "Legend") + ylab(ylab.name) + xlab("") +
        
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        theme_bw() + theme(legend.position="bottom") +
        theme(axis.text.x = element_text(angle = 45, hjust=1)) +
        geom_hline(yintercept = bio.thresholds.sub$Threshold50[1], lty = "dashed") +
        geom_hline(yintercept = bio.thresholds.sub$Threshold50[2], lty = "dashed") +
        geom_hline(yintercept = 0) +
        geom_hline(yintercept = bio.thresholds.sub$Threshold50[3], lty = "dashed", color= "green") +
        geom_hline(yintercept = bio.thresholds.sub$Threshold50[4], lty = "dashed", color= "green") 
      
      
    }
    
    print(barplot)
    
    #save
    file.name2 <- paste0(out.dir2, sites[i], "_", metrics[j], "_DeltaH_p10_barplots.jpg")
    ggsave(barplot, filename=file.name2, dpi=300, height=5, width=10)
    
    
  }
}


###########################################################
#Bar plot of p10 Delta H for each metric for each site

sites <- unique(results$Site)
metrics <- unique(results$metric)
metric.names <- c("Dry Season Baseflow Magnitude (cfs)", "Spring Recession Duration (days)", "Wet Season Baseflow Duration (days)", "Wet Season Baseflow Magnitude (cfs)", "Magnitude of Largest Annual Storm (cfs)")

#output director for p10 deltaH barplots
out.dir3 <- paste0(out.dir, "p90_barplots/")
dir.create(out.dir3)


for(i in 1:length(sites)){
  #subset to site i
  results.sub <- results %>% 
    filter(Site == sites[i])
  
  #make plots for each metric
  for(j in 1:length(metrics)){
    results.sub.metric <- results.sub %>% 
      filter(metric == metrics[j])
    
    #y-axis label
    ylab.name <- paste0("Change in ", metric.names[j])
    
    #find the thresholds for CSCI and/or ASCI for metric j
    bio.thresholds.sub <- bio.thresholds %>% 
      filter(metric == metrics[j])
    
    #make faceted bar plots by ScenarioType (Current, Future) and GCM, color = CRWMA and no CWRMA
    barplot <- ggplot(results.sub.metric, aes(x=bar.labels1, y=p90, fill=CWRMA)) +
      facet_wrap(~facet.gcm.name,  ncol=3) +
      geom_bar(stat="identity") + 
      labs(title = paste0(sites[i]), subtitle=metric.names[j],
           color = "Legend") + ylab(ylab.name) + xlab("") +
      
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
      
      theme_bw() + theme(legend.position="bottom") +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      geom_hline(yintercept = bio.thresholds.sub$Threshold50[1], lty = "dashed") +
      geom_hline(yintercept = bio.thresholds.sub$Threshold50[2], lty = "dashed") +
      geom_hline(yintercept = 0) 
    
    
    #if Q99, add in thresholds for CSCI and ASCI
    if(metrics[j] == "Q99"){
      #make faceted bar plots by ScenarioType (Current, Future) and GCM, color = CRWMA and no CWRMA
      barplot <- ggplot(results.sub.metric, aes(x=bar.labels1, y=p90, fill=CWRMA)) +
        facet_wrap(~facet.gcm.name,  ncol=3) +
        geom_bar(stat="identity") + 
        labs(title = paste0(sites[i]), subtitle=metric.names[j],
             color = "Legend") + ylab(ylab.name) + xlab("") +
        
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        theme_bw() + theme(legend.position="bottom") +
        theme(axis.text.x = element_text(angle = 45, hjust=1)) +
        geom_hline(yintercept = bio.thresholds.sub$Threshold50[1], lty = "dashed") +
        geom_hline(yintercept = bio.thresholds.sub$Threshold50[2], lty = "dashed") +
        geom_hline(yintercept = 0) +
        geom_hline(yintercept = bio.thresholds.sub$Threshold50[3], lty = "dashed", color= "green") +
        geom_hline(yintercept = bio.thresholds.sub$Threshold50[4], lty = "dashed", color= "green") 
      
      
    }
    
    print(barplot)
    
    #save
    file.name2 <- paste0(out.dir3, sites[i], "_", metrics[j], "_DeltaH_p90_barplots.jpg")
    ggsave(barplot, filename=file.name2, dpi=300, height=5, width=10)
    
    
  }
}

