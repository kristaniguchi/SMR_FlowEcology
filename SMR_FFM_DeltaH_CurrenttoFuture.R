#DeltaH Calculations for all sites and scenarios
#Current to Future DeltaH

#read in percentile data
#current condition time period WY 1995-2016 (calibration time period)
#data <- read.csv("C:/Users/KristineT/SCCWRP/Santa Margarita River Climate Change Analyses - FlowEcology/FlowData/FFM/FFM_percentiles_all_scenarios.csv")
#current condition time period WY 2002-2020
data <- read.csv("C:/Users/KristineT/SCCWRP/Santa Margarita River Climate Change Analyses - FlowEcology/FlowData/FFM/Current_2002_2020/FFM_percentiles_all_scenarios_mid_late_century.csv")


#subset data to reference and non reference (current/future scenarios)
#find index of current and future scenario
ind.current <- grep("Current", data$Scenario)
current.data <- data[ind.current,] %>% 
  filter(Scenario != "Current Condition HSPF Calibration")
#future mid-century and late century
ind.midcentury <- grep("Mid-Century", data$Scenario)
ind.latecentury <- grep("Late-Century", data$Scenario)
future.data <- data[c(ind.midcentury,ind.latecentury),]

#output dataframe for DeltaH values
deltaH.outputs <- data[1,]
#save as NA
deltaH.outputs[1,] <- NA


#loop through each site and metric and calculate the change in metric percentiles from reference to current
sites <- c("Gorge", "OldHospital")

for(i in 1:2){
  #subset ref and current/future data to site i
  current.data.i <- current.data %>% 
    filter(Site == sites[i])
  future.data.i <- future.data %>% 
    filter(Site == sites[i])
  
  #loop through each unique metric and calculate deltaH for all percentiles
  unique.metrics <- unique(future.data.i$metric)
  
  for(j in 1:length(unique.metrics)){
    #subset to unique metric i, sort by GCM
    future.data.i.metric <- future.data.i %>% 
      filter(metric == unique.metrics[j]) %>% 
      arrange(by=Scenario)
    current.data.i.metric <- current.data.i %>% 
      filter(metric == unique.metrics[j]) %>% 
      arrange(by=Scenario)
    #create a double of current since now mid and late century
    current.data.i.metric <- cbind(current.data.i.metric, current.data.i.metric)
    #subtract current.data.i.metric (as a list) from future percentile columns, replace values in new deltaH.sub
    deltaH.sub <- future.data.i.metric
    deltaH.sub[1:length(future.data.i.metric$p10),2:6] <- deltaH.sub[1:length(future.data.i.metric$p10),2:6] - as.list(current.data.i.metric[1,2:6])
    
    #save into DeltaH outputs
    deltaH.outputs <- data.frame(rbind(deltaH.outputs, deltaH.sub))
  }
}

#remove first NA row
deltaH.outputs2 <- deltaH.outputs[2:length(deltaH.outputs$p10),]

#write deltaH outputs
filename1 <- "C:/Users/KristineT/SCCWRP/Santa Margarita River Climate Change Analyses - FlowEcology/FlowData/FFM/Current_2002_2020/FFM_DeltaH_all_scenarios_current_future_alt_mid_late_century.csv"
write.csv(deltaH.outputs2, filename1, row.names=FALSE)






############did not run since CSCI/ASCI thresholds based on deviation from reference
####DeltaH limits on important metrics for CSCI and ASCI
#determine if the change in median is within our outside of bio threshold

#read in deltaH thresholds for CSCI and ASCI
#bio.thresholds <- read.csv("C:/Users/KristineT/SCCWRP/SOC WQIP - Flow Ecology Study - General/Tier2_analysis/08_all_delta_thresholds.csv")
#scaled curve from 0 to 1
bio.thresholds <- read.csv("C:/Users/KristineT/SCCWRP/SOC WQIP - Flow Ecology Study - General/Tier2_analysis/08_all_delta_thresholds_scaled.csv")
unique.metrics <- unique(bio.thresholds$metric)

#subset deltaH.outputs2 to unique metrics for CSCI/ASCI
deltaH.outputs2.sub <- deltaH.outputs2[deltaH.outputs2$metric  %in% unique.metrics,]
#add output columns for asci and csci suitability
deltaH.outputs2.sub$CSCI_Suitability_p50 <- NA
deltaH.outputs2.sub$ASCI_Suitability_p50 <- NA


#loop to determine if deltaH for p50 is outside or within the flow range
for(k in 1:length(deltaH.outputs2.sub$X)){
  #subset row k
  sub <- deltaH.outputs2.sub[k,]
  #metric
  metric.k <- sub$metric
  
  #find limits for metric.k
  limits <- bio.thresholds[bio.thresholds$metric == metric.k,] %>% 
    na.omit()
  
  #if metric is Q99, find both ASCI and CSCI limits; else just
  #unique.bio
  if(metric.k == "Q99"){
    #ASCI
    asci.limits <- limits[limits$Biol == "ASCI",]
    #if below negative limit or above positive limit, class as likely altered, else likely unaltered
    asci.p50.delta.class <- ifelse(sub$p50 < asci.limits$Threshold50[asci.limits$Delta == "negative"] | 
                                     sub$p50 > asci.limits$Threshold50[asci.limits$Delta == "positive"],
                                   "likely altered", "likely unaltered")
    #save into column
    deltaH.outputs2.sub$ASCI_Suitability_p50[k] <- asci.p50.delta.class
    
    #CSCI
    csci.limits <- limits[limits$Biol == "CSCI",]
    #if below negative limit or above positive limit, class as likely altered, else likely unaltered
    csci.p50.delta.class <- ifelse(sub$p50 < csci.limits$Threshold50[csci.limits$Delta == "negative"] | 
                                     sub$p50 > csci.limits$Threshold50[csci.limits$Delta == "positive"],
                                   "likely altered", "likely unaltered")
    #save into column
    deltaH.outputs2.sub$CSCI_Suitability_p50[k] <- csci.p50.delta.class
    
  }else{
    #else if other metrics, only one bioindex per metric
    #if below negative limit or above positive limit, class as likely altered, else likely unaltered
    p50.delta.class <- ifelse(sub$p50 < limits$Threshold50[limits$Delta == "negative"] | 
                                sub$p50 > limits$Threshold50[limits$Delta == "positive"],
                              "likely altered", "likely unaltered")
    #find bio-metric
    bio.metric <- unique(limits$Biol)
    #save into column
    col.ind <- ifelse(bio.metric == "ASCI", 
                      grep("ASCI",names(deltaH.outputs2.sub)), 
                      grep("CSCI",names(deltaH.outputs2.sub)))
    #save into column for bio.metric
    deltaH.outputs2.sub[k,col.ind] <- p50.delta.class
    
  }
}

filename2 <- "C:/Users/KristineT/SCCWRP/Santa Margarita River Climate Change Analyses - FlowEcology/FlowData/FFM/Current_2002_2020/FFM_DeltaH_all_scenarios_CSCIASCI_alteration.csv"
write.csv(deltaH.outputs2.sub, filename2, row.names=FALSE)

#read in to summarize the number of altered metric per scenario per CSCI and ASCI
results <- read.csv("C:/Users/KristineT/SCCWRP/Santa Margarita River Climate Change Analyses - FlowEcology/FlowData/FFM/Current_2002_2020/FFM_DeltaH_all_scenarios_CSCIASCI_alteration.csv")
library(tidyverse)

CSCI.sum.altered <- results %>% 
  group_by(Site, Scenario, CSCI_Suitability_p50) %>% 
  tally() %>% 
  ungroup() %>% 
  na.omit()
#if likely unaltered n==3, then change category to likely altered and set to 0
ind.noalteration <- which(CSCI.sum.altered$CSCI_Suitability_p50 == "likely unaltered" & CSCI.sum.altered$n == 3)
CSCI.sum.altered$CSCI_Suitability_p50[ind.noalteration] <- "likely altered"
CSCI.sum.altered$n[ind.noalteration] <- 0
#filter to only altered 
CSCI.sum.altered.only <- CSCI.sum.altered %>% 
  filter(CSCI_Suitability_p50 == "likely altered")

#if 2 metrics likely altered, considered index likely altered
CSCI.sum.altered.only$CSCI_Bio_Alteration <- NA
CSCI.sum.altered.only$CSCI_Bio_Alteration[CSCI.sum.altered.only$n > 1] <- "Likely Altered"
CSCI.sum.altered.only$CSCI_Bio_Alteration[CSCI.sum.altered.only$n < 2] <- "Likely Unaltered"

#add columns with GCM and Scenario type
#find indices of current scenarios
ind.current <- grep("Current", CSCI.sum.altered.only$Scenario)
#add scenario type column, set all to future, change current to current
CSCI.sum.altered.only$ScenarioType <- "Future"
CSCI.sum.altered.only$ScenarioType[ind.current] <- "Current"
#find indices of no cwrma, set CWRMA
ind.nocwrma <- grep("No CWRMA", CSCI.sum.altered.only$Scenario)
#add scenario type column, set all to CWRMA, change no CWRMA to no CWRMA
CSCI.sum.altered.only$CWRMA <- "CWRMA"
CSCI.sum.altered.only$CWRMA[ind.nocwrma] <- "No CWRMA"
#set the GCM column, string split by : and take second element
CSCI.sum.altered.only$GCM <- sapply(strsplit( CSCI.sum.altered.only$Scenario, ": " ), "[", 2 )
#save NA as HSPF Calibration
ind.hspf.cal <- grep("HSPF Calibration", CSCI.sum.altered.only$Scenario)
CSCI.sum.altered.only$GCM[ind.hspf.cal] <- "HSPF Calibration"
#create a column header col
CSCI.sum.altered.only$colheader <- paste0(CSCI.sum.altered.only$ScenarioType, "_", CSCI.sum.altered.only$CWRMA)

#pivot table wider, 
pivot.CSCI <- CSCI.sum.altered.only %>% 
  select(Site, GCM, CSCI_Bio_Alteration, colheader) %>% 
  pivot_wider(names_from = colheader, values_from= CSCI_Bio_Alteration)
#write.csv
write.csv(pivot.CSCI, file="C:/Users/KristineT/SCCWRP/Santa Margarita River Climate Change Analyses - FlowEcology/FlowData/FFM/Current_2002_2020/CSCI_alteration_summary_scenarios.csv", row.names=FALSE)




ASCI.sum.altered <- results %>% 
  group_by(Site, Scenario, ASCI_Suitability_p50) %>% 
  tally() %>% 
  ungroup() %>% 
  na.omit()

#if likely unaltered n==3, then change category to likely altered and set to 0
ind.noalteration <- which(ASCI.sum.altered$ASCI_Suitability_p50 == "likely unaltered" & ASCI.sum.altered$n == 3)
ASCI.sum.altered$ASCI_Suitability_p50[ind.noalteration] <- "likely altered"
ASCI.sum.altered$n[ind.noalteration] <- 0
#filter to only altered 
ASCI.sum.altered.only <- ASCI.sum.altered %>% 
  filter(ASCI_Suitability_p50 == "likely altered")

#if 2 metrics likely altered, considered index likely altered
ASCI.sum.altered.only$ASCI_Bio_Alteration <- NA
ASCI.sum.altered.only$ASCI_Bio_Alteration[ASCI.sum.altered.only$n > 1] <- "Likely Altered"
ASCI.sum.altered.only$ASCI_Bio_Alteration[ASCI.sum.altered.only$n < 2] <- "Likely Unaltered"

#add columns with GCM and Scenario type
#find indices of current scenarios
ind.current <- grep("Current", ASCI.sum.altered.only$Scenario)
#add scenario type column, set all to future, change current to current
ASCI.sum.altered.only$ScenarioType <- "Future"
ASCI.sum.altered.only$ScenarioType[ind.current] <- "Current"
#find indices of no cwrma, set CWRMA
ind.nocwrma <- grep("No CWRMA", ASCI.sum.altered.only$Scenario)
#add scenario type column, set all to CWRMA, change no CWRMA to no CWRMA
ASCI.sum.altered.only$CWRMA <- "CWRMA"
ASCI.sum.altered.only$CWRMA[ind.nocwrma] <- "No CWRMA"
#set the GCM column, string split by : and take second element
ASCI.sum.altered.only$GCM <- sapply(strsplit( ASCI.sum.altered.only$Scenario, ": " ), "[", 2 )
#save NA as HSPF Calibration
ind.hspf.cal <- grep("HSPF Calibration", ASCI.sum.altered.only$Scenario)
ASCI.sum.altered.only$GCM[ind.hspf.cal] <- "HSPF Calibration"
#create a column header col
ASCI.sum.altered.only$colheader <- paste0(ASCI.sum.altered.only$ScenarioType, "_", ASCI.sum.altered.only$CWRMA)

#pivot table wider, 
pivot.ASCI <- ASCI.sum.altered.only %>% 
  select(Site, GCM, ASCI_Bio_Alteration, colheader) %>% 
  pivot_wider(names_from = colheader, values_from= ASCI_Bio_Alteration)
#write.csv
write.csv(pivot.ASCI, file="C:/Users/KristineT/SCCWRP/Santa Margarita River Climate Change Analyses - FlowEcology/FlowData/FFM/Current_2002_2020/ASCI_alteration_summary_scenarios.csv", row.names=FALSE)



