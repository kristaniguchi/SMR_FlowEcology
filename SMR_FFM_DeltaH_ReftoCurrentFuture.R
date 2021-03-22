#DeltaH Calculations for all sites and scenarios

#read in percentile data
data <- read.csv("C:/Users/KristineT/SCCWRP/Santa Margarita River Climate Change Analyses - FlowEcology/FlowData/FFM/FFM_percentiles_all_scenarios.csv")

#subset data to reference and non reference (current/future scenarios)
ref.data <- data %>% 
  filter(Scenario == "Reference")
current.future.data <- data %>% 
  filter(Scenario != "Reference")

#output dataframe for DeltaH values
deltaH.outputs <- data[1,]
#save as NA
deltaH.outputs[1,] <- NA


#loop through each site and metric and calculate the change in metric percentiles from reference to current
sites <- c("Gorge", "OldHospital")

for(i in 1:2){
  #subset ref and current/future data to site i
  ref.data.i <- ref.data %>% 
    filter(Site == sites[i])
  current.future.data.i <- current.future.data %>% 
    filter(Site == sites[i])
  
  #loop through each unique metric and calculate deltaH for all percentiles
  unique.metrics <- unique(current.future.data.i$metric)
  
  for(j in 1:length(unique.metrics)){
    #subset to unique metric i
    current.future.data.i.metric <- current.future.data.i %>% 
      filter(metric == unique.metrics[j])
    ref.data.i.metric <- ref.data.i %>% 
      filter(metric == unique.metrics[j])
    #subtract ref.data.i.metric (as a list) from current.future percentile columns, replace values in new deltaH.sub
    deltaH.sub <- current.future.data.i.metric
    deltaH.sub[1:length(current.future.data.i.metric$p10),2:6] <- deltaH.sub[1:length(current.future.data.i.metric$p10),2:6] - as.list(ref.data.i.metric[1,2:6])
    
    #save into DeltaH outputs
    deltaH.outputs <- data.frame(rbind(deltaH.outputs, deltaH.sub))
  }
}

#remove first NA row
deltaH.outputs2 <- deltaH.outputs[2:length(deltaH.outputs$p10),]

#write deltaH outputs
filename1 <- "C:/Users/KristineT/SCCWRP/Santa Margarita River Climate Change Analyses - FlowEcology/FlowData/FFM/FFM_DeltaH_all_scenarios.csv"
write.csv(deltaH.outputs2, filename1, row.names=FALSE)

####DeltaH limits on important metrics for CSCI and ASCI
  #determine if the change in median is within our outside of bio threshold

#read in deltaH thresholds for CSCI and ASCI
bio.thresholds <- read.csv("C:/Users/KristineT/SCCWRP/SOC WQIP - Flow Ecology Study - General/Tier2_analysis/08_all_delta_thresholds.csv")
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

filename2 <- "C:/Users/KristineT/SCCWRP/Santa Margarita River Climate Change Analyses - FlowEcology/FlowData/FFM/FFM_DeltaH_all_scenarios_CSCIASCI_alteration.csv"
write.csv(deltaH.outputs2.sub, filename2, row.names=FALSE)


  