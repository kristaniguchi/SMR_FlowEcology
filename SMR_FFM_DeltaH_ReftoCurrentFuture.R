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
  