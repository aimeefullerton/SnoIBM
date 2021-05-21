# Pre-make annual Q and WT datasets so the whole thing doesn't need to be loaded each time the model runs.

# Load Functions
source("code/functions.R")
library(SSN)
loadDir = "data.in"

# Baseline climate from Livneh (2003-2013), 4 riparian scenarios
climate.scenario <- "climate0"
for(riparian.scenario in c("current_climate_riparian_0", "current_climate_riparian_1", "current_climate_riparian_2", "current_climate_riparian_3")){
  ssn = importSSN(paste0(loadDir, "/sno.rbm.ssn") , predpts='preds')
  
  Q_all.df = fncImportQ(paste0(loadDir, "/rbm.data/", climate.scenario, "/", riparian.scenario, "/Outflow.Only.csv")) #flow all years
  WT_all.df = fncImportWT(paste0(loadDir, "/rbm.data/", climate.scenario, "/", riparian.scenario, "/Tw_output.csv")) #temperature all years
  
  for(year in 2002:2013){
    Q.df = Q_all.df[Q_all.df$Date >= as.Date(paste0(year-1,"-09-01")) & Q_all.df$Date <= as.Date(paste0(year,"-09-30")),] #limit to the correct year
    write.csv(Q.df, paste0("data.in/rbm.data/", climate.scenario, "/", riparian.scenario, "/Q.",year, ".df"), row.names = FALSE)
    
    WT.df = WT_all.df[WT_all.df$Date >= as.Date(paste0(year-1,"-09-01")) & WT_all.df$Date <= as.Date(paste0(year,"-09-30")),] #limit to the correct year
    write.csv(WT.df, paste0("data.in/rbm.data/", climate.scenario, "/", riparian.scenario, "/WT.",year, ".df"), row.names = FALSE)
  }
}


# Historical (1995-2005) & Future (2089-2099) climate from GCMs, 4 riparian scenarios
riparian.scenario <- "riparian1"
rnm <- paste0("_s", substr(riparian.scenario, 9, 9))
if(riparian.scenario == "riparian0") rnm<- NULL

for(climate.scenario in c("bcc-csm1-1-m","CanESM2","CCSM4","CNRM-CM5","CSIRO-Mk3-6-0","HadGEM2-CC365", "HadGEM2-ES365", "IPSL-CM5A-MR", "MIROC5", "NorESM1-M")){
  ssn = importSSN(paste0(loadDir, "/sno.rbm.ssn") , predpts='preds')
  
  timeperiod = "historical"
  Q_all.df = fncImportQ(paste0(loadDir, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Outflow.Only.", timeperiod, rnm, ".csv")) #flow all years
  WT_all.df = fncImportWT(paste0(loadDir, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Tw_output.", timeperiod, rnm, ".csv")) #temperature all years
  
  for(year in 1993:2005){
    Q.df = Q_all.df[Q_all.df$Date >= as.Date(paste0(year-1,"-09-01")) & Q_all.df$Date <= as.Date(paste0(year,"-09-30")),] #limit to the correct year
    write.csv(Q.df, paste0("data.in/rbm.data/", riparian.scenario, "/", climate.scenario, "/Q.",year, ".df"), row.names = FALSE)
    
    WT.df = WT_all.df[WT_all.df$Date >= as.Date(paste0(year-1,"-09-01")) & WT_all.df$Date <= as.Date(paste0(year,"-09-30")),] #limit to the correct year
    write.csv(WT.df, paste0("data.in/rbm.data/", riparian.scenario, "/", climate.scenario, "/WT.",year, ".df"), row.names = FALSE)
  }
  timeperiod = "future"
  Q_all.df = fncImportQ(paste0(loadDir, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Outflow.Only.", timeperiod, rnm, ".csv")) #flow all years
  WT_all.df = fncImportWT(paste0(loadDir, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Tw_output.", timeperiod, rnm, ".csv")) #temperature all years
  
  for(year in 2087:2099){
    Q.df = Q_all.df[Q_all.df$Date >= as.Date(paste0(year-1,"-09-01")) & Q_all.df$Date <= as.Date(paste0(year,"-09-30")),] #limit to the correct year
    write.csv(Q.df, paste0("data.in/rbm.data/", riparian.scenario, "/", climate.scenario, "/Q.",year, ".df"), row.names = FALSE)
    
    WT.df = WT_all.df[WT_all.df$Date >= as.Date(paste0(year-1,"-09-01")) & WT_all.df$Date <= as.Date(paste0(year,"-09-30")),] #limit to the correct year
    write.csv(WT.df, paste0("data.in/rbm.data/", riparian.scenario, "/", climate.scenario, "/WT.",year, ".df"), row.names = FALSE)
  }
}
