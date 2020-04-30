# Pre-make annual Q and WT datasets so the whole thing doesn't need to be loaded each time the model runs.

# Load Functions
source("code/Functions4SnoIBMv2.0.R")
loadDir = "data.in"

for(scenario in c("current_climate_riparian_0", "current_climate_riparian_1", "current_climate_riparian_2", "current_climate_riparian_3")){
  ssn = importSSN(paste0(loadDir, "/sno.rbm.ssn") , predpts='preds')
  
  Q_all.df = fncImportQ(paste0(loadDir, "/rbm.data/", scenario, "/Outflow.Only.csv")) #flow 2001-2013
  WT_all.df = fncImportWT(paste0(loadDir, "/rbm.data/", scenario, "/Tw_output.csv")) #temperature 2001-2013
  
  for(year in 2002:2013){
    
    Q.df = Q_all.df[Q_all.df$Date >= as.Date(paste0(year-1,"-09-01")) & Q_all.df$Date <= as.Date(paste0(year,"-09-30")),] #limit to the correct year
    write.csv(Q.df, paste0("data.in/rbm.data/", scenario, "/Q.",year, ".df"), row.names = FALSE)
    
    WT.df = WT_all.df[WT_all.df$Date >= as.Date(paste0(year-1,"-09-01")) & WT_all.df$Date <= as.Date(paste0(year,"-09-30")),] #limit to the correct year
    write.csv(WT.df, paste0("data.in/rbm.data/", scenario, "/WT.",year, ".df"), row.names = FALSE)
    
  }
}


for(scenario in c("bcc-csm1-1-m","CanESM2","CCSM4","CNRM-CM5","CSIRO-Mk3-6-0","HadGEM2-CC365")){
  ssn = importSSN(paste0(loadDir, "/sno.rbm.ssn") , predpts='preds')
  
  timeperiod = "historical"
  Q_all.df = fncImportQ(paste0(loadDir, "/rbm.data/", scenario, "/Outflow.Only.",timeperiod,".csv")) #flow 2001-2013
  WT_all.df = fncImportWT(paste0(loadDir, "/rbm.data/", scenario, "/Tw_output.",timeperiod,".csv")) #temperature 2001-2013
  
  for(year in 1993:2005){
    Q.df = Q_all.df[Q_all.df$Date >= as.Date(paste0(year-1,"-09-01")) & Q_all.df$Date <= as.Date(paste0(year,"-09-30")),] #limit to the correct year
    write.csv(Q.df, paste0("data.in/rbm.data/", scenario, "/Q.",year, ".df"), row.names = FALSE)
    
    WT.df = WT_all.df[WT_all.df$Date >= as.Date(paste0(year-1,"-09-01")) & WT_all.df$Date <= as.Date(paste0(year,"-09-30")),] #limit to the correct year
    write.csv(WT.df, paste0("data.in/rbm.data/", scenario, "/WT.",year, ".df"), row.names = FALSE)
  }
  timeperiod = "future"
  Q_all.df = fncImportQ(paste0(loadDir, "/rbm.data/", scenario, "/Outflow.Only.",timeperiod,".csv")) #flow 2001-2013
  WT_all.df = fncImportWT(paste0(loadDir, "/rbm.data/", scenario, "/Tw_output.",timeperiod,".csv")) #temperature 2001-2013
  
  for(year in 2087:2099){
    Q.df = Q_all.df[Q_all.df$Date >= as.Date(paste0(year-1,"-09-01")) & Q_all.df$Date <= as.Date(paste0(year,"-09-30")),] #limit to the correct year
    write.csv(Q.df, paste0("data.in/rbm.data/", scenario, "/Q.",year, ".df"), row.names = FALSE)
    
    WT.df = WT_all.df[WT_all.df$Date >= as.Date(paste0(year-1,"-09-01")) & WT_all.df$Date <= as.Date(paste0(year,"-09-30")),] #limit to the correct year
    write.csv(WT.df, paste0("data.in/rbm.data/", scenario, "/WT.",year, ".df"), row.names = FALSE)
  }
}
