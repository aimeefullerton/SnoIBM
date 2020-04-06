# Pre-make annual Q and WT datasets so the whole thing doesn't need to be loaded each time the model runs.

# Load Functions
source("code/Functions4SnoIBMv2.0.R")


Q_all.df = fncImportQ(paste0(loadDir, "/rbm.data/Outflow.Only.csv")) #flow 2001-2013
WT_all.df = fncImportWT(paste0(loadDir, "/rbm.data/Tw_output.csv")) #temperature 2001-2013


for(year in 2002:2013){
  
  Q.df = Q_all.df[Q_all.df$Date >= as.Date(paste0(year-1,"-09-01")) & Q_all.df$Date <= as.Date(paste0(year,"-09-30")),] #limit to the correct year
  write.csv(Q.df, paste0("data.in/rbm.data/Q.",year, ".df"), row.names = FALSE)
  
  WT.df = WT_all.df[WT_all.df$Date >= as.Date(paste0(year-1,"-09-01")) & WT_all.df$Date <= as.Date(paste0(year,"-09-30")),] #limit to the correct year
  write.csv(WT.df, paste0("data.in/rbm.data/WT.",year, ".df"), row.names = FALSE)
  
}
