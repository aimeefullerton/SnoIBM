## Pre-calculate water temperature for each point on each date/time needed by IBM
# set directory to same as IBM

library(SSN)
source("code/Functions4SnoIBM.R")

# Read in combined water temperature data file
dat.df<- read.csv("/Users/aimee_fullerton/Box Sync/Current/Work/StreamTemperature/HoboNet/SSN.IBM/SnoqualmieWTdata/WT.2012-18.csv", header = T, stringsAsFactors = F)
dat.df$Date = as.Date(dat.df$Date)
ssn = importSSN("data.in/sno.ssn", predpts='preds')

wt.field<- "DatTim"
tsteps<- c(11,35) #6am & 6pm #c(5,29) #3am & 3pm or 1:48 for all

for(cs in c(2012:2018)){ # cs = climate scenario year
  wt.list = list()
  ii = 1
  first.date = as.Date(paste0(cs-1, "-", "09-01")) #starting date for simulation and for spawning
  last.date = as.Date(paste0(cs, "-", "08-31")) #last date of the simulation
  dat.idx = seq(from = first.date, to = last.date, by = 1) # list of all dates to model
  
  for(i in 1:length(dat.idx)){
    dd<- dat.idx[i]
    for(tt in tsteps){ 
      
      thetitle = fncGetTitle(dd,tt)
      cat(thetitle,"\n")
      thedata = t(dat.df[dat.df$Date == dd & dat.df$Time == tt, 4:ncol(dat.df),])
      thedata[thedata < 0] = 0 #prevent negative temperatures
      
      #if(length(thedata[is.na(thedata)])<length(thedata[!is.na(thedata)])){ #only continue if the number of records with data > the number with NA
      
      #predict over network:
      ssn<- fncLoadDateTime(ssn, thedata) #load a particular day/time to plot
      #sometimes NLCD_23 causes an invalid covariance matrix; in those cases, refit the model without this covariate
      mod = NULL; Worked = FALSE
      tryCatch({
        mod = glmssn(DatTim ~ MINELEVRAW + MAFLOWU + NLCD_23, ssn, CorModels = "Exponential.tailup", addfunccol = "afvFlow")
        Worked = TRUE
      }, error = function(err) {})
      if(Worked == FALSE){
        mod = glmssn(DatTim ~ MINELEVRAW + MAFLOWU, ssn, CorModels = "Exponential.tailup", addfunccol = "afvFlow")
      }
      thepreds = predict(mod, predpointsID = 'preds')
      
      wt.list[[ii]]<- thepreds$ssn.object@predpoints@SSNPoints[[1]]@point.data$DatTim
      
      ii<-ii+1
    }
  }
  #Save data array
  save("wt.list", file = paste0("data.in/sno.ssn/wTemps.ssn_", cs, ".RData"))
  
  # Save as flat file format same as RBM
  tmp = matrix(NA, (length(dat.idx)*2), 1219)
  for(i in 1:(length(dat.idx)*2)){tmp[i,] = wt.list[[i]]}
  wt.df = cbind.data.frame("Date" = sort(rep(dat.idx,2)), "Time" = rep(c(6,18)), tmp)
  colnames(wt.df) = c("Date", "Time", ssn@predpoints@SSNPoints[[1]]@point.data$pid) #colnames are preds$pid
  write.csv(wt.df, paste0("data.in/sno.ssn/WT.df", cs, ".csv"), row.names = FALSE)
}


  # Save as flat file format same as RBM
for(cs in c(2012:2018)){ # cs = climate scenario year
  load(paste0("data.in/sno.ssn/wTemps.ssn_", cs, ".RData"))
  first.date = as.Date(paste0(cs-1, "-", "09-01")) #starting date for simulation and for spawning
  last.date = as.Date(paste0(cs, "-", "08-31")) #last date of the simulation
  dat.idx = seq(from = first.date, to = last.date, by = 1) # list of all dates to model
  tmp = matrix(NA, (length(dat.idx)*2), 1219)
  for(i in 1:(length(dat.idx)*2)){tmp[i,] = wt.list[[i]]}
  wt.df = cbind.data.frame("Date" = sort(rep(dat.idx,2)), "Time" = rep(c(6,18)), tmp)
  colnames(wt.df) = c("Date", "Time", ssn@predpoints@SSNPoints[[1]]@point.data$pid) #colnames are preds$pid
  write.csv(wt.df, paste0("data.in/sno.ssn/WT.df", cs, ".csv"), row.names = FALSE)
}

