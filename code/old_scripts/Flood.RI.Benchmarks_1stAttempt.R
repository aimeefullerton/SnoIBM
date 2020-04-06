# Select date/times that represent Flood Recurrence Interval Benchmarks
# AHF 13 February 2020

# For estimating gg-to-fry survival from peak flows
# Use flood recurrence interval curves from DeGasperi et al. 2016 report for each of the 9 gages in the basin.
# Look at DHSVM historical record to identify peak flow events that correspond to each RI level
# Store these "maps" (i.e. values for all reaches) as recurrence interval benchmarks.
# When running scenarios, compare the "map" from each timestep to the benchmark maps to estimate probability of scour (see 'fncPeakFlowSurvival'). 


# Read in data from excel (see "SnoqualmieFloodRecurrenceIntervals.xls"), 
# which was based on DeGasperi 2016 report (https://www.researchgate.net/publication/306108517_Flood_Frequency_Analysis_of_King_County_Rivers_with_an_Emphasis_on_the_January_2009_Floods)
# for all the 9 gages in the Snoqualmie basin
ri_gages = read.csv("data.in/parameters/return_interval_flow_gages_cms.csv", stringsAsFactors = FALSE)
# Units for DHSVM flow = cubic meters per second

# Load the historical dataseries
# note: 'fncImport' is in 'Functions4SnoIBMv2.0.R' / Functions to Load Data into SSN
Q_all.df = fncImportQ(paste0(loadDir, "/rbm.data/Outflow.Only.csv")) #flow 2001-2013

# Isolate flow for the reaches where gages are located
Q_gages.df = Q_all.df[,c("Date","Time",colnames(ri_gages)[-1])] 
#Q.quantile = t(apply(Q.df[,3:ncol(Q.df)],2,quantile))
#Q.quantile = t(Q.quantile[rownames(Q.quantile) %in% colnames(ri_gages)[-1],])

# Get the sum of flow across the 9 gages for each date/time
# (used in minimization procedure below)
q_gages_sum = apply(Q_gages.df[,3:ncol(Q_gages.df)], 1, sum)

# Identify the dates/times from the historical flow record that best match the different recurrence interval flows:
ri.benchmarks = data.frame(NA,as.Date("2000-01-01"),NA); colnames(ri.benchmarks) = c("RI","Date","Time"); i = 1
for(ri in c(1.01,1.25,1.5,2,5,10,20,25,40,50,100,200)){
  # get flow at gage locations that corresponds to the recurrence interval 'ri'
  (bff = ri_gages[ri_gages$return.interval == ri,-1])
  # identify which date/time has the flow that best matches the flow at the recurrence interval 'ri'
  idx = which.min(abs(q_gages_sum - sum(bff)))
  #Q_gages.df[idx,] # have a look at the minimized selection
  #Q_gages.df[abs(q_gages_sum - sum(bff)) < 20,] # to see a handful of options; may return zero rows for higher 'ri'
  answer = Q_gages.df[idx, c("Date", "Time")]
  ri.benchmarks[i,] = cbind(ri, answer) #store date and time
  i = i + 1
}
row.names(ri.benchmarks) = NULL
#write.csv(ri.benchmarks,"data.in/parameters/ri.benchmarks.csv",row.names = FALSE)

# #one gage at a time (because RIs likely differ across space) #@@@-WIP not operationalized!
# gage.column = 11
# ri.benchmarks = data.frame(NA,as.Date("2000-01-01"),NA); colnames(ri.benchmarks) = c("RI","Date","Time"); i = 1
# for(ri in c(1.01,1.25,1.5,2,5,10,20,25,40,50,100,200)){
#   q_gages_sum = Q_gages.df[,gage.column]
#   (bff = ri_gages[ri_gages$return.interval == ri, gage.column-1])
#   idx = which.min(abs(q_gages_sum - sum(bff)))
#   answer = Q_gages.df[idx, c("Date", "Time")]
#   ri.benchmarks[i,] = cbind(ri, answer)
#   i = i + 1
# }
# row.names(ri.benchmarks) = NULL
# x517 = ri.benchmarks



# Create benchmark datasets to which flows for each date/time in scenario modeling will be compared to assess potential for egg scour
# The assumption is that the date of peak flood recurrence interval is representative of conditions across watershed (this is a simplification!)
Q.benchmarks = Q_all.df[Q_all.df$Date == ri.benchmarks$Date[1] & Q_all.df$Time == ri.benchmarks$Time[1],]
for(i in 2:nrow(ri.benchmarks)){
  Q.benchmarks[i,] = Q_all.df[Q_all.df$Date == ri.benchmarks$Date[i] & Q_all.df$Time == ri.benchmarks$Time[i],]
}
Q.RI.benchmarks = cbind("RI" = ri.benchmarks$RI, Q.benchmarks)
write.csv(Q.RI.benchmarks,"data.in/parameters/Q.RI.benchmarks.csv", row.names = FALSE)

# This is what the file looks like for the first 3 reaches:
# Q.benchmarks[1:5]
#       Date      Time        X0     X1        X2
# 1.01 2004-05-27   18  343.0890 0.1672  342.8099
# 1.25 2001-11-16    3  726.8039 0.2754  726.2791
# 1.5  2006-01-11   12  964.1965 0.5930  962.9716
# 2    2007-03-25    0 1124.6003 0.5179 1123.6769
# 5    2006-11-08   12 1592.0681 0.4473 1591.1290
# 10   2006-11-08    6 1881.8826 0.6146 1880.6735
# 20   2006-11-08    0 2193.5872 0.8364 2192.0552
# 25   2006-11-07   18 2280.5486 0.8334 2279.0596
# 40   2006-11-07    3 2373.4875 0.8117 2372.0798
# 50   2006-11-07    3 2373.4875 0.8117 2372.0798
# 100  2006-11-07    3 2373.4875 0.8117 2372.0798
# 200  2006-11-07    3 2373.4875 0.8117 2372.0798



# Egg-to-fry survival from peak flows
fncPeakFlowSurvival<- function(fish, aa = parameters["salmon","pk.q.a"], bb = parameters["salmon","pk.q.b"]){
  # where fish = salmon[salmon$emrg == -1 & salmon$survive == 1, c("pid", "seg", "survive", "Q", ri.vars)]
  
  # Beamer and Pess 1999: Egg to migrant fry survival as a function of peak flow recurrence interval during egg incubation 
  # (at Mt. Vernon) in the Skagit River basin (1989 to 1996) (y=0.1285e-0.0446x,R2=0.97).
  # Seiler et al. 1998: the data used in Beamer and Pess, and they fit linear relationship with flow (cfs) directly.
  # See Figure 14 in Kinsel et al. 2007 WDFW: file:///Users/aimee_fullerton/Box%20Sync/Current/Work/SnoBIA/resources/Kinsel_2007_wdfw00092.pdf
  # But the direct relationship isn't transferrable across basins. Therefore use recurrence intervals, which are transferrable.
  
  # For each redd at each time step, assign a "survival probability" for eggs based on flows for current date/time as compared to benchmark flows.
  # For instance, if flows on [date/time] > flows that occurred on 11-7-2006 (a 40-year recurrence interval flood), assign a survival probability of [0.0215835].
  # Units for DHSVM flow = cubic meters per second
  
  #aa = 0.1285; bb = -0.0446 #Beamer and Pess 1999 relationship for whole season, but presumably happens from a few isolated big flow events
  #aa = 1 # Making max survival for given time step 1. 
  # With this, survival from one 40-yr RI event is approx 0.72 (Pess 2016 fine sediment study mean was 0.58) but fish are going to experience this flood for
  # more than one time step and there may be other floods too. May have to calibrate this parameter.
  
  
  # Simple way to translate flow into survival based on thresholds
  probs = rep(0, nrow(fish))
  # probs[fish$Q < fish$Q1.5] = aa * exp(bb * 1)
  # probs[fish$Q >= fish$Q1.5 & fish$Q < fish$Q2] = aa * exp(bb * 1.5)
  # probs[fish$Q >= fish$Q2 & fish$Q < fish$Q5] = aa * exp(bb * 2)
  # probs[fish$Q >= fish$Q5 & fish$Q < fish$Q10] = aa * exp(bb * 5)
  # probs[fish$Q >= fish$Q10 & fish$Q < fish$Q20] = aa * exp(bb * 10)
  # probs[fish$Q >= fish$Q20 & fish$Q < fish$Q25] = aa * exp(bb * 20)
  # probs[fish$Q >= fish$Q25 & fish$Q < fish$Q40] = aa * exp(bb * 25)
  # #we don't have flows in the gage record greater than a 40-yr recurrence interval
  # # instead use this to get multipliers for approximating survival at higher flows:
  #   #apply(ri_gages[9,]/ri_gages[10,],1,mean) = 1.124
  #   #apply(ri_gages[9,]/ri_gages[11,],1,mean) = 1.521
  #   #apply(ri_gages[9,]/ri_gages[12,],1,mean) = 3.187
  # probs[fish$Q >= fish$Q40 & fish$Q < 1.124*fish$Q40] = aa * exp(bb * 40)
  # probs[fish$Q >= 1.124*fish$Q40 & fish$Q < 1.521*fish$Q40] = aa * exp(bb * 50)
  # probs[fish$Q >= 1.521*fish$Q40 & fish$Q < 3.187*fish$Q40] = aa * exp(bb * 100)
  # probs[fish$Q >= 3.187*fish$Q40] = aa * exp(bb * 200)

  #Slightly newer method
  probs[fish$Q < fish$Q.bm] = aa * exp(bb * 1)
  probs[fish$Q >= fish$Q.bm & fish$Q < fish$Q.bm * result[11]] = aa * exp(bb * 1.25)
  probs[fish$Q >= fish$Q.bm * result[11] & fish$Q < fish$Q.bm * result[10]] = aa * exp(bb * 1.5)
  probs[fish$Q >= fish$Q.bm * result[10] & fish$Q < fish$Q.bm * result[9]] = aa * exp(bb * 2)
  probs[fish$Q >= fish$Q.bm * result[9] & fish$Q < fish$Q.bm * result[8]] = aa * exp(bb * 5)
  probs[fish$Q >= fish$Q.bm * result[8] & fish$Q < fish$Q.bm * result[7]] = aa * exp(bb * 10)
  probs[fish$Q >= fish$Q.bm * result[7] & fish$Q < fish$Q.bm * result[6]] = aa * exp(bb * 20)
  probs[fish$Q >= fish$Q.bm * result[6] & fish$Q < fish$Q.bm * result[5]] = aa * exp(bb * 25)
  probs[fish$Q >= fish$Q.bm * result[5] & fish$Q < fish$Q.bm * result[4]] = aa * exp(bb * 40)
  probs[fish$Q >= fish$Q.bm * result[4] & fish$Q < fish$Q.bm * result[3]] = aa * exp(bb * 50)
  probs[fish$Q >= fish$Q.bm * result[3] & fish$Q < fish$Q.bm * result[2]] = aa * exp(bb * 100)
  probs[fish$Q >= fish$Q.bm * result[2] & fish$Q < fish$Q.bm * result[1]] = aa * exp(bb * 200)
  
  
  survivors = rbinom(n = nrow(fish), size = 1, prob = probs)
  #sum(survivors)/nrow(fish)
  
  #hist(probs)
  #plot(probs, ylim=c(0,1))
  #points(survivors,col=2)
  #plot(probs, survivors)
  
  return(survivors)
}

