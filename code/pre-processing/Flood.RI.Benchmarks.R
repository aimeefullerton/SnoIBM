# Select date/times that represent Flood Recurrence Interval Benchmarks
# AHF 13 February 2020; updated 24 March 2020
# see SnoqualmieFloodRecurrenceIntervals.xls/FRI_multipliers
# NEEDS TO BE RE-CALCULATED IF NEW DHSVM FLOW DATA ARE PROVIDED FOR HISTORICAL PERIOD!!

# Create benchmark datasets to which flows for each date/time in scenario modeling will be compared to assess potential for egg scour
# The assumption is that the date of peak flood recurrence interval is representative of conditions across watershed (this is a simplification!)
# Use flood recurrence interval curves from DeGasperi et al. 2016 report for each of the 9 gages in the basin.
# Look at DHSVM historical record to identify peak flow events that correspond to an FRI of about 1.01 - save this as a benchmark
# Use ratios from DeGasperi FRI curves to compare current flow conditions at dates/times during modeling to these benchmarked values

# Read in data from excel (see "SnoqualmieFloodRecurrenceIntervals.xls"), 
# which was based on DeGasperi 2016 report (https://www.researchgate.net/publication/306108517_Flood_Frequency_Analysis_of_King_County_Rivers_with_an_Emphasis_on_the_January_2009_Floods)
# for all the 9 gages in the Snoqualmie basin
ri_gages = read.csv("data.in/parameters/return_interval_flow_gages_cms.csv", stringsAsFactors = FALSE)
# Units for DHSVM flow = cubic meters per second

# plot ri_gages (DeGasperi's FRI curves) (x-axis nonlinear)
plot(ri_gages[,2], las=1, ylab = "Flow (cms)", xlab = "FRI", xaxt = 'n', ylim=c(0,3000), type = 'n')
axis(1, at = 1:12, labels = ri_gages[,1])
for(i in 1:10){lines(ri_gages[,i])}


# Load the historical dataseries
# note: 'fncImport' is in 'Functions4SnoIBMv2.0.R' / Functions to Load Data into SSN
Q_all.df = fncImportQ(paste0(loadDir, "/rbm.data/Outflow.Only.csv")) #flow 2001-2013

# Isolate flow for the reaches where gages are located
Q_gages.df = Q_all.df[,c("Date","Time",colnames(ri_gages)[-1])] 

# Find the DHSVM date/time where flow best corresponds to the FRI of 1.25 for each of the 9 gaged reaches
ri.benchmarks = data.frame(NA,as.Date("2000-01-01"),NA); colnames(ri.benchmarks) = c("Gage.Reach","Date","Time"); i = 1
ri = 1.25
for(g in 2:length(colnames(ri_gages))){
  gage.column = as.character(colnames(ri_gages)[g])
  q_gages_dat = Q_gages.df[,gage.column]
  (bff = ri_gages[ri_gages$return.interval == ri, gage.column])
  idx = which.min(abs(q_gages_dat - bff))
  (answer = Q_gages.df[idx, c("Date", "Time")])
  ri.benchmarks[i,] = cbind.data.frame(gage.column, answer, stringsAsFactors = F)
  i = i + 1
}
row.names(ri.benchmarks) = NULL

#Date and time that reach X243's FRI was 1.25 (gage with biggest flow, USGS 1214900, Snoqualmie nr Carnation)
bf.date = "2001-11-22"; bf.time = 18
  # second biggest gage is X488 (USGS 12144500, Snoqualmie at Snoqualmie, FRI of 1.25 on 11/06/2008, t=21)
Q_bm = Q_all.df[Q_all.df[,"Date"] == as.Date(bf.date) & Q_all.df[, "Time"] == bf.time,]; row.names(Q_bm) = NULL
v = c(t(Q_bm[1,3:ncol(Q_bm)]))
#sum(v)
write.csv(Q_bm, "data.in/rbm.data/Q_bm.csv", row.names = F)



# get ratios from gage curves
ri_gages = ri_gages[order(ri_gages$return.interval),]
result = c(apply(ri_gages[12,2:10]/ri_gages[2,2:10],1,median),apply(ri_gages[11,2:10]/ri_gages[2,2:10],1,median),apply(ri_gages[10,2:10]/ri_gages[2,2:10],1,median),apply(ri_gages[9,2:10]/ri_gages[2,2:10],1,median),
           apply(ri_gages[8,2:10]/ri_gages[2,2:10],1,median),apply(ri_gages[7,2:10]/ri_gages[2,2:10],1,median),apply(ri_gages[6,2:10]/ri_gages[2,2:10],1,median),apply(ri_gages[5,2:10]/ri_gages[2,2:10],1,median),
           apply(ri_gages[4,2:10]/ri_gages[2,2:10],1,median),apply(ri_gages[3,2:10]/ri_gages[2,2:10],1,median),apply(ri_gages[2,2:10]/ri_gages[2,2:10],1,median),apply(ri_gages[1,2:10]/ri_gages[2,2:10],1,median))
result = sort(result)
fri = c(1.01,1.25,1.5,2,5,10,20,25,40,50,100,200)

plot(fri,result)
lines(fri,(0.7184 * log(fri) + 0.8523))
# y = 0.7184 * ln(fri) + 0.8523; R2 = 0.9885; gives flow multiplier for a particular FRI

# y = 0.3182 * e^(1.376 * result); gives FRI for a particular flow multiplier
# here, compare flow for all reaches on a given date/time to the benchmark 1.25 FRI flow for the same location
flood.date = "2006-11-07"; flood.time = 3
m = Q_all.df[Q_all.df[,"Date"] == as.Date(flood.date) & Q_all.df[, "Time"] == flood.time, 3:ncol(Q_all.df)] / Q_bm[1,3:ncol(Q_bm)]
quantile(m)
fri2 = 0.3182 * exp(1.376 * m)
quantile(t(fri2))
plot(t(fri2))

fri2[,"X243"] #USGS 12149000 @ Carnation gives a FRI of 78 for DHSVM-predicted flow (gage data show it to be more like 40)
fri2[,"X488"] #USGS 12144500 @ Snoqualmie gives a FRI of 163 for DHSVM-predicted flow (gage data show it to be more like 40) 
fri2[fri2>100]
m[,which(fri2>100)] #reaches with highest flows on this date/time
# How many of these are modeling anomalies/artifacts? Seem pretty high. Wonder where they are.
length(fri2[fri2>100]) # 250 out of ~ 800 reaches

# IF this is okay, here's the lines of code to use for translating everyday flow into FRI currency for use in e2f survival function
flood.date = "2006-11-07"; flood.time = 3
m = Q_all.df[Q_all.df[,"Date"] == as.Date(flood.date) & Q_all.df[, "Time"] == flood.time, 3:ncol(Q_all.df)] / Q_bm[1,3:ncol(Q_bm)]
fri2 = 0.3182 * exp(1.376 * m)

