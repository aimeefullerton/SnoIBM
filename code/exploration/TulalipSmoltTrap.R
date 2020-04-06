setwd("Work/SnoBIA/resources")
td = read.csv("SnoqualmieTrap_ChinFKL_12-16.csv", header = T, stringsAsFactors = F)
td = td[, c(5,8)]
td = td[!is.na(td[,2]),]
colnames(td) = c("Date","FL")
td$Date = as.Date(td$Date,origin="1970-01-01",format = "%m/%d/%Y")
td$Date2 = as.POSIXlt(td$Date)

# Plot fork lengths sampled versus date
png("SnoTrapCountsVsDate.png",6,6,"in",res=300)
par(mfrow = c(3,2), mar = c(5,4,2,1), las = 1)
for(yy in 2012:2016){
  subset = td$Date2 > as.POSIXlt(paste0(yy,"-01-01")) & td$Date2 < as.POSIXlt(paste0(yy,"-12-31"))
  plot(td$Date2[subset], td$FL[subset], xlab = "Date", ylab = "Count", ylim = c(0,250), main = yy)
  subset2 = td$Date2 < as.POSIXlt(paste0(yy,"-05-01")) & td$FL > 65
  points(td$Date2[subset2], td$FL[subset2], pch = 19, col=4)

}
legend("topleft", legend = c("subyearling smolts", "yearling smolts"), col = c(1,4), pch = c(1,19), bty = 'n')
dev.off()


# Get proportion yearlings for a given year:
yy = 2014
(v1 =length(td$FL[td$Date2 > as.POSIXlt(paste0(yy+1,"-01-01")) & td$Date2 < as.POSIXlt(paste0(yy+1,"-05-01")) & td$FL >= 65]))
(v2 =length(td$FL[td$Date2 > as.POSIXlt(paste0(yy,"-01-01")) & td$Date2 < as.POSIXlt(paste0(yy,"-12-31")) & td$FL < 65]))
(v1/v2)  
  
# Plot estimated weekly production by statistical week sampled
td = read.csv("SnoqualmieTrap_Chin_WeeklyProd.csv", header = T, stringsAsFactors = F)
png("SnoTrapEstProdVsWeek.png",6,6,"in",res=300)
par(mfrow = c(2,2), mar = c(5,4,2,1), las = 1)
for(yy in 2012:2015){
  td.sub = td[td$Year==yy,]
  plot(td.sub$Stat.Week,td.sub$Est..Week.Prod., xlim = c(6,25), ylim = c(0,21000), ylab = "no. Fish", xlab = "statistical week", main = yy)
  points(td.sub$Stat.Week[which.max(td.sub$Est..Week.Prod.)], td.sub$Est..Week.Prod.[which.max(td.sub$Est..Week.Prod.)],pch=19,col=2)
  legend("topleft",legend = paste0("Peaks week ",td.sub$Stat.Week[which.max(td.sub$Est..Week.Prod.)]), bty='n')
}
dev.off()


