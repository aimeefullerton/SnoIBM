
library(SSN)
library(lubridate)
source("code/Functions4SnoIBMv2.0.R")

# COLORS 
fncColors4Quantiles <- function(){
  #Min/max
  c1.0<- rgb(226,226,226,150,NULL,255) #hex #e2e2e2, light gray
  c1.1<- rgb(252,217,156,150,NULL,255) #hex #fcd99c96 orange
  c1.2<- rgb(247,246,187,150,NULL,255) #hex #f7f6bb96 yellow
  c1.3<- rgb(176,191,252,150,NULL,255) #hex #b0bffc96 blue
  c1.4<- rgb(209,167,207,150,NULL,255) #hex #d1a7cf purple
  c1.5<- rgb(196,237,197,150,NULL,255) #hex #e2e2e2 green
  c1.6<- rgb(171,201,205,150,NULL,255) #hex #abc9cd aqua
  
  #Q1/Q3
  c2.0<- rgb(142,142,142,200,NULL,255) #hex #8e8e8e, gray
  c2.1<- rgb(234,173,68,200,NULL,255) #hex #eaad44c8 orange
  c2.2<- rgb(239,237,95,200,NULL,255) #hex #efed5fc8 yellow
  c2.3<- rgb(128,153,252,200,NULL,255) #hex #809ffcc8 blue
  c2.4<- rgb(137,101,136,200,NULL,255) #hex #896588 purple
  c2.5<- rgb(81,198,83,200,NULL,255) #hex #8e8e8e green
  c2.6<- rgb(74,146,155,200,NULL,255) #hex #4a929b aqua
  
  #Median
  c3.0<- rgb(5,5,5,255,NULL,255) #black
  c3.1<- rgb(244,155,2,255,NULL,255) #hex #f49b02ff orange
  c3.2<- rgb(206,193,8,255,NULL,255) # hex #cec108ff yellow
  c3.3<- rgb(3,38,178,255,NULL,255) #hex #0326b2ff blue
  c3.4<- rgb(97,18,104,255,NULL,255) #hex #611268 purple
  c3.5<- rgb(1,137,3,255,NULL,255) #green
  c3.6<- rgb(66,110,130,255,NULL,255) #hex #426e82 aqua
  
  c1 <- c(c1.0, c1.1, c1.2, c1.3, c1.4, c1.5, c1.6)
  c2 <- c(c2.0, c2.1, c2.2, c2.3, c2.4, c2.5, c2.6)
  c3 <- c(c3.0, c3.1, c3.2, c3.3, c3.4, c3.5, c3.6)

  return(list(c1, c2, c3))
}
mycolors <- fncColors4Quantiles()


loadDir <- "/Volumes/BluPassport/SnoIBM/data.in"
timeperiod <- "historical"
riparian.scenario.list <- c("riparian0", "riparian1", "riparian2", "riparian3")
riparian.scenario <- riparian.scenario.list[1]
climate.scenario.list <- c("bcc-csm1-1-m", "CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "HadGEM2-CC365", "HadGEM2-ES365", "IPSL-CM5A-MR", "MIROC5", "NorESM1-M")
climate.scenario <- climate.scenario.list[1]
ssn <- importSSN(paste0("data.in/sno.rbm.ssn"), predpts = 'preds')


# Gather data for lowest mainstem reach across all years and GCMs
for(timeperiod in c("historical", "future")){
  for(riparian.scenario in riparian.scenario.list){
    
    suffix <- paste0("_s", substr(riparian.scenario,9,9))
    if(riparian.scenario == "riparian0") suffix <- ""

    Q.df <- fncImportQ(paste0(loadDir, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Outflow.Only.", timeperiod, suffix, ".csv"))[, 1:3]
    T.df <- fncImportWT(paste0(loadDir, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Tw_output.", timeperiod, suffix, ".csv"))[, 1:3]
    
    for(climate.scenario in climate.scenario.list[2:length(climate.scenario.list)]){
      Qq <- fncImportQ(paste0(loadDir, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Outflow.Only.", timeperiod, suffix, ".csv"))[,3]
      Tt <- fncImportWT(paste0(loadDir, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Tw_output.", timeperiod, suffix, ".csv"))[,3]
      Q.df <- cbind(Q.df, Qq)
      T.df <- cbind(T.df, Tt)
    }
    colnames(Q.df) <- c("Date", "Time", climate.scenario.list)
    colnames(T.df) <- c("Date", "Time", climate.scenario.list)
    if(timeperiod == "historical"){
      Q.df <- Q.df[Q.df$Date >= as.Date("1994-10-01"),]
      T.df <- T.df[T.df$Date >= as.Date("1994-10-01"),]
    } else if(timeperiod == "future"){
      Q.df <- Q.df[Q.df$Date >= as.Date("2088-10-01"),]
      T.df <- T.df[T.df$Date >= as.Date("2088-10-01"),]
    }
    assign(paste0(riparian.scenario, ".", timeperiod, ".Q"), Q.df)
    assign(paste0(riparian.scenario, ".", timeperiod, ".T"), T.df)
    write.csv(Q.df, paste0("data.in/rbm.data/", riparian.scenario, ".", timeperiod, ".Q.csv"), row.names = F)
    write.csv(T.df, paste0("data.in/rbm.data/", riparian.scenario, ".", timeperiod, ".T.csv"), row.names = F)
  }
}



# Read back in
for(riparian.scenario in riparian.scenario.list){
  for(timeperiod in c("historical", "future")){
    Qdat <- read.csv(paste0("data.in/rbm.data/", riparian.scenario, ".", timeperiod, ".Q.csv"), header = T, stringsAsFactors = F)
    Qdat$Date = as.Date(Qdat$Date)
    assign(paste0(riparian.scenario, ".", timeperiod, ".Q"), Qdat)
    Tdat <- read.csv(paste0("data.in/rbm.data/", riparian.scenario, ".", timeperiod, ".T.csv"), header = T, stringsAsFactors = F)
    Tdat$Date = as.Date(Tdat$Date)
    assign(paste0(riparian.scenario, ".", timeperiod, ".T"), Tdat)
    rm(Qdat, Tdat)
  }
}



# Plot time series of GCM ensemble medians and quantiles with transparent shading

png("plots/FigureS2_QT_timeseries.png", width = 10, height = 7, units = "in", res = 300)
par(mfrow = c(2,1), las = 1, mfrow = c(2,2), mar = c(3,4.5,3,1), oma = rep(0.5,4), cex = 1.1)

for(timeperiod in c("historical", "future")){
  
  if(timeperiod == "historical"){col2use <- 4; labls <- c("(a)","(b)")}
  if(timeperiod == "future"){col2use <- 2; labls <- c("(c)","(d)")}
  
  # Temperature
  Tdat <- get(paste0(riparian.scenario, ".", timeperiod, ".T"))
  themedian <- apply(Tdat[3:ncol(Tdat)], 1, median)
  themin <- apply(Tdat[3:ncol(Tdat)], 1, quantile, probs = 0.05)
  themax <- apply(Tdat[3:ncol(Tdat)], 1, quantile, probs = 0.95)
  xvals <- c(Tdat$Date, rev(Tdat$Date))
  
  plot(Tdat$Date, themedian, type = 'n', ylab = expression("Water temperature ("*degree*C*")"), xlab = "", ylim = c(0, 30))
  polygon(xvals, c(themin, rev(themax)), border = NA, col = mycolors[[1]][col2use])
  lines(Tdat$Date, themedian, col = mycolors[[3]][col2use])
  legend("topleft", legend = labls[1], bty = 'n')
  
  # Flow
  Qdat <- get(paste0(riparian.scenario, ".", timeperiod, ".Q"))
  themedian <- apply(Qdat[3:ncol(Qdat)], 1, median)
  themin <- apply(Qdat[3:ncol(Qdat)], 1, quantile, probs = 0.05)
  themax <- apply(Qdat[3:ncol(Qdat)], 1, quantile, probs = 0.95)
  xvals <- c(Qdat$Date, rev(Qdat$Date))
  
  plot(Qdat$Date, themedian, type = 'n', ylab = expression("Flow (m s"^-1*")"), xlab = "", ylim = c(0,1200))
  polygon(xvals, c(themin, rev(themax)), border = NA, col = mycolors[[1]][col2use])
  lines(Qdat$Date, themedian, col = mycolors[[3]][col2use])
  legend("topleft", legend = labls[2], bty = 'n')
}

dev.off()

# Get all data into same year for plotting
# change year so that day-month is the comparable across scenarios.
for(riparian.scenario in riparian.scenario.list){
  for(timeperiod in c("historical", "future")){
    if(timeperiod == "historical") theyears <- 1995:2005
    if(timeperiod == "future") theyears <- 2089:2099
    Q.data <- get(paste0(riparian.scenario, ".", timeperiod, ".Q"))
    T.data <- get(paste0(riparian.scenario, ".", timeperiod, ".T"))
    Q.data$Year = year(Q.data$Date)
    T.data$Year = year(T.data$Date)
    
    for(yy in theyears){
      
      # Remove leap year's extra day:
      if(yy %in% c(1996, 2000, 2004, 2092, 2096)){
        idx <- which(Q.data$Date == as.Date(paste0(yy, "-02-29")))
        Q.data <- Q.data[-idx,]
        T.data <- T.data[-idx,]
        rm(idx)
      }
      
      #Flow
      foo = Q.data[Q.data[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & Q.data[, "Date"] < as.Date(paste0(yy, "-10-01")) & !is.na(Q.data[, "Date"]),]
      year(foo[foo[, "Year"] == yy - 1, "Date"]) <- 1900
      year(foo[foo[, "Year"] == yy, "Date"]) <- 1901
      Q.data[Q.data[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & Q.data[, "Date"] < as.Date(paste0(yy, "-10-01")) & !is.na(Q.data[, "Date"]),] <- foo
      #Temp
      foo = T.data[T.data[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & T.data[, "Date"] < as.Date(paste0(yy, "-10-01")) & !is.na(T.data[, "Date"]),]
      year(foo[foo[, "Year"] == yy - 1, "Date"]) <- 1900
      year(foo[foo[, "Year"] == yy, "Date"]) <- 1901
      T.data[T.data[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & T.data[, "Date"] < as.Date(paste0(yy, "-10-01")) & !is.na(T.data[, "Date"]),] <- foo
    }
    
    # remove extra erroneous records
    idx <- which(Q.data$Date > as.Date("1901-09-30"))
    if(length(idx) > 0) {Q.data <- Q.data[-idx,]; rm(idx)}
    idx <- which(T.data$Date > as.Date("1901-09-30"))
    if(length(idx) > 0) {T.data <- T.data[-idx,]; rm(idx)}
    
    
    assign(paste0(riparian.scenario, ".", timeperiod, ".Qdat"), Q.data)
    assign(paste0(riparian.scenario, ".", timeperiod, ".Tdat"), T.data)
  }
}


png("plots/Figure6_QT_annual_change.png", width = 5.5, height = 9, units = "in", res = 300)
par(las = 1, mfrow = c(5,1), mar = c(2.5,4.5,0,0.5), oma = rep(0.5,4))

# Top row Riparian baseline Future minus Historical
  # Flow
  Qdat.h <- riparian0.historical.Qdat
  Qdat.f <- riparian0.future.Qdat
  Qdat <- Qdat.f - Qdat.h; Qdat <- Qdat[-c(1,2,ncol(Qdat))]; Qdat$Date <- Qdat.h$Date
  themin <- aggregate(Qdat[,1:10], by = list(Qdat$Date), quantile, probs = 0.05); colnames(themin)[1] <- "Date"
  themin <- apply(themin[,2:11], 1, mean)
  themedian <- aggregate(Qdat[,1:10], by = list(Qdat$Date), quantile, probs = 0.5); colnames(themedian)[1] <- "Date"
  themedian <- apply(themedian[,2:11], 1, mean)
  themax <- aggregate(Qdat[,1:10], by = list(Qdat$Date), quantile, probs = 0.95); colnames(themax)[1] <- "Date"
  themax <- apply(themax[,2:11], 1, mean)
  xvals <- c(unique(Qdat.h$Date), rev(unique(Qdat.h$Date)))
  
  plot(unique(Qdat.h$Date), themedian, type = 'n', ylab = expression(Delta~"Flow (m s"^-1*")"), xlab = "", ylim = c(-600, 1200), xaxt = 'n', cex.lab = 1.3)
  axis(1, at = c(as.Date("1900-11-01"), as.Date("1901-01-01"), as.Date("1901-03-01"), as.Date("1901-05-01"), as.Date("1901-07-01"), as.Date("1901-09-01")), labels = F)
  polygon(xvals, c(themin, rev(themax)), border = NA, col = mycolors[[1]][7])
  lines(unique(Qdat.h$Date), themedian, col = mycolors[[3]][7])
  abline(h = 1, lty = 3, col = "darkgray")
  legend("topleft", legend = "(a) Climate effect, Baseline riparian", bty = 'n', cex = 1.4)

  # Temperature
  Tdat.h <- riparian0.historical.Tdat
  Tdat.f <- riparian0.future.Tdat
  Tdat <- Tdat.f - Tdat.h; Tdat <- Tdat[-c(1,2,ncol(Tdat))]; Tdat$Date <- Tdat.h$Date
  themin <- aggregate(Tdat[,1:10], by = list(Tdat$Date), quantile, probs = 0.05); colnames(themin)[1] <- "Date"
  themin <- apply(themin[,2:11], 1, mean)
  themedian <- aggregate(Tdat[,1:10], by = list(Tdat$Date), quantile, probs = 0.5); colnames(themedian)[1] <- "Date"
  themedian <- apply(themedian[,2:11], 1, mean)
  themax <- aggregate(Tdat[,1:10], by = list(Tdat$Date), quantile, probs = 0.95); colnames(themax)[1] <- "Date"
  themax <- apply(themax[,2:11], 1, mean)
  xvals <- c(unique(Tdat.h$Date), rev(unique(Tdat.h$Date)))
  
  plot(unique(Tdat.h$Date), themedian, type = 'n', ylab = expression(Delta~"Temperature ("*degree*C*")"), xlab = "", ylim = c(-5, 15), xaxt = 'n', cex.lab = 1.3)
  axis(1, at = c(as.Date("1900-11-01"), as.Date("1901-01-01"), as.Date("1901-03-01"), as.Date("1901-05-01"), as.Date("1901-07-01"), as.Date("1901-09-01")), labels = F)
  polygon(xvals, c(themin, rev(themax)), border = NA, col = mycolors[[1]][5])
  lines(unique(Tdat.h$Date), themedian, col = mycolors[[3]][5])
  abline(h = 1, lty = 3, col = "darkgray")
  legend("topleft", legend = "(b) Climate effect, Baseline riparian", bty = 'n', cex = 1.4)
  
# Next 3 rows: Riparian scenario future X minus riparian baseline scenario future
for(riparian.scenario in riparian.scenario.list[c(2,4,3)]){

  # Temperature
  Tdat.0 <- riparian0.future.Tdat
  Tdat.x <- get(paste0(riparian.scenario, ".future.Tdat"))
  Tdat <- Tdat.x - Tdat.0; Tdat <- Tdat[-c(1,2,ncol(Tdat))]; Tdat$Date <- Tdat.0$Date
  themin <- aggregate(Tdat[,1:10], by = list(Tdat$Date), quantile, probs = 0.05); colnames(themin)[1] <- "Date"
  themin <- apply(themin[,2:11], 1, mean)
  themedian <- aggregate(Tdat[,1:10], by = list(Tdat$Date), quantile, probs = 0.5); colnames(themedian)[1] <- "Date"
  themedian <- apply(themedian[,2:11], 1, mean)
  themax <- aggregate(Tdat[,1:10], by = list(Tdat$Date), quantile, probs = 0.95); colnames(themax)[1] <- "Date"
  themax <- apply(themax[,2:11], 1, mean)
  xvals <- c(unique(Tdat.0$Date), rev(unique(Tdat.0$Date)))
  
  if(riparian.scenario == riparian.scenario.list[3]) plot(unique(Tdat.0$Date), themedian, type = 'n', ylab = expression(Delta~"Temperature ("*degree*C*")"), xlab = "", ylim = c(-1.25, 1.25), cex.lab = 1.3)
  else{
    plot(unique(Tdat.0$Date), themedian, type = 'n', ylab = expression(Delta~"Temperature ("*degree*C*")"), xlab = "", ylim = c(-1.25, 1.25), xaxt = 'n', cex.lab = 1.3)
    axis(1, at = c(as.Date("1900-11-01"), as.Date("1901-01-01"), as.Date("1901-03-01"), as.Date("1901-05-01"), as.Date("1901-07-01"), as.Date("1901-09-01")), labels = F)
  }
  polygon(xvals, c(themin, rev(themax)), border = NA, col = mycolors[[1]][5])
  lines(unique(Tdat.0$Date), themedian, col = mycolors[[3]][5])
  abline(h = 0, lty = 3, col = "darkgray")
  lg <- c("(c) Full restoration effect, Future climate", "(e) Riparian degradation effect, Future climate", "(d) Partial restoration effect, Future climate")[as.numeric(substr(riparian.scenario, 9, 9))]
  legend("topleft", legend = lg, bty = 'n', cex = 1.4)
  
}

dev.off()
