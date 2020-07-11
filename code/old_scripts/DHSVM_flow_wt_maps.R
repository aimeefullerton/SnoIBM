

# Libraries
library(SSN)
library(RColorBrewer)
library(tidyverse)
library(sf)
library(lubridate)

source("code/Functions4SnoIBMv2.0.R")

# Setup
imageDir <- "plots"
loadDir <- "/Volumes/BluPassport/SnoIBM/data.in"
ssn.folder <- "sno.rbm.ssn"


# Read in basin outline & streams
basin <- read_sf(paste0(loadDir, "/shapefiles"), "Basin_snq2")
# dissolve on area to just get the outline
basin2 <- basin %>% summarise(area = sum(AreaSqKm))
streams <- read_sf(paste0(loadDir, "/", ssn.folder), "edges")

# Set extent for plotting (will be updated later once ssn is loaded)
ex <- raster::extent(basin2)

# Load data
timeperiod <- "historical"
riparian.scenario.list <- c("riparian0", "riparian1", "riparian2", "riparian3")
riparian.scenario <- riparian.scenario.list[1]
climate.scenario.list <- c("bcc-csm1-1-m", "CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "HadGEM2-CC365", "HadGEM2-ES365", "IPSL-CM5A-MR", "MIROC5", "NorESM1-M")
climate.scenario <- climate.scenario.list[1]
ssn <- importSSN(paste0("data.in/sno.rbm.ssn"), predpts = 'preds')

# Gather data 
for(timeperiod in c("historical", "future")){
    suffix <- paste0("_s", substr(riparian.scenario,9,9))
    if(riparian.scenario == "riparian0") suffix <- ""
    
    # FLOW
    # Import and collect all raw data
    Q.df <- fncImportQ(paste0(loadDir, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Outflow.Only.", timeperiod, suffix, ".csv"))
    for(climate.scenario in climate.scenario.list[2:length(climate.scenario.list)]){
      Qq <- fncImportQ(paste0(loadDir, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Outflow.Only.", timeperiod, suffix, ".csv"))
    Q.df <- rbind(Q.df, Qq)  
    }
    
    # Get dates into consistent year
    if(timeperiod == "historical") {idx <- which(Q.df[,"Date"] < as.Date("1994-10-01") | Q.df[,"Date"] > as.Date("2005-09-30")); years <- 1995:2005}
    if(timeperiod == "future") {idx <- which(Q.df[,"Date"] < as.Date("2089-10-01") | Q.df[,"Date"] > as.Date("2099-09-30")); years <- 2089:2099}
    Q.df <- Q.df[-idx,]; Q.df<- Q.df[!is.na(Q.df$Date),]
    Q.df$Year <- year(Q.df$Date)
    for(yy in years){
      foo <- Q.df[Q.df[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & Q.df[, "Date"] <= as.Date(paste0(yy, "-09-30")) & !is.na(Q.df[, "Date"]),]
      year(foo[foo[, "Year"] == yy - 1, "Date"]) <- 1900
      year(foo[foo[, "Year"] == yy, "Date"]) <- 1901
      Q.df[Q.df[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & Q.df[, "Date"] <= as.Date(paste0(yy, "-09-30")) & !is.na(Q.df[, "Date"]),] <- foo
    }
    
    # Add season column & summarize by season 
    Q.df$Season <- NA
    Q.df$Season[Q.df$Date >= as.Date("1900-10-01") & Q.df$Date <= as.Date("1900-11-30") | Q.df$Date >= as.Date("1901-09-01")] <- "fall"
    Q.df$Season[Q.df$Date >= as.Date("1900-12-01") & Q.df$Date <= as.Date("1901-02-28")] <- "winter"
    Q.df$Season[Q.df$Date >= as.Date("1901-03-01") & Q.df$Date <= as.Date("1901-05-31")] <- "spring"
    Q.df$Season[Q.df$Date >= as.Date("1901-06-01") & Q.df$Date <= as.Date("1901-08-31")] <- "summer"
    
    Q.seasons <- Q.df %>% 
      group_by(Season) %>%
      summarise_if(is.numeric, median, na.rm = TRUE) #older dplyr
      #summarise(across(where(is.numeric), median, na.rm = TRUE)) #newer dplyr
    Q.seasons <- Q.seasons[!is.na(Q.seasons$Season), -2] #remove NAs and Time column
    write.csv(Q.seasons, file = paste0("data.in/rbm.data/Q.seasonal.medians.", timeperiod, ".csv"))
    rm(Q.df, Qq, foo)
    
    assign(paste0("Q.seasons.", timeperiod), Q.seasons); rm(Q.seasons)
    
    # TEMPERATURE
    # Import and collect all raw data
    T.df <- fncImportWT(paste0(loadDir, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Tw_output.", timeperiod, suffix, ".csv"))
    for(climate.scenario in climate.scenario.list[2:length(climate.scenario.list)]){
      Tt <- fncImportQ(paste0(loadDir, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Tw_output.", timeperiod, suffix, ".csv"))
      T.df <- rbind(T.df, Tt)  
    }
    
    # Get dates into consistent year
    if(timeperiod == "historical") {idx <- which(T.df[,"Date"] < as.Date("1994-10-01") | T.df[,"Date"] > as.Date("2005-09-30")); years <- 1995:2005}
    if(timeperiod == "future") {idx <- which(T.df[,"Date"] < as.Date("2089-10-01") | T.df[,"Date"] > as.Date("2099-09-30")); years <- 2089:2099}
    T.df <- T.df[-idx,]; T.df<- T.df[!is.na(T.df$Date),]
    T.df$Year <- year(T.df$Date)
    for(yy in years){
      foo <- T.df[T.df[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & T.df[, "Date"] <= as.Date(paste0(yy, "-09-30")) & !is.na(T.df[, "Date"]),]
      year(foo[foo[, "Year"] == yy - 1, "Date"]) <- 1900
      year(foo[foo[, "Year"] == yy, "Date"]) <- 1901
      T.df[T.df[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & T.df[, "Date"] <= as.Date(paste0(yy, "-09-30")) & !is.na(T.df[, "Date"]),] <- foo
    }

    # Add season column & summarize by season 
    T.df$Season <- NA
    T.df$Season[T.df$Date >= as.Date("1900-10-01") & T.df$Date <= as.Date("1900-11-30") | T.df$Date >= as.Date("1901-09-01")] <- "fall"
    T.df$Season[T.df$Date >= as.Date("1900-12-01") & T.df$Date <= as.Date("1901-02-28")] <- "winter"
    T.df$Season[T.df$Date >= as.Date("1901-03-01") & T.df$Date <= as.Date("1901-05-31")] <- "spring"
    T.df$Season[T.df$Date >= as.Date("1901-06-01") & T.df$Date <= as.Date("1901-08-31")] <- "summer"
    
    T.seasons <- T.df %>% 
      group_by(Season) %>%
      summarise_if(is.numeric, median, na.rm = TRUE) #older dplyr
      #summarise(across(where(is.numeric), median, na.rm = TRUE)) #newer dplyr
      T.seasons <- T.seasons[!is.na(T.seasons$Season), -2] #remove NAs and Time column
      write.csv(T.seasons, file = paste0("data.in/rbm.data/T.seasonal.medians.", timeperiod, ".csv"))
      rm(T.df, Tt, foo)
    
    assign(paste0("T.seasons.", timeperiod), T.seasons); rm(T.seasons)
}

# Change (future minus historical for T and future/historical ration for Q)
Q.seasons <- Q.seasons.future[2:ncol(Q.seasons.future)] - Q.seasons.historical[2:ncol(Q.seasons.historical)]
rownames(Q.seasons) <- Q.seasons.historical$Season
T.seasons <- T.seasons.future[2:ncol(T.seasons.future)] - T.seasons.historical[2:ncol(T.seasons.historical)]
rownames(T.seasons) <- T.seasons.historical$Season
Q.seasons <- Q.seasons[,-ncol(Q.seasons)]; T.seasons <- T.seasons[,-ncol(T.seasons)] #remove Year column

T.seasons <- T.seasons[c(1,4,2,3),] #reorder seasons for plotting
Q.seasons <- Q.seasons[c(1,4,2,3),] #reorder seasons for plotting

# water temperature
cb <- brewer.pal(9, "Reds")
theseq <- quantile(t(T.seasons), probs = c(0, 0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
left <- theseq[1:8]
rght <- theseq[2:9]

#flow
cb2 <- brewer.pal(9, "RdBu")
theseq2 <- quantile(t(Q.seasons), probs = c(0, 0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))
left2 <- theseq2[1:8]
rght2 <- theseq2[2:9]

# Make maps
png(paste0(imageDir, "/Figure5_DHSVM-RBM_Maps.png"), width = 7.5, height = 14, units = "in", res = 300)
par(mfrow = c(5, 2), mar = c(1, 0, 4, 0), oma = rep(0.5,4), las = 1)

  seasons <- c("Fall", "Winter", "Spring", "Summer", "Blank")
  Qletters <- c("(a)", "(b)", "(c)", "(d)")
  Tletters <- c("(e)", "(f)", "(g)", "(h)")
  
for(ss in 1:length(seasons)){
  season <- seasons[ss]
  
  for(var in c("Flow", "Temperature")){
    if(var == "Flow") letter <- Qletters[ss]
    if(var == "Temperature") letter <- Tletters[ss]
    
  if(ss != 5){

  # plot background
  plot(basin2, col = "gray40", border = 1, lwd = 2, main = "", reset = FALSE)
  #plot(streams[,"afvArea"], col = 1, lwd = afvArea, type = 'l', add = TRUE)
  
  if(var == "Flow"){
    # Flow:
    if("Q" %in% colnames(ssn@data)){ssn = fncUnloadWQ("Q",ssn)} # unload if it's already loaded
    dat <- cbind("rid" = colnames(Q.seasons), "Q" = t(Q.seasons[ss,]))
    dat[,"rid"] <- as.numeric(gsub("X", "", dat[,"rid"])); colnames(dat)[2] <- "Q"
    ssn@data <- merge(ssn@data, dat, by.x = "rid", by.y = "rid", all.x = T)
    ssn@data$Q <- as.numeric(ssn@data$Q) #just in case, sometimes they show up as characters
    
    for(n in 1:length(cb2)) {ssn@data$Q.color[ssn@data$Q >= left2[n] & ssn@data$Q <= rght2[n]]= n}
    
    for (i in 1:length(ssn@lines)) {
      for (j in 1:length(ssn@lines[[i]])) {
        lines(ssn@lines[[i]]@Lines[[j]]@coords, col = cb2[ssn@data[i,"Q.color"]], lwd = 8 * (ssn@data[i, "afvArea"] + 0.1))
      }
    }
  }
  
  if(var == "Temperature"){
    # Water temperature
    if("WT" %in% colnames(ssn@data)){ssn = fncUnloadWQ("WT",ssn)} # unload if it's already loaded
    dat <- cbind("rid" = colnames(T.seasons), "WT" = t(T.seasons[ss,]))
    dat[,"rid"] <- as.numeric(gsub("X", "", dat[,"rid"])); colnames(dat)[2] <- "WT"
    ssn@data <- merge(ssn@data, dat, by.x = "rid", by.y = "rid", all.x = T)
    ssn@data$WT <- as.numeric(ssn@data$WT) #just in case, sometimes they show up as characters
    
    for(n in 1:length(cb)) {ssn@data$WT.color[ssn@data$WT >= left[n] & ssn@data$WT <= rght[n]]= n}
    
    for (i in 1:length(ssn@lines)) {
      for (j in 1:length(ssn@lines[[i]])) {
        lines(ssn@lines[[i]]@Lines[[j]]@coords, col = cb[ssn@data[i,"WT.color"]], lwd = 8 * (ssn@data[i, "afvArea"] + 0.1))
      }
    }
  }
  
  # # Add scale bar
  # rect(xleft = ex[1] + 5000, ybottom = ex[3] + 3000, xright = ex[1] + 7500, ytop = ex[3] + 3500)
  # rect(xleft = ex[1] + 7500, ybottom = ex[3] + 3000, xright = ex[1] + 10000, ytop= ex[3] + 3500, col = 1)
  # rect(xleft = ex[1] + 10000, ybottom = ex[3] + 3000, xright = ex[1] + 12500, ytop = ex[3] + 3500)
  # rect(xleft = ex[1] + 12500, ybottom = ex[3] + 3000, xright = ex[1] + 15000, ytop = ex[3] + 3500, col = 1)
  # segments(x0 = ex[1] + 5000, y0 = ex[3] + 3000, x1 = ex[1] + 5000, y1 = ex[3] + 2500)
  # segments(x0 = ex[1] + 10000, y0=ex[3] + 3000, x1 = ex[1] + 10000, y1 = ex[3] + 2500)
  # segments(x0 = ex[1] + 15000, y0 = ex[3] + 3000, x1 = ex[1] + 15000, y1 = ex[3] + 2500)
  # text(x = ex[1] + 5000, y = ex[3] + 1500, "0", cex = 0.8)
  # text(x = ex[1] + 10000, y = ex[3] + 1500, "5", cex = 0.8)
  # text(x = ex[1] + 15000, y = ex[3] + 1500, "10", cex = 0.8)
  # text(x = ex[1] + 18000, y = ex[3] + 1500, "km", cex = 0.8) 
  # 
  # # Add north arrow
  # arrows(ex[1] + 2000, ex[3] + 2300, ex[1] + 2000, ex[3] + 4000, length = 0.1, lwd = 5)
  # text(ex[1] + 2000, ex[3] + 1000, "N")
  
  # Add title
  #mtext(season, side = 3, line = 0, cex = 1)
  text(x = ex[1] + 5000, y = ex[3] + 3500, paste(letter, season), cex = 1.5)
  }
    
  if(ss == 5){
    # Add flow legend
    if(var == "Flow"){
      plot(1:10, 1:10, type = 'n', axes = F, xlab = 'n', ylab = 'n')
      leglabs = paste(round(left2, 2), "to", round(rght2, 2))
      legend("center", legend = leglabs, title = expression(Delta~"Flow (m s"^-1*")"), bty = "n", pch = 19, col = cb2, cex = 1.5)
    }
    # Add temperature legend
    if(var == "Temperature"){
      plot(1:10, 1:10, type = 'n', axes = F, xlab = 'n', ylab = 'n')
      leglabs = paste(round(left, 1), "to", round(rght , 1))
      legend("center", legend = leglabs, title = expression(Delta~"Water temperature ("*degree*C*")"), bty = "n", pch = 19, col = cb, cex = 1.5)
    }
  }
  }
}

dev.off()
