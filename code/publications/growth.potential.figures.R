## Growth potential figure for Snoqualmie fish for Yan et al. manuscript and growth potential figure for report
# Last updated 23 Jan 2021 #23 Oct 2020

# Setup ####

# load packages
library(RColorBrewer)
library(sf)
library(tidyverse)

# seg directory where data are stored
load.dir <- "data.out/growth.potential" #"/Volumes/BluPassport/SnoIBM/data.out_growth.potential"

# would you like to save these figures?
save.figures <- TRUE
if (save.figures) {
  # specify directory for plot output
  plot.directory <- paste0(getwd(), "/plots")
  if (!dir.exists(plot.directory)) {dir.create(plot.directory)}
}

#Load reaches for summary
load("data.in/sno.rbm.ssn/dnsegs.RData")
reaches <- dnsegs[488][[1]]
species.info <- read.csv("data.in/sno.rbm.ssn/DHSVM_streams_AnadromousUse.csv")
species.info <- species.info[, c("arcid", "grid_code", "segorder", "SELEV", "EELEV", "CHIN", "Chin_Use", "COHO", "Coho_Use", "STHD", "Sthd_Use", "PINK", "Pink_Use", "LMB", "WAC173201A")]
ssn <- SSN::importSSN(paste0("data.in/sno.rbm.ssn"), predpts = 'preds')
edges <- ssn@obspoints@SSNPoints[[1]]@point.data[, c("arcid", "rid")]
species.info <- merge(species.info, edges, by = "arcid", all.x = T)
#change protection codes to degrees C thresholds for 7-DADMax
species.info$WAC173201A[species.info$WAC173201A == 1] <- 17.5 
species.info$WAC173201A[species.info$WAC173201A == 2] <- 16 
species.info$WAC173201A[species.info$WAC173201A == 3] <- 12 
species.info$ANAD <- NA
species.info$ANAD[species.info$CHIN == 1] <- 1
species.info$ANAD[species.info$COHO == 1] <- 1
species.info$ANAD[species.info$STHD == 1] <- 1
species.info$ANAD[species.info$PINK == 1] <- 1

nonanad <- paste0("X", (species.info$arcid[is.na(species.info$ANAD)] - 1))
anad <- sites[!sites %in% nonanad]

# load data froom scenarios to be compared
species.list <- c("chinook", "steelhead", "coho", "pink", "lmb", "rainbow")
species <- "chinook"
iter <- 1
scenario.list <- c("bcc-csm1-1-m", "CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "HadGEM2-CC365", "HadGEM2-ES365", "IPSL-CM5A-MR", "MIROC5", "NorESM1-M")
#scenario.list <- c("current_climate_riparian_0", "current_climate_riparian_1", "current_climate_riparian_2", "current_climate_riparian_3")
years.h <- 1995:2005
years.f <- 2089:2099

# function to extract the necessary data from each array
fncIsolateBySeason <- function(array, field, scenario, year, season, start, stop){
  dat <- salmon.array[,field, start:stop]
  dat <- apply(dat, 1, sum)
  dat <- as.data.frame(dat, drop = F)
  dat <- cbind(scenario, season, year, dat)
  dat$rid <- salmon.array[,"seg", 1]
  colnames(dat) <- c("scenario", "season", "year", "growth.potential", "rid")
  dat <- dat[!is.na(dat[,"growth.potential"]),]
  return(dat)
}

# Get data collated for historical and future for each species (Run first time only; takes a long time) ####
for(species in species.list){
for(period in c("h", "f")){
  if(period == "h"){years <- years.h}
  if(period == "f"){years <- years.f}
  scen.dat <- NULL
  for(scenario in scenario.list){
    for(yy in years){
      outdir <- paste0("GrwPot.", species, ".", scenario, ".", yy)
      load(file = paste0(load.dir, "/", outdir, "/salmon.array.", yy, ".", iter, ".RData"))
      
      # total growth potential for winter at each location
      aut <- fncIsolateBySeason(salmon.array, "growth", scenario, yy, "aut", 1, (90*2)) #SON
      win <- fncIsolateBySeason(salmon.array, "growth", scenario, yy, "win", (90*2), (181*2)) #DJF
      spr <- fncIsolateBySeason(salmon.array, "growth", scenario, yy, "spr", (181*2), (273*2)) #MAM
      sum <- fncIsolateBySeason(salmon.array, "growth", scenario, yy, "sum", (273*2), 730) #JJA
      # combine
      dat <- rbind(aut, win, spr, sum)
      rm(aut, win, spr, sum)
      
      scen.dat <- rbind(scen.dat, dat)
    }
  }
  mainstem <- scen.dat[scen.dat$rid %in% reaches,]
  row.names(mainstem) <- NULL
  
  assign(paste0(species, ".scen.dat.", period), scen.dat)
  assign(paste0(species, ".mainstem.", period), mainstem)
  
  save(scen.dat, file = paste0(load.dir, "/", paste0(species, ".scen.dat.", period), ".RData"))
  save(mainstem, file = paste0(load.dir, "/", paste0(species, ".mainstem.", period), ".RData"))
  
  rm(scen.dat, mainstem)
}
}



# Reload data (skip above step) ####
species.list <- species.list[-which(species.list == "rainbow")] #remove rainbow trout; different spatial extent than other species

# re-load AND append species use and protection information
for(species in species.list){
  for(period in c("h", "f")){
    load(paste0(load.dir, "/", paste0(species, ".scen.dat.", period), ".RData"))
    load(paste0(load.dir, "/", paste0(species, ".mainstem.", period), ".RData"))
    scen.dat <- merge(scen.dat, species.info, by = "rid", all.x = T)
    mainstem <- merge(mainstem, species.info, by = "rid", all.x = T)
    if(species == "chinook") cols2keep <- c("CHIN", "Chin_Use")
    if(species == "coho") cols2keep <- c("COHO", "Coho_Use")
    if(species == "steelhead") cols2keep <- c("STHD", "Sthd_Use")
    if(species == "pink") cols2keep <- c("PINK", "Pink_Use")
    if(species == "lmb") cols2keep <- c("LMB")
    cols2keep <- c("rid", "arcid", "grid_code", "segorder", "scenario", "season", "year", "growth.potential", "WAC173201A", cols2keep)
    scen.dat <- scen.dat[,cols2keep]
    mainstem <- mainstem[,cols2keep]
    if(species == "lmb") {scen.dat <- cbind(scen.dat, "use" = 1); mainstem <- cbind(mainstem, "use" = 1)}
    colnames(scen.dat) <- c(cols2keep[1:9], "presence", "use")
    colnames(mainstem) <- c(cols2keep[1:9], "presence", "use")
    scen.dat <- scen.dat[scen.dat$presence == 1,]
    mainstem <- mainstem[mainstem$presence == 1,]
    cols2keep <- c("rid", "arcid", "segorder", "scenario", "season", "year", "growth.potential", "WAC173201A", "use")
    scen.dat <- scen.dat[,cols2keep]
    mainstem <- mainstem[,cols2keep]
    assign(paste0(species, ".scen.dat.", period), scen.dat)
    assign(paste0(species, ".mainstem.", period), mainstem)
  }
}
rm(scen.dat, mainstem)

species <- "chinook"
mainstem.h <- get(paste0(species, ".mainstem.h"))
mainstem.f <- get(paste0(species, ".mainstem.f"))

tapply(mainstem.h$growth.potential,mainstem.h$season, sum)
tapply(mainstem.f$growth.potential,mainstem.f$season, sum)

# calculate the mean growth potential for each reach and season across the 11 year time series
dat2plot.h <- tapply(mainstem.h$growth.potential, list(mainstem.h$season, mainstem.h$rid), mean) #sum gives same pattern result
dat2plot.f <- tapply(mainstem.f$growth.potential, list(mainstem.f$season, mainstem.f$rid), mean) #sum gives same pattern result
dat2plot.h <- dat2plot.h[c(3,2,4,1),] #re-arrange seasons for plotting
dat2plot.f <- dat2plot.f[c(3,2,4,1),]


# get quantiles & mean for each species (pooled across climate scenarios and years, not broken out by season and reach)
for(species in species.list){
  mainstem.h <- get(paste0(species, ".mainstem.h"))
  mainstem.f <- get(paste0(species, ".mainstem.f"))
  mainstem <- cbind(mainstem.h, mainstem.f[, c("year", "growth.potential")])
  colnames(mainstem)[ncol(mainstem)] <- "growth.potential.f"
  mainstem$growth.diff <- mainstem$growth.potential.f - mainstem$growth.potential
  cat(species, ": ", summary(mainstem$growth.diff), "\n")
}


# Line & quantile plot for Yan et al. manuscript ####

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
c1 <- fncColors4Quantiles()[[1]][c(2,6,7,3)] #orange, green, blue, yellow
c2 <- fncColors4Quantiles()[[2]][c(2,6,7,3)]
c3 <- fncColors4Quantiles()[[3]][c(2,6,7,3)]

fncGetSpecies <- function(species){
  if(species == "chinook") spnm <- "Chinook salmon"
  if(species == "steelhead") spnm <- "Steelhead"
  if(species == "coho") spnm <- "Coho salmon"
  if(species == "pink") spnm <- "Pink salmon"
  if(species == "rainbow") spnm <- "Rainbow trout"
  if(species == "lmb") spnm <- "Largemouth bass"
  return (spnm)
}

# Difference (F - H)
png(paste0(plot.directory, "/growth_potential_diff.png"), width = 7, height = 7, units = "in", res = 300)
  par(mfrow = c(3, 2), mar = c(5, 4, 2, 0.5), oma = c(0, 4, 0, 0), las = 1)
  
  species2plot <- c("chinook", "blank", "coho", "pink", "steelhead", "lmb")
  for(species in species2plot){
    
    if(species == "blank"){
    plot(1:10, 1:10, type = 'n', axes = F, xlab = "", ylab = "")
    legend ("center", legend = c("winter", "spring", "autumn", "summer"), lty = 1, lwd = 4, col = c3[c(3,2,4,1)], bty = 'n', cex = 1.5)
    } else {
    
    species.nm <- fncGetSpecies(species)
    ifelse(species == "pink", i.list <- 2:4, i.list <- 1:4)
    mainstem.h <- get(paste0(species, ".mainstem.h"))
    mainstem.f <- get(paste0(species, ".mainstem.f"))
    mainstem <- cbind(mainstem.h, mainstem.f[, c("year", "growth.potential")])
    colnames(mainstem)[ncol(mainstem)] <- "growth.potential.f"
    mainstem$growth.diff <- mainstem$growth.potential.f - mainstem$growth.potential
    mainstem$growth.diff.pct <- mainstem$growth.diff / mainstem$growth.potential * 100
    
    dat2plot.Md <- tapply(mainstem$growth.diff, list(mainstem$season, mainstem$rid), quantile, probs = 0.5)[c(3,2,4,1),]
    dat2plot.Q1 <- tapply(mainstem$growth.diff, list(mainstem$season, mainstem$rid), quantile, probs = 0.25)[c(3,2,4,1),]
    dat2plot.Q3 <- tapply(mainstem$growth.diff, list(mainstem$season, mainstem$rid), quantile, probs = 0.75)[c(3,2,4,1),]

    #ylm <- c(min(dat2plot.Q1), max(dat2plot.Q3))
    ylm <- c(-4.2, 2)
    
  plot(dat2plot.Md[1,], type = 'n', col = c3[1], ylim = ylm,  ylab = "", xlab = "", axes = F)
  axis(2)
  if(species %in% c("chinook", "coho", "steelhead")) mtext("\u0394 Growth\npotential\n(g/g/d)", side = 2, line = 4.5, adj = 0.5, cex = 0.9)
  
  if(species %in% c("lmb", "steelhead")){
    axis(1, at = seq(1, 31, 5), labels = round(seq(1, 62, length.out = 7),0))
    mtext("Distance upstream (km)", side = 1, line = 3)
  }
  if(!species %in% c("lmb", "steelhead")) axis(1, at = seq(1, 31, 5), labels = NA)
  for(i in i.list){polygon(c(1: 31, rev(1: 31)), c(dat2plot.Q1[i, ], rev(dat2plot.Q3[i, ])), border = NA, col = c2[i])} #Q1/Q3
  for(i in i.list){ lines(dat2plot.Md[i,], col = c3[i])} #Median
  abline(h = 0, lty = 3)
  mtext(species.nm, 3, cex = 1.2)
  }
  }
  
dev.off()

# Hist / Fut
# png(paste0(plot.directory, "/", species, "_growth_potential_h.png"), width = 5, height = 8, units = "in", res = 300)
#   mybreaks <- sort(c(0,quantile(c(dat2plot.h, dat2plot.f), probs = seq(0,1,0.1))))
#   
#   par(mfrow = c(2,1), mar = c(4,3.5,0.5,1), las = 1)
#   plot(dat2plot.h[1,], type = 'l', ylim = range(mybreaks),  ylab = "", xlab = "", xaxt = 'n')
#   for(i in 2:4){ lines(dat2plot.h[i,], col = i)}
#   axis(1, at = seq(1, 31, 5), labels = reaches.inv[seq(1, length(reaches.inv), 5)])
#   
#   legend ("topright", legend = c("aut", "win", "spr", "sum"), lty = 1, col = c(4,3,2,1), bty = 'n')
# dev.off()


# Growth potential maps ####

# Read in basin outline & streams
basin <- read_sf(paste0("data.in/shapefiles"), "Basin_snq2")
# dissolve on area to just get the outline
basin2 <- basin %>% summarise(area = sum(AreaSqKm))
streams <- read_sf("data.in/sno.rbm.ssn", "edges")

# Set extent for plotting (will be updated later once ssn is loaded)
ex <- raster::extent(basin2)

# Read in SSN
ssn <- SSN::importSSN(paste0("data.in/sno.rbm.ssn"), predpts = 'preds')


# Make maps
for(season in c("aut", "win", "spr", "sum")){
png(paste0(plot.directory, paste0("/GrowthPotentialMaps_", season, ".png")), width = 7.5, height = 14, units = "in", res = 300)
par(mfrow = c(3, 2), mar = rep(0, 4), oma = rep(0.5, 4), las = 1)

letters <- c("(a)", "(b)", "(c)", "(d)", "(e)")

for(ss in 1:(length(species.list) + 1)){
  species <- species.list[ss]
  
    
    if(ss != 6){
      
      # plot background
      plot(basin2, col = "gray40", border = 1, lwd = 2, main = "", reset = FALSE)
      #plot(streams[,"afvArea"], col = 1, lwd = afvArea, type = 'l', add = TRUE)
      
      # Get growth potential data for plotting
      # remove these columns if they exist
      idx <- which(colnames(ssn@data)%in% c("aut", "spr", "sum", "win"))
      if(colnam %in% colnames(ssn@data)) ssn@data <- ssn@data[,-idx] 
      # get historical data
      dat.h <- get(paste0(species, ".scen.dat.h"))
      dat.h <- t(tapply(dat.h$growth.potential, list(dat.h$season, dat.h$rid), mean))
      dat.h <- cbind("rid" = as.numeric(row.names(dat.h)), dat.h)
      # get future data
      dat.f <- get(paste0(species, ".scen.dat.f"))
      dat.f <- t(tapply(dat.f$growth.potential, list(dat.f$season, dat.f$rid), mean))
      dat.f <- cbind("rid" = as.numeric(row.names(dat.f)), dat.f)
      # get future minus historical and merge into ssn
      dat <- cbind.data.frame("rid" = dat.h[,"rid"], dat.f[,2:5] - dat.h[,2:5])
      ssn@data <- merge(ssn@data, dat, by.x = "rid", by.y = "rid", all.x = T) #merge growth pot. data into SSN
      
      # colors
      cb <- RColorBrewer::brewer.pal(9, "Purples")
      cb <- c("#fcfafa", cb)
      ddd <- c(dat[,2], dat[,3], dat[,4], dat[,5])
      theseq <- round(quantile(ddd, probs = seq(0, 1, 0.1)), 3)
      left <- theseq[1:(length(theseq) - 1)]
      rght <- theseq[2:length(theseq)]
      
      for(n in 1:length(cb)) {ssn@data$color[ssn@data[,season] >= left[n] & ssn@data[,season] <= rght[n]]= n}
      
      for (i in 1:length(ssn@lines)) {
        for (j in 1:length(ssn@lines[[i]])) {
          lines(ssn@lines[[i]]@Lines[[j]]@coords, col = cb[ssn@data[i,"color"]], lwd = 8 * (ssn@data[i, "afvArea"] + 0.1))
        }
      }
      text(x = ex[1], y = ex[3] + 3500, paste(letters[ss], species), adj = 0, cex = 1.5)
    }
    
    if(ss == 6){
      # Add  legend
        plot(1:10, 1:10, type = 'n', axes = F, xlab = 'n', ylab = 'n')
        leglabs = paste(round(left, 2), "to", round(rght, 3))
        legend("center", legend = leglabs, title = expression(Delta~"Growth potential (g d"^-1* "d"^-1*")"), bty = "n", pch = 19, col = cb, cex = 1.5)
    }
  
}

dev.off()
}


# Not Used: Grid plot of historical and future separately in same plot ####
mycolors <- c("darkorange", "orange", brewer.pal(9, "Purples"))
mybreaks <- quantile(c(dat2plot.h, dat2plot.f), probs = seq(0,1,0.1))
mybreaks <- c(0, mybreaks)

png(paste0(plot.directory, "/", species, "_growth_potential1.png"), width = 5, height = 8, units = "in", res = 300)
par(mfrow = c(2,1), mar = c(4,3.5,0.5,1), las = 1)
image(1:31, 1:4, t(dat2plot.h), axes = F, ylab = "", xlab = "", col = mycolors, breaks = sort(mybreaks))
axis(2, at = 1:4, labels = c("Sum", "Spr", "Win", "Aut"))
reaches.inv <- reaches[length(reaches):1]
#axis(1, at = seq(1, 31, 5), labels = reaches.inv[seq(1, length(reaches.inv), 5)])

image(1:31, 1:4, t(dat2plot.f), axes = F, ylab = "", xlab = "Distance upstream (km)", col = mycolors, breaks = sort(mybreaks))
axis(2, at = 1:4, labels = c("Sum", "Spr", "Win", "Aut"))
#axis(1, at = seq(1, 31, 5), labels = reaches.inv[seq(1, length(reaches.inv), 5)])
dev.off()


# Not Used: Grid plot of future minus historical ####
mainstem <- cbind(mainstem.h, mainstem.f[, c("year", "growth.potential")])
colnames(mainstem)[ncol(mainstem)] <- "growth.potential.f"
mainstem$growth.diff <- mainstem$growth.potential.f - mainstem$growth.potential
quantile(mainstem$growth.diff)
dat2plot <- tapply(mainstem$growth.diff, list(mainstem$season, mainstem$rid), mean)
dat2plot <- dat2plot[c(3,2,4,1),]

mycolors1 <- brewer.pal(9, "Purples")
mycolors2 <- brewer.pal(8, "Oranges")[8:1]
mycolors <- c(mycolors2, mycolors1)
mybreaks1 <- quantile(mainstem$growth.diff[mainstem$growth.diff > 0], probs = seq(0, 1, 0.13))
mybreaks2 <- quantile(mainstem$growth.diff[mainstem$growth.diff < 0], probs = seq(0, 1, 0.13))
mybreaks <- c(mybreaks2, 0, mybreaks1, max(mainstem$growth.diff))

png(paste0(plot.directory, "/", species, "_growth_differential2.png"), width = 5, height = 4, units = "in", res = 300)
par(mar = c(4,3.5,0.5,1), las = 1)
image(1:31, 1:4, t(dat2plot), axes = F, ylab = "", xlab = "", col = mycolors, breaks = sort(mybreaks))
axis(2, at = 1:4, labels = c("Sum", "Spr", "Win", "Aut"))
#reaches.inv <- reaches[length(reaches):1]
#axis(1, at = seq(1, 31, 5), labels = reaches.inv[seq(1, length(reaches.inv), 5)])
dev.off()


# Legend
par(mfrow=c(2,1))
image(cbind(1:9, 1:9), col = mycolors1, axes = F)
image(cbind(1:9, 1:9), col = c(mycolors2, "white"), axes = F)





# NEW Exceedance Summaries ####

library(tidyverse)
#library(sf)
# Depends (packages that must be installed but don't need to be loaded):
# lubridate, ggpubr, SSN, abind, RColorBrewer

source("code/functions.R")

# Set up directory structure
data.in <- "data.in" #"/Volumes/BluPassport/SnoIBM/data.in"
data.out<- "data.out" #"/Volumes/BluPassport/SnoIBM/data.out"
ssn.folder <- "sno.rbm.ssn"
save.figures <- TRUE
plot.directory <- "plots"
if (!dir.exists(plot.directory)) {dir.create(plot.directory)}

riparian.scenario.list <- c("riparian0", "riparian1", "riparian2", "riparian3") #Baseline, Full restoration, Least protective, Partial restoration
climate.scenario.list <- c("bcc-csm1-1-m", "CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "HadGEM2-CC365", "HadGEM2-ES365", "IPSL-CM5A-MR", "MIROC5", "NorESM1-M")
ssn <- SSN::importSSN(paste0("data.in/sno.rbm.ssn"), predpts = 'preds')

# 1. Import and collect all raw temperature data
for(time.period  in c("historical", "future")){
  riparian.scenario <- "riparian0"; suffix <- ""
  climate.scenario <- climate.scenario.list[1]
  
T.df <- fncImportWT(paste0(data.in, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Tw_output.", time.period , suffix, ".csv"))
T.df <- cbind(T.df, "Scenario" = climate.scenario)
for(climate.scenario in climate.scenario.list[2:length(climate.scenario.list)]){
  Tt <- fncImportWT(paste0(data.in, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Tw_output.", time.period , suffix, ".csv"))
  Tt <- cbind(Tt, "Scenario" = climate.scenario)
  T.df <- rbind(T.df, Tt)  
}

# Get dates into consistent year
if(time.period  == "historical") {idx <- which(T.df[,"Date"] < as.Date("1994-10-01") | T.df[,"Date"] > as.Date("2005-09-30")); years <- 1995:2005}
if(time.period  == "future") {idx <- which(T.df[,"Date"] < as.Date("2088-10-01") | T.df[,"Date"] > as.Date("2099-09-30")); years <- 2089:2099}
T.df <- T.df[-idx,]; T.df<- T.df[!is.na(T.df$Date),]
T.df$Year <- lubridate::year(T.df$Date)
T.df$WY <- NA
for(yy in years){
  foo <- T.df[T.df[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & T.df[, "Date"] <= as.Date(paste0(yy, "-09-30")) & !is.na(T.df[, "Date"]),]
  foo$WY <- yy
  lubridate::year(foo[foo[, "Year"] == yy - 1, "Date"]) <- 1900
  lubridate::year(foo[foo[, "Year"] == yy, "Date"]) <- 1901
  T.df[T.df[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & T.df[, "Date"] <= as.Date(paste0(yy, "-09-30")) & !is.na(T.df[, "Date"]),] <- foo
}
sites <- colnames(T.df)[grep("X",colnames(T.df))]
T.df <- T.df[,c("Date", "Time", "Year", "WY", "Scenario", sites)]
assign(paste0("T.df.", time.period ), T.df)
}

# 2. Calculate 1-DMax & 7-DADMax
for(time.period  in c("historical", "future")){
  T.df <- get(paste0("T.df.", time.period ))
# Calculate 1-DMax
  # This step takes a REALLY LONG time to run:
themetric <- "max"
T.1DMax <- T.df %>% 
  group_by(Date, WY, Scenario) %>%
  summarise_if(is.numeric, themetric, na.rm = TRUE) #older dplyr
#summarise(across(where(is.numeric), median, na.rm = TRUE)) #newer dplyr
# there are 30 NAs (10 scenarios, 3 years) for Feb 29 in leap years (1996, 2000, 2004) because when translated to the year 1901 is not a date

# Add season column
T.1DMax$Season <- NA
T.1DMax$Season[T.1DMax$Date >= as.Date("1900-10-01") & T.1DMax$Date <= as.Date("1900-11-30") | T.1DMax$Date >= as.Date("1901-09-01")] <- "fall"
T.1DMax$Season[T.1DMax$Date >= as.Date("1900-12-01") & T.1DMax$Date <= as.Date("1901-02-28")] <- "winter"
T.1DMax$Season[T.1DMax$Date >= as.Date("1901-03-01") & T.1DMax$Date <= as.Date("1901-05-31")] <- "spring"
T.1DMax$Season[T.1DMax$Date >= as.Date("1901-06-01") & T.1DMax$Date <= as.Date("1901-08-31")] <- "summer"

T.1DMax <- T.1DMax[,c("Date", "Time", "Year", "WY", "Scenario", "Season", sites)]
assign(paste0("T.1DMax.", time.period ), T.1DMax)
write.csv(T.1DMax, file = paste0("data.in/rbm.data/T.1DMax.", time.period , ".csv"))
#T.1DMax <- read.csv(paste0("data.in/rbm.data/T.1DMax.", time.period , ".csv"), header = T)[,-1]
#T.1DMax$Date <- as.Date(T.1DMax$Date)

# Calculate 7-DADMax
T.7DADMax <- T.1DMax[,c("Date", "Year", "WY", "Scenario", "Season")]
T.7DADMax <- cbind.data.frame(T.7DADMax, matrix(NA, dim(T.1DMax)[1], (dim(T.1DMax)[2] - 6)))
colnames(T.7DADMax)[6:ncol(T.7DADMax)] <- sites
if(time.period  == "historical"){years <- 1995:2005}
if(time.period  == "future"){years <- 2089:2099}
for(yy in years){
  for(ss in climate.scenario.list){
    for(ii in sites){
      T.7DADMax[T.7DADMax$WY == yy & T.7DADMax$Scenario == ss, ii] <- fncMvgAvg(c(T.1DMax[T.1DMax$WY == yy & T.1DMax$Scenario == ss, ii])[[1]], n = 7, centered = T)
    }
  }
}
assign(paste0("T.7DADMax.", time.period ), T.7DADMax)
write.csv(T.7DADMax, file = paste0("data.in/rbm.data/T.7DADMax.", time.period , ".csv"))
#T.7DADMax <- read.csv(paste0("data.in/rbm.data/T.7DADMax.", time.period , ".csv"), header = T)[,-1]
#T.7DADMax$Date <- as.Date(T.7DADMax$Date)
rm(T.df, Tt, foo)
}

# 3. Compare 7-DADMax to state standards to get "Exceedances"
Standards <- T.7DADMax[,c("Date", "Year", "WY", "Season", "Scenario")]
std <- unique(species.info[, c("arcid", "WAC173201A", "ANAD")])
idx <- which(std$arcid %in% c(18,21,22,40,215,281)) #These need to be removed - they are isolated segments!
std <- std[-idx,]
std$WAC173201A.Anad <- std$WAC173201A
std$WAC173201A.Anad[is.na(std$ANAD)] <- std$WAC173201A.Anad[is.na(std$ANAD)] <- NA
row.names(std) <- NULL
std1 <- t(std[, c("WAC173201A")]); colnames(std1) <- sites
std2 <- t(std[, c("WAC173201A.Anad")]); colnames(std2) <- sites
Exceedance <- cbind.data.frame(Standards, sweep(T.7DADMax[,6:ncol(T.7DADMax)], 2, std1, FUN = "-"))
Exceed.Anad <- cbind.data.frame(Standards, sweep(T.7DADMax[,6:ncol(T.7DADMax)], 2, std2, FUN = "-"))

# Overwrite exceedance values during non-core summer habitat dates (from 16 to 17.5 C)
thesites <- paste0("X", species.info$arcid[species.info$WAC173201A == 16])
thesites <- unique(thesites[thesites %in% sites])
whichrows <- which(Exceedance$Date > as.Date("1900-10-01") & Exceedance$Date < as.Date("1901-06-15") | Exceedance$Date > as.Date("1901-09-15") & Exceedance$Date < as.Date("1901-09-30"))
tmp <- Exceedance[whichrows, thesites] - 1.5
Exceedance[whichrows, thesites] <- tmp

# and for anadromous
thesites <- thesites[thesites %in% anad]
whichrows <- which(Exceed.Anad$Date > as.Date("1900-10-01") & Exceed.Anad$Date < as.Date("1901-06-15") | Exceed.Anad$Date > as.Date("1901-09-15") & Exceed.Anad$Date < as.Date("1901-09-30"))
tmp <- Exceed.Anad[whichrows, thesites] - 1.5
Exceed.Anad[whichrows, thesites] <- tmp

write.csv(Exceedance, file = paste0("data.in/rbm.data/Exceedance.", time.period , ".csv"))
write.csv(Exceed.Anad, file = paste0("data.in/rbm.data/Exceed.Anad.", time.period , ".csv"))
#Exceedance <- read.csv(paste0("data.in/rbm.data/Exceedance.", time.period , ".csv"), header = T)[,-1]
#Exceed.Anad <- read.csv(paste0("data.in/rbm.data/Exceed.Anad.", time.period , ".csv"), header = T)[,-1]


# 4. Collapse to date (rows) by sites (columns), mean over climate scenarios and years
MnEx <- Exceedance %>% 
  group_by(Date) %>%
  summarise_if(is.numeric, "mean", na.rm = TRUE) #older dplyr
MnEx <- MnEx[,-which(colnames(MnEx) %in% c("Year", "WY"))]
MnEx <- data.frame(MnEx)
write.csv(MnEx, file = paste0("data.in/rbm.data/MnEx.", time.period, ".csv"))
#MnEx <- read.csv(paste0("data.in/rbm.data/MnEx.", time.period, ".csv"), header = T)[,-1]
#MnEx$Date <- as.Date(MnEx$Date)

# Anadromous only
MnExAn <- MnEx
MnExAn <- MnExAn[,-which(colnames(MnExAn) %in% nonanad)]
#MnExAn <- Exceed.Anad %>% 
#  group_by(Date) %>%
#  summarise_if(is.numeric, "mean", na.rm = TRUE)
#MnExAn <- MnExAn[,-which(colnames(MnExAn) %in% c("Year", "WY"))]
#MnExAn <- data.frame(MnExAn)
write.csv(MnExAn, file = paste0("data.in/rbm.data/MnExAn.", time.period, ".csv"))
#MnExAn <- read.csv(paste0("data.in/rbm.data/MnExAn.", time.period, ".csv"), header = T)[,-1]
#MnExAn$Date <- as.Date(MnExAn$Date)

# Make temporal plot of exceedance versus month
png(paste0("plots/AnnualExceedance.", time.period, ".png"), width = 6, height = 6, units = "in", res = 300)
par(las = 1)
plot(MnEx$Date, MnEx$X0, type = 'n', ylim = c(-15, 23), ylab = expression("7-DADMax minus State standard ("*degree*C*")"), xlab = "")
for(i in sites){points(MnEx$Date, MnEx[,i], pch = 19, cex = 0.2, col = "darkgray")} # all reaches
for(i in anad){points(MnExAn$Date, MnExAn[,i], pch = 19, cex = 0.2, col = "gray20")} # anadromous reaches
abline(h = 0, lty = 2)
abline(v = MnExAn$Date[MnExAn$Date == as.Date("1901-06-15")], lty = 2)
abline(v = MnExAn$Date[MnExAn$Date == as.Date("1901-09-15")], lty = 2)
legend("topleft", legend = c("All reaches", "Anadromous accessible"), pch = 19, col = c("darkgray", "gray20"), bty = 'n')
dev.off()


# 5. Re-arrange to sites (rows) by date (columns)
Exceedance <- Exceedance[!is.na(Exceedance$Date),]
Exceedance <- Exceedance[order(Exceedance$Scenario, Exceedance$WY, Exceedance$Date),]

SpatialExc <- NULL
for(iter in 1:(10 * 11)){#one scenario, one year at a time
  if(iter == 1) therow <- 1 else therow <- therow + 365
    # first, transpose the sites by dates portion of the matrix
    tExceedance <- t(Exceedance[therow:(therow + 364), sites])
    colnames(tExceedance) <- paste0("D", 1:365)
    
    # next, create identifier columns of appropriate length
    identifiers <- cbind("Site" = sites, Exceedance[therow, c("Year", "WY", "Scenario")]) #ignore the warning, this works
    
    # then, attach appropriate identifier columns to transposed data matrix
    combined <- cbind.data.frame(identifiers, tExceedance)
    
    # finally, append this set to the previous set (scenario/year)
    SpatialExc <- rbind.data.frame(SpatialExc, combined)
}
# attach 'segorder' field
segs <- species.info$segorder; names(segs) <- paste0("X", (species.info$arcid - 1))
SpatialExc$segorder <- fncGetValue(mykey = SpatialExc$Site, mylookupvector = segs)
segs2 <- species.info$SELEV; names(segs2) <- paste0("X", (species.info$arcid - 1))
#segs2 <- species.info$EELEV; names(segs2) <- paste0("X", (species.info$arcid - 1))
SpatialExc$elev <- fncGetValue(mykey = SpatialExc$Site, mylookupvector = segs2)
SpatialExc <- SpatialExc[, c("Site", "segorder", "elev", "Year", "WY", "Scenario", paste0("D", 1:365))]

write.csv(SpatialExc, file = paste0("data.in/rbm.data/SpatialExc.", time.period, ".csv"))
#SpatialExc <- read.csv(paste0("data.in/rbm.data/SpatialExc.", time.period, ".csv"), header = T)[,-1]

# by segorder
MnExReach <- SpatialExc %>% 
  group_by(segorder) %>%
  summarise_if(is.numeric, "mean", na.rm = TRUE)
MnExReach <- MnExReach[,-which(colnames(MnExReach) %in% c("Year", "WY"))]
MnExReach <- data.frame(MnExReach)
write.csv(MnExReach, file = paste0("data.in/rbm.data/MnExReach.", time.period, ".csv"))

# by elev
MnExReach2 <- SpatialExc %>% 
  group_by(elev) %>%
  summarise_if(is.numeric, "mean", na.rm = TRUE)
MnExReach2 <- MnExReach2[,-which(colnames(MnExReach2) %in% c("Year", "WY"))]
MnExReach2 <- data.frame(MnExReach2)
write.csv(MnExReach2, file = paste0("data.in/rbm.data/MnExReach2.", time.period, ".csv"))
#MnExReach2 <- read.csv(paste0("data.in/rbm.data/MnExReach2.", time.period, ".csv"), header = T)[,-1]

# Anadromous reaches only
SpatialExcAn <- SpatialExc[SpatialExc$Site %in% anad,]
write.csv(SpatialExcAn, file = paste0("data.in/rbm.data/SpatialExcAn.", time.period, ".csv"))
#SpatialExcAn <- read.csv(paste0("data.in/rbm.data/SpatialExcAn.", time.period, ".csv"), header = T)[,-1]
MnExRcAn <- SpatialExcAn %>% 
  group_by(segorder) %>%
  summarise_if(is.numeric, "mean", na.rm = TRUE)
MnExRcAn <- MnExRcAn[,-which(colnames(MnExRcAn) %in% c("Year", "WY", "elev"))]
MnExRcAn <- data.frame(MnExRcAn)
write.csv(MnExRcAn, file = paste0("data.in/rbm.data/MnExRcAn.", time.period, ".csv"))

# by elev
SpatialExcAn2 <- SpatialExc[SpatialExc$Site %in% anad,]
write.csv(SpatialExcAn2, file = paste0("data.in/rbm.data/SpatialExcAn2.", time.period, ".csv"))
MnExRcAn2 <- SpatialExcAn2 %>% 
  group_by(round(elev,2)) %>%
  summarise_if(is.numeric, "mean", na.rm = TRUE)
MnExRcAn2 <- MnExRcAn2[,-which(colnames(MnExRcAn2) %in% c("Year", "WY", "segorder"))]
MnExRcAn2 <- data.frame(MnExRcAn2)
write.csv(MnExRcAn2, file = paste0("data.in/rbm.data/MnExRcAn2.", time.period, ".csv"))
#MnExRcAn2 <- read.csv(paste0("data.in/rbm.data/MnExRcAn2.", time.period, ".csv"), header = T)[,-1]

# denote which columns (dates) fall into which seasons; first column is 1 Oct, last is 30 Sep
fall.cols <- paste0("D", 1:(31 + 30 + 31))
win.cols <- paste0("D", (31 + 30 + 31 + 1):(31 + 30 + 31 + 1 + 31 + 28 + 31))
spr.cols <- paste0("D", (31 + 30 + 31 + 1 + 31 + 28 + 31 + 1) : (31 + 30 + 31 + 1 + 31 + 28 + 31 + 30 + 31 + 30))
sum.cols <- paste0("D", (31 + 30 + 31 + 1 + 31 + 28 + 31 + 30 + 31 + 30 + 1): (31 + 30 + 31 + 1 + 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 - 1))

# # Make spatial plot of exceedance versus segorder
# png(paste0("plots/SpatialExceedance.", time.period, ".png"), width = 6, height = 6, units = "in", res = 300)
# par(las = 1)
# plot(MnExReach$segorder, MnExReach[,5], type = 'n', ylim = c(-20, 30), ylab = expression("7-DADMax minus State standard ("*degree*C*")"), xlab = "")
# for(i in 2:ncol(MnExReach)){points(MnExReach$segorder, MnExReach[,i], pch = 19, cex = 0.2, col = "darkgray")} # all reaches
# for(i in 2:ncol(MnExRcAn)){points(MnExRcAn$segorder, MnExRcAn[,i], pch = 19, cex = 0.2, col = "gray20")} # anadromous reaches
# abline(h = 0, lty = 2)
# legend("topleft", legend = c("All reaches", "Anadromous accessible"), pch = 19, col = c("darkgray", "gray20"), bty = 'n')
# dev.off()
# 
# # Make spatial plot of exceedance versus elev, anadromous
# png(paste0("plots/SpatialExceedance2.", time.period, ".png"), width = 6, height = 6, units = "in", res = 300)
# par(las = 1)
# plot(MnExReach2$elev, MnExReach2[,5], type = 'n', ylim = c(-20, 30), ylab = expression("7-DADMax minus State standard ("*degree*C*")"), xlab = "")
# for(i in 3:ncol(MnExReach2)){points(MnExReach2$elev, MnExReach2[,i], pch = 19, cex = 0.2, col = "darkgray")} # all reaches
# for(i in 3:ncol(MnExRcAn2)){points(MnExRcAn2$elev, MnExRcAn2[,i], pch = 19, cex = 0.2, col = "gray20")} # anadromous reaches
# abline(h = 0, lty = 2)
# legend("topleft", legend = c("All reaches", "Anadromous accessible"), pch = 19, col = c("darkgray", "gray20"), bty = 'n')
# dev.off()

mycolors <- c(rgb(209,167,207,150,NULL,255), rgb(196,237,197,150,NULL,255), rgb(252,217,156,150,NULL,255), rgb(176,191,252,150,NULL,255)) #purple, green, orange, blue
#mycolors <- c(rgb(137,101,136,200,NULL,255), rgb(81,198,83,200,NULL,255), rgb(234,173,68,200,NULL,255), rgb(128,153,252,200,NULL,255))

# Make spatial plot of exceedance versus elev, colored by season, with anadromous reaches indicated in black at bottom
png(paste0("plots/SpatialExceedance.", time.period, ".png"), width = 6, height = 6, units = "in", res = 300)
par(las = 1)
plot(MnExReach2$elev, MnExReach2[,5], type = 'n', ylim = c(-20, 30), ylab = expression("7-DADMax minus State standard ("*degree*C*")"), xlab = "Elevation (m)")
for(i in sum.cols){points(MnExReach2$elev, MnExReach2[,i], cex = 0.2, col = mycolors[1])} # purple
for(i in spr.cols){points(MnExReach2$elev, MnExReach2[,i], cex = 0.2, col = mycolors[2])} # green
for(i in fall.cols){points(MnExReach2$elev, MnExReach2[,i], cex = 0.2, col = mycolors[3])} # orange
for(i in win.cols){points(MnExReach2$elev, MnExReach2[,i], cex = 0.2, col = mycolors[4])} # blue
for(i in 3:ncol(MnExRcAn2)){points(MnExRcAn2$elev, rep(-20, nrow(MnExRcAn2)), pch = 19, cex = 0.5, col = "gray20")} # anadromous reaches
abline(h = 0, lty = 2)
#legend("topright", legend = c("Summer", "Spring", "Fall", "Winter", "Anadromous"), pch = 19, col = c(mycolors, "gray20"), bty = 'n')
dev.off()


## State Standards Map ####

# Read in basin outline & streams
basin <- read_sf(paste0("data.in/shapefiles"), "Basin_snq2")
basin2 <- basin %>% summarise(area = sum(AreaSqKm)) # dissolve on area to just get the outline
ex <- raster::extent(basin2)
streams <- read_sf("data.in/shapefiles", "DHSVM-RBM_anadr")
idx <- which(streams$downarc == -1)[2:7] #isolated reaches that need to be removed
streams <- streams[-idx,]


png("plots/Map_StateStandards.png", width = 7.5, height = 7, units = "in", res = 300)
par(mar = c(1, 1, 1, 0), oma = rep(0.5, 4), las = 1)

# plot background
plot(basin2, col = "gray40", border = 1, lwd = 2, main = "", reset = FALSE)

# plot colored stream lines
thecolors <- c(rgb(244,155,2,255,NULL,255), rgb(206,193,8,255,NULL,255), rgb(128,153,252,200,NULL,255)) #orange, yellow, blue
plot(streams, add = T, lwd = (streams$segorder * 0.1) + 0.5, col = thecolors[streams$WAC173201A])

legend("topright", legend = c("Salmonid spawning, rearing, and migration", "Core summer salmonid habitat", "Char spawning and rearing"), lwd = 3, col = thecolors, bty = 'n')
  
dev.off()


# Statistics for paper
time.period <- "historical" #"future"
MnEx <- read.csv(paste0("data.in/rbm.data/MnEx.", time.period, ".csv"), header = T)[,-1]; MnEx$Date <- as.Date(MnEx$Date)
MnExAn <- read.csv(paste0("data.in/rbm.data/MnExAn.", time.period, ".csv"), header = T)[,-1]; MnExAn$Date <- as.Date(MnExAn$Date)
MnEx <- MnEx[,2:ncol(MnEx)]
MnEx[MnEx <= 0] <- 0; MnEx[MnEx > 0] <- 1
MnExAn <- MnExAn[,2:ncol(MnExAn)]
MnExAn[MnExAn <= 0] <- 0; MnExAn[MnExAn > 0] <- 1

# proportion of days in year exceeding standard, for each reach in 'all reaches' (n=797)
(mnex.days <- apply(MnEx[,2:ncol(MnEx)], 2, sum, na.rm = T) /365)
# averaged across 797 reaches
mean(mnex.days) 
# proportion of days in year exceeding standard, for each reach in 'anadromous accessible reaches' (n=200)
(mnexan.days <- apply(MnExAn[,2:ncol(MnExAn)], 2, sum, na.rm = T) /365)
# averaged across 200 reaches
mean(mnexan.days) 

# proportion of 'all reaches' exceeding standard, for each day (n=365)
(mnex.rchs <- apply(MnEx[,2:ncol(MnEx)], 1, sum, na.rm = T) /797)
# averaged across 365 days in year
mean(mnex.rchs) 
# proportion of 'anadromous accessible reaches' exceeding standard, for each day (n=365)
(mnexan.rchs <- apply(MnExAn[,2:ncol(MnExAn)], 1, sum, na.rm = T) /200)
# averaged across 365 days in year
mean(mnexan.rchs) 

# proportion of 'all reaches' exceeding standard, for each SUMMER day (n=365)
sum.rows <- c((31 + 30 + 31 + 1 + 31 + 28 + 31 + 30 + 31 + 30 + 1): (31 + 30 + 31 + 1 + 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 - 1))
(mnex.rchs <- apply(MnEx[sum.rows, 2:ncol(MnEx)], 1, sum, na.rm = T) /797)
# averaged across 365 days in year
mean(mnex.rchs) 
# proportion of 'anadromous accessible reaches' exceeding standard, for each day (n=365)
(mnexan.rchs <- apply(MnExAn[sum.rows, 2:ncol(MnExAn)], 1, sum, na.rm = T) /200)
# averaged across 365 days in year
mean(mnexan.rchs) 



