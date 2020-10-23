## Growth potential for Snoqualmie salmonids and LMB for Yan et al. manuscript

# Setup ####
rm(list=ls()); gc()

# load packages
library(RColorBrewer)
library(sf)
library(tidyverse)

# seg directory where data are stored
load.dir <- "/Volumes/BluPassport/SnoIBM/data.out_growth.potential"

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

# load data froom scenarios to be compared
species.list <- c("chinook", "steelhead", "coho", "pink", "lmb", "rainbow")
species <- "chinook"
iter = 1
scenario.list = c("bcc-csm1-1-m", "CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "HadGEM2-CC365", "HadGEM2-ES365", "IPSL-CM5A-MR", "MIROC5", "NorESM1-M")
#scenario.list = c("current_climate_riparian_0", "current_climate_riparian_1", "current_climate_riparian_2", "current_climate_riparian_3")
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
species.list <- species.list[-which(species.list == "rainbow")] #remove rainbow trout; different spatial extent

for(species in species.list){
  for(period in c("h", "f")){
    load(paste0(load.dir, "/", paste0(species, ".scen.dat.", period), ".RData"))
    load(paste0(load.dir, "/", paste0(species, ".mainstem.", period), ".RData"))
    assign(paste0(species, ".scen.dat.", period), scen.dat)
    assign(paste0(species, ".mainstem.", period), mainstem)
  }
}

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


# Grid plot: plot historical and future separately but in same plot ####
mycolors <- c("darkorange", "orange", brewer.pal(9, "Purples"))
mybreaks <- quantile(c(dat2plot.h, dat2plot.f), probs = seq(0,1,0.1))
mybreaks <- c(0, mybreaks)

png(paste0(plot.directory, "/", species, "_growth_potential.png"), width = 5, height = 8, units = "in", res = 300)
par(mfrow = c(2,1), mar = c(4,3.5,0.5,1), las = 1)
image(1:31, 1:4, t(dat2plot.h), axes = F, ylab = "", xlab = "", col = mycolors, breaks = sort(mybreaks))
axis(2, at = 1:4, labels = c("Sum", "Spr", "Win", "Aut"))
reaches.inv <- reaches[length(reaches):1]
#axis(1, at = seq(1, 31, 5), labels = reaches.inv[seq(1, length(reaches.inv), 5)])

image(1:31, 1:4, t(dat2plot.f), axes = F, ylab = "", xlab = "Distance upstream (km)", col = mycolors, breaks = sort(mybreaks))
axis(2, at = 1:4, labels = c("Sum", "Spr", "Win", "Aut"))
#axis(1, at = seq(1, 31, 5), labels = reaches.inv[seq(1, length(reaches.inv), 5)])
dev.off()


# Grid plot: plot difference future minus historical ####
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

png(paste0(plot.directory, "/", species, "_growth_differential.png"), width = 5, height = 4, units = "in", res = 300)
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

