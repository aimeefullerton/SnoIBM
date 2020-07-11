rm(list=ls()); gc()

# load packages
library(RColorBrewer)

# seg directory where data are stored
setwd("/Volumes/BluPassport/SnoIBM")

# would you like to save these figures?
save.figures <- TRUE
if (save.figures) {
  # specify directory for plot output
  plot.directory <- paste0(getwd(), "/data.out/Figures")
  if (!dir.exists(plot.directory)) {dir.create(plot.directory)}
}

#Load reaches for summary
load("/Users/aimee_fullerton/OneDrive/Work/Research/SnoBIA/SnoIBM/data.in/sno.rbm.ssn/dnsegs.RData")
reaches <- dnsegs[488][[1]]

# load data froom scenarios to be compared
species.list <- c("chinook", "steelhead", "coho", "pink", "rainbow", "lmb")
species <- "chinook"
iter = 1
scenario.list = c("bcc-csm1-1-m", "CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "HadGEM2-CC365", "HadGEM2-ES365", "IPSL-CM5A-MR", "MIROC5", "NorESM1-M")
#scenario.list = c("current_climate_riparian_0", "current_climate_riparian_1", "current_climate_riparian_2", "current_climate_riparian_3")
years.h <- 1995:2005
years.f <- 2089:2099

# function to extract the necessary data from each array
fncIsolateBySeason <- function(array, field, scenario, year, season, start, stop){
  dat <- salmon.array[,field,start:stop]
  dat <- apply(dat, 1, sum)
  dat <- as.data.frame(dat, drop = F)
  dat <- cbind(scenario, season, year, dat)
  dat$rid <- salmon.array[,"seg",1]
  colnames(dat) <- c("scenario", "season", "year", "growth.potential", "rid")
  dat <- dat[!is.na(dat[,"growth.potential"]),]
  return(dat)
}

# Get data collated for historical and future for each species
for(species in species.list){
for(period in c("h", "f")){
  if(period == "h"){years <- years.h}
  if(period == "f"){years <- years.f}
  scen.dat <- NULL
  for(scenario in scenario.list){
    for(yy in years){
      outdir <- paste0("GrwPot.", species, ".", scenario, ".", yy)
      load(file = paste0("data.out/", outdir, "/salmon.array.", yy, ".", iter, ".RData"))
      
      # total growth potential for winter at each location
      aut <- fncIsolateBySeason(salmon.array, "growth", scenario, yy, "aut", 1, (90*2))
      win <- fncIsolateBySeason(salmon.array, "growth", scenario, yy, "win", (90*2), (181*2))
      spr <- fncIsolateBySeason(salmon.array, "growth", scenario, yy, "spr", (181*2), (273*2))
      sum <- fncIsolateBySeason(salmon.array, "growth", scenario, yy, "sum", (273*2), 730)
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
  rm(scen.dat, mainstem)
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

# Plot historical and future separately but in same plot
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


# Plot difference future minus historical
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


# get quantiles & mean for each species (pooled across climate scenarios and years, not broken out by season and reach)
for(species in species.list[-5]){
  mainstem.h <- get(paste0(species, ".mainstem.h"))
  mainstem.f <- get(paste0(species, ".mainstem.f"))
  mainstem <- cbind(mainstem.h, mainstem.f[, c("year", "growth.potential")])
  colnames(mainstem)[ncol(mainstem)] <- "growth.potential.f"
  mainstem$growth.diff <- mainstem$growth.potential.f - mainstem$growth.potential
  cat(species, ": ", summary(mainstem$growth.diff), "\n")
}





# Alternative line & quantile plot format 

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


# Difference (F - H)
png(paste0(plot.directory, "/growth_potential_diff.png"), width = 7, height = 7, units = "in", res = 300)
  par(mfrow = c(3, 2), mar = c(1, 3.5, 2, 1), las = 1)
  
  for(species in (species.list[-5])){
    ifelse(species == "pink", i.list <- 2:4, i.list <- 1:4)
    mainstem.h <- get(paste0(species, ".mainstem.h"))
    mainstem.f <- get(paste0(species, ".mainstem.f"))
    mainstem <- cbind(mainstem.h, mainstem.f[, c("year", "growth.potential")])
    colnames(mainstem)[ncol(mainstem)] <- "growth.potential.f"
    mainstem$growth.diff <- mainstem$growth.potential.f - mainstem$growth.potential
    
    dat2plot.Md <- tapply(mainstem$growth.diff, list(mainstem$season, mainstem$rid), quantile, probs = 0.5)[c(3,2,4,1),]
    dat2plot.Q1 <- tapply(mainstem$growth.diff, list(mainstem$season, mainstem$rid), quantile, probs = 0.25)[c(3,2,4,1),]
    dat2plot.Q3 <- tapply(mainstem$growth.diff, list(mainstem$season, mainstem$rid), quantile, probs = 0.75)[c(3,2,4,1),]

  plot(dat2plot.Md[1,], type = 'n', col = c3[1], ylim = c(-4.2, 2.5),  ylab = "", xlab = "", axes = F, main = species) #ylim = range(c(dat2plot.Q1, dat2plot.Q3))
  axis(2)
  #axis(1, at = seq(1, 31, 5), labels = reaches.inv[seq(1, length(reaches.inv), 5)])
  for(i in i.list){polygon(c(1: 31, rev(1: 31)), c(dat2plot.Q1[i, ], rev(dat2plot.Q3[i, ])), border = NA, col = c2[i])} #Q1/Q3
  for(i in i.list){ lines(dat2plot.Md[i,], col = c3[i])} #Median
  abline(h = 0, lty = 3)
  abline(h = -4.2)
  }
  plot(1:10, 1:10, type = 'n', axes = F, xlab = "", ylab = "")
  legend ("center", legend = c("winter", "spring", "autumn", "summer"), lty = 1, lwd = 4, col = c3[c(3,2,4,1)], bty = 'n')
dev.off()

