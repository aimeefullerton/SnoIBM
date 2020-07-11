library(abind)
plot.directory <- "plots"

# Load data
scenario.list = c("bcc-csm1-1-m", "CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "HadGEM2-CC365", "HadGEM2-ES365", "IPSL-CM5A-MR", "MIROC5", "NorESM1-M")

# Gather historical climate scenario data
scenarios.data <- NULL
for(scenario in scenario.list){
  scenario.data = NULL
  years = 1995:2005
  #years = 2089:2099
  for(yy in years){
    outdir <- paste0(scenario, ".A.", yy)
    load(file = paste0("/Volumes/BluPassport/SnoIBM/data.out_riparian0/", outdir, "/salmon.array.", yy, ".1.RData"))
    
    # Post-emergence filter
    emgd <- salmon.array[,"emrg",]
    emgd[emgd != 1] <- NA
    
    # Potential yearling survival filter
    srv.age1 <- salmon.array[,"survive", dim(salmon.array)[3]]
    srv.age1[srv.age1 != 1] <- NA
    s.age1 <- array(srv.age1, dim = c(dim(salmon.array)[1], dim(salmon.array)[3]))
    
    # Get growth for potential yearlings only once emerged and only for fish that survived to the end
    dat <- salmon.array[,"growth",] * s.age1 * emgd
    dat <- dat[,seq(2, dim(salmon.array)[3], 2)] + dat[,seq(1, dim(salmon.array)[3], 2)]
    
    # Take mean of growth across these fish
    dat <- apply(dat, 2, mean, na.rm = T) 
    
    # Remove leap year's extra day:
    if(yy %in% c(1996, 2000, 2004, 2092, 2096)){
      dat <- dat[-60]
    }
    scenario.data <- cbind(scenario.data, dat)
  }
  scenario.data[is.nan(scenario.data)] <- NA
  scenarios.data <- abind(scenarios.data, scenario.data, along = 3)
}
assign("growth.historical", scenarios.data)
save(growth.historical, file = "data.out/growth.historical.RData")

load("data.out/growth.historical.RData")
load("data.out/growth.future.RData")





# Plot
png(paste0(plot.directory, "/Figure9_growth_over_time.png"), width = 7.5, height = 9, units = "in", res = 300)
par(mfrow = c(2, 1), las = 1, mar = c(3,4.5,1,1), oma = rep(0.5, 4), cex = 1.1)

for(timeperiod in c("historical", "future")){
  td <- get(paste0("growth.", timeperiod))
  dat <- apply(td, 1, quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = T)
  dat<- t(dat)
  
  # Set up date axis
  first.date <- as.Date("1994-09-01") #starting date for simulation and for spawning
  last.date <- as.Date("1995-08-31") #last date of the simulation
  months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  minX <- trunc(which(dat[,3] > 0))[1]
  val1 <- as.numeric(format(as.Date(minX + 1, origin = first.date), "%m"))
  val2 <- as.numeric(format(last.date, "%m"))
  if(val1 >= 9 & val1 <= 12) months <- months[c(val1:length(months), 1:val2)]
  if(val1 >= 1 & val1 <= 8) months <- months[val1:val2]
  lng.out<- length(months)
  maxX <- length(dat[,3])
  
  #ylm <- range(dat, na.rm = T); ylm[2] <- ylm[2] + 0.005

  if(timeperiod == "historical") {mycols <- c("#B0BFFC96", "#8099FCC8", "#0326B2FF"); leglab = "a"; ylm <- c(-0.007, 0.033)}
  if(timeperiod == "future") {mycols <- c("#fcd99c96", "#eaad44c8", "#f49b02ff"); leglab = "b"; ylm <- c(-0.0263, 0.037)}
  
  plot(dat[,3], ylim = ylm, ylab = expression(paste("Growth (g g ",d^-1,")")), type = 'n', xaxt = 'n', xlab = "", xlim = c(minX, maxX), cex.axis = 1)
  axis(1, at = seq(minX, maxX, length.out = lng.out), labels = months)
  
  #Min/Max
  polygon(c(minX: maxX, rev(minX: maxX)), c(dat[minX: maxX, 1], rev(dat[minX: maxX, 5])), border = NA, col = mycols[1])
  
  #Q1/Q3
  polygon(c(minX: maxX, rev(minX: maxX)), c(dat[minX: maxX, 2], rev(dat[minX: maxX, 4])), border = NA, col = mycols[2])
  
  #Median
  lines(dat[,3], lwd = 2, col = mycols[3])
  
  abline(h = 0, lty = 3, col = "darkgray")
  
  legend("topleft", legend = paste0("(", leglab,")"), bty = 'n')
}

dev.off()

