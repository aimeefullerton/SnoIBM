## Stack up results for plotting ####

library(dplyr)
library(chron)
library(lubridate)

# specify directory for plot output
plot.directory <- paste0(getwd(), "/data.out/Figures")
if (!dir.exists(plot.directory)) {dir.create(plot.directory)}


# COLORS ----------------------------------------------------------
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



tag = "A"
iter = 1
scenario.list = c("current_climate_riparian_0", "current_climate_riparian_1", "current_climate_riparian_2", "current_climate_riparian_3")
years = 2003:2013

# Phenology (emergence & outmigration timing)
summarize.by = "week"
for(scenario in scenario.list){
  emergence = data.frame(JD = 1:365)
  if(summarize.by == "week") emergence$week = week(as.Date(emergence$JD - 1, origin = "1970-01-01"))
  outmigration = data.frame(JD = 1:365)
  if(summarize.by == "week") outmigration$week = week(as.Date(outmigration$JD - 1, origin = "1970-01-01"))
  
  for(yy in years){
    # load data
    dataDir = paste0("data.out/", scenario, ".", tag, ".", yy)
    load(paste0(dataDir, "/salmon.finalstep.",yy,".",iter,".RData")) 
    td = as.data.frame(salmon.finalstep)
    td$dateEm = as.POSIXct(td$dateEm,origin="1970-01-01")
    td$dateOm = as.POSIXct(td$dateOm,origin="1970-01-01")
    
    # emergence
    if(summarize.by == "day"){
      emgd = td %>% group_by(as.Date(dateEm)) %>% summarise(Count = length(pid)); emgd = emgd[-nrow(emgd),]; colnames(emgd)[1] = "Date"
      emgd$JD = julian.Date(emgd$Date) - julian.Date(as.Date(paste0(yy, "-01-01")))
      emgd = as.data.frame(emgd)
      mykey = paste0("X", emergence$JD) #has to match data.frame setup
      mylookupvector = emgd[,"Count"]; names(mylookupvector) = paste0("X", emgd[,"JD"])
      emergence[,as.character(yy)] = unname(mylookupvector[mykey])
    }
    if(summarize.by == "week"){
      td$week = week(td$dateEm)
      td$JD = round(julian(td$dateEm) - julian(as.Date(paste0(yy, "-01-01"))))
      emgd = td %>% group_by(week) %>% summarise(Count = length(pid)); emgd = emgd[-nrow(emgd),]; colnames(emgd)[1] = "Week"
      emgd = as.data.frame(emgd)
      mykey = paste0("X", emergence$week) #has to match data.frame setup
      mylookupvector = emgd[,"Count"]; names(mylookupvector) = paste0("X", emgd[,"Week"])
      emergence[,as.character(yy)] = unname(mylookupvector[mykey])
    }
    
    # outmigration
    if(summarize.by == "day"){
      smolts = td %>% group_by(as.Date(dateOm)) %>% summarise(Count = length(pid)); smolts = smolts[-nrow(smolts),]; colnames(smolts)[1] = "Date"
      smolts$JD = julian.Date(smolts$Date) - julian.Date(as.Date(paste0(yy, "-01-01")))
      smolts = as.data.frame(smolts)
      mykey = paste0("X", outmigration$JD) #has to match data.frame setup
      mylookupvector = smolts[,"Count"]; names(mylookupvector) = paste0("X", smolts[,"JD"])
      outmigration[,as.character(yy)] = unname(mylookupvector[mykey])
    }
    if(summarize.by == "week"){
      td$week = week(td$dateOm)
      td$JD = round(julian(td$dateOm) - julian(as.Date(paste0(yy, "-01-01"))))
      smolts = td %>% group_by(week) %>% summarise(Count = length(pid)); smolts = smolts[-nrow(smolts),]; colnames(smolts)[1] = "Week"
      smolts = as.data.frame(smolts)
      mykey = paste0("X", outmigration$week) #has to match data.frame setup
      mylookupvector = smolts[,"Count"]; names(mylookupvector) = paste0("X", smolts[,"Week"])
      outmigration[,as.character(yy)] = unname(mylookupvector[mykey])
    }
    
  }
  rm(emgd, smolts, td, mykey, mylookupvector)  
  assign(paste0(scenario,".emergence"), emergence); rm(emergence)
  assign(paste0(scenario,".outmigration"), outmigration); rm(outmigration)
}


# Final size (subyearlings & yearlings)
for(scenario in scenario.list){
  subyearlings = data.frame(Weight = seq(0, 15, 0.1))
  yearlings = data.frame(Weight = seq(0, 100, 1))
  
  for(yy in years){
    # load data
    dataDir = paste0("data.out/", scenario, ".", tag, ".", yy)
    load(paste0(dataDir, "/salmon.finalstep.",yy,".",iter,".RData")) 
    td = as.data.frame(salmon.finalstep)
    
    # subyearlings
    td$weight2 = round(td$weight, 1)
    subs = td %>% filter(survive == 2) %>% group_by(weight2) %>% summarise(Count = length(pid)); subs = subs[-nrow(subs),]; colnames(subs)[1] = "Weight"
    subs = as.data.frame(subs)
    mykey = paste0("X", subyearlings$Weight) #has to match data.frame setup
    mylookupvector = subs[,"Count"]; names(mylookupvector) = paste0("X", subs[,"Weight"])
    subyearlings[,as.character(yy)] = unname(mylookupvector[mykey])
    
    # yearlings
    td$weight3 = round(td$weight)
    yrlgs = td %>% filter(survive == 1) %>% group_by(weight3) %>% summarise(Count = length(pid)); yrlgs = yrlgs[-nrow(yrlgs),]; colnames(yrlgs)[1] = "Weight"
    yrlgs = as.data.frame(yrlgs)
    mykey = paste0("X", yearlings$Weight) #has to match data.frame setup
    mylookupvector = yrlgs[,"Count"]; names(mylookupvector) = paste0("X", yrlgs[,"Weight"])
    yearlings[,as.character(yy)] = unname(mylookupvector[mykey])
    
  }
  rm(subs, yrlgs, td, mykey, mylookupvector)  
  assign(paste0(scenario,".subyearlings"), subyearlings); rm(subyearlings)
  assign(paste0(scenario,".yearlings"), yearlings); rm(yearlings)
}

# Add quantiles
for(scenario in scenario.list){
  for(metric in c("emergence", "outmigration", "subyearlings", "yearlings")){
    foo = get(paste0(scenario, ".", metric))
    foo[is.na(foo)] <- 0
    if("week" %in% colnames(foo)) start.col = 3 else start.col = 2
    foo = cbind.data.frame(foo, t(apply(foo[,start.col:(ncol(foo) - 5)], 1, quantile, na.rm = T)))
    assign(paste0(scenario, ".", metric), foo); rm(foo)
  }
}

# Plot setup
for(scenario in scenario.list){
  for(metric in c("emergence", "outmigration", "subyearlings", "yearlings")){
      dat = get(paste0(scenario, ".", metric))
      ylm = c(0, max(dat$`100%`,na.rm=T))
      ylb = metric
      if(metric %in% c("emergence", "outmigration")){
        xx = trunc(which(dat[,"100%"] > 0))
        minX = xx[1]
        min.date = month.day.year(minX)
        min.date = as.Date(paste0(yy, "-", min.date$month, "-", min.date$day))
        maxX = xx[length(xx)]
        max.date = month.day.year(maxX)
        max.date = as.Date(paste0(yy, "-", max.date$month, "-", max.date$day))
        x.seq = seq(min.date, max.date, length.out = 4)
        x.seq = format(x.seq, "%d-%b")
        lng.out = length(x.seq)
      }
      if(metric %in% c("subyearlings", "yearlings")){
        xx = trunc(which(dat[,"50%"] > 0))
        minX = xx[1]
        maxX = xx[length(xx)]
        x.seq = seq(dat$Weight[minX], dat$Weight[maxX], length.out = 6)
        lng.out = length(x.seq)
      }
      
      png(paste0(plot.directory,"/", scenario, ".", metric, ".png"), width = 6, height = 6, units = "in", res = 150)   
      par(mar = c(3, 4, 1, 1.5) + 0.1, las = 1, oma = c(0, 0.5, 1, 0))
      
      plot(dat[minX: maxX,"100%"], ylim = ylm, ylab = ylb, type = 'n', xaxt = 'n', xlab = "", xlim = c(minX, maxX))
      axis(1, at = seq(minX, maxX, length.out = lng.out), labels = x.seq, cex.axis = 0.9)
  
      #Min/Max
      polygon(c(minX: maxX, rev(minX: maxX)), c(dat[minX: maxX, "0%"], rev(dat[minX: maxX, "100%"])), border = NA, col = c1.3)
      
      #Q1/Q3
      polygon(c(minX: maxX, rev(minX: maxX)), c(dat[minX: maxX, "25%"], rev(dat[minX: maxX, "75%"])), border = NA, col = c2.3)
      
      #Median
      lines(minX:maxX, dat[minX: maxX,"50%"], lwd = 2, col = c3.3)
      
      abline(h = 0, lty = 3, col = "gray50")
      
      dev.off()
  }
}
