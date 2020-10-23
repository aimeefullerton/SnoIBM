library(dplyr)

#Summarize run:
#spp = "salmon"
#yy = 2014
#tag = "P"
#mydir = getwd()
#outputDir = dataDir = paste0("data.out/", tag, ".", yy)
#first.date = as.Date(paste0(yy-1, "-", "09-01")) #starting date for simulation and for spawning
#last.date = as.Date(paste0(yy, "-", "08-31")) #last date of the simulation
nFish = parameters["salmon","nFish"]

#source("code/plotBasicRunSummary.R")
#iter = 1
#load(paste0(outputDir, "/",spp,".finalstep.",yy,".",iter,".RData")) 
fo = as.data.frame(salmon.finalstep)
fo$dateSp = as.POSIXct(fo$dateSp,origin="1970-01-01")
fo$dateEm = as.POSIXct(fo$dateEm,origin="1970-01-01")
fo$dateOm = as.POSIXct(fo$dateOm,origin="1970-01-01")
fo$dateDi = as.POSIXct(fo$dateDi,origin="1970-01-01")
fo$datePr = as.POSIXct(fo$datePr,origin="1970-01-01")
fo$dateSc = fo$dateDi; fo$dateSc[fo$survive != -2] = NA
fo$dateMo = fo$dateDi; fo$dateMo[fo$survive != 0] = NA

spnd = fo %>% group_by(as.Date(dateSp)) %>% summarise(Count = length(pid)); spnd = spnd[-nrow(spnd),]; colnames(spnd)[1] = "Date"
emgd = fo %>% group_by(as.Date(dateEm)) %>% summarise(Count = length(pid)); emgd = emgd[-nrow(emgd),]; colnames(emgd)[1] = "Date"
smolts = fo %>% group_by(as.Date(dateOm)) %>% summarise(Count = length(pid)); smolts = smolts[-nrow(smolts),]; colnames(smolts)[1] = "Date"
preds = fo %>% group_by(as.Date(datePr)) %>% summarise(Count = length(pid)); preds = preds[-nrow(preds),]; colnames(preds)[1] = "Date"
morts = fo %>% group_by(as.Date(dateDi)) %>% summarise(Count = length(pid)); morts = morts[-nrow(morts),]; colnames(morts)[1] = "Date"
scour = fo %>% group_by(as.Date(dateSc)) %>% summarise(Count = length(pid)); scour = scour[-nrow(scour),]; colnames(scour)[1] = "Date"
ssmort = fo %>% group_by(as.Date(dateMo)) %>% summarise(Count = length(pid)); ssmort = ssmort[-nrow(ssmort),]; colnames(ssmort)[1] = "Date"

# Save summary metrics
spawnDates = substr(summary(fo$dateSp[fo$dateSp > as.POSIXct(paste0(yy-1,"-09-01"))]), 1, 10) #spawning dates
emrgDates = substr(summary(fo$dateEm[fo$dateEm > as.POSIXct(paste0(yy-1,"-09-01")) & fo$emrg ==1]), 1, 10) #emergence dates
smoltDates = substr(summary(fo$dateOm[fo$dateOm > as.POSIXct(paste0(yy-1,"-09-01")) & fo$survive ==2]), 1, 10) #outmigration dates
predDates = substr(summary(fo$datePr[fo$datePr > as.POSIXct(paste0(yy-1,"-09-01"))]), 1, 10) #dates eaten by predators
mortDates = substr(summary(fo$dateDi[fo$dateDi > as.POSIXct(paste0(yy-1,"-09-01"))]), 1, 10) #total mortality
scourDates = substr(summary(fo$dateDi[fo$dateDi > as.POSIXct(paste0(yy-1,"-09-01")) & fo$survive == -2]), 1, 10) #dates eggs scoured
ssMortDates = substr(summary(fo$dateDi[fo$dateDi > as.POSIXct(paste0(yy-1,"-09-01")) & fo$survive == 0]), 1, 10) #dates died randomly

subYMass = round(quantile(fo$weight[fo$survive ==2]),2) #subyearling weights
ylgMass = round(quantile(fo$weight[fo$survive ==1]),2) # yearling weights

subyearlings = length(fo$weight[fo$survive ==2]) #no. of surviving subyearlings
yearlings = length(fo$weight[fo$survive ==1]) #no. of surviving yearlings

prop.yearlings = round(yearlings/(subyearlings + yearlings), 3) #proportion yearlings
survival = round((subyearlings + yearlings) / nFish, 3) #proportion survived



# Store info on no. replicates, climate scenarios, and runtime
summaryDir = paste0(outputDir, "/quick.summary.", yy, ".", iter, ".txt")
file.create(summaryDir)

summary.info<- c(paste0("survival: ", survival),
             paste0("pYrlngs: ", prop.yearlings), 
             paste0("nSubyearlings: ", subyearlings),
             paste0("nYearlings: ", yearlings),
             paste0("subYMass: ", subYMass),
             paste0("ylgMass: ", ylgMass),
             paste0("spawnDates: ", spawnDates),
             paste0("scourDates: ", scourDates),
             paste0("emrgDates: ",emrgDates),
             paste0("ssMortDates: ",ssMortDates),
             paste0("smoltDates:",smoltDates))

write(c("Basic Summary:", summary.info),file = summaryDir)


#Plots
png(paste0(outputDir, "/phenology.png"), width = 6, height = 5, units = "in", res = 300)
par(mfrow = c(2,2), las = 1, mar = c(3,4,0.5,1), cex = 0.9)
plot(spnd, xlim=c(as.Date(paste0(yy-1,"-09-01-"),origin="1970"), as.Date(paste0(yy,"-08-31-"),origin="1970")), xlab = "", col="gray60", cex.axis = 0.9, cex = 0.5)
  points(emgd, col = 4, cex = 0.5)
  points(smolts, col = 3, cex = 0.5)
  points(ssmort, col = 1, cex = 0.5)
  points(scour, col = 2, cex = 0.5)
  legend("topright",legend = c("Spawn", "Scour", "Emerge", "Die", "Smolt"), pch = 19, col = c("gray60",2,4,1,3), bty='n', cex = 0.9)
plot(emgd$Date, emgd$Count, xlim=c(min(emgd$Date), max(smolts$Date)), ylab = "Phenology", xlab = "", col = 4, cex = 0.9)
  points(smolts$Date, smolts$Count, col = 3)
  legend("topleft",legend = c("Emerge", "Smolt"), col = c(4,3), pch = 19, bty = 'n', cex = 0.9)
plot(spnd$Date, spnd$Count, ylab = "Spawned", xlab = "", col = "gray60", cex.axis = 0.9)
plot(morts$Date, morts$Count, ylab = "Mortalities", xlab = "", type = 'n', cex.axis = 0.9)
  points(scour$Date, scour$Count, col = 2)
  points(ssmort$Date, ssmort$Count, col = 1)
  legend("topright",legend = c("Scour", "Die"), col = c(2,1), pch = 19, bty = 'n', cex = 0.9)
dev.off()

png(paste0(outputDir, "/mass.png"), width = 6, height = 5, units = "in", res = 300)
par(mfrow = c(2,1), las = 1, mar = c(4,4,0.5,1), cex = 0.9)
  #hist(fo$weight[fo$survive == 2], xlim = c(0,30),ylab = "Count", xlab = "mass (g)", main = "", col = "blue")
  #hist(fo$weight[fo$survive == 1], add = TRUE, col = "orange")
  hist(fo$weight[fo$survive == 2], ylab = paste0(subyearlings, " subyearlings"), xlab = "", main = "", col = "blue")
  hist(fo$weight[fo$survive == 1], ylab = paste0(yearlings, " yearlings"), xlab = "mass (g)", main = "", col = "orange")
dev.off()


# Translate Weight into Length for comparison with smolt trap data:
W = 1
# Exponential relationship
# W = a* L ^ b
a = 6.401e-6; b = 3.099 #wet weight exponential relationship from Jellyman et al. (R2 = 0.990)
#a = 1.092e-7; b = 3.735 #dry weight exponential relationship from Jellyman et al. (R2 = 0.903)
# log(W) = log(a) + b * log(L)
exp((log(W) - log(a))/b) #exponential equation 

