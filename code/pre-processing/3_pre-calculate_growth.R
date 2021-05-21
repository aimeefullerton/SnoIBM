## Pre-calculate growth at each temperature that can be Looked Up instead of having to run model on the fly each time

# Consumption Eq. 3 for coldwater species, from Hansen et al. 1997, p. 2-4
# product of 2 sigmoid curves, one fit to the increasing portion of the temperature dependence fucnction and the other to the decreasing portion.
# CA = intercept of the mass dependence function for a 1-g fish at optimum water temperature
# CB = coefficient of the mass dependence
# CQ = for increasing portion of curve, is the lower water temp. at which temp. dependence is a small fraction (CK1) of the maximum rate
# CTO = for increasing portion of curve, is the water temperature corresponding to 0.98 of max consumption rate 
# CTM = for decreasing portion of curve, is the water temp. at which dependence is still 0.98 of Cmax
# CTL = temp. at which dependence is some reduced fraction (CK4) of maximum rate
# CK1 & CK4: see above

source("code/functions.R")

loadDir = "data.in"
salmon.nm = "chinook"
fish_other.nm = "lmb"


# Load model parameters
parameters.all = read.csv(paste0(loadDir, "/parameters/parameters.csv"), as.is = TRUE)
parameters = cbind.data.frame(rbind(salmon.nm, fish_other.nm), t(parameters.all[, c(salmon.nm, fish_other.nm)]), stringsAsFactors = FALSE)
colnames(parameters) = c("species",parameters.all$parameter); row.names(parameters) = c("salmon", "fish_other")
for(c in 8:ncol(parameters)){ parameters[, c] = as.numeric(parameters[,c])}
for(i in c("spawn.date.begin", "om.date.taper", "om.date.end")){ parameters[, i] = gsub("x", "", parameters[, i])}

ration = seq(0.01,0.3,0.001)

## juvenile Chinook salmon ####
numFish = 1
salmon.constants = fncReadConstants("salmon") #using ration
waterTemps = cbind("pid" = seq(1 , length(seq(0.05, 25, 0.05))), "WT" = seq(0.05, 25, 0.05))
wt.growth = array(NA, dim = c(nrow(waterTemps), length(ration), length(seq(1, 500, 1)))) #dim=c(500 WT, 291 rations, 500 weights)
for(w in 1:dim(wt.growth)[3]){
  for(r in 1:dim(wt.growth)[2]){
    salmon.input = fncGetBioEParms(parameters["salmon","species"], 
                                  parameters["salmon","pred.en.dens"], parameters["salmon","prey.en.dens"],
                                  waterTemps, startweights=rep(w,nrow(waterTemps)), pvals=rep(-9,nrow(waterTemps)),
                                  ration = rep(ration[r],nrow(waterTemps)))
    
    Results = BioE(salmon.input, salmon.constants) #run BioE code given input and constants
    wt.growth[,r,w] = c(Results$Growth)/w
  }
}
if (!dir.exists(file.path("data.in/fish.growth.lookup"))) {dir.create(file.path("data.in/fish.growth.lookup"))}
save("wt.growth",file = paste0("data.in/fish.growth.lookup/wt.growth.array.", salmon.nm, ".RData"))

# Read back in and plot
load(paste0(mydir, "/data.in/fish.growth.lookup/wt.growth.array.", salmon.nm, ".RData"))
assign("wt.growth.salmon", wt.growth)
rm(wt.growth)

par(mfrow=c(2,2), las=1)
for(fish.mass in c(1,5,10,25)){
  plot(waterTemps[,"WT"],wt.growth.salmon[,dim(wt.growth.salmon)[2],fish.mass],type='l',ylab="Growth (g/g/d)",xlab="Water temperature (C)",col=4,ylim=range(wt.growth.salmon[,,fish.mass]),main=paste0("fish weight =",fish.mass," g")) #max ration
  lines(waterTemps[,"WT"],wt.growth.salmon[,1,fish.mass],col=2) #min ration
  for(i in round(seq(3,(dim(wt.growth.salmon)[2]-1),length.out = 10),0)){ lines(waterTemps[,"WT"],wt.growth.salmon[,i,fish.mass],col="gray30",lty=2)}
  abline(h=0)
  #abline(v=7)
}
par(mfrow=c(3,3), las=1)
for(fish.mass in c(1,5,10,15,30,75,100,150)){
  plot(waterTemps[,"WT"],wt.growth.salmon[,dim(wt.growth.salmon)[2],fish.mass],type='l',ylab="Growth (g/g/d)",xlab="Water temperature (C)",col=4,ylim=range(wt.growth.salmon[,,fish.mass]),main=paste0("fish weight =",fish.mass," g")) #max ration
  lines(waterTemps[,"WT"],wt.growth.salmon[,1,fish.mass],col=2) #min ration
  for(i in round(seq(3,(dim(wt.growth.salmon)[2]-1),length.out = 10),0)){ lines(waterTemps[,"WT"],wt.growth.salmon[,i,fish.mass],col="gray30",lty=2)}
  abline(h=0)
}



## Largemouth bass ####
numFish = 1
fish_other.constants = fncReadConstants("largemouth_bass") #using ration
waterTemps=cbind("pid"=seq(1,length(seq(0.05,25,0.05))),"WT"=seq(0.05,25,0.05))
mass.seq = c(1:100,seq(105,995,5), seq(1000,6000,10)) #need to use this and can't index by the sequence because numbers jump by different amounts
wt.growth=array(NA,dim=c(nrow(waterTemps),length(ration),length(mass.seq))) #dim=c(500 WT, 291 rations, 780 weights)
for(w in mass.seq){
  for(r in 1:dim(wt.growth)[2]){
    fish_other.input= fncGetBioEParms("largemouth_bass", parameters[1,"pred.en.dens"],
          parameters[1,"prey.en.dens"],waterTemps, startweights=rep(w,nrow(waterTemps)), 
          pvals=rep(-9,nrow(waterTemps)),ration = rep(ration[r],nrow(waterTemps)))
    
    Results = BioE(fish_other.input, fish_other.constants) #run BioE code given input and constants
    wt.growth[,r,which(mass.seq == w)] = c(Results$Growth)/w
  }
}
if (!dir.exists(file.path("data.in/fish.growth.lookup"))) {dir.create(file.path("data.in/fish.growth.lookup"))}
save("wt.growth",file = paste0("data.in/fish.growth.lookup/wt.growth.array.", parameters[2,"species"], ".RData"))

# Read back in and plot
load(paste0(mydir, "/data.in/fish.growth.lookup/wt.growth.array.", parameters[2,"species"], ".RData"))
assign("wt.growth.lmb", wt.growth)
rm(wt.growth)

par(mfrow=c(3,3), las=1)
for(fish.mass in c(1,10,100,500,1000,1500,2000,3000,5000)){
  ylm = range(wt.growth.lmb[,,which.min(abs(fish.mass - mass.seq))])
  plot(waterTemps[,"WT"],wt.growth.lmb[,dim(wt.growth.lmb)[2], which(mass.seq == fish.mass)],type='l',ylab="Growth (g/g/d)",xlab="Water temperature (C)",
       col=4,main=paste0("fish weight =",fish.mass," g"), ylim = ylm) #max ration
  lines(waterTemps[,"WT"],wt.growth.lmb[,1, which(mass.seq == fish.mass)],col=2) #min ration
  for(i in round(seq(3,(dim(wt.growth.lmb)[2]-1),length.out = 10),0)){ lines(waterTemps[,"WT"],wt.growth.lmb[,i, which(mass.seq == fish.mass)],col="gray30",lty=2)}
  abline(h=0)
}

