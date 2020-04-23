# Plot temperature profiles for the reaches where eggs are incubating

#Run Setup and Initialize Habitat and Fish, then:

redd.rids = sort(unique(ssn@predpoints@SSNPoints[[salmon.id]]@point.data$rid))

par(mfrow=c(1,1), las = 1)
  plot(WT.df[,paste0("X",redd.rids[1])][WT.df[,"Time"] == 18], type = 'l', axes = F, xlab = "Julian Day", ylab = "Water temperature (C)", ylim = c(0,23), main = cs, lty = 3)
  axis(2); axis(1, at = seq(1:395), labels = c(245:365,1:274))
  abline(h = 5, lty = 2)
  for(r in 2:length(redd.rids)){
    lines(WT.df[,paste0("X",redd.rids[r])][WT.df[,"Time"] == 18], lty = 3)
  }

par(mfrow=c(4,4), las = 1)
for(r in redd.rids){
  plot(WT.df[,paste0("X",r)][WT.df[,"Time"] == 18], type = 'l', axes = F, xlab = "Julian Day", ylab = "Water temperature (C)", ylim = c(0,23), main = r)
  axis(2); axis(1, at = seq(1:395), labels = c(245:365,1:274))
  lines(WT.df[,paste0("X",r)][WT.df[,"Time"] == 6], lty = 3)
  abline(h = 5, lty = 2)
}


# Plot accumulated TU by eggs during incubation
load(paste0(outputDir, "/",spp,".array.",cs,".",iter,".RData")) 
d<- seq(2,730,2)
plot(salmon.array[1,"TU",d][salmon.array[1,"emrg",d] == -1 & salmon.array[1,"survive",d] > 0], type = 'l', xlim = c(1,175),xlab = "Days since spawned", ylab = "Accumulated TU (degree days)", main = cs)
#axis(2); axis(1, at = seq(1:365), labels = c(244:365,1:243))
for(r in 2:dim(salmon.array)[1]){
  lines(salmon.array[r,"TU",d][salmon.array[r,"emrg",d] == -1 & salmon.array[r,"survive",d] > 0], col = r)
}
