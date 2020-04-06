#temporary: old movement algorithm

# 5. MOVE FISH
# After emergence, assess probability of movement and move fish if appropriate based on water temp and fish density

# index for which salmon have emerged and can begin to grow, and are still alive
salmon.alive.emerge.index= salmon$pid[salmon$emrg==1 & salmon$survive==1]
# index for which fish_other are alive
fish_other.alive.index = fish_other$pid[fish_other$survive==1]

# Run the next steps if there are alive emerged salmon or alive fish_other
if(length(salmon.alive.emerge.index)>0 | length(fish_other.alive.index) > 0){
  
  #5a. First, get fish densities for determining how far a fish will move
  if (interspecific.competition.flag == T) {
    fish1 = salmon; fish2 = fish_other
    density = fncFishDensity(ssn=sno.ssn)
    salmon$conspecificdensity = density[,1]
    salmon$otherdensity = density[,2]
    salmon$totaldensity = density[,3]
    
    fish1 = fish_other; fish2 = salmon
    density = fncFishDensity(ssn=sno.ssn)
    fish_other$conspecificdensity = density[,1]
    fish_other$otherdensity = density[,2]
    fish_other$totaldensity = density[,3]
    
  } else { # interspecific.competition.flag == F
    fish1 = salmon; fish2 = NULL
    density = fncFishDensity(ssn=sno.ssn)
    salmon$conspecificdensity = density[,1]
    salmon$otherdensity = density[,2]
    salmon$totaldensity = density[,3]
    
    fish1 = fish_other; fish2 = NULL
    density = fncFishDensity(ssn=sno.ssn)
    fish_other$conspecificdensity = density[,1]
    fish_other$otherdensity = density[,2]
    fish_other$totaldensity = density[,3]
  }
  rm(density,fish1,fish2)
  
  #5b. Second, get movement distance for fish that can move during this time step
  fish1 = salmon
  fish1<- fish1[salmon.alive.emerge.index,] #subset to get only the ones that have emerged and are still alive
  if (interspecific.competition.flag == T) {fish1.density = fish1$totaldensity
  } else if(interspecific.competition.flag==F){fish1.density = fish1$conspecificdensity}
  fish1.moveDist<- fncMoveDistance(spp=character.parameters[1,"spp"], 
                                   fish1.density,fish1$weight,fish1$pid,fish1$pvals, fish1[,wt.field],
                                   moveLength.lo=numeric.parameters[1,"moveLength.lo"],
                                   moveLength.hi=numeric.parameters[1,"moveLength.hi"],
                                   mvdst.sdlog=numeric.parameters[1,"mvdst.sdlog"],
                                   maxDensity4Mvmt=numeric.parameters[1,"maxDensity4Mvmt"],
                                   maxMass4Mvmt=numeric.parameters[1,"maxMass4Mvmt"]) #uses fish density and fish size
  
  fish2 = fish_other
  fish2<- fish2[fish_other.alive.index==1,] #subset to get only the ones that have emerged and are still alive
  if (interspecific.competition.flag == T) {fish2.density = fish2$totaldensity
  } else if(interspecific.competition.flag==F){fish2.density = fish2$conspecificdensity}
  fish2.moveDist<- fncMoveDistance(spp=character.parameters[2,"spp"], 
                                   fish2.density,fish2$weight,fish2$pid,fish2$pvals, fish2[,wt.field],
                                   moveLength.lo=numeric.parameters[2,"moveLength.lo"],
                                   moveLength.hi=numeric.parameters[2,"moveLength.hi"],
                                   mvdst.sdlog=numeric.parameters[2,"mvdst.sdlog"],
                                   maxDensity4Mvmt=numeric.parameters[2,"maxDensity4Mvmt"],
                                   maxMass4Mvmt=numeric.parameters[2,"maxMass4Mvmt"]) #uses fish density and fish size
  
  # Incorporating flow influence on movement distance, depending on direction of travel (up or downstream)
  #uses new field 'movedir'. will also have to track this later so it stays updated. @@@-WIP
  fish1.moveDist<- cbind(fish1.moveDist, fish1$movedir, fish1$Q)
  colnames(fish1.moveDist)<- c(colnames(fish1.moveDist)[1:3],"movedir",q.field)
  #impede upstream movement (@@@-WIP should do this more wisely)
  fish1.moveDist[,"moveDist"][fish1.moveDist[,"movedir"]==1]<- fish1.moveDist[,"moveDist"][fish1.moveDist[,"movedir"]==1]/fish1.moveDist[,q.field][fish1.moveDist[,"movedir"]==1] 
  #assist downstream movement (@@@-WIP should do this more wisely)
  fish1.moveDist[,"moveDist"][fish1.moveDist[,"movedir"]==1]<- fish1.moveDist[,"moveDist"][fish1.moveDist[,"movedir"]==1]*fish1.moveDist[,q.field][fish1.moveDist[,"movedir"]==1] 
  
  
#5c. Third, move all of the fish that can move

# Move salmon individually
for (x in fish1$pid) {
  #Bias movement direction downstream as fish grows larger (100% downstream drive if larger than smolt.mass)
  # but after smolt.date.cutoff, fish loses the outmigration drive and becomes a yearling
  smolt.date.cutoff<- as.Date(paste0(cs,"-",character.parameters[1,"smolt.date.cutoff"]))
  smolt.mass<- numeric.parameters[1,"smolt.mass"]
  if(dat.idx[dd]>smolt.date.cutoff){
    om.prob<- (fish1$weight[fish1$pid==x]/smolt.mass)*(1-as.numeric(dat.idx[dd]-smolt.date.cutoff)/as.numeric(dat.idx[length(dat.idx)]-smolt.date.cutoff))
  } else{
    om.prob<- fish1$weight[fish1$pid==x]/smolt.mass
  }
  # constrain between 0.5 (equal chance of moving up or down) and 1 (maximum downstream drive)
  if(om.prob>1) om.prob=1; if(om.prob<0.5) om.prob=0.5
  roll<- rbinom(n=1,size=1,prob=om.prob)
  if(roll==1) initDirection<- 1 else initDirection<- 0
  
  moved <- move(spp=character.parameters[1,"spp"], fish1, Constants=salmon.constants, 
                pred.en.dens=numeric.parameters[1,"pred.en.dens"],prey.en.dens=numeric.parameters[1,"prey.en.dens"], 
                oxy=numeric.parameters[1,"oxy"], pff=numeric.parameters[1,"pff"],
                obs.df, fishPID=x, initDirection=initDirection, seg=fish1$seg[fish1$pid==x], 
                length2segBase=fish1$length2segBase[fish1$pid==x],
                moveLength=fish1.moveDist[,"moveDist"][fish1.moveDist[,"pid"]==x], ssn=sno.ssn)
  
  # assign new data back to the fish1 data.frame after fish have moved
  fish1$seg[fish1$pid==x] <- moved[[1]][1]
  fish1$length2segBase[fish1$pid==x] <- moved[[1]][2]
  fish1$ratio[fish1$pid==x] <- moved[[1]][3]
  fish1$direction[fish1$pid==x] <- moved[[2]]
  fish1$upDist[fish1$pid==x] <- fncDist2base4fish(fish1, x, sno.ssn)
  fish1$movedist[fish1$pid==x]<- fish1.moveDist[,"moveDist"][fish1.moveDist[,"pid"]==x]
  fish1[,so.field][fish1$pid==x]<- fncGetSO(moved[[1]][1],sno.ssn)
}

#filter out fish that have reached the "ocean" as smolts (count segs 0 through 5 as the watershed mouth)
fish1$survive[fish1$seg<=5]<- 2

#Update x and y coordinates
#if this was a generated network, use straight-line distance calcs
#if this network was created in GIS, use different approach for moving along a potentially curvy reach
if(length(grep("network-",netnm))>0) {
  locs<- ddply(fish1, .(pid), function(x) data.frame(fngGetXY(x$seg, x$ratio, sno.ssn)))
}  else {
  locs<- ddply(fish1, .(pid), function(x) data.frame(fncGetXY.arc(x$seg, x$ratio, sno.ssn)))
}
fish1$xloc<- locs$xloc
fish1$yloc<- locs$yloc

#put fish that moved back into whole original dataframe
salmon[salmon$pid%in%fish1$pid,]<- fish1

#put updated location info back into ssn
tmp.df<-getSSNdata.frame(sno.ssn, "salmon")
tmp.df$upDist<-salmon$upDist
tmp.df$ratio<-salmon$ratio
tmp.df[,xlat]<-salmon$xloc
tmp.df[,ylon]<-salmon$yloc
tmp.df$rid<-salmon$seg
tmp.df[,so.field]<- salmon[,so.field]
sno.ssn<-putSSNdata.frame(tmp.df, sno.ssn, "salmon")
sno.ssn@predpoints@SSNPoints[[2]]@point.coords[,1]<-tmp.df[,xlat]
sno.ssn@predpoints@SSNPoints[[2]]@point.coords[,2]<-tmp.df[,ylon]
rm(tmp.df,fish1)

#update the alive/emerged index to account for fish that have smolted
salmon.alive.emerge.index= salmon$pid[salmon$emrg==1 & salmon$survive==1]
}
