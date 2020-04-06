# Old versions of functions before updating to match SWH model

# Calculates movement distance potentials based on fish densities and fish sizes
# returns data frame of max movement distances for each fish and whether they will move or not (zi)
fncMoveDistance <- function(fdens = fish$conspecificdensity, fweight = fish$weight, fpid = fish$pid, fpval = fish$pvals, 
                            fdir = fish$direction, fsh.wt = fish[,wt.field], fsh.q = fish[q.field],
                            moveLength.lo = moveLength.lo, moveLength.hi = moveLength.hi, mvdst.sdlog = mvdst.sdlog, 
                            maxDensity4Mvmt = maxDensity4Mvmt, maxMass4Mvmt = maxMass4Mvmt) {
  
  #movement distance is initially drawn from a lognormal distribution, and is influenced by conspecific density such that crowded conditions cause larger movements
  beta0 = log(moveLength.lo)
  beta1 = (log(moveLength.hi) - beta0) / maxDensity4Mvmt
  nFish = length(fdens)
  fdens[fdens > maxDensity4Mvmt] = maxDensity4Mvmt
  
  #  #look up WT to use based on a fish's weight and p-value assuming a 1-g fish at full ration
  #  lookup = cbind("WT"=seq(0,25,0.05)[-1],"growth"=wt.growth[,1,1]) 
  #    #note: the growth lookup table 'wt.growth' is chosen within the function fncMoveOneStep
  
  #look up WT to use based on a fish's actual weight and p-value
  fweight[fweight < 1] = 1 #minimum value that can be looked up
  growth = watemp = vector(length = length(fpid))
  for(x in 1:length(fpid)){
    row.idx = which.min(abs(seq(0, 25, 0.05)[-1] - fsh.wt[x]))
    growth[x] = wt.growth[row.idx, fpval[x] * 100, round(fweight[x])]
    #note: the growth lookup table "wt.growth" is chosen within the function fncMoveOneStep
    watemp[x] = seq(0, 25, 0.05)[-1][row.idx]
  }
  lookup = cbind("WT" = watemp,growth)
  
  #movement probability and distance is influenced by how close fish's current temperature is to optimal temperature
  wt.opt = lookup[which.max(lookup[,"growth"]), wt.field]
  wt.diff = abs(wt.opt - fsh.wt) / wt.opt #max WT diff possible
  wt.diff[wt.diff > 1] = 1 #cap at 1 in case temps go negative
  
  #movement probability and distance is influenced by how crowded it is in the reach
  dens.opt = 1
  dens.diff = abs(dens.opt-fdens) / maxDensity4Mvmt #max density diff possible
  dens.diff[dens.diff > 1] = 1 #cap at 1 
  
  #zero-inflated part: will a fish move at all?
  #option a:
  ####probs = (dens.diff + wt.diff) / 2 #average effect of fish density standardized by maxDensity and difference in water temp from optimal
  #probs = wt.diff #based only on difference in water temp from optimal
  #zi = rbinom(n = length(fdens), size = 1, prob = probs) #1 means move, 0 means stay
  #option b:
  zi = rep(1, length(fdens))
  zi[which(abs(wt.opt-fsh.wt) <= 2)] = 0
  
  #move distance if fish is moving
  mulog = beta0 + beta1 * fdens * wt.diff
  mvdst = rlnorm(nFish, meanlog = mulog, sdlog = mvdst.sdlog)
  
  #movement distance is then adjusted according to fish size (weight)
  fweight[fweight > maxMass4Mvmt] = maxMass4Mvmt
  mvdst = mvdst * (1 + fweight / maxMass4Mvmt)
  
  #collect fields
  moveDist = cbind(fpid, fdir, fdens, fweight, fsh.wt, fsh.q, mvdst, zi)
  
  # Incorporate flow influence on movement distance, depending on direction of travel (up or downstream)
  #impede upstream movement (@@@-WIP should do this more wisely)
  moveDist[,"mvdst"][moveDist[,"fdir"] == 1] = moveDist[,"mvdst"][moveDist[,"fdir"] == 1] / moveDist[,"fsh.q"][moveDist[,"fdir"] == 1] 
  
  #assist downstream movement (@@@-WIP should do this more wisely)
  moveDist[,"mvdst"][moveDist[,"fdir"] == 0] = moveDist[,"mvdst"][moveDist[,"fdir"] == 0]*moveDist[,"fsh.q"][moveDist[,"fdir"] == 0] 
  
  
  colnames(moveDist) = c("pid", "direction", "density", "weight", wt.field, q.field, "moveDist", "zi")
  
  return(moveDist)
}

# Determines if a fish can stop before its assigned movement distance if it encounters good habitat; returns list(T/F, pStop, dist2move)
#target.temp = 15 was chosen because this is the temperature at which growth is maximized for O. mykiss adults.
fncStopEarly<- function(seg, fishPID = x, length2segBase = length2segBase, remainingDist = remainingDist, 
                        moveLength = moveLength, target.temp = 15, ssn = sno.ssn){
  
  #option 1: the probability of stopping increases as growth increases
  #via lookup of previously calculated growth values based on a fish's actual p-value and weight
  waterTemp = cbind(obs.df[obs.df$rid == seg,c("pid", wt.field)], 0); colnames(waterTemp)[3] = "growth"
  p = fish[,"pvals"][fish[,"pid"] == fishPID]
  mass = fish[,"weight"][fish[,"pid"] == fishPID]
  if(mass < 1) mass = 1 #minimum possible to look up
  
  for(w in 1:length(waterTemp[,wt.field])){
    row.idx = which.min(abs(seq(0, 25, 0.05)[-1] - waterTemp[w,wt.field]))
    waterTemp[w,"growth"] = wt.growth[row.idx, p*100, round(mass)]
    #note: the growth lookup table "wt.growth" is chosen within the function fncMoveOneStep
  }
  best.obs = which.max(waterTemp$growth)
  growth.here = waterTemp$growth[best.obs]
  if(growth.here < 0) add.amt = abs(growth.here) else add.amt = 0
  probStop1 = ((growth.here + add.amt) / (max(wt.growth[,p*100, round(mass)]) + add.amt))
  
  # #option 2: the probably of stopping increases as habitat temperature approaches 15 C
  # water.temps.in.seg = obs.df$WT[obs.df$rid == seg]
  # best.obs = which.min(target.temp - water.temps.in.seg)
  # diff.wt = abs(target.temp - water.temps.in.seg[best.obs])
  # probStop1 = 1 - (diff.wt / target.temp)
  
  #the smaller remainingDist is, the higher the probability of stopping
  obs.seg.ratio = obs.df$ratio[obs.df$rid == seg][best.obs] #seg ratio at best.obs
  obs.dist2Base = ssn@data[ssn@data[,"rid"] == seg,length.field] * obs.seg.ratio
  
  #determine position of fish relative to obs to stop at
  ifelse(length2segBase >= obs.dist2Base, moveDirection<- 1, moveDirection<- 0) 
  
  dist2move = abs(length2segBase-obs.dist2Base)
  if(remainingDist > dist2move){
    probStop2 = 1 - ((remainingDist - dist2move) / moveLength)
  } else {
    probStop2 = 1
    dist2move = remainingDist #this doesn't put them at the exact best point but they're out of distance
  }
  
  w1 = 1.5 #weight for how close to optimal growth or optimal WT
  w2 = 0.5 #weight for how close to pre-determined distance
  probStop = (w1 * probStop1 + w2 * probStop2) / 2 #weighted average
  
  pStop = c(probStop, (1 - probStop)) #prob. of stopping, prob. of continuing
  StopEarly = sample(c(TRUE,FALSE), size=1, prob =pStop) #T=stop, F=continue
  
  return(list(StopEarly, pStop, dist2move, moveDirection))
}

# Determines if there's room within the segment to move (segLength is required if moving upstream); returns T/F
fncRoom2Move<- function(seg = seg, moveDist = 1, length2segBase = length2segBase, moveDirection = 1, ssn = sno.ssn) {
  if (moveDirection == 1 & moveDist <= length2segBase) {
    return(TRUE)
  } else if (moveDirection == 0 & moveDist <= (ssn@data[ssn@data$rid == seg, length.field] - length2segBase))  {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Determines which segment a fish moves into as it encounters junctions; returns 'seg'
fncJunctionDecisions<- function(fishPID = x, sp.idx = sp.idx, seg = seg, ssn = sno.ssn, currMoveDirection = 1) {
  
  # As a fish gets to a junction, whether heading up or down
  # it looks at all possible options for movement and chooses the best one
  
  seg.orig = seg # store the original segment
  
  # We will always look at the junction upstream of a particular segment,
  # So, if a fish is heading downstream
  # it's easiest to go to the downstream segment and look at all options from there
  if (currMoveDirection == 1) seg = fncGetSegDownstream(seg)
  
  # Get conditions at downstream segment and at each of the upstream segments
  waterTemp = fncGetJunctionWTs(dat.df = obs.df[,c("pid", "rid", wt.field)], field = wt.field, seg)
  waterTemp = cbind(waterTemp, "pid" = NA)
  waterTemp[,"pid"] = fishPID
  
  #if any water Temp values are negative, make them positive for BioE calcs
  waterTemp[,wt.field][waterTemp[,wt.field] < 0] = 0.1
  
  #check that these reaches are accessible to fish & remove any that aren't
  rg.col = character.parameters[sp.idx, "spp.range"]
  idx = ssn@data[,rg.col][ssn@data$rid %in% waterTemp[,"r"]]
  if(sum(idx) <= 1) waterTemp = t(waterTemp[idx == 1,]) else waterTemp = waterTemp[idx == 1,]
  
  #Look up previously calculated growth values
  p = fish[,"pvals"][fish[,"pid"] == fishPID]
  mass = fish[,"weight"][fish[,"pid"] == fishPID]
  mass[mass <1 ] = 1 #minimum lookup value
  waterTemp = cbind(waterTemp, 0); colnames(waterTemp)[4] = "growth"
  
  for(w in 1:nrow(waterTemp)){
    row.idx = which.min(abs(seq(0, 25, 0.05)[-1] - waterTemp[w,"Value"]))
    waterTemp[w,"growth"] = wt.growth[row.idx, p * 100, round(mass)]
    #note: the growth lookup table "wt.growth" is chosen within the function fncMoveOneStep
  }
  
  # weight the options by their potential growth
  if (length(waterTemp[,"growth"]) > 1 & any(waterTemp[,"growth"] < 0)) { #if growth is negative in one or more, the worst will never be chosen (prob==0)
    waterTemp[,"growth"][which(waterTemp[,"growth"] < 0)] = 0
  }
  #set probabilities
  ifelse(sum(waterTemp[,"growth"]) > 0, probs<- waterTemp[,"growth"] / abs(sum(waterTemp[,"growth"])), probs<- rep(0.5, length(waterTemp[,"growth"])))
  # choose one
  ifelse(length(waterTemp[,"growth"]) > 1,bestSeg<- sample(waterTemp[,"r"], size = 1, prob = probs), bestSeg<- seg)
  
  # # Calculate on the fly using Wisconsin BioEn model
  # #give it nearest water temperature, fish weight (grams), and p-values
  # Input = fncGetBioEParms(spp, pred.en.dens,prey.en.dens, oxy, pff, waterTemp, 
  #                         startweights=rep(fish$weight[fish$pid==fishPID],length(waterTemp[,1])),
  #                         pvals=1) 
  # # Run bioenergetics for all fish at this time step
  # Results = BioE(Input, Constants) #run BioE code given input and constants
  # 
  # # weight the options by their potential growth
  # if (length(Results$Growth)>1 & any(Results$Growth<0)) { #if growth is negative in one or more, the worst will never be chosen (prob==0)
  #   scaledGrowth = Results$Growth+abs(min(Results$Growth))
  #   ifelse(sum(scaledGrowth)>0,probs<- scaledGrowth/sum(scaledGrowth),probs<- rep(0.5,length(scaledGrowth)))
  # } else {
  #   ifelse(sum(Results$Growth)>0,probs<- Results$Growth/abs(sum(Results$Growth)),probs<- rep(0.5,length(Results$Growth)))
  # }
  # # choose one
  # ifelse(length(Results$Growth)>1,bestSeg<- sample(waterTemp[,"r"], size=1, prob = probs),bestSeg<- seg)
  
  return(bestSeg)
}

# Determines a fish's position based on it's current circumstances; returns list(dist2move,remainingDist,length2segBase,segRatio)
fncUpdatePosition<- function(seg = seg, newSeg = seg, sp.idx = sp.idx, moveDirection = moveDirection, StopEarly = FALSE, Rm2Move = FALSE, 
                             dist2move = dist2move, remainingDist = remainingDist, length2segBase = length2segBase, ssn = sno.ssn){
  
  if(StopEarly == TRUE | Rm2Move == TRUE){ #if fish stopped in the reach, update its position
    
    if (moveDirection == 1) {
      length2segBase = length2segBase - dist2move 
    } else if (moveDirection == 0) {
      length2segBase = length2segBase + dist2move 
    }
    # Then update it's segRatio
    segRatio = fncLength2segRatio(seg, length2segBase)
    remainingDist = 0
    #seg and moveDirection stay the same
    
  } else { #move fish to next upstream or downstream reach and update its position
    
    if(moveDirection == 0){ # if heading upstream
      
      dist2move = ssn@data[ssn@data$rid == seg, length.field] - length2segBase #travel the rest of the way up the reach
      
      # if no accessible upstream reaches or if best choice is to stay in same reach,
      # then locate fish at top of existing reach facing downstream
      rg.col = character.parameters[sp.idx, "spp.range"]
      if(sum(ssn@data[,rg.col][ssn@data$rid %in% fncGetSegUpstream(seg)]) == 0 | seg == newSeg){
        segRatio = 1
        length2segBase = ssn@data[ssn@data$rid == seg, length.field] #length of current reach
        moveDirection = 1
        
      } else{ #otherwise, 
        
        #if newSeg is upstream of seg or if both seg and newSeg are upper branches
        ifelse (fncGetSegDownstream(seg) > 0,test.seg<- fncGetSegUpstream(fncGetSegDownstream(seg)),test.seg<- fncGetSegUpstream(seg)) #(no seg zero)
        if(newSeg %in% fncGetSegUpstream(seg) | newSeg %in% test.seg){ 
          # then locate fish at the base of the new reach and maintain original direction
          segRatio = 0 
          length2segBase = 0
          seg=newSeg
        } 
      }
      
    } else if(moveDirection==1){ # if heading downstream
      
      dist2move = length2segBase
      
      # if no accessible downstream reaches or if best choice is to stay in same reach,
      # then locate fish at bottom of existing reach facing upstream
      if(newSeg <= min(ssn@data$rid) | seg == newSeg){
        segRatio = 0 
        length2segBase = 0
        moveDirection = 0
        
      } else{ #otherwise, 
        
        if(newSeg %in% fncGetSegDownstream(seg)){ #if newSeg is downstream of seg
          # then locate fish at the top of the new reach and maintain original direction
          segRatio = 1
          length2segBase = ssn@data[ssn@data$rid == newSeg, length.field]
          
        } else { #i.e., newSeg and seg are both upper branches
          # then locate fish at the bottom of the new reach facing upstream
          segRatio = 0
          length2segBase = 0
          moveDirection = 0
        }
        seg = newSeg
      }
    } #end move up or down
    
    #update distance remaining (i.e., that a fish can still move this time step)
    remainingDist = remainingDist - dist2move
    
  } #end stop early or continue
  
  return(list(dist2move, remainingDist, length2segBase, segRatio, seg, moveDirection))
}

# Run one full step of movement (all fish, one time step)
fncMoveOneStep<- function(fish = salmon, sp.idx = 1, ssn = sno.ssn, plotit = "none") {
  
  #1. Get global parameter values needed, etc.
  moveLength.lo = numeric.parameters[sp.idx,"moveLength.lo"]
  moveLength.hi = numeric.parameters[sp.idx,"moveLength.hi"]
  mvdst.sdlog = numeric.parameters[sp.idx,"mvdst.sdlog"]
  maxDensity4Mvmt = numeric.parameters[sp.idx,"maxDensity4Mvmt"]
  maxMass4Mvmt = numeric.parameters[sp.idx]
  
  fish <<- fish
  
  
  #2. Get movement distance for fish that can move during this time step 
  # based on fish density, size, and proximity to cold water
  if (interspecific.competition.flag == TRUE) {fish.density = fish[,"totaldensity"]
  } else if(interspecific.competition.flag == FALSE){fish.density = fish[,"conspecificdensity"]}
  
  moveDist = fncMoveDistance(fish.density, fish[,"weight"], fish[,"pid"], fish[,"pvals"], fish[,"direction"],
                             fish[,wt.field], fish[,q.field],
                             moveLength.lo, moveLength.hi, mvdst.sdlog, maxDensity4Mvmt, maxMass4Mvmt)
  
  
  #3. Move each fish individually
  for (x in fish$pid) {
    
    set.seed(x) 
    
    #Get starting reach and location info
    seg = fish$seg[fish$pid == x]
    segLength = ssn@data[ssn@data$rid == seg,length.field]
    length2segBase = fish$length2segBase[fish$pid == x]
    segRatio = fncLength2segRatio(seg, length2segBase)
    initDirection = fish$direction[fish$pid == x]
    moveDirection = initDirection
    
    # Only move if zero-inflated part of likelihood to move is 1:
    if(moveDist[moveDist[,"pid"] == x, "zi"] == 1){
      
      remainingDist = moveLength = moveDist[,"moveDist"][moveDist[,"pid"] == x]
      dist2move = dist.moved.total = 0
      
      #\\for testing only:
      #\\temp.df = cbind.data.frame(0,"start", F, F, dist2move, dist.moved.total, remainingDist, seg, moveDirection, length2segBase, segRatio)
      #\\colnames(temp.df) = c("seg", "moveDir.orig", "Rm2Move", "StopEarly", "dist2move", "dist.moved.total", "remainingDist", "newSeg", "moveDirection", "length2segBase", "segRatio")
      
      while(remainingDist>0){ #Continue as long as the fish has not yet moved its allocated distance
        
        seg.orig = seg
        
        #Determine if there is room within the reach to move
        Rm2Move = fncRoom2Move(seg = seg, moveDist = remainingDist, length2segBase = length2segBase, moveDirection = moveDirection)
        if(Rm2Move == TRUE){
          dist2move = remainingDist
        }
        
        #Determine if fish will stop early due to good thermal habitat somewhere in the reach
        #takes precedent over room to move
        StopResult = fncStopEarly(seg = seg, fishPID = x, length2segBase = length2segBase, remainingDist = remainingDist, moveLength = moveLength)
        StopEarly = StopResult[[1]] #Did the fish stop early in good habitat? T=stop, F=continue
        if(StopEarly == TRUE) {
          dist2move = StopResult[[3]]
          moveDirection = StopResult[[4]]
        }
        
        #If fish is not stopping in the reach, determine which will be the next reach it enters
        #(only allowed to enter accessible reaches; allows fish to change direction at any segment edge, not just confluences)
        if (StopEarly == FALSE & Rm2Move == FALSE) {
          newSeg = fncJunctionDecisions(fishPID = x, sp.idx = sp.idx, seg = seg, ssn = ssn, currMoveDirection = moveDirection)
        } else {newSeg=seg}
        
        #First move the fish within the current reach and update its position
        #either somewhere within the reach, or the rest of the way up/down the reach to the junction   
        Position = fncUpdatePosition(seg = seg, newSeg = newSeg, sp.idx = sp.idx, moveDirection = moveDirection, StopEarly = StopEarly, 
                                     Rm2Move = Rm2Move, dist2move = dist2move, remainingDist = remainingDist, length2segBase = length2segBase)
        dist2move = Position[[1]]
        remainingDist = Position[[2]]
        length2segBase = Position[[3]]
        segRatio = Position[[4]]
        seg = Position[[5]]
        moveDir.orig = moveDirection
        moveDirection = Position[[6]]
        dist.moved.total = dist.moved.total + dist2move
        
        #stop if errors
        if(remainingDist < 0) browser()
        if(segRatio > 1 | segRatio < 0) browser()
        
        #\\temp.df = rbind(temp.df, cbind(seg, moveDir.orig, Rm2Move, StopEarly, dist2move, dist.moved.total, remainingDist, newSeg, moveDirection, length2segBase, segRatio)) #testing only
        
      } #end while remaining dist loop
      
      #Put info back in fish table
      fish$seg[fish$pid == x] = seg
      fish$length2segBase[fish$pid == x] = length2segBase
      fish$ratio[fish$pid == x] = segRatio
      fish$direction[fish$pid == x] = moveDirection
      #fish$upDist[fish$pid == x] = fncDist2base4fish(x) # @@@-WIP: CHECK THIS FUNCTION! fncGetUpDist seems better.
      fish$upDist[fish$pid == x] = fncGetUpDist(x) 
      fish$movedist[fish$pid == x] = dist.moved.total
      
    } #end zero-inflated part of likelihood to move
  } #end for each fish
  
  
  # 4. Update x and y coordinates
  #if this was a generated network, use straight-line distance calcs
  #if this network was created in GIS, use different approach for moving along a potentially curvy reach
  ifelse(length(grep("network-", netnm)) > 0,
         locs <- ddply(fish, .(pid), function(x) data.frame(fncGetXY(x$seg, x$ratio))), # straight lines
         locs <- ddply(fish, .(pid), function(x) data.frame(fncGetXY.arc(x$seg, x$ratio)))) # curvy lines
  fish$xloc = locs$xloc
  fish$yloc = locs$yloc
  
  
  # 5. Put updated data into SSN
  # data table
  ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.data$upDist[salmon.alive.emerge.index] = fish$upDist
  ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.data$ratio[salmon.alive.emerge.index] = fish$ratio
  ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.data[,xlat][salmon.alive.emerge.index] = fish$xloc
  ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.data[,ylon][salmon.alive.emerge.index] = fish$yloc
  ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.data$rid[salmon.alive.emerge.index] = fish$seg
  # note: recall that 'preds' is in the 1st position, so the first fish is in the second position, etc.
  
  # coordinates
  # note: subtract 1 because of shifted indexing (was re-indexed in data but not in coordinates)
  ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.coords[,1][salmon.alive.emerge.index - 1] = fish$xloc 
  ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.coords[,2][salmon.alive.emerge.index - 1] = fish$yloc
  
  # 6. Create plot (optional) showing new fish locations
  if(plotit != "none"){
    if(plotit != "screen"){png(paste0(plotDir, "/", plotit, ".png"), width=6, height=6, units="in", res=300)}
    #plot(ssn, lwdLineCol = "addfunccol", lwdLineEx = 8, lineCol="blue", xlab="X", ylab="Y", cex=0)
    #plot(ssn, PredPointsID = ssn@predpoints@ID[[sp.idx + 1]], add = TRUE, cex = 0.7)
    plot(basin,col="gray80",border=NA, main = "Chinook salmon")
    plot(streams, col="darkgray", add = TRUE)
    points(ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.coords)
    points(ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.coords[salmon.alive.emerge.index - 1,], col = 2)
    
    if(plotit != "screen"){dev.off()}
  }
  
  return(list(fish, ssn))
}
