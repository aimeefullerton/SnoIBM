#=== RUN SIMULATION ============================================================

# Make temporary array to hold fish data for this time step (numFish x tt)
salmon.array.temporary = array(NA, dim = dim(salmon.array)[1:3])
if(SecondSpecies == TRUE) fish_other.array.temporary = array(NA, dim = dim(fish_other.array)[1:3])

# Loop over each day (dd) and time step (tt) in an iteration (iter)
ii= 1
day = 1 # initialize day counter for spawning
for(dd in 1:length(dat.idx)){
  for(tt in c(6, 18)){ #6am, 6pm
    thetitle = fncGetTitle(dat.idx[dd], tt)
    cat(iter, ": ", thetitle, "\n")
    #cat(sum(salmon$survive[salmon$emrg == -1]), "spawned;", sum(salmon$survive[salmon$emrg == 1]), "emerged", "\n")
    cat(sum(salmon$survive[salmon$survive == 2])/2, "smolted;", sum(salmon$survive[salmon$survive == 1]), "alive in stream", "\n")
    if(sum(salmon$survive[salmon$survive == 2]) > 0) cat("smolt weights (quantile):", quantile(salmon$weight[salmon$survive == 2]), "\n")
    if(sum(salmon$survive[salmon$emrg == 1]) > 0) cat("alive weights (quantile):", quantile(salmon$weight[salmon$survive == 1 & salmon$emrg == 1]), "\n")
    
    
    # 1. UPDATE HABITAT: Get predicted stream temperatures and flow
    
    # Load flow and temperature for this date and time
    if("Q" %in% colnames(ssn@predpoints@SSNPoints[[1]]@point.data)){
      ssn = fncUnloadWQ("Q",ssn) # unload if it's already loaded
    }
    if(network == "rbm") ssn = fncLoadWQdata(type = "Q", dat.df = Q.df, thedate = dat.idx[dd], thetime = tt, ssn = ssn, plotit = FALSE)
    if(network == "nhd1") ssn@predpoints@SSNPoints[[1]]@point.data$Q = 0
    
    if("WT" %in% colnames(ssn@predpoints@SSNPoints[[1]]@point.data)){
      ssn = fncUnloadWQ("WT",ssn) # unload if it's already loaded
    }
    ssn = fncLoadWQdata(type = "WT", dat.df = WT.df, thedate = dat.idx[dd], thetime = tt, ssn = ssn, plotit = FALSE)
    
    # Extract data frame that has had flow and temperautre loaded
    wq.df = getSSNdata.frame(ssn,"preds")
    
    # changing factor/character fields to numeric, if necessary
    for(field in c("rid", so.field, wt.field, q.field)){
      if (is.factor(wq.df[,field])) {
        wq.df[,field] = as.numeric(levels(wq.df[,field])[as.numeric(wq.df[,field])])
      }
      if (is.character(wq.df[,field])) {
        wq.df[,field] = as.numeric(wq.df[,field])
      }
    }
    
    # Plot updated water temperature/flow and fish locations
    # (only for this iteration due to storage constraints)
    if(plot.flag == TRUE & iter == plot.iter){
      
      png(paste0(mydir, "/", imageDir, "/", dat.idx[dd], "-", sprintf("%03d", tt), ".png"), width = 9, height = 6, units = "in", res = 150)
      par(mar = c(1, 0, 4, 0))
      
      # plot background
      if (outline) {
        plot(basin,col="gray80",border=NA)
      } else {
        plot(basin,col=grays,ext=ex,axes=FALSE, xlab = "", ylab = "", box = FALSE, legend = FALSE)
      }
      
      plot(streams, col="darkgray", add = TRUE)
      
      #plot water temperature/flow for dd & tt as colored stream lines:
      if(network == "rbm"){
        #ssn@data$Q = ssn@predpoints@SSNPoints[[1]]@point.data$Q[ssn@data$rid == ssn@predpoints@SSNPoints[[1]]@point.data$rid]
        ssn@data$WT = ssn@predpoints@SSNPoints[[1]]@point.data$WT[ssn@data$rid == ssn@predpoints@SSNPoints[[1]]@point.data$rid]
        #for(n in 1:length(cb2)) {ssn@data$Q.color[ssn@data$Q >= left2[n] & ssn@data$Q <= rght2[n]]= n}
        for(n in 1:length(cb)) {ssn@data$WT.color[ssn@data$WT >= left[n] & ssn@data$WT <= rght[n]]= n}
        
        for (i in 1:length(ssn@lines)) {
          for (j in 1:length(ssn@lines[[i]])) {
            lines(ssn@lines[[i]]@Lines[[j]]@coords, col = cb[ssn@data[i,"WT.color"]], lwd = 10 * (ssn@data[i, "afvArea"] + 0.1))
          }
        }
      }
      if(network == "nhd1"){
        datap = wq.df[,c("rid", wt.field)]
        datap2 = tapply(datap[,wt.field], datap$rid, mean, na.rm = TRUE)
        datap3 = cbind.data.frame("rid" = as.numeric(names(datap2)), "WT" = datap2)
        
        linedata = ssn@data
        rownames(linedata) = NULL
        linedata$sort.order = as.integer(rownames(linedata))
        if("WT" %in% colnames(linedata)) linedata = linedata[,-which(colnames(linedata) == "WT")] #remove WT field
        linedata2 = merge(linedata, datap3, by = "rid", all.x = TRUE) 
        linedata2 = linedata2[order(linedata2$sort.order),]
        for(n in 1:length(cb)) {
          linedata2$col.class[linedata2[,wt.field] >= left[n] & linedata2[,wt.field] <= rght[n]] = n
        }
        ssn@data = linedata2
        for (i in 1:length(ssn@lines)) {
          for (j in 1:length(ssn@lines[[i]])) {
            lines(ssn@lines[[i]]@Lines[[j]]@coords, col = cb[ssn@data[i,"col.class"]], lwd = 5*(ssn@data[i,"afvFlow"] + 0.4))
          }
        }
      }
      
      # Plot fish locations for dd & tt
      open.salmon = salmon$pid[salmon$emrg == 0 & salmon$survive == 1]
      spawned.salmon = salmon$pid[salmon$emrg == -1 & salmon$survive == 1]
      emerged.salmon = salmon$pid[salmon$emrg == 1 & salmon$survive == 1]
      
      if(SecondSpecies == TRUE){
        alive.fish_other = fish_other$pid[fish_other$survive == 1]
        points(ssn@predpoints@SSNPoints[[fish_other.id]]@point.coords[,1][ssn@predpoints@SSNPoints[[fish_other.id]]@point.data$pid %in% alive.fish_other],
               ssn@predpoints@SSNPoints[[fish_other.id]]@point.coords[,2][ssn@predpoints@SSNPoints[[fish_other.id]]@point.data$pid %in% alive.fish_other],
               col = "gray50", pch = 15, cex = 0.5)
      }
      
      # salmon spawning sites
      points(ssn@predpoints@SSNPoints[[salmon.id]]@point.coords[,1][ssn@predpoints@SSNPoints[[salmon.id]]@point.data$pid %in% open.salmon],
             ssn@predpoints@SSNPoints[[salmon.id]]@point.coords[,2][ssn@predpoints@SSNPoints[[salmon.id]]@point.data$pid %in% open.salmon],
             col = 1, pch = 1, cex =0.5)
      
      # incubating salmon eggs
      points(ssn@predpoints@SSNPoints[[salmon.id]]@point.coords[,1][ssn@predpoints@SSNPoints[[salmon.id]]@point.data$pid %in% spawned.salmon],
             ssn@predpoints@SSNPoints[[salmon.id]]@point.coords[,2][ssn@predpoints@SSNPoints[[salmon.id]]@point.data$pid %in% spawned.salmon],
             col = 2, pch = 20, cex = 0.8)
      
      # emerged salmon
      points(ssn@predpoints@SSNPoints[[salmon.id]]@point.coords[,1][ssn@predpoints@SSNPoints[[salmon.id]]@point.data$pid %in% emerged.salmon],
             ssn@predpoints@SSNPoints[[salmon.id]]@point.coords[,2][ssn@predpoints@SSNPoints[[salmon.id]]@point.data$pid %in% emerged.salmon],
             col = 4, pch = 20, cex = 0.8)
      
      # all points: points(ssn@predpoints@SSNPoints[[id]]@point.coords[,1],ssn@predpoints@SSNPoints[[id]]@point.coords[,2],col=1,pch=20,cex=0.8)
      
      # track a few individuals:
      if(track.individuals == TRUE){
        fish.to.track = 1:5 #sample(salmon$pid, 5)
        i = 10
        for(f in fish.to.track){
          points(ssn@predpoints@SSNPoints[[salmon.id]]@point.coords[,1][ssn@predpoints@SSNPoints[[salmon.id]]@point.data$pid == f],
                 ssn@predpoints@SSNPoints[[salmon.id]]@point.coords[,2][ssn@predpoints@SSNPoints[[salmon.id]]@point.data$pid == f],
                 col = i, pch = 20, cex = 0.8)
          i= i + 1
        }
        rm(i)
      }
      
      # Add legend
      leglabs = paste(left, "to", rght)
      legend("right", legend = leglabs, title=expression("Temperature "~degree(C)),bty = "n", pch = 19, col = cb, cex = 0.8)
      
      # Add scale bar
      rect(xleft = ex[1] + 5000, ybottom = ex[3] + 3000, xright = ex[1] + 7500, ytop = ex[3] + 3500)
      rect(xleft = ex[1] + 7500, ybottom = ex[3] + 3000, xright = ex[1] + 10000, ytop= ex[3] + 3500, col = 1)
      rect(xleft = ex[1] + 10000, ybottom = ex[3] + 3000, xright = ex[1] + 12500, ytop = ex[3] + 3500)
      rect(xleft = ex[1] + 12500, ybottom = ex[3] + 3000, xright = ex[1] + 15000, ytop = ex[3] + 3500, col = 1)
      segments(x0 = ex[1] + 5000, y0 = ex[3] + 3000, x1 = ex[1] + 5000, y1 = ex[3] + 2500)
      segments(x0 = ex[1] + 10000, y0=ex[3] + 3000, x1 = ex[1] + 10000, y1 = ex[3] + 2500)
      segments(x0 = ex[1] + 15000, y0 = ex[3] + 3000, x1 = ex[1] + 15000, y1 = ex[3] + 2500)
      text(x = ex[1] + 5000, y = ex[3] + 1500, "0", cex = 0.8)
      text(x = ex[1] + 10000, y = ex[3] + 1500, "5", cex = 0.8)
      text(x = ex[1] + 15000, y = ex[3] + 1500, "10", cex = 0.8)
      text(x = ex[1] + 18000, y = ex[3] + 1500, "km", cex = 0.8) 
      
      # Add north arrow
      arrows(ex[1] + 2000, ex[3] + 2300, ex[1] + 2000, ex[3] + 4000, length = 0.1, lwd = 5)
      text(ex[1] + 2000, ex[3] + 1000, "N")
      
      # Add title
      mtext(thetitle, side = 3, line=2, cex = 1.3)
      
      dev.off()
    } # end plotting for the selected iter
    
    
    # 2. SPAWN salmon
    
    # If any potential redd locations do not yet have a spawned fish,...
    if (any(salmon$emrg == 0)){
      spawn.firstdate = parameters["salmon", "spawn.firstdate"]
      spawn.init = as.Date(paste0(cs - 1, "-", spawn.firstdate))
      # and if the date is at or past the beginning of the spawning window,...
      if(dat.idx[dd] >= spawn.init){
        # and if spawning will be modeled as variable dates, not fixed,...
        if(spawn.date.variable == TRUE){
          # and if the date is not past the end of the variable spawning window,...
          if(day <= length(salmon.spawn.times)){
            # then get an index of which locations to spawn, from species.spawn.times[day]
            # and limited to only what's left unspawned
            spawn.index = sample(salmon$pid[salmon$emrg == 0], min(length(salmon$pid[salmon$emrg == 0]), salmon.spawn.times[day]))
            # mark these locations as spawned (-1)
            salmon$emrg[salmon$pid %in% spawn.index] = -1
            # and record the date that spawning took place for this fish
            salmon$dateSp[salmon$dateSp == day1 & salmon$emrg == -1] = strptime(dat.idx[dd], format = "%Y-%m-%d")
          }
          #move the day counter forward through the spawning window:
          day = day + 1 
        } else{ 
          # if spawning date is fixed, spawn all fish on the initial spawn date (spawn.init date):
          salmon$emrg = -1
          salmon$dateSp = strptime(dat.idx[dd], format = "%Y-%m-%d")
        } #end variable spawning condition
      } #end spawn window condition
    } #end checking for unspawned locations
    
    
    # 3. INCUBATE salmon & accumulate thermal exposure
    
    # Assign closest water temperature to salmon
    salmon$WT[salmon$survive == 1 & salmon$seg > 0] = fncGetNearestAttribute(wq.df[,c("pid", "rid", "ratio", wt.field)], salmon[salmon$survive == 1 & salmon$seg > 0, c("pid", "seg", "ratio")], ssn)[,wt.field]
    
    # Accumulate thermal units (TUs) While salmon are spawned or emerged
    salmon$TU[salmon$survive == 1] = salmon$TU[salmon$survive == 1] + salmon$WT[salmon$survive == 1] * 0.5 #halved to account for half-day time step
    
    # Assign closest flow to salmon
    if(network == "rbm") salmon$Q[salmon$survive == 1 & salmon$seg > 0] = fncGetNearestAttribute(wq.df[,c("pid", "rid", "ratio", q.field)], salmon[salmon$survive == 1 & salmon$seg > 0,c("pid", "seg", "ratio")], ssn)[,q.field]
    
    if(SecondSpecies == TRUE){
      fish_other$WT[fish_other$survive == 1 & fish_other$seg > 0] = fncGetNearestAttribute(wq.df[,c("pid", "rid", "ratio", wt.field)], fish_other[fish_other$survive == 1 & fish_other$seg > 0, c("pid", "seg", "ratio")], ssn)[,wt.field]
      fish_other$TU[fish_other$survive == 1] = fish_other$TU[fish_other$survive == 1] + fish_other$WT[fish_other$survive == 1] * 0.5 #halved to account for half-day time step
      fish_other$Q[fish_other$survive == 1 & fish_other$seg > 0] = fncGetNearestAttribute(wq.df[,c("pid", "rid", "ratio", q.field)], fish_other[fish_other$survive == 1 & fish_other$seg > 0,c("pid", "seg", "ratio")], ssn)[,q.field]
    }
    
    
    # 4. EMERGE salmon
    # Alevins emerge after reaching an accumulated thermal unit (TU) threshold
    if (any(salmon$emrg == -1)) {
      ATU.crit = parameters["salmon", "ATU.crit"]
      salmon$emrg[salmon$emrg == -1 & salmon$TU >= ATU.crit] = 1
      salmon$dateEm[salmon$dateEm == day1 & salmon$emrg == 1] = strptime(dat.idx[dd], format="%Y-%m-%d")
    }
    
    
    # 5. MOVE FISH
    # After emergence, assess probability of movement and move fish if appropriate
    
    # 5a. Move salmon individually
    
    # index for which salmon have emerged and can grow, and are still alive
    salmon.alive.emerge.index = salmon$pid[salmon$emrg == 1 & salmon$survive == 1]
    
    if(length(salmon.alive.emerge.index) > 0){
      
      # Set which growth lookup table to use
      sp.idx = 1
      wt.growth = wt.growth.salmon
      fish = salmon[salmon.alive.emerge.index,]
      
      # Bias movement direction downstream as fish grows larger (100% downstream drive if larger than smolt.mass)
      # but after smolt.date.cutoff, fish loses the outmigration drive and becomes a yearling
      smolt.date.cutoff = as.Date(paste0(cs, "-", parameters[1,"smolt.date.cutoff"]))
      smolt.mass = parameters[1, "smolt.mass"]
      if(dat.idx[dd] > smolt.date.cutoff){
        om.prob = (fish$weight / smolt.mass) * (1 - as.numeric(dat.idx[dd] - smolt.date.cutoff) / as.numeric(dat.idx[length(dat.idx)] - smolt.date.cutoff))
      } else{
        om.prob = fish$weight / smolt.mass
      }
      # constrain between 0.5 (equal chance of moving up or down) and 1 (maximum downstream drive)
      om.prob[om.prob > 1] = 1; om.prob[om.prob < 0.5] = 0.5
      # assign fish initial direction based on outmigration probability
      fish$direction = rbinom(n = length(om.prob), size = 1, prob = om.prob)
      
      # Run movement decision functions
      #1. Get movement distance for fish that can move during this time step
      moveDist = fncMoveDistance(fish, sp.idx, ssn)
      
      #2. Move fish individually (one fish, one time step)
      moved = lapply(fish[,"pid"], function(x) fncMoveIndividual(fpid = x, fish, sp.idx, ssn))
      fish = do.call(rbind, moved)
      
      #3. Update x and y coordinates
      # if this was a generated network, use straight-line distance calcs
      # if this network was created in GIS, use different approach for moving along a potentially curvy reach
      ifelse(length(grep("network-", netnm)) > 0, 
             locs<- ddply(fish, .(pid), function(x) data.frame(fncGetXY(x$seg, x$ratio, ssn))), 
             locs<- ddply(fish, .(pid), function(x) data.frame(fncGetXY.arc(x$seg, x$ratio, ssn))))
      fish$xloc<- locs$xloc
      fish$yloc<- locs$yloc
      
      #4. Update data in SSN
      # data table
      ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.data$upDist[salmon.alive.emerge.index] = fish$upDist
      ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.data$ratio[salmon.alive.emerge.index] = fish$ratio
      ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.data[,xlat][salmon.alive.emerge.index] = fish$xloc
      ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.data[,ylon][salmon.alive.emerge.index] = fish$yloc
      ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.data$rid[salmon.alive.emerge.index] = fish$seg
      # note: sp.idx + 1 because 'preds' is in the 1st position, so the first fish is in the second position, etc.
      
      # coordinates
      ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.coords[,1][salmon.alive.emerge.index] = fish$xloc 
      ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.coords[,2][salmon.alive.emerge.index] = fish$yloc
      
      # # Create optional plot showing new fish locations
      # if(plotit != "none"){
      #   if(plotit != "screen"){png(paste0(plotDir,"/",plotit,".png"), width = 6, height = 6, units = "in", res = 300)}
      #   plot(basin,col="gray80",border=NA, main = "")
      #   plot(streams, col="darkgray", add = TRUE)
      #   points(ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.coords)
      #   points(ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.coords[salmon.alive.emerge.index,], col = 2)
      #   if(plotit != "screen"){dev.off()}
      # }
      
      salmon[salmon.alive.emerge.index,] = fish
      rm(fish)
      
      # Filter out fish that have outmigrated as subyearlings (count segs 0 - 5 as the watershed mouth)
      salmon$survive[salmon$seg <= 5] = 2
      salmon.alive.emerge.index = salmon$pid[salmon$survive == 1 & salmon$seg > 0]
    }
    
    # 5b. Move fish_other individually
    
    if(SecondSpecies == TRUE){
      # index for which fish_other are alive
      fish_other.alive.index = fish_other$pid[fish_other$survive == 1]
      
      if(length(fish_other.alive.index) > 0){
        
        # set which growth lookup table to use based on species 'spp'
        sp.idx = 2
        wt.growth = wt.growth.fish_other
        fish = fish_other[fish_other.alive.index,]
        
        # Run movement decision functions
        # Get movement distance for fish that can move during this time step
        moveDist = fncMoveDistance(fish, sp.idx, ssn)
        
        # Move fish individually (one fish, one time step)
        moved = lapply(fish[,"pid"], function(x) fncMoveIndividual(fpid = x, fish, sp.idx, ssn))
        fish = do.call(rbind, moved)
        
        # Update x and y coordinates
        # if this was a generated network, use straight-line distance calcs
        # if this network was created in GIS, use different approach for moving along a potentially curvy reach
        ifelse(length(grep("network-", netnm)) > 0, 
               locs<- ddply(fish, .(pid), function(x) data.frame(fncGetXY(x$seg, x$ratio, ssn))), 
               locs<- ddply(fish, .(pid), function(x) data.frame(fncGetXY.arc(x$seg, x$ratio, ssn))))
        fish$xloc<- locs$xloc
        fish$yloc<- locs$yloc
        
        # Update data in SSN
        # data table
        ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.data$upDist[fish_other.alive.index] = fish$upDist
        ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.data$ratio[fish_other.alive.index] = fish$ratio
        ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.data[,xlat][fish_other.alive.index] = fish$xloc
        ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.data[,ylon][fish_other.alive.index] = fish$yloc
        ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.data$rid[fish_other.alive.index] = fish$seg
        # note: sp.idx + 1 because 'preds' is in the 1st position, so the first fish is in the second position, etc.
        
        # coordinates
        ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.coords[,1][fish_other.alive.index] = fish$xloc 
        ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.coords[,2][fish_other.alive.index] = fish$yloc
        
        # # Create optional plot showing new fish locations
        # if(plotit != "none"){
        #   if(plotit != "screen"){png(paste0(plotDir,"/",plotit,".png"), width = 6, height = 6, units = "in", res = 300)}
        #   plot(basin,col="gray80",border=NA, main = "")
        #   plot(streams, col="darkgray", add = TRUE)
        #   points(ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.coords)
        #   points(ssn@predpoints@SSNPoints[[sp.idx + 1]]@point.coords[fish_other.alive.index,], col = 2)
        #   if(plotit != "screen"){dev.off()}
        # }
        
        fish_other[fish_other.alive.index,] = fish
        rm(fish)
        
      }
      fish_other.alive.index = fish_other$pid[fish_other$survive == 1 & fish_other$seg > 0] # index for which fish_other are alive
    }
    
    
    # 6. RE-ASSESS HABITAT QUALITY (to incorporate changes after movement into growth calcs)
    
    # 6a. Get nearest water temperature (& flow)
    # Update these fields in fish dataframes
    salmon$WT[salmon.alive.emerge.index] = fncGetNearestAttribute(wq.df[,c("pid", "rid", "ratio", wt.field)], salmon[salmon.alive.emerge.index,c("pid", "seg", "ratio")], ssn)[,wt.field]
    if(network == "rbm"){
      salmon$Q[salmon.alive.emerge.index] = fncGetNearestAttribute(wq.df[,c("pid", "rid", "ratio", q.field)], salmon[salmon.alive.emerge.index,c("pid","seg","ratio")], ssn)[,q.field] 
    }
    
    if(SecondSpecies == TRUE){
      fish_other$WT[fish_other.alive.index] = fncGetNearestAttribute(wq.df[,c("pid", "rid", "ratio", wt.field)], fish_other[fish_other.alive.index,c("pid","seg","ratio")], ssn)[,wt.field] 
      if(network == "rbm"){
        fish_other$Q[fish_other.alive.index] = fncGetNearestAttribute(wq.df[,c("pid", "rid", "ratio", q.field)], fish_other[fish_other.alive.index,c("pid","seg","ratio")], ssn)[,q.field] 
      }
    }
    
    # 6b. Kill (freeze) fish if they have experienced too-cold overwinter temperatures - NOT FUNCTIONAL
    #pids.to.freeze = fish_other.wt.nearest[,"pid"][fish_other.wt.nearest[,"WT"] < parameters[2,"freeze.temp"]]
    #pids.to.freeze = pids.to.freeze * rbinom(n = length(pids.to.freeze), size = 1, prob = (1 -parameters[2,"freeze.prob"])) #Percent chance of dying below freeze.temp
    #pids.to.freeze = pids.to.freeze[pids.to.freeze > 0]
    ## could add this or similar later: fish_other$survive[fish_other$pid%in%pids.to.freeze & fish_other$TU < freeze.TU] = 0 #only kill fish_other if it hasn't accumulated sufficient thermal units
    #fish_other.alive.index = fish_other$pid[fish_other$survive == 1] #reset this index
    
    # 6c. Update fish densities to incorporate fish that just moved into density estimates
    if(SecondSpecies == FALSE) {fish_other = NULL; fish_other.alive.index = NULL}
    density = as.data.frame(fncFishDensity(salmon, fish_other, salmon.alive.emerge.index, fish_other.alive.index, ssn = ssn))
    namedvec = density$conspecific.bio.density; names(namedvec) = paste0("x",density$seg)
    salmon$conspecificdensity = fncGetValue(mykey = paste0("x",salmon$seg), mylookupvector = namedvec)
    namedvec = density$other.bio.density; names(namedvec) = paste0("x",density$seg)
    salmon$otherdensity = fncGetValue(mykey = paste0("x",salmon$seg), mylookupvector = namedvec)
    salmon$totaldensity = salmon$conspecificdensity + salmon$otherdensity
    
    if(SecondSpecies == TRUE){
      namedvec = density$conspecific.bio.density; names(namedvec) = paste0("x",density$seg)
      fish_other$conspecificdensity = fncGetValue(mykey = paste0("x",fish_other$seg), mylookupvector = namedvec)
      namedvec = density$other.bio.density; names(namedvec) = paste0("x",density$seg)
      fish_other$otherdensity = fncGetValue(mykey = paste0("x",fish_other$seg), mylookupvector = namedvec)
      fish_other$totaldensity = fish_other$conspecificdensity + fish_other$otherdensity
    }
    rm(density, namedvec)
    
    # 6d. Update available ration after fish have moved and after accounting for density
    # (ration linearly decreases with fish density at the new location)
    
    # For reaches with salmon in them:
    # choose which density column based on scenario
    ifelse(interspecific.competition.flag == TRUE, fdens<- salmon$totaldensity, fdens<- salmon$conspecificdensity)
    
    # force high densities to cap out at this parameter value
    fdens[fdens > parameters[1, "maxDensity4Growth"]] = parameters[1, "maxDensity4Growth"]
    
    # calculate density effect
    density.effect = fncRescale((1 - c(fdens, 0.01, parameters[1, "maxDensity4Growth"])), c((0.5), 1))
    density.effect = density.effect[-c(length(density.effect), (length(density.effect) -1 ))] #remove the last 2 values that were used to standardize the range
    density.effect = cbind("rid" = salmon$seg, "pid" = salmon$pid, density.effect)
    
    # Update ration in SSN & wq.df based on fish density from above
    namedvec = wq.df$ration_base; names(namedvec) = paste0("x", wq.df$rid)
    ration1 = fncGetValue(paste0("x", wq.df$rid), namedvec)
    namedvec = tapply(density.effect[,"density.effect"], density.effect[,"rid"], mean, na.rm = TRUE); names(namedvec) = paste0("x",names(namedvec))
    dns.eff1 = fncGetValue(paste0("x", wq.df$rid), namedvec)
    updated.ration = ration1 * dns.eff1
    updated.ration = updated.ration[1:nwq]
    ssn@predpoints@SSNPoints[[1]]@point.data$ration[!is.na(updated.ration)] = 
      wq.df$ration[!is.na(updated.ration)] = updated.ration[!is.na(updated.ration)]
    
    # For reaches with fish_other in them:
    if(SecondSpecies == TRUE){
      # choose which density column based on scenario
      ifelse(interspecific.competition.flag == TRUE, fdens<- fish_other$totaldensity, fdens<- fish_other$conspecificdensity)
      
      # force high densities to cap out at this parameter value
      fdens[fdens > parameters[1, "maxDensity4Growth"]] = parameters[1, "maxDensity4Growth"]
      
      # calculate density effect
      density.effect = fncRescale((1 - c(fdens, 0.01, parameters[1, "maxDensity4Growth"])), c((0.5), 1))
      density.effect = density.effect[-c(length(density.effect), (length(density.effect) -1 ))] #remove the last 2 values that were used to standardize the range
      density.effect = cbind("rid" = fish_other$seg, "pid" = fish_other$pid, density.effect)
      
      # Update ration in SSN & wq.df based on fish density from above
      namedvec = wq.df$ration_base; names(namedvec) = paste0("x", wq.df$rid)
      ration1 = fncGetValue(paste0("x", wq.df$rid), namedvec)
      namedvec = tapply(density.effect[,"density.effect"], density.effect[,"rid"], mean, na.rm = TRUE); names(namedvec) = paste0("x",names(namedvec))
      dns.eff1 = fncGetValue(paste0("x", wq.df$rid), namedvec)
      updated.ration = ration1 * dns.eff1
      updated.ration = updated.ration[1:nwq]
      ssn@predpoints@SSNPoints[[1]]@point.data$ration[!is.na(updated.ration)] = 
        wq.df$ration[!is.na(updated.ration)] = updated.ration[!is.na(updated.ration)]
    }
    
    # Update ration in fish dataframes
    salmon$ration[salmon.alive.emerge.index] = fncGetNearestAttribute(wq.df[,c("pid", "rid", "ratio", "ration")], salmon[salmon.alive.emerge.index,c("pid","seg","ratio")], ssn)[,"ration"] 
    
    if(SecondSpecies == TRUE){
      fish_other$ration[fish_other.alive.index] = fncGetNearestAttribute(wq.df[,c("pid", "rid", "ratio", "ration")], fish_other[fish_other.alive.index,c("pid","seg","ratio")], ssn)[,"ration"] 
    }
    
    # 7. GROW FISH. (calculated using the Wisconsin Bioenergetics model equations)
    
    # salmon:
    if(length(salmon.alive.emerge.index) > 0){
      # Get parameters and inputs for Bioenergetics model
      # provide nearest water temperature, fish weight (grams), and pvals or ration
      salmon.input= fncGetBioEParms(parameters[1,"spp"], 
                                    parameters[1,"pred.en.dens"], parameters[1,"prey.en.dens"],
                                    parameters[1,"oxy"], parameters[1,"pff"], 
                                    wt.nearest = salmon[salmon.alive.emerge.index,c("pid",wt.field)], 
                                    startweights = salmon$weight[salmon.alive.emerge.index],
                                    pvals = rep(-9, nrow(salmon[salmon.alive.emerge.index,])), 
                                    ration = salmon$ration[salmon.alive.emerge.index])
      
      # Run bioenergetics for all fish at this 12-hour time step
      salmon.results = BioE(salmon.input, salmon.constants)
      
      # Put results back into fish dataframe
      # note: splitting results in half because bioenergetics model operates on 
      # a daily time step and we are using a half-day time step
      
      # instantaneous growth (units are g/d; halved and divided by fish weight to get g/g/12h)
      salmon$growth[salmon.alive.emerge.index]= t(salmon.results$Growth / 2) / salmon$weight[salmon.alive.emerge.index]
      # cumulative growth
      salmon$weight[salmon.alive.emerge.index] = salmon$weight[salmon.alive.emerge.index] + t(salmon.results$Growth / 2)
      # instantaneous consumption (units are g/g/d; halved to get g/g/12h)
      salmon$consInst[salmon.alive.emerge.index] = t(salmon.results$Consumption / 2)
      # cumulative amount consumed
      salmon$consCum[salmon.alive.emerge.index] = salmon$consCum[salmon.alive.emerge.index] + t(salmon.results$Consumption / 2)
      # bioenergetic p-values
      salmon$pvals[salmon.alive.emerge.index] = salmon.results$pp
    }
    
    # fish_other:
    if(SecondSpecies == TRUE){
      if(length(fish_other.alive.index) > 0){
        # Get parameters and inputs for Bioenergetics model
        # provide nearest water temperature, fish weight (grams), and pvals or ration
        fish_other.input= fncGetBioEParms(parameters[2,"spp"], 
                                          parameters[2,"pred.en.dens"], parameters[2,"prey.en.dens"],
                                          parameters[2,"oxy"], parameters[2,"pff"], 
                                          wt.nearest = fish_other[fish_other.alive.index,c("pid",wt.field)], 
                                          startweights = fish_other$weight[fish_other.alive.index],
                                          pvals = rep(-9, nrow(fish_other[fish_other.alive.index,])), 
                                          ration = fish_other$ration[fish_other.alive.index])
        
        # Run bioenergetics for all fish at this 12-hour time step
        fish_other.results = BioE(fish_other.input, fish_other.constants)
        
        # Put results back into fish dataframe
        # note: splitting results in half because bioenergetics model operates on 
        # a daily time step and we are using a half-day time step
        
        # instantaneous growth (units are g/d; halved and divided by fish weight to get g/g/12h)
        fish_other$growth[fish_other.alive.index] = t(fish_other.results$Growth / 2) / fish_other$weight[fish_other.alive.index]
        # cumulative growth
        fish_other$weight[fish_other.alive.index] = fish_other$weight[fish_other.alive.index] + t(fish_other.results$Growth / 2)
        # instantaneous consumption (units are g/g/d; halved to get g/g/12h)
        fish_other$consInst[fish_other.alive.index] = t(fish_other.results$Consumption / 2)
        # cumulative amount consumed
        fish_other$consCum[fish_other.alive.index] = fish_other$consCum[fish_other.alive.index] + t(fish_other.results$Consumption / 2)
        # bioenergetic p-values
        fish_other$pvals[fish_other.alive.index] = fish_other.results$pp
      }
    }
    
    # Record the date fish smolt (oumigrate as subyearling)
    salmon$dateOm[salmon$dateOm == day1 & salmon$survive == 2] = strptime(dat.idx[dd], format = "%Y-%m-%d")
    
    
    # 8. PREDATION
    # If alive, emerged salmon and alive fish_other are in the same segment,
    # seg is at least as warm as temp.threshold,
    # fish_other has max.pred.prob of catching a salmon,
    # each salmon's chances of being eaten decrease as the number
    # of salmon in that segment increase,
    # and 75% of fish have emerged
    if(SecondSpecies == FALSE) interspecific.predation.flag = FALSE
    if (interspecific.predation.flag == TRUE) {
      #if (length(salmon$emrg[salmon$emrg == 1]) >= trunc(nrow(salmon) * 0.75)) {
      salmon$survive = fncPredation(prey.survive = salmon$survive,
                                    prey.emerge  = salmon$emrg,
                                    prey.seg     = salmon$seg,
                                    prey.weight  = salmon$weight,
                                    prey.dist    = salmon$length2segBase,
                                    pred.survive = fish_other$survive,
                                    pred.temp    = fish_other$WT,
                                    pred.seg     = fish_other$seg,
                                    pred.weight  = fish_other$weight,
                                    pred.dist    = fish_other$length2segBase,
                                    pred.move    = fish_other$movedist,
                                    pred.temp.crit   = parameters[2,"pred.temp.crit"],
                                    pred.mass.crit = parameters[2,"pred.mass.crit"],
                                    max.pred.prob    = parameters[2,"max.pred.prob"])
      salmon.alive.emerge.index = salmon$pid[salmon$emrg == 1 & salmon$survive == 1]
      #}
    }
    
    # 9. OTHER MORTALITY
    # If survival.flag is on and 75% of fish have emerged,
    # emerged fish are susceptible to mortality
    if (survival.flag == TRUE) {
      #if (length(salmon$emrg[salmon$emrg == 1]) >= trunc(nrow(salmon) * 0.75)) {
      salmon$survive[salmon.alive.emerge.index] = fncSurvive(df = salmon[salmon.alive.emerge.index, c("weight","growth")], minprob = parameters[1,"min.survive.prob"])
      #}
      if(SecondSpecies == TRUE){
        fish_other$survive[fish_other.alive.index] = fncSurvive(df = fish_other[fish_other.alive.index, c("weight","growth")], minprob=parameters[2,"min.survive.prob"])
      }
      # If survival.flag is off and it's the last time step,
      # fish die if weight < survival.size (survival size threshold)
    } else { # survival.flag == F
      if (ii == dim(salmon.array.temporary)[3]) {
        salmon$survive[salmon$weight < parameters[1,"survive.mass"]] = 0
        if(SecondSpecies == TRUE) fish_other$survive[fish_other$weight<parameters[2,"survive.mass"]] = 0
      }
    }
    
    
    # Remove Q & WT 
    ssn = fncUnloadWQ("Q",ssn) 
    ssn = fncUnloadWQ("WT",ssn) 
    
    
    # Force dates to numeric for storage in arrays
    # Can be reformatted as date again later as follows:
    # as.POSIXct(salmon$dateSp, origin = "1970-01-01")
    salmon.numeric = salmon
    salmon.numeric[,"dateSp"][salmon.numeric[,"dateSp"] == day1] = NA #unemerged fish
    salmon.numeric[,"dateEm"][salmon.numeric[,"dateEm"] == day1] = NA #unemerged fish
    salmon.numeric[,"dateOm"][salmon.numeric[,"dateOm"] == day1] = NA #unemerged fish
    salmon.numeric[,"dateEm"] = as.numeric(salmon.numeric[,"dateEm"])
    salmon.numeric[,"dateSp"] = as.numeric(salmon.numeric[,"dateSp"])
    salmon.numeric[,"dateOm"] = as.numeric(salmon.numeric[,"dateOm"])
    
    if(SecondSpecies == TRUE){
      fish_other.numeric = fish_other
      fish_other.numeric[,"dateSp"] = NA
      fish_other.numeric[,"dateEm"] = NA
      fish_other.numeric[,"dateOm"] = NA
    }
    
    # Save fish in a multidimensional array through time
    salmon.array.temporary[,,ii] = as.matrix(salmon.numeric)[,array.cols2keep]
    if(SecondSpecies == TRUE) fish_other.array.temporary[,,ii] = as.matrix(fish_other.numeric)[,array.cols2keep]
    ii= ii+1
    
    
  } #end time steps (tt)
} #end dates (dd)

# Mark any unemerged fish as dead
colnames(salmon.array.temporary) = array.cols2keep
salmon.array.temporary[,"survive",dim(salmon.array.temporary)[3]][salmon.array.temporary[,"emrg",dim(salmon.array.temporary)[3]] != 1] = 0

# Store results
iii= iter-iter.list[1] + 1
# Store arrays
salmon.array[,,,iii] = salmon.array.temporary
colnames(salmon.array) = array.cols2keep
# Store outputs
salmon.finalstep[,,iii] = as.matrix(salmon.numeric)[,output.cols2keep]
colnames(salmon.finalstep) = output.cols2keep
if(SecondSpecies == TRUE){
  fish_other.array[,,,iii]  = fish_other.array.temporary
  colnames(fish_other.array) = array.cols2keep
  fish_other.finalstep[,,iii] = as.matrix(fish_other.numeric)[,output.cols2keep]
  colnames(fish_other.finalstep) = output.cols2keep
}


#} #end iteration

#Save data
save("salmon.array", file = paste0(outputDir,"/salmon.array.",cs,".RData"))
save("salmon.finalstep",file = paste0(outputDir,"/salmon.finalstep.",cs,".RData"))
if(SecondSpecies == TRUE){
  save("fish_other.array", file = paste0(outputDir,"/fish_other.array.",cs,".RData"))
  save("fish_other.finalstep",file = paste0(outputDir,"/fish_other.finalstep.",cs,".RData"))
}

end.time = proc.time()
runtime = (end.time[3] - start.time[3]) / 60

# Store info on no. replicates, climate scenarios, and runtime
textDir = paste0(outputDir, "/run.info.", cs, ".txt")
file.create(textDir)

run.info<- c(paste0("netnwork: ",netnm),
             paste0("replicates: ", length(iter.list)), 
             paste0("year: ", cs), 
             paste0("runtime (minutes): ", runtime),
             paste0("seed: ", iter),
             paste0("run: ", run),
             paste0("interspecific.competition.flag: ", interspecific.competition.flag),
             paste0("interspecific.predation.flag: ", interspecific.predation.flag),
             paste0("spawn.date.variable: ",spawn.date.variable),
             paste0("survival.flag: ",survival.flag),
             paste0("plot.flag: ",plot.flag),
             paste0("first date:",first.date),
             paste0("last.date:",last.date))
salmon.parms = NULL; for(i in 1:ncol(parameters)) {var = colnames(parameters)[i]; salmon.parms[i] = paste0(var,": ",parameters[1,var]); fish_other.parms = NULL}
if(SecondSpecies == TRUE) for(i in 1:ncol(parameters)) {var = colnames(parameters)[i]; fish_other.parms[i] = paste0(var,": ",parameters[2,var])}

write(c("RUN INFO:", run.info," ","SALMON PARAMETERS:",salmon.parms," ","FISH_OTHER PARAMETERS:",fish_other.parms),file = textDir)
