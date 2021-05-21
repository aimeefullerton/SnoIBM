#*******************************************************************************
#
# A growth potential model for evaluating 
#   how growth of juvenile salmonids may respond 
#   to altered thermal and flow regimes 
#   in the Snoqualmie River watershed (Washington, USA).
#
# Last functional update 5 May 2020; minor editorial updates 23 Oct 2020
#
#*******************************************************************************

for(salmon.nm in c("chinook", "coho", "steelhead", "pink", "lmb", "rainbow")){
  for(scenario in c("bcc-csm1-1-m", "CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "HadGEM2-CC365", "HadGEM2-ES365", "IPSL-CM5A-MR", "MIROC5", "NorESM1-M")){
    for(yy in c(1995:2005, 2089:2099)){ # where 1995:2005 = Historical climate; 2089:2099 = Future climate 
    
    #=== SETUP =====================================================================
    start.time = proc.time() #get initial time stamp for calculating processing time
    
    # Libraries
      library(SSN)
      library(plyr)
      library(rgdal)
      library(raster)
      library(RColorBrewer)
    
    # Load Functions
      source("code/functions.R")
    
    # Global Variables
      netnm <- "sno" # used for plotting and some functions
      network <- "rbm" #nhd1"
      first.date <- as.Date(paste0(yy - 1, "-", "09-01")) #starting date for simulation and for spawning
      last.date <- as.Date(paste0(yy, "-", "08-31")) #last date of the simulation
      dat.idx <- seq(from = first.date, to = last.date, by = 1) # list of all dates to model
      day1 <- strptime(dat.idx[1], format = "%Y-%m-%d")
      iter <- 1
      run <- fncGetRun()
      show.progress <- TRUE # send statements to the console showing progress
    
    # Network-specific Fields
      if(network == "rbm"){
        length.field <- "Length"
        so.field <- "segorder"
        width.field <- "effwidth"
      } else if(network == "nhd1"){
        length.field <- "LENGTHKM"
        so.field <- "SO"
        width.field <- "WIDTH_M"
      }
        wt.field <- "WT"
        xlat <- "NEAR_X"
        ylon <- "NEAR_Y"
        lng2b.field <- "lngth2B" #this is in fish shapefiles
    
    # Directories
      loadDir <- "data.in"
      if(network == "rbm") ssn.folder <- "sno.rbm.ssn"
      if(network == "nhd1") ssn.folder <- "sno.ssn"
      outputDir <- paste0("data.out/GrwPot.", salmon.nm, ".", scenario, ".", yy) 
      # make directories, if they don't already exist
      if (! dir.exists(file.path(outputDir))) {
        dir.create(file.path(outputDir))
      }
    
    # Plotting
      plot.flag <- FALSE
      plot.WT <- FALSE
      plot.GP <- FALSE
      
      imageDir <- paste0(outputDir, "/images")
      if (!dir.exists(file.path(imageDir))) {
        dir.create(file.path(imageDir))
      }
      if(plot.WT) dir.create(file.path(paste0(imageDir, "/WT")))
      if(plot.GP) dir.create(file.path(paste0( imageDir, "/GrwPot")))
      
      # Color palette for plotting
      grays <- brewer.pal(9, "Greys")
      grays <- grays[c(9, 8, 7, 6, 5, 4, 3, 2, 1)]
      
      # water temperature
      cb <-  fncColBrewPlus(n = 14, paint = F)
      left <- c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24)
      rght <- c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 27)
      
      # Growth colors
      cb2 <- brewer.pal(n = 11, name = "PuOr"); cb2 <- cb2[11:1]
      breaks2 <- seq(-0.02, 0.02, length.out = 11)
      left2 <- breaks2[1:10]
      rght2 <- breaks2[2:11]
      
      # Read in basin outline or hillshade for plotting
      outline <- TRUE
      if (outline) {
        basin <- readOGR(paste0(loadDir, "/", ssn.folder), "Basin_snq")
      } 
      
      streamlines <- TRUE
      if(streamlines) streams<- readOGR(paste0(loadDir, "/", ssn.folder), "edges")
      # plot(streams, lwd <- streams@data$meanmsq / 100000000)
      # plot(streams, col <- streams@data$rid)
    
      # Set extent for plotting (will be updated later once ssn is loaded)
      ex <- extent(basin)
    
    # Load model parameters
      parameters.all <- read.csv(paste0(loadDir, "/parameters/parameters.csv"), as.is = TRUE)
      parameters <- cbind.data.frame(salmon.nm, t(parameters.all[, salmon.nm]), stringsAsFactors = FALSE)
      colnames(parameters) <- c("species",parameters.all$parameter); row.names(parameters) <- "salmon"
      for(c in 8:ncol(parameters)){ parameters[, c] <- as.numeric(parameters[,c])}
      for(i in c("spawn.date.begin", "om.date.taper", "om.date.end")){ parameters[, i] <- gsub("x", "", parameters[, i])}
      parameters.static <- parameters
      
      # Species-specific constants used in Wisconsin bioenergetics model
      salmon.constants <- fncReadConstants("salmon")
    
    # Load copy of SSN (will be kept as loaded backup)
      sno.ssn <- ssn <- importSSN(paste0(loadDir, "/", ssn.folder) , predpts = 'preds')
    
    # Load Attribute, Network, and Fish growth data
      if(network == "rbm"){
        # Load DHSVM-RBM water temperature data for the climate scenario
        WT.df <- read.csv(paste0("data.in/rbm.data/", scenario, "/WT.", yy, ".df"), header = TRUE, stringsAsFactors = FALSE)
    
      } else if(network == "nhd1"){
        # Load pre-calculated SSN water temperature data for the climate scenario
        WT.df <- read.csv(paste0(loadDir, "/", ssn.folder, "/WT.df", yy, ".csv"), header = TRUE, stringsAsFactors = FALSE)
      }
        
    # Get missed segs and network base
      if(network == "nhd1"){
        # There are 15 points where ratio is >1; checked in GIS, this changes them to 1:
        sno.ssn@predpoints@SSNPoints[[1]]@point.data$ratio[sno.ssn@predpoints@SSNPoints[[1]]@point.data$ratio > 1] <- 1
        # Reaches with no prediction points
        missed <- c(0, which(!1:nrow(sno.ssn@data)%in%sort(unique(sno.ssn@predpoints@SSNPoints[[1]]@point.data$rid))))
        network.base.segs <- 1:5
      }
      if(network == "rbm"){
        # Reaches with no prediction points
        arcID <- as.numeric(gsub("X", "", colnames(WT.df)[3:ncol(WT.df)]))
        missed <- setdiff(sno.ssn@data$rid, arcID) 
        network.base.segs <- c(2, 6, 9, 14)
      }
      
    # Create vector of all reaches
      allsegs <- sno.ssn@data$rid
      
    # Load topology lists for the network (previously created)
      # all segments downstream from a given seg
      load(paste0(loadDir, "/", ssn.folder, "/dnsegs.RData"))
      # all segments upstream from a given seg
      load(paste0( loadDir, "/", ssn.folder, "/upsegs.RData"))
      # all segments at the upper end/confluence of a given seg
      load(paste0(loadDir, "/", ssn.folder, "/jct.list.RData"))
    
      
    # Set up arrays that will store data
      # List of tracked fields to store permanently
      array.cols2keep <- c("pid", "seg", "xloc", "yloc", "upDist", "SO", "WT", "TU","emrg", "survive", 
                          "conspecificdensity", "otherdensity", "totaldensity", "direction", "movedist", 
                          "pvals", "consInst", "consCum", "growth", "weight", "ration", "om.prob",
                          "pred.prob", "num.prey", "num.eaten")
      output.cols2keep <- c("pid", "TU","emrg", "survive", "consCum", "weight", "dateSp", "dateEm", "dateOm", "datePr", "dateDi")
    
    
      # Seed for reproducible results, change for each iteration
      set.seed(iter)
      
      # Salmon arrays
      # no. of fish, no. variables, no. time steps, no. iterations
      nFish <- 370 #length(segsAccessible)
      if(parameters["salmon","species"] == "rainbow") nFish <- 811 #length(unique(ssn@data$rid))
      salmon.array <- array(NA, dim = c(nFish, length(array.cols2keep), length(dat.idx) * 2)) 
      # no. of fish, no. variables, no. iterations; final values so no time component
      salmon.finalstep <- array(NA, dim = c(nFish, length(output.cols2keep))) 
      
      rm(nFish)
      
    #=== INITIALIZE HABITAT ========================================================
    
      # Load the Snoqualmie Spatial Stream Network (SSN)
      if(network == "rbm"){
        ssn@data[,width.field] <- ssn@data[,width.field] * 100 #get into meters
        ssn@data[,length.field] <- ssn@data[,length.field] / 1000 #get into kilometers
        ssn@data[,"upDist"] <- ssn@data[,"upDist"] / 1000 #get into kilometers
      }
      
      if(plot.flag == TRUE){ #set plotting extent based on SSN bounding box
        ex@xmin <- ssn@bbox[1]
        ex@xmax <- ssn@bbox[3]
        ex@ymin <- ssn@bbox[2]
        ex@ymax <- ssn@bbox[4]
      }
      
      # Get reach widths, adjusted to represent the proportion useable by fish, which decreases as width increases
      useable.widths <- fncUseableWidths(dat = ssn@data[,c("rid", width.field)])
      ssn@data$UseableWidth <- useable.widths[useable.widths[,1] == ssn@data$rid,3]
      
      # Extract dataset (DHSVM-RBM and virtual networks have one value per stream segment; NHD networks have variable numbers)
      wq.df <- getSSNdata.frame(ssn, "preds")
      # change factor fields to numeric, if necessary
      if(is.factor(wq.df$rid)) wq.df$rid <- as.numeric(levels(wq.df$rid)[as.numeric(wq.df$rid)])
    
      # Get lists of network-specific segments above and below Snoqualmie Falls (a natural anadromous barrier) and Tolt Reservoir dam
      if(network == "rbm"){
        segsAbvFalls <- upsegs[[488]]
        segsAbvRes <- upsegs[[179]]
      }
      if(network == "nhd1"){
        segsAbvFalls<- upsegs[[53]]
        segsAbvRes <- upsegs[[308]]
      }
      segsBlwFalls <- unique(ssn@data$rid)[! unique(ssn@data$rid) %in% segsAbvFalls]
      segsBlwRes <- unique(segsBlwFalls)[! unique(segsBlwFalls) %in% segsAbvRes]
      segsAccessible <- intersect(segsBlwFalls, segsBlwRes)
      if(parameters["salmon","species"] == "rainbow") segsAccessible <- unique(ssn@data$rid)
    
      # Add column to denote which reaches are accessible to fish (for blocking movement)
      ssn@data$accessible <- 0
      ssn@data$accessible[ssn@data$rid %in% segsAccessible] <- 1 
      ssn@data$Chinook_rg[is.na(ssn@data$Chinook_rg)] <- 0
      ssn@data$Chinook_sp[is.na(ssn@data$Chinook_sp)] <- 0
    
      # Calculate ration (g/g/d) available to fish; More productive food webs in lower mainstem habitats
      # linearly relate to log(stream order) and position within reach
      ration_1 <- fncRescale(log(wq.df[,so.field] + (1 - wq.df$ratio)), c(parameters["salmon", "ration.lo"], parameters["salmon", "ration.hi"]))
      # inverse-linearly relate to distance from the mouth (upDist)
      ration_2 <- fncRescale(wq.df$upDist, c(parameters["salmon","ration.hi"], parameters["salmon","ration.lo"])) 
      wq.df$ration <- ssn@predpoints@SSNPoints[[1]]@point.data$ration = (ration_1 + ration_2) / 2
      # plot(ssn, "ration", breaktype = "even", nclasses = 6)
    
      # Save this 'base case' ration because we will be updating ration later
      wq.df$ration_base <- ssn@predpoints@SSNPoints[[1]]@point.data$ration_base <- wq.df$ration
    
      # Initialize fish density
      wq.df$density <- ssn@predpoints@SSNPoints[[1]]@point.data$density <- 0
      
      # Reduce field set
      ration.cols <- c("ration", "ration_base")
      wq.df <- wq.df[, c("pid", "rid", so.field, xlat, ylon, "ratio", "upDist", "afvArea", "locID", "netID", width.field, ration.cols, "density")]
      nwq <- nrow(wq.df)
      
      # Put wq.df back into ssn
      ssn <- putSSNdata.frame(wq.df, ssn, "preds")
      #ssn@obspoints@SSNPoints[[1]]@point.data$ration <- ssn@predpoints@SSNPoints[[1]]@point.data$ration
      #plot(ssn, "ration", breaktype = "even", nclasses = 6)
      
      # re-extract dataset and change factor fields to numeric, if necessary
      wq.df <- getSSNdata.frame(ssn, "preds")
      if(is.factor(wq.df$rid)) wq.df$rid <- as.numeric(levels(wq.df$rid)[as.numeric(wq.df$rid)])
      
      
    
    #=== INITIALIZE FISH ===========================================================
    
      # 1. Import the fish locations into the SSN
      
      # Salmon
      shp <- readOGR(paste0(loadDir,"/", ssn.folder), "preds")
      
      for(col in colnames(shp@data)){
        if (is.factor(shp@data[,col])) {
          shp@data[,col] <- as.numeric(levels(shp@data[,col])[as.numeric(shp@data[,col])])
        }
        if (is.character(shp@data[,col])) {
          shp@data[,col] <- as.numeric(shp@data[,col])
        }
      }
    
      shp <- subset(shp, shp@data$rid %in% segsAccessible)
      row.names(shp@data) <- NULL
      writeOGR(shp, paste0(loadDir,"/", ssn.folder), "accessible_reaches", driver = "ESRI Shapefile", overwrite_layer = T)
    
      ssn <- importPredpts(ssn, "accessible_reaches", "ssn") # load into SSN
      salmon.id <- 2 # this is the 2nd 'preds' file loaded in the SSN
      ssn@predpoints@ID[salmon.id] <- parameters["salmon","species"] # name it
      # plot(streams, col = "darkgray") # plot streams
      # add fish locations to plot:
      # points(ssn@predpoints@SSNPoints[[salmon.id]]@point.coords[,1], ssn@predpoints@SSNPoints[[salmon.id]]@point.coords[,2], col = 1, pch = 1, cex = 0.5)
      # test_rids <- c(167, 185, 206, 236, 279, 372, 432, 450, 517, 574)
      # points(ssn@predpoints@SSNPoints[[salmon.id]]@point.coords[,1][ssn@predpoints@SSNPoints[[salmon.id]]@point.data$rid %in% test_rids],
        #ssn@predpoints@SSNPoints[[salmon.id]]@point.coords[,2][ssn@predpoints@SSNPoints[[salmon.id]]@point.data$rid %in% test_rids], col = 2, pch = 1, cex = 0.5)
    
      # Update fish data files with some basic tracking info and fields that will hold scenario data
      # rename fish ID sequentially (critical for tracking movement later)
      ssn@predpoints@SSNPoints[[salmon.id]]@point.data$pid <- 
        rownames(ssn@predpoints@SSNPoints[[salmon.id]]@point.data) <- 1:nrow(ssn@predpoints@SSNPoints[[salmon.id]]@point.data) 
      
      # create 'seg' field and set equal to 'rid' field
      ssn@predpoints@SSNPoints[[salmon.id]]@point.data$seg <- ssn@predpoints@SSNPoints[[salmon.id]]@point.data$rid
      
      # create 'wt.field'
      ssn@predpoints@SSNPoints[[salmon.id]]@point.data[,wt.field] <- 0 # water temperature field
    
      if(network == "rbm"){
        # get into kilometers:
        ssn@predpoints@SSNPoints[[salmon.id]]@point.data[,"upDist"] <- ssn@predpoints@SSNPoints[[salmon.id]]@point.data[,"upDist"] / 1000 
      }
      
    
    
      # 2. Create fish data frame for manipulating during simulation
      # pid: unique identification for each fish
      # seg: which network segment the fish is in (same as rid)
      # ratio: relative distance along the segment (from bottom to top [0-1])
      # xloc: x coordinate of fish
      # yloc: y coordinate of fish
      # length2segBase is the length from a fish's position in the current segment to its base
      # upDist is the distance from a fish's position to the base of the whole network
      # emrg values: 0 = egg placed (initial value), -1 = egg spawned, 1 = egg emerged
      # survive values: 1 = alive (initial value), 0 = dead, 2 = outmigrated/smolted, -1 = eaten by predator, -2 = egg scoured
      
      # Get data frame from SSN
      salmon.df <- getSSNdata.frame(ssn, parameters["salmon","species"])[c(xlat, ylon, "pid", "rid", "ratio", "upDist", wt.field)]
    
      # Create new dataframe
      salmon <- data.frame(pid = 1:nrow(salmon.df), seg = 0, xloc = 0, yloc = 0, ratio = 0, length2segBase = 0, upDist = 0, 
              SO = 0, WT = 0, TU = 0, emrg = 0, survive = 1, conspecificdensity = parameters["salmon", "initial.mass"], 
              otherdensity = 0, totaldensity = 0, direction = 0, movedist = 0, pvals = 0, ration = 0, consInst = 0, consCum = 0,  
              growth = 0, weight = parameters["salmon", "initial.mass"], dateSp = day1, dateEm = day1, dateOm = day1,
              datePr = day1, dateDi = day1, om.prob = 0, pred.prob = 0, num.prey = 0, num.eaten = 0, stringsAsFactors = FALSE)
    
      # Fill in what we can from the ssn
      salmon$seg <- salmon.df$rid
      salmon$ratio <- salmon.df$ratio
      salmon$xloc <- salmon.df[,xlat]
      salmon$yloc<- salmon.df[,ylon]
      salmon$upDist <- salmon.df$upDist
      rm(salmon.df)
      
      salmon.alive.emerge.index <- salmon$pid
    
    #=== RUN SIMULATION ============================================================
    
      # Loop over each day (dd) and time step (tt) in an iteration (iter)
      dt <- 1 # day/time counter
      for(dd in 1:length(dat.idx)){
        for(tt in c(6, 18)){ #6am, 6pm
          set.seed(iter)
          thetitle <- fncGetTitle(dat.idx[dd], tt)
          cat(iter, ": ", thetitle, "runtime (h): ", proc.time()[3]/60/60, "\n")
    
      # 1. UPDATE HABITAT: Get predicted stream temperatures ====
          
          # Load temperature for this date and time
    
          if("WT" %in% colnames(ssn@predpoints@SSNPoints[[1]]@point.data)){
            ssn <- fncUnloadWQ("WT",ssn) # unload if it's already loaded
          }
          ssn <- fncLoadWQdata(type = "WT", dat.df = WT.df, thedate = dat.idx[dd], thetime = tt, ssn = ssn, plotit = FALSE)
          
          # Extract data frame that has had temperautre loaded
          wq.df <- getSSNdata.frame(ssn,"preds")
          
          # changing factor/character fields to numeric, if necessary
          for(field in c("rid", so.field, wt.field)){
            if (is.factor(wq.df[,field])) {
              wq.df[,field] <- as.numeric(levels(wq.df[,field])[as.numeric(wq.df[,field])])
            }
            if (is.character(wq.df[,field])) {
              wq.df[,field] <- as.numeric(wq.df[,field])
            }
          }
          
          
      # 2. GROW FISH. (calculated using the Wisconsin Bioenergetics model equations) ====
    
          # salmon:
          if(length(salmon.alive.emerge.index) > 0){
            # Update WT & ration in fish dataframe
            salmon$WT[salmon.alive.emerge.index] <- fncGetNearestAttribute(wq.df[,c("pid", "rid", "ratio", wt.field)], 
                   salmon[salmon.alive.emerge.index,c("pid", "seg", "ratio")], ssn)[,wt.field]
            salmon$ration[salmon.alive.emerge.index] <- fncGetNearestAttribute(wq.df[,c("pid", "rid", "ratio", "ration")], 
                   salmon[salmon.alive.emerge.index,c("pid","seg","ratio")], ssn)[,"ration"] 
            
            # Get parameters and inputs for Bioenergetics model
            # provide nearest water temperature, fish weight (grams), and pvals or ration
            salmon.input <- fncGetBioEParms(parameters["salmon","species"], 
                  parameters["salmon","pred.en.dens"], parameters["salmon","prey.en.dens"],
                  wt.nearest = salmon[salmon.alive.emerge.index,c("pid",wt.field)], 
                  startweights = salmon$weight[salmon.alive.emerge.index],
                  pvals = rep(-9, nrow(salmon[salmon.alive.emerge.index,])), 
                  ration = salmon$ration[salmon.alive.emerge.index])
    
            # Run bioenergetics for all fish at this 12-hour time step
            salmon.results <- BioE(salmon.input, salmon.constants)
            
            # Put results back into fish dataframe
            # note: splitting results in half because bioenergetics model operates on 
            # a daily time step and we are using a half-day time step
            
            # instantaneous growth (units are g/d; halved and divided by fish weight to get g/g/12h)
            salmon$growth[salmon.alive.emerge.index] <- t(salmon.results$Growth / 2) / salmon$weight[salmon.alive.emerge.index]
            # cumulative growth
            salmon$weight[salmon.alive.emerge.index] <- 1 # setting to 1-g fish
            # instantaneous consumption (units are g/g/d; halved to get g/g/12h)
            salmon$consInst[salmon.alive.emerge.index] <- t(salmon.results$Consumption / 2)
            # cumulative amount consumed
            salmon$consCum[salmon.alive.emerge.index] <- salmon$consCum[salmon.alive.emerge.index] + t(salmon.results$Consumption / 2)
            # bioenergetic p-values
            pp <- salmon.input$ration/salmon.results$CMAX; pp[pp > 1] <- 1
            salmon$pvals[salmon.alive.emerge.index] <- pp
            
            # # Mark any salmon with negative weights as dead and reset index
            # salmon$survive[salmon$weight <= 0] <- 0
            # salmon.alive.emerge.index <- salmon$pid[salmon$survive == 1]
            
          }
          
      # 3. PLOT MAPS ====
          
          # (a) Plot water temperature ====
          if(plot.WT){
            
            #pPlot water temperature for dd & tt as colored stream lines:
            png(paste0(imageDir, "/WT/WT_", dat.idx[dd], "-", sprintf("%03d", tt), ".png"), width = 9, height = 6, units = "in", res = 150)
            par(mar = c(1, 0, 4, 0))
            
            # plot background
            if (outline) {
              plot(basin, col = "gray80", border = NA)
            } else {
              plot(basin, col = grays, ext = ex, axes = FALSE, xlab = "", ylab = "", box = FALSE, legend = FALSE)
            }
            
            plot(streams, col="darkgray", add = TRUE, lwd = 8 * streams@data$afvArea + 0.2)
            
            if(network == "rbm"){
              ssn@data$WT <- ssn@predpoints@SSNPoints[[1]]@point.data$WT[ssn@data$rid == ssn@predpoints@SSNPoints[[1]]@point.data$rid]
              for(n in 1:length(cb)) {ssn@data$WT.color[ssn@data$WT >= left[n] & ssn@data$WT <= rght[n]] <- n}
              
              for (i in 1:length(ssn@lines)) {
                for (j in 1:length(ssn@lines[[i]])) {
                  lines(ssn@lines[[i]]@Lines[[j]]@coords, col = cb[ssn@data[i,"WT.color"]], lwd = 8 * (ssn@data[i, "afvArea"] + 0.2))
                }
              }
            }
            if(network == "nhd1"){
              # aggregate prediction points to stream reaches
              datap <- wq.df[,c("rid", wt.field)]
              datap2 <- tapply(datap[,wt.field], datap$rid, mean, na.rm = TRUE)
              datap3 <- cbind.data.frame("rid" = as.numeric(names(datap2)), "WT" = datap2)
              
              linedata <- ssn@data
              rownames(linedata) <- NULL
              linedata$sort.order <- as.integer(rownames(linedata))
              if("WT" %in% colnames(linedata)) linedata <- linedata[,-which(colnames(linedata) == "WT")] #remove WT field
              linedata2 <- merge(linedata, datap3, by = "rid", all.x = TRUE) 
              linedata2 <- linedata2[order(linedata2$sort.order),]
              for(n in 1:length(cb)) {
                linedata2$col.class[linedata2[,wt.field] >= left[n] & linedata2[,wt.field] <= rght[n]] <- n
              }
              ssn@data <- linedata2
              for (i in 1:length(ssn@lines)) {
                for (j in 1:length(ssn@lines[[i]])) {
                  lines(ssn@lines[[i]]@Lines[[j]]@coords, col = cb[ssn@data[i,"col.class"]], lwd = 5 * (ssn@data[i,"afvFlow"] + 0.4))
                }
              }
            }
            
            # Add legend
            leglabs <- paste(left, "to", rght)
            legend("right", legend = leglabs, title=expression("Temperature "~degree(C)), bty = "n", pch = 19, col = cb, cex = 0.8)
            
            # Add scale bar
            rect(xleft = ex[1] + 5000, ybottom = ex[3] + 3000, xright = ex[1] + 7500, ytop = ex[3] + 3500)
            rect(xleft = ex[1] + 7500, ybottom = ex[3] + 3000, xright = ex[1] + 10000, ytop = ex[3] + 3500, col = 1)
            rect(xleft = ex[1] + 10000, ybottom = ex[3] + 3000, xright = ex[1] + 12500, ytop = ex[3] + 3500)
            rect(xleft = ex[1] + 12500, ybottom = ex[3] + 3000, xright = ex[1] + 15000, ytop = ex[3] + 3500, col = 1)
            segments(x0 = ex[1] + 5000, y0 = ex[3] + 3000, x1 = ex[1] + 5000, y1 = ex[3] + 2500)
            segments(x0 = ex[1] + 10000, y0 = ex[3] + 3000, x1 = ex[1] + 10000, y1 = ex[3] + 2500)
            segments(x0 = ex[1] + 15000, y0 = ex[3] + 3000, x1 = ex[1] + 15000, y1 = ex[3] + 2500)
            text(x = ex[1] + 5000, y = ex[3] + 1500, "0", cex = 0.8)
            text(x = ex[1] + 10000, y = ex[3] + 1500, "5", cex = 0.8)
            text(x = ex[1] + 15000, y = ex[3] + 1500, "10", cex = 0.8)
            text(x = ex[1] + 18000, y = ex[3] + 1500, "km", cex = 0.8) 
            
            # Add north arrow
            arrows(ex[1] + 2000, ex[3] + 2300, ex[1] + 2000, ex[3] + 4000, length = 0.1, lwd = 5)
            text(ex[1] + 2000, ex[3] + 1000, "N")
            
            # Add title
            mtext(thetitle, side = 3, line = 2, cex = 1.3)
            
            dev.off()
          }
            
            
          # (b) Plot growth potential map ====
          if(plot.GP){
              
            wq.df$growth <- ssn@data$growth <- 0
            wq.df$growth[wq.df$rid %in% segsAccessible] <- salmon$growth[salmon$seg %in% segsAccessible]
            ssn <- putSSNdata.frame(wq.df, ssn, "preds")
            
            png(paste0(imageDir, "/GrwPot/GrwPot_", dat.idx[dd], "-", sprintf("%03d", tt), ".png"), width = 9, height = 6, units = "in", res = 150)
            par(mar = c(1, 0, 4, 0))
            
            # plot background
            if (outline) {
              plot(basin, col = "gray80", border = NA)
            } else {
              plot(basin, col = grays, ext = ex, axes = FALSE, xlab = "", ylab = "", box = FALSE, legend = FALSE)
            }
            
            plot(streams, col = "darkgray", add = TRUE, lwd = 8 * streams@data$afvArea + 0.2)
            
            
            if(network == "rbm"){
              ssn@data$growth <- ssn@predpoints@SSNPoints[[1]]@point.data$growth[ssn@data$rid == ssn@predpoints@SSNPoints[[1]]@point.data$rid]
              ssn@data$growth[ssn@data$growth == 0] <- NA
              ssn@data$GP.color <- 0
              for(n in 1:length(cb2)) {ssn@data$GP.color[ssn@data$growth >= left2[n] & ssn@data$growth <= rght2[n]] <- n}
              
              for (i in 1:length(ssn@lines)) {
                for (j in 1:length(ssn@lines[[i]])) {
                  lines(ssn@lines[[i]]@Lines[[j]]@coords, col = cb2[ssn@data[i,"GP.color"]], lwd = 8 * (ssn@data[i, "afvArea"] + 0.2))
                }
              }
            }
            
    
            # Add legend
            leglabs <- paste(left2, "to", rght2)
            legend("right", legend = leglabs, title = "Growth potential (g/g/d)",bty = "n", pch = 19, col = cb2, cex = 0.8)
            
            # Add scale bar
            rect(xleft = ex[1] + 5000, ybottom = ex[3] + 3000, xright = ex[1] + 7500, ytop = ex[3] + 3500)
            rect(xleft = ex[1] + 7500, ybottom = ex[3] + 3000, xright = ex[1] + 10000, ytop = ex[3] + 3500, col = 1)
            rect(xleft = ex[1] + 10000, ybottom = ex[3] + 3000, xright = ex[1] + 12500, ytop = ex[3] + 3500)
            rect(xleft = ex[1] + 12500, ybottom = ex[3] + 3000, xright = ex[1] + 15000, ytop = ex[3] + 3500, col = 1)
            segments(x0 = ex[1] + 5000, y0 = ex[3] + 3000, x1 = ex[1] + 5000, y1 = ex[3] + 2500)
            segments(x0 = ex[1] + 10000, y0 = ex[3] + 3000, x1 = ex[1] + 10000, y1 = ex[3] + 2500)
            segments(x0 = ex[1] + 15000, y0 = ex[3] + 3000, x1 = ex[1] + 15000, y1 = ex[3] + 2500)
            text(x = ex[1] + 5000, y = ex[3] + 1500, "0", cex = 0.8)
            text(x = ex[1] + 10000, y = ex[3] + 1500, "5", cex = 0.8)
            text(x = ex[1] + 15000, y = ex[3] + 1500, "10", cex = 0.8)
            text(x = ex[1] + 18000, y = ex[3] + 1500, "km", cex = 0.8) 
            
            # Add north arrow
            arrows(ex[1] + 2000, ex[3] + 2300, ex[1] + 2000, ex[3] + 4000, length = 0.1, lwd = 5)
            text(ex[1] + 2000, ex[3] + 1000, "N")
            
            # Add title
            mtext(thetitle, side = 3, line = 2, cex = 1.3)
            
            dev.off()
          }
    
          
          
          # Remove WT 
          ssn <- fncUnloadWQ("WT",ssn) 
        
          # Force dates to numeric for storage in arrays
          # Can be reformatted as date again later as follows:
          # as.POSIXct(salmon$dateSp, origin = "1970-01-01")
          salmon.numeric <- salmon
          salmon.numeric[,"dateSp"][salmon.numeric[,"dateSp"] == day1] <- NA #unemerged fish
          salmon.numeric[,"dateEm"][salmon.numeric[,"dateEm"] == day1] <- NA #unemerged fish
          salmon.numeric[,"dateOm"][salmon.numeric[,"dateOm"] == day1] <- NA #unemerged fish
          salmon.numeric[,"datePr"][salmon.numeric[,"datePr"] == day1] <- NA #unemerged fish
          salmon.numeric[,"dateDi"][salmon.numeric[,"dateDi"] == day1] <- NA #unemerged fish
          salmon.numeric[,"dateEm"] <- as.numeric(salmon.numeric[,"dateEm"])
          salmon.numeric[,"dateSp"] <- as.numeric(salmon.numeric[,"dateSp"])
          salmon.numeric[,"dateOm"] <- as.numeric(salmon.numeric[,"dateOm"])
          salmon.numeric[,"datePr"] <- as.numeric(salmon.numeric[,"datePr"])
          salmon.numeric[,"dateDi"] <- as.numeric(salmon.numeric[,"dateDi"])
          
          # Store multidimensional array of results for this date/time
          salmon.array[,,dt] <- as.matrix(salmon.numeric)[,array.cols2keep]; colnames(salmon.array) <- array.cols2keep
    
          dt <- dt + 1
        } #end time steps (tt)
      } #end dates (dd)
    
      # Mark any unemerged fish as dead
      salmon.array[,"survive",dim(salmon.array)[3]][salmon.array[,"emrg",dim(salmon.array)[3]] != 1] <- 0
      
      # Store final step output
      salmon.finalstep <- as.matrix(salmon.numeric)[,output.cols2keep]; colnames(salmon.finalstep) <- output.cols2keep
    
    
    # Save data for the iteration
      save("salmon.array", file = paste0(outputDir,"/salmon.array.", yy, ".", iter, ".RData"))
      save("salmon.finalstep", file = paste0(outputDir,"/salmon.finalstep.", yy, ".", iter, ".RData"))
    
      end.time <- proc.time()
      runtime <- (end.time[3] - start.time[3]) / 60 / 60
      
      # Store info on no. replicates, climate scenarios, and runtime
      textDir <- paste0(outputDir, "/run.info.", yy, ".", iter, ".txt")
      file.create(textDir)
      
      run.info <- c(paste0("netnwork: ",netnm),
              paste0("year: ", yy), 
              paste0("runtime (h): ", runtime),
              paste0("seed: ", iter),
              paste0("run: ", run),
              paste0("first date:",first.date),
              paste0("last.date:",last.date))
      salmon.parms <- NULL; for(i in 1:ncol(parameters)) {var <- colnames(parameters)[i]; salmon.parms[i] <- paste0(var,": ",parameters["salmon",var])
      fish_other.parms <- NULL}
    
      write(c("RUN INFO:", run.info," SALMON PARAMETERS:", salmon.parms), file = textDir)
    
    #=== SUMMARY MAPS ==============================================================
    
    if(plot.flag){
    #load(paste0(outputDir, "/salmon.array.", yy, ".1.RData"))  
      
    # Make GP maps
      gr <- salmon.array[,"growth",]
      gr.year <- t(apply(gr, 1, quantile, na.rm = T))
      gr.sum <- t(apply(gr[,c(1:60,607:730)], 1, quantile, na.rm = T)) # Sep prior year + Jul & Aug
      gr.fall <- t(apply(gr[,61:244], 1, quantile, na.rm = T)) # Oct, Nov, Dec
      gr.win <- t(apply(gr[,243:424], 1, quantile, na.rm = T)) # Jan, Feb, Mar
      gr.spr <- t(apply(gr[,425:606], 1, quantile, na.rm = T)) # Apr, May, Jun
      
    for(s in 1:5){
      if(s == 1) {gr.dat <- gr.year; gr.nam1 <- yy}
      if(s == 2) {gr.dat <- gr.fall; gr.nam1 <- "Autumn"}
      if(s == 3) {gr.dat <- gr.win; gr.nam1 <- "Winter"}
      if(s == 4) {gr.dat <- gr.spr; gr.nam1 <- "Spring"}
      if(s == 5) {gr.dat <- gr.sum; gr.nam1 <- "Summer"}
      
      for(k in 1:5){
        if(k == 1) kk <- "Min"
        if(k == 2) kk <- "Q1"
        if(k == 3) kk <- "Median"
        if(k == 4) kk <- "Q3"
        if(k == 5) kk <- "Max"
        gr.nam <- paste0(gr.nam1, ".", kk)
        
      png(paste0(imageDir, "/GrwPot_", gr.nam, ".png"), width = 9, height = 6, units = "in", res = 150)
      par(mar = c(1, 0, 4, 0))
      
      # plot background
      if (outline) {
        plot(basin, col = "gray80", border = NA)
      } else {
        plot(basin, col = grays, ext = ex, axes = FALSE, xlab = "", ylab = "", box = FALSE, legend = FALSE)
      }
      
      plot(streams, col = "darkgray", add = TRUE, lwd = 8 * streams@data$afvArea + 0.2)
      
      
      if(network == "rbm"){
        ssn@data$growth <- 0
        ssn@data$growth[ssn@data$rid %in% segsAccessible] <- gr.dat[,k] 
        ssn@data$growth[ssn@data$growth == 0] <- NA
        ssn@data$GP.color <- 0
        for(n in 1:length(cb2)) {ssn@data$GP.color[ssn@data$growth >= left2[n] & ssn@data$growth <= rght2[n]] <- n}
        
        for (i in 1:length(ssn@lines)) {
          for (j in 1:length(ssn@lines[[i]])) {
            lines(ssn@lines[[i]]@Lines[[j]]@coords, col = cb2[ssn@data[i,"GP.color"]], lwd = 8 * (ssn@data[i, "afvArea"] + 0.2))
          }
        }
      }
      
      
      # Add legend
      leglabs <- paste(left2, "to", rght2)
      legend("right", legend = leglabs, title = "Growth potential (g/g/d)", bty = "n", pch = 19, col = cb2, cex = 0.8)
      
      # Add scale bar
      rect(xleft = ex[1] + 5000, ybottom = ex[3] + 3000, xright = ex[1] + 7500, ytop = ex[3] + 3500)
      rect(xleft = ex[1] + 7500, ybottom = ex[3] + 3000, xright = ex[1] + 10000, ytop = ex[3] + 3500, col = 1)
      rect(xleft = ex[1] + 10000, ybottom = ex[3] + 3000, xright = ex[1] + 12500, ytop = ex[3] + 3500)
      rect(xleft = ex[1] + 12500, ybottom = ex[3] + 3000, xright = ex[1] + 15000, ytop = ex[3] + 3500, col = 1)
      segments(x0 = ex[1] + 5000, y0 = ex[3] + 3000, x1 = ex[1] + 5000, y1 = ex[3] + 2500)
      segments(x0 = ex[1] + 10000, y0 = ex[3] + 3000, x1 = ex[1] + 10000, y1 = ex[3] + 2500)
      segments(x0 = ex[1] + 15000, y0 = ex[3] + 3000, x1 = ex[1] + 15000, y1 = ex[3] + 2500)
      text(x = ex[1] + 5000, y = ex[3] + 1500, "0", cex = 0.8)
      text(x = ex[1] + 10000, y = ex[3] + 1500, "5", cex = 0.8)
      text(x = ex[1] + 15000, y = ex[3] + 1500, "10", cex = 0.8)
      text(x = ex[1] + 18000, y = ex[3] + 1500, "km", cex = 0.8) 
      
      # Add north arrow
      arrows(ex[1] + 2000, ex[3] + 2300, ex[1] + 2000, ex[3] + 4000, length = 0.1, lwd = 5)
      text(ex[1] + 2000, ex[3] + 1000, "N")
      
      # Add title
      mtext(gr.nam, side = 3, line = 2, cex = 1.3)
      
      dev.off()
      }
    }
      
    # Make WT maps
      wt <- salmon.array[,"WT",]
      wt.year <- t(apply(wt, 1, quantile, na.rm = T))
      wt.sum <- t(apply(wt[,c(1:60,607:730)], 1, quantile, na.rm = T))
      wt.fall <- t(apply(wt[,61:244], 1, quantile, na.rm = T))
      wt.win <- t(apply(wt[,243:424], 1, quantile, na.rm = T))
      wt.spr <- t(apply(wt[,425:606], 1, quantile, na.rm = T))
      
    for(s in 1:5){
      if(s == 1) {wt.dat <- wt.year; wt.nam1 <- yy}
      if(s == 2) {wt.dat <- wt.fall; wt.nam1 <- "Autumn"}
      if(s == 3) {wt.dat <- wt.win; wt.nam1 <- "Winter"}
      if(s == 4) {wt.dat <- wt.spr; wt.nam1 <- "Spring"}
      if(s == 5) {wt.dat <- wt.sum; wt.nam1 <- "Summer"}
      
      for(k in 1:5){
        if(k == 1) kk <- "Min"
        if(k == 2) kk <- "Q1"
        if(k == 3) kk <- "Median"
        if(k == 4) kk <- "Q3"
        if(k == 5) kk <- "Max"
        wt.nam <- paste0(wt.nam1, ".", kk)
        
        png(paste0(imageDir, "/WT_", wt.nam, ".png"), width = 9, height = 6, units = "in", res = 150)
        par(mar = c(1, 0, 4, 0))
        
        # plot background
        if (outline) {
          plot(basin, col = "gray80", border = NA)
        } else {
          plot(basin, col = grays, ext = ex, axes = FALSE, xlab = "", ylab = "", box = FALSE, legend = FALSE)
        }
        
        plot(streams, col = "darkgray", add = TRUE, lwd = 8 * streams@data$afvArea + 0.2)
        
        
        if(network == "rbm"){
          ssn@data$WT <- 0
          ssn@data$WT[ssn@data$rid %in% segsAccessible] <- wt.dat[,k] 
          ssn@data$WT[ssn@data$WT == 0] <- NA
          ssn@data$WT.color <- 0
          for(n in 1:length(cb)) {ssn@data$WT.color[ssn@data$WT >= left[n] & ssn@data$WT <= rght[n]] <- n}
          
          for (i in 1:length(ssn@lines)) {
            for (j in 1:length(ssn@lines[[i]])) {
              lines(ssn@lines[[i]]@Lines[[j]]@coords, col = cb[ssn@data[i,"WT.color"]], lwd = 8 * (ssn@data[i, "afvArea"] + 0.2))
            }
          }
        }
        
        
        # Add legend
        leglabs <- paste(left, "to", rght)
        legend("right", legend = leglabs, title = expression("Temperature "~degree(C)), bty = "n", pch = 19, col = cb, cex = 0.8)
        
        # Add scale bar
        rect(xleft = ex[1] + 5000, ybottom = ex[3] + 3000, xright = ex[1] + 7500, ytop = ex[3] + 3500)
        rect(xleft = ex[1] + 7500, ybottom = ex[3] + 3000, xright = ex[1] + 10000, ytop = ex[3] + 3500, col = 1)
        rect(xleft = ex[1] + 10000, ybottom = ex[3] + 3000, xright = ex[1] + 12500, ytop = ex[3] + 3500)
        rect(xleft = ex[1] + 12500, ybottom = ex[3] + 3000, xright = ex[1] + 15000, ytop = ex[3] + 3500, col = 1)
        segments(x0 = ex[1] + 5000, y0 = ex[3] + 3000, x1 = ex[1] + 5000, y1 = ex[3] + 2500)
        segments(x0 = ex[1] + 10000, y0 = ex[3] + 3000, x1 = ex[1] + 10000, y1 = ex[3] + 2500)
        segments(x0 = ex[1] + 15000, y0 = ex[3] + 3000, x1 = ex[1] + 15000, y1 = ex[3] + 2500)
        text(x = ex[1] + 5000, y = ex[3] + 1500, "0", cex = 0.8)
        text(x = ex[1] + 10000, y = ex[3] + 1500, "5", cex = 0.8)
        text(x = ex[1] + 15000, y = ex[3] + 1500, "10", cex = 0.8)
        text(x = ex[1] + 18000, y = ex[3] + 1500, "km", cex = 0.8) 
        
        # Add north arrow
        arrows(ex[1] + 2000, ex[3] + 2300, ex[1] + 2000, ex[3] + 4000, length = 0.1, lwd = 5)
        text(ex[1] + 2000, ex[3] + 1000, "N")
        
        # Add title
        mtext(wt.nam, side = 3, line = 2, cex = 1.3)
        
        dev.off()
      }
    }
    }
    }
  }
}