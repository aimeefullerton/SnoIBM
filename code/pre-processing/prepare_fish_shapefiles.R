

## When fish points were created in GIS with STARS tool, the upDist column did not calculate (error with no explanation)
# so we are going to create it here instead

mydir = getwd()
loadDir = "data.in"

network = "nhd1" #"rbm
if(network == "nhd1") ssn.folder = "sno.ssn"
if(network == "rbm") ssn.folder = "sno.rbm.ssn"

# Load Functions
source("code/Functions4SnoIBM.R")

# Network-specific Fields
if(network == "rbm"){
  length.field = "Length"
  wt.field = "WT"
  q.field = "Q"
  so.field = "segorder"
  width.field = "hydwidth"
  xlat = "NEAR_X"
  ylon = "NEAR_Y"
} else if(network == "nhd1"){
  length.field= "LENGTHKM"
  wt.field= "WT"
  q.field = "Q"
  so.field= "SO"
  width.field = "WIDTH_M"
  xlat= "NEAR_X"
  ylon= "NEAR_Y"
}

# Load model parameters
parameters = read.csv(paste0(getwd(), "/", loadDir, "/parameters/model_parameters.csv"), stringsAsFactors = FALSE, header = TRUE, row.names = 1)[1:2,]

#CHINOOK
# Read in the shapefile that has "ratio" field
if(network == "rbm") chin_shp<- readOGR(paste0(mydir,"/",loadDir,"/shapefiles"),"chin_spawn2") #RBM ssn
if(network == "nhd1") chin_shp<- readOGR(paste0(mydir,"/",loadDir,"/shapefiles"),"chin_sp1") #NHD ssn

if(is.factor(chin_shp@data$rid)) chin_shp@data$rid= as.numeric(levels(chin_shp@data$rid)[as.numeric(chin_shp@data$rid)])
chin_shp@data$pid<- seq_along(chin_shp@data$rid) #generate unique "pid" field
chin_shp@data$locID = chin_shp@data$netID = chin_shp@data$upDist = 1 #add other necessary columns for using with SSN
# also need "afvArea" and so_field?
writeOGR(chin_shp, paste0(mydir,"/",loadDir,"/", ssn.folder),"chin_spawn", driver="ESRI Shapefile", overwrite_layer = T) #save back out, now has all fields needed to be read into SSN

# Load into SSN
sno.ssn = importSSN(paste0(loadDir, "/", ssn.folder), predpts = 'preds')
sno.ssn = importPredpts(sno.ssn,"chin_spawn","ssn")
salmon.id = 2 #this is the 2nd preds file loaded in the SSN (see sno.ssn@predpoints@ID)
sno.ssn@predpoints@ID[salmon.id] = "chinook"

# Calculate upDist and Length2SegBase fields
# NOTE 3/22/20: fncGetUpDist is missing; these two items are calculated in GIS and these shouldn't need to be re-run here. Skip to 'Reload' section.
sno.ssn@predpoints@SSNPoints[[salmon.id]]@point.data$upDist<- 
  ddply(sno.ssn@predpoints@SSNPoints[[salmon.id]]@point.data, .(pid), function(x) data.frame(dist=fncGetUpDist(pID=x$pid,ssn=sno.ssn)))$dist
sno.ssn@predpoints@SSNPoints[[salmon.id]]@point.data$length2segBase <-
  ddply(sno.ssn@predpoints@SSNPoints[[salmon.id]]@point.data, .(pid), function(x) data.frame(dist=fncSegRatio2length(x$rid, x$ratio, sno.ssn)))$dist

# Save back out as shapefile, now with all appropriate data needed by SSN
chin_spawn = sno.ssn@predpoints@SSNPoints[[salmon.id]]
chin_spawn@point.data = chin_spawn@point.data[,c("pid","NEAR_X","NEAR_Y","rid","ratio","upDist","netID","locID","length2segBase")]
chin_out = SpatialPointsDataFrame(coords=chin_spawn@point.coords,proj4string = chin_shp@proj4string,bbox = chin_spawn@points.bbox,data = chin_spawn@point.data)
writeOGR(chin_out,paste0(mydir,"/",loadDir,"/", ssn.folder),"chin_spawn", driver="ESRI Shapefile", overwrite_layer = T)


# Reload
chin_shp2 = readOGR(paste0(mydir,"/",loadDir,"/", ssn.folder),"chin_spawn") #test
sno.ssn = importSSN(paste0(loadDir,"/", ssn.folder), predpts = 'preds')
sno.ssn = importPredpts(sno.ssn,"chin_spawn","ssn")
salmon.id = 2 #this is the 2nd preds file loaded in the SSN (see sno.ssn@predpoints@ID)
sno.ssn@predpoints@ID[salmon.id] = "chinook"
td = getSSNdata.frame(sno.ssn,"chinook")

# Remove fish from 'missed' segs
missed = c(0, which(!1:nrow(sno.ssn@data)%in%sort(unique(sno.ssn@predpoints@SSNPoints[[1]]@point.data$rid))))
if(network == "rbm") missed = c(missed, 797:810)
chin_shp2 = subset(chin_shp2, !chin_shp2@data$rid %in% missed)

# Subsample fish to keep; produce 10 randomizations to choose from later
n = 1
for(i in 1:n){
  set.seed(i)
  #chin2use = sample(sno.ssn@predpoints@SSNPoints[[salmon.id]]@point.data$pid, parameters["salmon","nFish"])
  chin2use = sample(chin_shp2@data$pid, parameters["salmon","nFish"])
  #streams<- readOGR(paste0(getwd(), "/", loadDir, "/", ssn.folder), "edges")
  #plot(streams,lwd = streams@data$meanmsq / 100000000, col = "darkgray")
  #plot(chin_shp2, add = 2, col = 2, cex = 0.5)
  #plot(chin_shp2[chin_shp2@data$pid %in% chin2use,], add = 2, col = 4, cex = 0.5)
  chin_spawn = subset(chin_shp2, chin_shp2@data$pid %in% chin2use)
  row.names(chin_spawn@data) = NULL
  chin_spawn@data$pid = as.numeric(row.names(chin_spawn@data))
  writeOGR(chin_spawn, paste0(mydir,"/",loadDir,"/", ssn.folder), paste0("chin_spawn_",parameters["salmon","nFish"]), driver="ESRI Shapefile", overwrite_layer = T)
}

# Read back in subsetted shapefile [this how a file will be loaded for modeling]
sno.ssn = importSSN(paste0(loadDir,"/", ssn.folder), predpts = 'preds')
sno.ssn = importPredpts(sno.ssn,paste0("chin_spawn_",parameters["salmon","nFish"]), "ssn")
salmon.id = 2 #this is the 2nd preds file loaded in the SSN (see sno.ssn@predpoints@ID)
sno.ssn@predpoints@ID[salmon.id] = "chinook"
td = getSSNdata.frame(sno.ssn, "chinook")




# LMB
# Read in the shapefile that has "ratio" field
if(network == "rbm") lmb_shp<- readOGR(paste0(mydir,"/",loadDir,"/shapefiles"),"lmb_prevalent2") #RBM ssn
if(network == "nhd1") lmb_shp<- readOGR(paste0(mydir,"/",loadDir,"/shapefiles"),"lmb_sp1") #NHD ssn

if(is.factor(lmb_shp@data$rid)) lmb_shp@data$rid= as.numeric(levels(lmb_shp@data$rid)[as.numeric(lmb_shp@data$rid)])
lmb_shp@data$pid<- seq_along(lmb_shp@data$rid) #generate unique "pid" field
lmb_shp@data$locID = lmb_shp@data$netID = lmb_shp@data$upDist = 1 #add other necessary columns for using with SSN
# also need "afvArea" and so_field?
writeOGR(lmb_shp, paste0(mydir,"/",loadDir,"/", ssn.folder),"lmb_prevalent", driver="ESRI Shapefile", overwrite_layer = T) #save back out, now has all fields needed to be read into SSN

# Load into SSN
sno.ssn = importSSN(paste0(loadDir, "/", ssn.folder), predpts = 'preds')
sno.ssn = importPredpts(sno.ssn, "lmb_prevalent", "ssn")
fish_other.id = 2 #this is the 2nd preds file loaded in the SSN (see sno.ssn@predpoints@ID)
sno.ssn@predpoints@ID[fish_other.id] = "fish_other"

# Calculate upDist and Length2SegBase fields
sno.ssn@predpoints@SSNPoints[[fish_other.id]]@point.data$upDist<- 
  ddply(sno.ssn@predpoints@SSNPoints[[fish_other.id]]@point.data, .(pid), function(x) data.frame(dist=fncGetUpDist(pID=x$pid,ssn=sno.ssn)))$dist
sno.ssn@predpoints@SSNPoints[[fish_other.id]]@point.data$length2segBase <-
  ddply(sno.ssn@predpoints@SSNPoints[[fish_other.id]]@point.data, .(pid), function(x) data.frame(dist=fncSegRatio2length(x$rid, x$ratio, sno.ssn)))$dist

if(network == "rbm"){
  # Subset to remove fish above Snoqualmie Falls
  
  # Load topology lists for the network (previously created)
  # all segments downstream from a given seg
  load(paste0(mydir, "/", loadDir, "/sno.rbm.ssn/dnsegs.RData"))
  # all segments upstream from a given seg
  load(paste0(mydir, "/", loadDir, "/sno.rbm.ssn/upsegs.RData"))
  # all segments at the upper end/confluence of a given seg
  load(paste0(mydir, "/", loadDir, "/sno.rbm.ssn/jct.list.RData"))

  # Get lists of DHSVM-specific segments above and below Snoqualmie Falls (a natural anadromous barrier) and Tolt Reservoir dam
  segsAbvFalls = upsegs[[488]]
  segsAbvRes = upsegs[[179]]
  segsBlwFalls = unique(sno.ssn@data$rid)[! unique(sno.ssn@data$rid) %in% segsAbvFalls]
  segsBlwRes = unique(segsBlwFalls)[! unique(segsBlwFalls) %in% segsAbvRes]
  segsAccessible = intersect(segsBlwFalls, segsBlwRes)
  
  bass.segs = unique(sno.ssn@predpoints@SSNPoints[[fish_other.id]]@point.data$rid)
  segs2keep = bass.segs[bass.segs%in%segsAccessible]
  sno.ssn@predpoints@SSNPoints[[fish_other.id]]@point.data$segs2keep = 0
  sno.ssn@predpoints@SSNPoints[[fish_other.id]]@point.data$segs2keep[sno.ssn@predpoints@SSNPoints[[fish_other.id]]@point.data$rid %in% segs2keep] = 1
}

# Save back out as shapefile, now with all appropriate data needed by SSN
lmb_prevalent = sno.ssn@predpoints@SSNPoints[[fish_other.id]]
lmb_prevalent@point.data = lmb_prevalent@point.data[,c("pid","NEAR_X","NEAR_Y","rid","ratio","upDist","netID","locID","length2segBase")]
lmb_out = SpatialPointsDataFrame(coords=lmb_prevalent@point.coords,proj4string = lmb_shp@proj4string,bbox = lmb_prevalent@points.bbox,data = lmb_prevalent@point.data)
writeOGR(lmb_out, paste0(mydir,"/",loadDir,"/", ssn.folder),"lmb_prevalent", driver="ESRI Shapefile", overwrite_layer = T)

if(network =="rbm"){
  pids2keep = lmb_out@data$pid[lmb_out@data$rid %in% segs2keep]
  lmb_sub = subset(lmb_out, lmb_out@data$pid %in% pids2keep)
  writeOGR(lmb_sub, paste0(mydir,"/",loadDir,"/", ssn.folder),"lmb_prevalent", driver="ESRI Shapefile", overwrite_layer = T)
}


# Reload
lmb_shp2 = readOGR(paste0(mydir,"/",loadDir,"/", ssn.folder),"lmb_prevalent") #test
sno.ssn = importSSN(paste0(loadDir, "/", ssn.folder), predpts = 'preds')
sno.ssn = importPredpts(sno.ssn, "lmb_prevalent", "ssn")
fish_other.id = 2 #this is the 2nd preds file loaded in the SSN (see sno.ssn@predpoints@ID)
sno.ssn@predpoints@ID[fish_other.id] = "fish_other"
td = getSSNdata.frame(sno.ssn, "fish_other")

# Remove fish from 'missed' segs
missed = c(0, which(!1:nrow(sno.ssn@data)%in%sort(unique(sno.ssn@predpoints@SSNPoints[[1]]@point.data$rid))))
lmb_shp2 = subset(lmb_shp2, !lmb_shp2@data$rid %in% missed)


# Subsample fish to keep; produce 10 randomizations to choose from later
n = 1
for(i in 1:n){
  set.seed(i)
  #lmb2use = sample(sno.ssn@predpoints@SSNPoints[[bass.id]]@point.data$pid, numeric.parameters[2,"nFish"])
  lmb2use = sample(lmb_shp2@data$pid, parameters["fish_other","nFish"])
  #plot(streams, lwd = streams@data$meanmsq / 100000000, col = "darkgray")
  #plot(lmb_shp2, add = 2, col = 2, cex = 0.5)
  #plot(lmb_shp2[lmb_shp@data$pid %in% lmb2use,], add = 2, col = 4, cex = 0.5)
  lmb_prevalent = subset(lmb_shp2, lmb_shp2@data$pid %in% lmb2use)
  row.names(lmb_prevalent@data) = NULL
  lmb_prevalent@data$pid = as.numeric(row.names(lmb_prevalent@data))
  writeOGR(lmb_prevalent, paste0(mydir,"/",loadDir,"/", ssn.folder), paste0("lmb_prevalent_",i), driver="ESRI Shapefile", overwrite_layer = T)
}

# Read back in subsetted shapefile [this how a file will be loaded for modeling]
sno.ssn = importSSN(paste0(loadDir, "/", ssn.folder), predpts = 'preds')
sno.ssn = importPredpts(sno.ssn, "lmb_prevalent_1", "ssn")
fish_other.id = 2 #this is the 2nd preds file loaded in the SSN (see sno.ssn@predpoints@ID)
sno.ssn@predpoints@ID[fish_other.id] = "fish_other"
td = getSSNdata.frame(sno.ssn, "fish_other")
#plot(sno.ssn,PredPointsID = "fish_other")

