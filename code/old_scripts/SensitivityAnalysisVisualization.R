# Sensitivity Analysis Visualization

spp = "salmon"
cs = 2014
tag = "P" #A"
mydir = getwd()
#dataDir = "G:/HollingsIBM_Dec19/A.2014"
dataDir = paste0("data.out/", tag, ".2014")
outputDir = paste0(dataDir, "/SA")
# A.2014:
  iter.list = 1:300
  missing = c(178, 180, 235)
  iter.list = setdiff(iter.list,missing)
# P.2014:
  iter.list = 1:375
  missing = c(104,124,130,214,306,310,330,363,374) #P.2014
  not.finished = c(60,89,90,150,174:180,203:210,233:240,264:270,294:300)
  iter.list = setdiff(iter.list, missing); iter.list = setdiff(iter.list,not.finished)

run.summary = read.csv(paste0(outputDir, "/run.info.SA_all.csv"), header = TRUE, stringsAsFactors = FALSE)
for(i in c("spawn.date.begin","om.date.end","om.date.taper")){ run.summary[,i] = as.Date(run.summary[,i],origin = "1970-01-01")}
# for(nFish in c(2241, 1120, 1792, 2689, 3361)){ # have to do these separately due to different sized arrays
#   load(paste0(outputDir,"/", spp, ".finalstep.",tag,".",cs,".SA", nFish, ".RData"))
#   assign(paste0("salmon.finalstep.",nFish),salmon.finalstep)
# }
load(paste0(outputDir,"/", spp, ".finalstep.",tag,".",cs,".SA.all.RData"))



# See how survival infuenced runtime ####
quantile(run.summary$runtime)
plot(run.summary$runtime, run.summary$survival.min) #no pattern with other variables


# Identify legitimate runs and remove runs that gave unrealistic results ####
# which runs had no smolts 
  fish.finalstep = salmon.finalstep
  nosmolts = 0
  for(i in 1:dim(fish.finalstep)[3]) ifelse(any(fish.finalstep[,"survive",i] == 2, na.rm = T), nosmolts[i]<- 0, nosmolts[i]<- i)
  (nosmolts = nosmolts[nosmolts > 0 & !is.na(nosmolts)])
  rm(fish.finalstep)
  # A.2014: 5 runs out of 300
  # P.2014: 40 runs out of 325

# which runs had no survivors
  fish.finalstep = salmon.finalstep
  nosurvivors = 0
  for(i in 1:dim(fish.finalstep)[3]) ifelse(any(fish.finalstep[,"survive",i] > 0, na.rm = T), nosurvivors[i]<- 0, nosurvivors[i]<- i)
  (nosurvivors = nosurvivors[nosurvivors > 0 & !is.na(nosurvivors)])
  rm(fish.finalstep)
  # A.2014: 0 runs
  # P.2014: 4 runs

# which runs did fish grow way too large
  fish.finalstep = salmon.finalstep
  toobig = 0
  for(i in 1:dim(fish.finalstep)[3]) ifelse(any(fish.finalstep[,"survive",i] < 100, na.rm = T), toobig[i]<- 0, toobig[i]<- i)
  (toobig = toobig[toobig > 0 & !is.na(toobig)])
  rm(fish.finalstep)
  # A.2014: 0 runs
  # P.2014: 0 runs


# # REMOVE these runs
# runs2rmv = sort(c(nosmolts,toobig))
# run.summary.original = run.summary
# run.summary = run.summary[-runs2rmv,]
# dim(run.summary)
# salmon.finalstep.original = salmon.finalstep
# salmon.finalstep = salmon.finalstep[,,-runs2rmv]
# dim(salmon.finalstep)

# look at distribution of parameters ####
# A.2014
png(paste0(outputDir,"/SA.plots/parameter_distributions.png"),units="in",height=9,width=12,res=600)
par(mfrow=c(4,4), cex = 0.7, mar = c(4,2,1,1), oma=c(1,1,1,1), las = 1)
hist(as.numeric(run.summary$spawn.date.begin), main = "", xlab = "spawn.date.begin", ylab = "")
hist(run.summary$nSpawnDays, main = "", xlab = "nSpawnDays", ylab = "")
hist(run.summary$spawn.date.shape, main = "", xlab = "spawn.date.shape", ylab = "")
hist(run.summary$ATU.crit, main = "", xlab = "ATU.crit", ylab = "")
hist(run.summary$mvmt.scalar, main = "", xlab = "mvmt.scalar", ylab = "")
hist(run.summary$mvdist.shape, main = "", xlab = "mvdist.shape", ylab = "")
hist(run.summary$ration.hi, main = "", xlab = "ration.hi", ylab = "")
hist(run.summary$pred.en.dens, main = "", xlab = "pred.en.dens", ylab = "")
hist(run.summary$prey.en.dens, main = "", xlab = "prey.en.dens", ylab = "")
hist(run.summary$max.density, main = "", xlab = "max.density", ylab = "")
hist(run.summary$survival.min, main = "", xlab = "survival.min", ylab = "")
hist(run.summary$survival.max, main = "", xlab = "survival.max", ylab = "")
hist(run.summary$survival.shape, main = "", xlab = "survival.shape", ylab = "")
hist(run.summary$om.mass, main = "", xlab = "om.mass", ylab = "")
hist(as.numeric(run.summary$om.date.taper), main = "", xlab = "om.date.taper", ylab = "")
hist(as.numeric(run.summary$om.date.end), main = "", xlab = "om.date.end", ylab = "")
dev.off()

# P.2014
png(paste0(outputDir,"/SA.plots/parameter_distributions.png"),units="in",height=9,width=12,res=600)
par(mfrow=c(5,5), cex = 0.7, mar = c(4,2,1,1), oma=c(1,1,1,1), las = 1)
hist(as.numeric(run.summary$spawn.date.begin), main = "", xlab = "spawn.date.begin", ylab = "")
hist(run.summary$ATU.crit, main = "", xlab = "ATU.crit", ylab = "")
hist(run.summary$ration.hi, main = "", xlab = "ration.hi", ylab = "")
hist(run.summary$pred.en.dens, main = "", xlab = "pred.en.dens", ylab = "")
hist(run.summary$prey.en.dens, main = "", xlab = "prey.en.dens", ylab = "")
hist(run.summary$survival.min, main = "", xlab = "survival.min", ylab = "")
hist(run.summary$survival.max, main = "", xlab = "survival.max", ylab = "")
hist(run.summary$om.mass, main = "", xlab = "om.mass", ylab = "")
hist(as.numeric(run.summary$om.date.end), main = "", xlab = "om.date.end", ylab = "")
plot(1:10,1:10,type = 'n', xlab="", ylab="", axes=F)

hist(run.summary$max.initial.mass.b, main = "", xlab = "max.initial.mass.b", ylab = "")
hist(run.summary$mass.shape.b, main = "", xlab = "mass.shape.b", ylab = "")
hist(run.summary$mvdist.shape.b, main = "", xlab = "mvdist.shape.b", ylab = "")
hist(run.summary$mvmt.scalar.b, main = "", xlab = "mvmt.scalar.b", ylab = "")
hist(run.summary$ration.hi.b, main = "", xlab = "ration.hi.b", ylab = "")
hist(run.summary$max.density.b, main = "", xlab = "max.density.b", ylab = "")
hist(run.summary$pred.en.dens.b, main = "", xlab = "pred.en.dens.b", ylab = "")
hist(run.summary$prey.en.dens.b, main = "", xlab = "prey.en.dens.b", ylab = "")
hist(run.summary$survival.min.b, main = "", xlab = "survival.min.b", ylab = "")
hist(run.summary$max.pred.prob.b, main = "", xlab = "max.pred.prob.b", ylab = "")
hist(run.summary$pred.temp.crit.b, main = "", xlab = "pred.temp.crit.b", ylab = "")
hist(run.summary$pred.mass.crit.b, main = "", xlab = "pred.mass.crit.b", ylab = "")
hist(run.summary$pred.move.b, main = "", xlab = "pred.move.b", ylab = "")

dev.off()


# Look at relationships between parameters and metrics ####
colnames(run.summary)
colnames(salmon.finalstep)


# Filters
alive = salmon.finalstep[,"survive",]
subyearlings = alive; subyearlings[subyearlings != 2] = NA; subyearlings[subyearlings == 2] = 1
yearlings = alive; yearlings[yearlings != 1] = NA
alive[alive > 0] = 1; alive[alive == 0] = NA

(subs = apply(subyearlings,2,sum,na.rm=T)) #no. of surviving subyearlings
(years = apply(yearlings,2,sum,na.rm=T)) #no. of surviving yearlings

(prop.yearlings = years/(subs + years)) #proportion yearlings
(survival = (subs + years) / run.summary$nFish) #proportion survived

quantile(survival, na.rm = T); quantile(prop.yearlings, na.rm = T)
boxplot(list(survival, prop.yearlings))



## Random Forest
library(randomForest)
library(randomForestExplainer)

parms = colnames(run.summary)[!colnames(run.summary) %in% c("runtime","seed", "ration.lo")]
x = run.summary[,parms]
colnames(salmon.finalstep)
var = "weight" 
# pick one:
  y = apply(salmon.finalstep[,var,], 2, median, na.rm=T)
  #y = apply(salmon.finalstep[,var,] * alive, 2, median, na.rm=T)
  y = apply(salmon.finalstep[,var,] * subyearlings, 2, median, na.rm=T); var = "weight.subyearlings"
  y = apply(salmon.finalstep[,var,] * yearlings, 2, median, na.rm=T); var = "weight.yearlings"
  y = survival; var = "survival"
  y = prop.yearlings; var = "prop.yearlings"
mydat = cbind(y, x)
na.idx = which(is.na(mydat[,1]))
if(length(na.idx) > 0) mydat = mydat[-na.idx,]
set.seed(1)
#(rf1 = randomForest(mydat[,c(2:ncol(mydat))], mydat[,1], mtry = 10, ntree=5000, importance = TRUE, importanceSD = TRUE))
(rf1 = randomForest(y ~ ., data = mydat, mtry = 10, ntree=5000, importance = TRUE, importanceSD = TRUE))
#(rf2 = randomForest(mydat[,c(2:ncol(mydat))], mydat[,1], mtry = 10, ntree=5000, localImp = TRUE))
imp = importance(rf1) #type:	either 1 or 2, specifying the type of importance measure (1=mean decrease in accuracy, 2=mean decrease in node impurity).
rf1$importanceSD
imp = cbind(imp, "impSD" = rf1$importanceSD)
imp[order(imp[,1], decreasing = TRUE),]

png(paste0(outputDir,"/SA.plots/",var,"_RF_default_plot.png"),units="in",height=6,width=8,res=150)
  varImpPlot(rf1)
dev.off()
rf1$rsq[5000]*100 #variance explained




# Visualize with RF Explainer package

# min depth of variable in trees
mindepth_frame = min_depth_distribution(rf1)
save(mindepth_frame, file = paste0(outputDir, "/SA.RF/", var,".mindepth.RData"))
load(paste0(outputDir, "/SA.RF/", var,".mindepth.RData"))
png(paste0(outputDir,"/SA.plots/",var,"_RF_mindepth.png"),units="in",height=6,width=6,res=150)
  plot_min_depth_distribution(mindepth_frame, mean_sample = "top_trees", min_no_of_trees = 100, mean_scale = TRUE, k = 14) #, main = var)
dev.off()

importance_frame = measure_importance(rf1)
save(importance_frame, file = paste0(outputDir, "/SA.RF/", var,".importance_frame.RData"))
load(paste0(outputDir, "/SA.RF/", var,".importance_frame.RData"))
importance_frame

png(paste0(outputDir,"/SA.plots/",var,"_RF_multiway_importance0.png"),units="in",height=4.5,width=5.5,res=200)
  plot_multi_way_importance(importance_frame, x_measure = "mean_min_depth", y_measure = "mse_increase", size_measure = "p_value", no_of_labels = 5)
dev.off()

# png(paste0(outputDir,"/SA.plots/",var,"_RF_multiway_importance1.png"),units="in",height=6,width=7,res=150)
#   plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes", no_of_labels = 5)
# dev.off()
# png(paste0(outputDir,"/SA.plots/",var,"_RF_multiway_importance2.png"),units="in",height=6,width=7,res=150)
#   plot_multi_way_importance(importance_frame, x_measure = "mse_increase", y_measure = "node_purity_increase", size_measure = "p_value", no_of_labels = 5)
# dev.off()
# png(paste0(outputDir,"/SA.plots/",var,"_RF_ggpairs_raw.png"),units="in",height=6,width=6,res=150)
#   plot_importance_ggpairs(importance_frame)
# dev.off()
# png(paste0(outputDir,"/SA.plots/",var,"_RF_multiway_ggpairs_ranks.png"),units="in",height=6,width=6,res=150)
#   plot_importance_rankings(importance_frame)
# dev.off()


# Interactions
var1 = "ATU.crit"
var2 = "spawn.date.begin"
png(paste0(outputDir,"/SA.plots/",var,"_RF_interactions_top2_grid.png"),units="in",height=4.5,width=5.5,res=200)
plot_predict_interaction(forest = rf1, data = mydat, variable1 = var1, variable2 = var2) #have to use formula version in RF call, above
dev.off()

# the following takes a LONG TIME to run
(vars <- important_variables(importance_frame, k = 5, measures = c("mean_min_depth", "no_of_trees")))
interactions_frame = min_depth_interactions(rf1, vars)
save(interactions_frame, file = paste0(outputDir, "/SA.RF/", var, ".interactions_frame.RData"))
load(paste0(outputDir, "/SA.RF/", var, ".interactions_frame.RData"))
head(interactions_frame[order(interactions_frame$occurrences, decreasing = TRUE), ])
png(paste0(outputDir,"/SA.plots/",var,"_RF_interactions.png"),units="in",height=6,width=10,res=150)
  plot_min_depth_interactions(interactions_frame)
dev.off()


# Summary GridPlot
num.parms = length(parameters)-2

for(importance.metric in c("mse_increase", "mean_min_depth", "no_of_nodes", "node_purity_increase", "no_of_trees", "times_a_root", "p_value")){
  vars = c("weight.yearlings","weight.subyearlings","dateOm","survival","prop.yearlings","dateEm")
  var = vars[1]
  load(paste0(outputDir, "/SA.RF/", var,".importance_frame.RData"))
  imp.dat = importance_frame[,c("variable",importance.metric)]
  if(tag == "A") imp.dat = imp.dat[-13,] # for A.2014 only - remove 'ration.lo' because we didn't test it
  for(var in vars[2:5]){
    load(paste0(outputDir, "/SA.RF/", var,".importance_frame.RData"))
    if(tag == "A") imp.dat = cbind.data.frame(imp.dat, importance_frame[-13,importance.metric]) #A.2014
    if(tag == "P") imp.dat = cbind.data.frame(imp.dat, importance_frame[,importance.metric])
  }
  colnames(imp.dat) = c("parameter", vars[1:5])
  imp.dat$dateEm = NA
  var = "dateEm"
  load(paste0(outputDir, "/SA.RF/", var,".importance_frame.RData"))
  if(tag == "A") em.list = c("ATU.crit","nSpawnDays","spawn.date.begin","spawn.date.shape") else em.list = c("ATU.crit","spawn.date.begin")
  for(v in em.list){
  imp.dat[imp.dat[,"parameter"] == v, 7] = importance_frame[importance_frame[,"variable"] == v, importance.metric]
  }
  
  row.names(imp.dat) = imp.dat[,1]
  imp.dat = imp.dat[,-1]
  imp.mat = t(as.matrix(scale(imp.dat)))
  
  png(paste0(outputDir,"/SA.plots/gridplot.", importance.metric, ".png"),units="in",height=6,width=6,res=300)
  par(las=1, cex=0.8, oma = c(0,4,0,0))
  if(importance.metric %in% c("mean_min_depth", "p_value")){
    image(1:6,1:num.parms,imp.mat[c(6,1:5),num.parms:1], axes= F, col = gray.colors(num.parms), xlab="", ylab="")
  } else{
    image(1:6,1:num.parms,imp.mat[c(6,1:5),num.parms:1], axes= F, col = gray.colors(num.parms)[num.parms:1], xlab="", ylab="")
  }
  #axis(1, at = seq(1:6), labels = colnames(imp.dat))
  axis(1, at = seq(1:6), labels = c("dateEm","mass.y", "mass.s", "dateOm", "survival", "p.yrlg"))
  axis(2, at = seq(1:num.parms), labels = row.names(imp.dat)[num.parms:1])
  box()
  dev.off()
}


# Original RF description
#https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm

# Random Forest Explainer package: some cool plots and summaries in here!!
#https://cdn.staticaly.com/gh/geneticsMiNIng/BlackBoxOpener/master/randomForestExplainer/inst/doc/randomForestExplainer.html
#https://cran.rstudio.com/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html

# How to implement RF
# https://www.r-bloggers.com/how-to-implement-random-forests-in-r/
#set.seed(100)
#train <- sample(nrow(mydat), 0.7*nrow(mydat), replace = FALSE)
#TrainSet <- mydat[train,]
#ValidSet <- myat[-train,]



# OLD one at a time plots

# Non-date Metrics (TU, consCum)
for(var in colnames(salmon.finalstep)[c(2,5)]){
  for(parm in colnames(run.summary)[c(1,3:10,12:17)]){
    png(paste0(outputDir,"/SA.plots/",var,"_",parm,".png"),units="in",height=6,width=6,res=150)
      ylm = range(salmon.finalstep[,var,] * alive, na.rm=T)
      plot(run.summary[,parm],apply(salmon.finalstep[,var,] * yearlings, 2, median, na.rm=T), xlab = parm, ylab = var, ylim = ylm, las = 1)
      points(run.summary[,parm], apply(salmon.finalstep[,var,] * subyearlings, 2, median, na.rm=T), col = 2)
      legend("topleft", legend=c("yearlings", "subyearlings"), col=c(1,2), pch = 19, bty='n')
    dev.off()
  }
}
var="weight" # needed different y-axis limits for subyearlings and yearlings
for(parm in colnames(run.summary)[c(1,3:10,12:17)]){
  png(paste0(outputDir,"/SA.plots/",var,"_",parm,".png"),units="in",height=6,width=4,res=150)
  par(mfrow=c(2,1), oma=c(0,0,0,0), mar = c(4,4,1,1))
  plot(run.summary[,parm],apply(salmon.finalstep[,var,] * yearlings, 2, median, na.rm=T), xlab = parm, ylab = var, las = 1)
  plot(run.summary[,parm],apply(salmon.finalstep[,var,] * subyearlings, 2, median, na.rm=T), xlab = parm, ylab = var, las = 1, col=2)
  dev.off()
}


# Date metrics
# @@@-WIP: dateEm should probably not be just for surviving subyearlings and yearlings...
for(var in colnames(salmon.finalstep)[c(7,8,9,11)]){
  for(parm in colnames(run.summary)[c(1,3:10,12:17)]){
    if(! var %in% c("dateDi", "datePr")){
      td.sub = salmon.finalstep[,var,] * subyearlings
      td.sub = as.data.frame(apply(td.sub,2,median,na.rm=T))
      td.sub = as.POSIXct(td.sub[,1], origin="1970-01-01")
      td.yrl = salmon.finalstep[,var,] * yearlings
      td.yrl = as.data.frame(apply(td.yrl,2,median,na.rm=T))
      td.yrl = as.POSIXct(td.yrl[,1], origin="1970-01-01")
      ylm = as.POSIXct(range(salmon.finalstep[,var,] * alive, na.rm=T), origin="1970-01-01")
    } else {
      td.sub = salmon.finalstep[,var,]
      td.sub = as.data.frame(apply(td.sub,2,median,na.rm=T))
      td.sub = as.POSIXct(td.sub[,1], origin="1970-01-01")
      td.yrl = td.sub
      ylm = as.POSIXct(range(salmon.finalstep[,var,], na.rm=T), origin="1970-01-01")
    }
      
    png(paste0(outputDir,"/SA.plots/",var,"_",parm,".png"),units="in",height=6,width=6,res=150)
    plot(run.summary[,parm], td.yrl, xlab = parm, ylab = var, ylim = ylm, las = 1)
    points(run.summary[,parm], td.sub, col = 2)
    legend("topleft", legend=c("yearlings", "subyearlings"), col=c(1,2), pch = 19, bty='n')
    dev.off()
  }
}

for(var in c("prop.yearlings", "survival")){
  if(var == "prop.yearlings") td = prop.yearlings
  if(var == "survival") td = survival
  for(parm in colnames(run.summary)[c(1,3:10,12:17)]){
    png(paste0(outputDir,"/SA.plots/",var,"_",parm,".png"),units="in",height=6,width=6,res=150)
    plot(run.summary[,parm], td, xlab = parm, ylab = var, ylim = c(0,1), las = 1)
    dev.off()
  }
}


# # Trying boxplot to capture more of the data - pretty messy/ugly
# for(var in colnames(salmon.finalstep)[c(7,8,9,11)]){
#   for(parm in colnames(run.summary)[c(1,3:10,12:17)]){
#     td = salmon.finalstep[,var,]
#     td2 = apply(td,2,quantile,na.rm=T)
#     td2 = as.data.frame(td2)
#     for(c in 1:25) td2[,c] = as.POSIXct(td2[,c], origin="1970-01-01")
#     png(paste0(outputDir,"/",var,"_",parm,"_.png"),units="in",height=6,width=6,res=150)
#     boxplot(td2, at = run.summary[,parm], names = round(run.summary[,parm],0), 
#             xlab = parm, ylab = var, las = 1, cex = 0.5)
#     dev.off()
#   }
# }
