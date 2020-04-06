
## First go run everything until "RUN SIMULATION" in SnoIBMvx.R


library(profvis)

rprof.name = "Rprof1.out"
Rprof(rprof.name,line.profiling = T)

# The following file can't have a loop for species, iterations or climate scenarios,
# and it can't change the working directory.
source("code/SnoIBM_benchmark.R")

Rprof(NULL)
prv.obj<- profvis(prof_input = rprof.name)
print(prv.obj)



end.time = proc.time()
runtime = (end.time[3]-start.time[3])/60

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
salmon.parms<-NULL;for(i in 1:ncol(parameters)) {var<-colnames(parameters)[i];salmon.parms[i]<-paste0(var,": ",parameters[1,var]); fish_other.parms = NULL}
if(SecondSpecies == TRUE) for(i in 1:ncol(parameters)) {var<-colnames(parameters)[i];fish_other.parms[i]<-paste0(var,": ",parameters[2,var])}

write(c("RUN INFO:", run.info," ","FISH 1 PARAMETERS:",salmon.parms," ","FISH 2 PARAMETERS:",fish_other.parms),file = textDir)
