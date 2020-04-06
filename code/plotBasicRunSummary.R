
# Script to plot data from one species, one year

# COLORS ----------------------------------------------------------
# Script currently uses blue and orange
#Min/max
c1.0<- rgb(226,226,226,150,NULL,255) #hex #e2e2e2, light gray
c1.1<- rgb(252,217,156,150,NULL,255) #hex #fcd99c96 orange
c1.2<- rgb(247,246,187,150,NULL,255) #hex #f7f6bb96 yellow
c1.3<- rgb(176,191,252,150,NULL,255) #hex #b0bffc96 blue
c1.4<- rgb(209,167,207,150,NULL,255) #hex #d1a7cf purple
c1.5<- rgb(196,237,197,150,NULL,255) #hex #e2e2e2 green
c1.6<- rgb(171,201,205,150,NULL,255) #hex #abc9cd aqua

#Q1/Q3
c2.0<- rgb(142,142,142,200,NULL,255) #hex #8e8e8e, gray
c2.1<- rgb(234,173,68,200,NULL,255) #hex #eaad44c8 orange
c2.2<- rgb(239,237,95,200,NULL,255) #hex #efed5fc8 yellow
c2.3<- rgb(128,153,252,200,NULL,255) #hex #809ffcc8 blue
c2.4<- rgb(137,101,136,200,NULL,255) #hex #896588 purple
c2.5<- rgb(81,198,83,200,NULL,255) #hex #8e8e8e green
c2.6<- rgb(74,146,155,200,NULL,255) #hex #4a929b aqua

#Median
c3.0<- rgb(5,5,5,255,NULL,255) #black
c3.1<- rgb(244,155,2,255,NULL,255) #hex #f49b02ff orange
c3.2<- rgb(206,193,8,255,NULL,255) # hex #cec108ff yellow
c3.3<- rgb(3,38,178,255,NULL,255) #hex #0326b2ff blue
c3.4<- rgb(97,18,104,255,NULL,255) #hex #611268 purple
c3.5<- rgb(1,137,3,255,NULL,255) #green
c3.6<- rgb(66,110,130,255,NULL,255) #hex #426e82 aqua


# ----------------------------------------------------

ifelse(SecondSpecies == FALSE, spp.list <- "salmon", spp.list <- c("salmon", "fish_other"))
for(spp in spp.list){
  
# DIRECTORIES -----------------------------------------------------
dataDir<- plotDir<- paste0(mydir,"/",outputDir)

# DATE FORMATS ------------------------------------------------------
ndays<- 365
JulianDate.Begin = julian(first.date, as.Date(paste0(cs,"-01-01")))
JulianDate.End = julian(last.date, first.date) + JulianDate.Begin
#LeapYears<- c("2008","2012","2016","2020","2024")
#jd.adj = abs(first.date - as.Date(paste0(cs,"-01-01")))
#if(cs==2014) {jd.adj<- 16070}
#if(cs==2015) {jd.adj<- 16435}
#JulianDate.Begin<- ndays-(jd.adj-julian(first.date)) #258 is Sep 15, 274 is Oct 1
#JulianDate.End = julian(last.date)-jd.adj+1

# LOAD DATA ------------------------------------------------------------
load(paste0(dataDir, "/",spp,".array.",cs,".",iter,".RData"))
load(paste0(dataDir, "/",spp,".finalstep.",cs,".",iter,".RData")) 

if (spp == "salmon") {
  # limit to first replicate simulation
  fish.array = salmon.array
  fish.output = salmon.finalstep
} else { 
  # limit to first replicate simulation
  fish.array = fish_other.array
  fish.output = fish_other.finalstep
}

#Dimensions of arrays
nfish<- dim(fish.array)[1]
nvars<- dim(fish.array)[2]
ntime<- dim(fish.array)[3]

# MAKE FILTERS ------------------------------------------------------------

#Spawned filter (same as pre-emergence filter, if needed)
spwnd<- fish.array[,"emrg",]
spwnd[spwnd!=0]<-1

#Post-emergence filter
emgd<- fish.array[,"emrg",]
if(spp != "salmon") emgd[emgd!=1]<- 1
emgd[emgd!=1]<-NA

#Smolted filter
#for daily metrics
smolted<- fish.array[,"survive",] #could also get from smolted columm
smolted[smolted!=2]<-NA
smolted[smolted==2]<-1
#for one-time metrics
sm.final<- fish.output[,"survive"]
sm.final[sm.final!=2]<- NA
sm.final[sm.final==2]<- 1

#All fish survival filter
#for one-time metrics (dateEm, dateOm, weight)
srv.all<- fish.output[,"survive"]
srv.all[srv.all<=0]<- NA
srv.all[srv.all>0]<- 1
#for daily metrics (growth, consumption etc.)
s.all<- array(srv.all,dim=c(nfish,ntime))

#Yearling survival filter (those remaining in the stream)
#for one-time metrics (dateEm, dateOm, weight)
srv.age1<- fish.output[,"survive"]
srv.age1[srv.age1!=1]<- NA
#for daily metrics (growth, consumption etc.)
s.age1<- array(srv.age1,dim=c(nfish,ntime))

#Subyearling survival filter (those that smolted in first year)
#for one-time metrics (dateEm, dateOm, weight)
srv.age0<- fish.output[,"survive"]
srv.age0[srv.age0!=2]<- NA
srv.age0[srv.age0==2]<- 1
#for daily metrics (growth, consumption etc.)
s.age0<- array(srv.age0,dim=c(nfish,ntime))


# QUANTILE PLOTS -----------------------------------------------------------

#Daytime values of quantiles across the network for any given point in time 
png(paste0(plotDir,"/1SimThruTime.",spp,".",iter,".png"),width=7,height=10,units="in",res=300)
par(mfcol=c(5,3),mar=c(3,4.1,1,1.5)+0.1,las=1,oma=c(0,0.5,1,0))

#Cycle through variables to plot
var.list<- c("WT","pSpwnd","pEmgd","pSmolt","survive","conspecificdensity","totaldensity","movedist","upDist","pvals","consInst","growth","weight.age0","weight.age1")
if(spp!="salmon") var.list<- c("WT","blank","blank","blank","survive","conspecificdensity","totaldensity","movedist","upDist","pvals","consInst","growth","weight.all")
for(var in var.list){
  
if(var!="blank"){ 
  if(var=="WT"){
    dat<- fish.array[,var,]*s.all
    dat<- apply(dat,2,quantile,na.rm=T)
  } else if(var=="pSpwnd") { #Proportion spawned
    dat<- apply(spwnd,2,sum,na.rm=T)/nfish
  } else if(var=="pEmgd") { #Proportion emerged
    dat<- apply(emgd,2,sum,na.rm=T)/nfish
  } else if(var=="pSmolt") { #Proportion of survivors that smolted
    dat<- apply(smolted,2,sum,na.rm=T)/sum(srv.all,na.rm=T)
  } else if(var=="survive") { #Proportion surviving
    s<- fish.array[,var,]
    s[s>0]<-1; s[s<=0]<-NA #values: 2=smolted, 1=alive yearling, 0=killed randomly, -1=predation victim
    dat<- apply(s,2,sum,na.rm=T)/nfish
  } else if(var=="weight.age0"){ #final weight for fish that smolted
    dat<- fish.array[,"weight",]*s.age0*emgd
    dat<- apply(dat,2,quantile,na.rm=T)
  } else if(var=="weight.age1"){ #final weight for fish that stayed
    dat<- fish.array[,"weight",]*s.age1*emgd
    dat<- apply(dat,2,quantile,na.rm=T)
  } else if(var=="weight.all"){ #final weight for all fish
    dat<- fish.array[,"weight",]*s.all*emgd
    dat<- apply(dat,2,quantile,na.rm=T)
  } else{
    dat<- fish.array[,var,]*s.all*emgd
    dat<- apply(dat,2,quantile,na.rm=T)
  }
  dat<- t(dat)
  if(var=="upDist" & network == "nhd1") dat<- dat/1000
  
  #day or night
  d<- seq(2,ntime,2)
  n<- seq(1,ntime,2)
  if(var %in% c("pSpwnd","pEmgd","pSmolt","survive")) {
    dat.d<- dat[d]
  } else {
    dat.d<- dat[d,]
    dat.n<- dat[n,]
  }
  if(var %in% c("consInst","growth","movedist")) {dat.d<- dat.d+dat.n} #need to account for day and night values
  if(var=="WT") {dat.d<- (dat.d+dat.n)/2}
  
  if(var=="upDist"){ylm<-c(1,75); ylb<-"Distance upstream (km)"}
  if(var=="pvals"){ylm<-c(0,1); ylb<-"Proportion Consumed"}
  if(var=="consInst"){ylm<-range(dat.d[,3], na.rm = T); ylb<-expression(paste("Consumption (g g ",d^-1,")"))}
  if(var=="movedist"){ylm<-c(0,7); ylb<-"Movement distance (km)"}
  if(var=="conspecificdensity"){ylm<-range(dat.d[,3], na.rm = T); ylb<-expression("Conspecific density per"~m^2)}
  if(var=="totaldensity"){ylm<-range(dat.d[,3], na.rm = T); ylb<-expression("Total density per"~m^2)}
  if(var=="growth"){ylm<-c(0,0.04); ylb<-expression(paste("Growth (g g ",d^-1,")"))}
  if(var=="weight.all"){ylm<-range(dat.d[,3], na.rm = T); ylb<-"Fish mass (g)"}
  if(var=="weight.age0"){ylm<-range(dat.d[,3], na.rm = T); ylb<-"Smolt mass (g)"}
  if(var=="weight.age1"){ylm<-range(dat.d[,3], na.rm = T); ylb<-"Yearling mass (g)"}
  if(var=="WT"){ylm<-c(0,30); ylb<-expression("Experienced temperature "~(degree ~ C))}
  if(var=="survive"){ylm<- c(0,1); ylb<- "Proportion alive"}
  if(var=="pSpwnd"){ylm<- c(0,1); ylb<- "Proportion spawned"}
  if(var=="pEmgd"){ylm<- c(0,1); ylb<- "Proportion emerged"}
  if(var=="pSmolt"){ylm<- c(0,1); ylb<- "Proportion smolted"}
  
  months<- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

  # Beginning date to start plotting some variables; starts after fish begin emerging
  if(var %in% c("WT","survive","pSpwnd","pEmgd","pSmolt")) {
    minX<- 1
  } else{
    minX<- apply(emgd[,d],2,sum,na.rm=T); minX<- trunc(which(minX>0))[1]
  }
  if(minX==1){
    val1<- as.numeric(format(first.date,"%m"))
    val2<- as.numeric(format(last.date,"%m"))
    months<- months[c(val1:12, 1:val2)]
    lng.out<- length(months)
  } else{
    val1<- as.numeric(format(as.Date(minX + 1, origin = first.date),"%m"))
    val2<- as.numeric(format(last.date,"%m"))
    if(val1>=9 & val1<=12) months<- months[c(val1:length(months),1:val2)]
    if(val1>=1 & val1<=8) months<- months[val1:val2]
    lng.out<- length(months)
  }
  maxX<- ceiling(ntime/2) - 1
}
  
  if(var %in% c("pSpwnd","pEmgd","pSmolt","survive")){
    plot(dat.d,ylim=ylm,ylab=ylb,type='n',xaxt='n',xlab="",xlim=c(minX,maxX))
    axis(1,at=seq(minX,maxX,length.out=lng.out),labels=months,cex.axis=0.8)
    lines(dat.d,lwd=2,col=c3.3)
    
  } else if(var=="blank"){ plot(1:2,type='n',ylab="",xlab="",axes=F)
  } else{
    plot(dat.d[,3],ylim=ylm,ylab=ylb,type='n',xaxt='n',xlab="",xlim=c(minX,maxX))
    axis(1,at=seq(minX,maxX,length.out=lng.out),labels=months,cex.axis=0.8)
    
    #Min/Max
    polygon(c(minX:maxX,rev(minX:maxX)),c(dat.d[minX:maxX,1],rev(dat.d[minX:maxX,5])),border=NA,col=c1.3)
    
    #Q1/Q3
    polygon(c(minX:maxX,rev(minX:maxX)),c(dat.d[minX:maxX,2],rev(dat.d[minX:maxX,4])),border=NA,col=c2.3)
    
    #Median
    lines(dat.d[,3],lwd=2,col=c3.3)
  }
  if(var=="weight.age0") legend("topleft",legend=paste0("n=",sum(srv.age0,na.rm=T)," fish"),bty='n')
  if(var=="weight.age1") legend("topleft",legend=paste0("n=",sum(srv.age1,na.rm=T)," fish"),bty='n')
  if(var=="pSmolt") legend("topleft",legend=paste0("out of ",sum(srv.all,na.rm=T)," final survivors"),bty='n')
  if(var=="survive") legend("bottomleft",legend=paste0("n=",sum(srv.all,na.rm=T),"/",nfish," fish"),bty='n')
}
plot(1:5,type='n',axes=F,ylab="",xlab="")
legend("topleft",legend=cs,col=c(c3.3,c3.1),lwd=2,bty='n')
dev.off()

# BOXPLOTS --------------------------------------------------------------
set.list<- NULL

if (spp == "salmon") {
  var.list<- c("WT","dateSp","dateEm","dateOm","totaldensity","movedist","upDist","legend","pvals","consInst","growth","weight")
} else {
  var.list<- c("WT","blank","blank","blank","totaldensity","movedist","upDist","legend","pvals","consInst","growth","weight")
}
for(var in var.list){
  if(var!="blank" & var!="legend"){ 
    
  if(var=="WT"){ #Temperatures experienced by survivors
    df.y1<- fish.array[,var,]*s.all
    
  } else if(var=="survive") { #Proportion alive
    df.y1<- fish.output[,var]
    df.y1[df.y1==0]<- NA
    df.y1[df.y1>0]<-1
    df.y1<- sum(df.y1,na.rm=T)/nfish

  } else if( var=="weight"){
    df.y1<- fish.output[,var]*s.all
    df.y1[df.y1==0]<-NA

  } else if( var %in% c("dateSp","dateEm","dateOm")){ #dates Spawned, Emerged, or Smolted
    df.y1<- fish.output[,var]
    df.y1[is.na(df.y1)]<- 0 #for dateOm where there are NAs
    # Re-set date formats and then turn into Julian date in numeric format:
    df.y1<- as.POSIXlt(df.y1,origin="1970-01-01")
    df.y1<- floor(julian(df.y1,first.date)) + JulianDate.Begin
    df.y1<- as.numeric(df.y1)
    df.y1[df.y1<0]<- ndays+df.y1[df.y1<0] #returning numeric to NAs for dateOm
    df.y1[df.y1==(ndays)]<- NA #ditto

  } else if(var=="blank"){
    set.list<- list(seq(1:5))
  } else{
    df.y1<- fish.array[,var,]*s.all*emgd
    if(var%in%c("growth","consInst","conspecificdensity","movedist")){ #need to double the half-day time step
      df.y1<- df.y1*2
    }
  }
  if(var=="upDist") df.y1<- df.y1/1000
  
  set.list[[which(var.list==var)]]<- list(df.y1)
  
  }
}
names(set.list)<- var.list


png(paste0(plotDir,"/1Boxplots.",spp,".",iter,".png"),width=9,height=9,units="in",res=600)
par(mfcol=c(4,3),mar=c(3,4.1,2,1.5)+0.1,las=1,oma=c(1,0.5,1,0),cex=0.8)

for(var in var.list){
  if(var=="upDist"){ylb<-"Distance upstream (km)"}
  if(var=="pvals"){ylb<-"Proportion Consumed"}
  if(var=="consInst"){ylb<-expression(paste("Consumption (g g ",d^-1,")"))}
  if(var=="movedist"){ylb<-"Movement distance (km)"}
  if(var=="conspecificdensity"){ylb<-expression("Conspecific density per"~m^2)}
  if(var=="totaldensity"){ylb<-expression("Total density per"~m^2)}
  if(var=="growth"){ylb<-expression(paste("Growth (g g ",d^-1,")"))}
  if(var=="weight"){ylb<-"Final mass (g)"}
  if(var=="WT"){ylb<-expression("Experienced temperature "~(degree ~ C))}
  if(var=="dateSp"){ylb<- "Spawn date"}
  if(var=="dateEm"){ylb<- "Emergence date"}
  if(var=="dateOm"){ylb<- "Smolt date"}
  
  if(var!="blank" & var!="legend"){ 
    boxplot(list(set.list[[var]][[1]]),ylab=ylb,outline=F,names=F,xaxt='n')
    boxplot(list(set.list[[var]][[1]]),add=T,col=c1.3,border=c3.3,outline=F,names=F,xaxt='n')
    #axis(side=1,at=1.5,labels=cs,cex.axis=0.9,padj=-0.6) #only use this for multiple years
    box()
  } else if(var=="blank"){
    plot(1:11,0:10,type='n',axes=F,xlab="",ylab="")
  } else if(var=="legend"){
    #Legend
    plot(1:11,0:10,type='n',axes=F,xlab="",ylab="")
    polygon(c(2:3,rev(2:3)),c(rep(5.1,2),rep(5.9,2)),border=NA,col=rgb(226,226,226,150,NULL,255)) #"#e2e2e2" gray
    text(3.5,5.5,"Range",cex=0.9,adj=0)
    polygon(c(2:3,rev(2:3)),c(rep(6.3,2),rep(6.7,2)),border=NA,col=rgb(142,142,142,200,NULL,255)) #"#8e8e8e" gray
    text(3.5,6.4,"IQR",cex=0.9,adj=0)
    lines(2:3,rep(7.25,2),lwd=2,col=1,lty=3) #black
    text(3.5,7.25,"Median",cex=0.9,adj=0)
    
    polygon(c(2:3,rev(2:3)),c(rep(3.3,2),rep(3.7,2)),border=NA,col=c3.3)
    text(3.5,3.5,cs,adj=0)
  }
}

dev.off()
} #end spp

