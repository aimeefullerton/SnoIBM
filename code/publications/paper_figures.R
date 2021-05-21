# Figures used for Fullerton et al. manuscript describing results from climate and riparian scenario effects on Chinook salmon
# Last updated 23 Oct 2020

#--- Setup #####
# Load packages
library(tidyverse)
library(sf)
# Depends (packages that must be installed but don't need to be loaded):
# lubridate, ggpubr, SSN, abind, RColorBrewer

source("code/functions.R")

# Set up directory structure
data.in <- "data.in" #"/Volumes/BluPassport/SnoIBM/data.in"
data.out<- "data.out" #"/Volumes/BluPassport/SnoIBM/data.out"
ssn.folder <- "sno.rbm.ssn"
save.figures <- TRUE
plot.directory <- "plots"
if (!dir.exists(data.out)) {dir.create(data.out)}
if (!dir.exists(plot.directory)) {dir.create(plot.directory)}

# COLORS 
fncColors4Quantiles <- function(){
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
  
  c1 <- c(c1.0, c1.1, c1.2, c1.3, c1.4, c1.5, c1.6)
  c2 <- c(c2.0, c2.1, c2.2, c2.3, c2.4, c2.5, c2.6)
  c3 <- c(c3.0, c3.1, c3.2, c3.3, c3.4, c3.5, c3.6)
  
  return(list(c1, c2, c3))
}
mycolors <- fncColors4Quantiles()

riparian.scenario.list <- c("riparian0", "riparian1", "riparian2", "riparian3") #Baseline, Full restoration, Least protective, Partial restoration
climate.scenario.list <- c("bcc-csm1-1-m", "CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "HadGEM2-CC365", "HadGEM2-ES365", "IPSL-CM5A-MR", "MIROC5", "NorESM1-M")
ssn <- SSN::importSSN(paste0("data.in/sno.rbm.ssn"), predpts = 'preds')


#--- Gather data from scenarios to be compared #####
#  -- For IBM output figures ----
for(riparian.scenario in riparian.scenario.list){
  
# Gather historical climate scenario data
time.period  <- "historical"
for(scenario in climate.scenario.list){
  scenario.data <- NULL
  years <- 1995:2005
  for(yy in years){
    outdir <- paste0(scenario, ".A.", yy)
    load(file = paste0(data.out, "_", riparian.scenario, "/", outdir, "/salmon.finalstep.", yy, ".1.RData"))
    scenario.data <- rbind(scenario.data, salmon.finalstep)
  }
  scenario.data <- as.data.frame(scenario.data)
  scenario.data <- cbind(scenario = scenario, scenario.data)
  assign(paste0(scenario, ".data"), scenario.data); rm(scenario.data)
  rm(salmon.finalstep)
}

# combine scenarios into one dataframe
his.scenarios.data <- NULL
for(climate.scenario in climate.scenario.list){
  sd <- get(paste0(climate.scenario,".data"))
  his.scenarios.data <- rbind(his.scenarios.data, sd)
  rm(sd)
}

his.scenarios.data$dateSc <- his.scenarios.data$dateDi; his.scenarios.data$dateSc[his.scenarios.data$survive != -2] <- NA
his.scenarios.data$dateMo <- his.scenarios.data$dateDi; his.scenarios.data$dateMo[his.scenarios.data$survive != 0] <- NA

# prep data for ggplot2
his.scenarios.data <- his.scenarios.data %>% 
  select(scenario, survive, weight, dateSp, dateEm, dateOm, dateSc, dateMo) %>% 
  transmute(Scenario = as.factor(scenario),
            FinalState = as.factor(survive),
            Weight = as.numeric(weight),
            DateSpawn = lubridate::date(lubridate::as_datetime(dateSp, origin = "1970-01-01")),
            DateEmerge = lubridate::date(lubridate::as_datetime(dateEm, origin = "1970-01-01")),
            DateOutmigrate = lubridate::date(lubridate::as_datetime(dateOm, origin = "1970-01-01")),
            DateScour = lubridate::date(lubridate::as_datetime(dateSc, origin = "1970-01-01")),
            DateMort = lubridate::date(lubridate::as_datetime(dateMo, origin = "1970-01-01")))
levels(his.scenarios.data$FinalState) <- c("Scoured", "Stochastic", "Yearling", "Subyearling")
his.scenarios.data$YearSpawn <- lubridate::year(his.scenarios.data$DateSpawn)

# Save
assign(paste0(riparian.scenario, ".", time.period ), his.scenarios.data)
rm(his.scenarios.data, `bcc-csm1-1-m.data`, CanESM2.data, CCSM4.data, `CNRM-CM5.data`, `CSIRO-Mk3-6-0.data`, `HadGEM2-CC365.data`, `HadGEM2-ES365.data`, `IPSL-CM5A-MR.data`, MIROC5.data, `NorESM1-M.data`)
thedata <- get(paste0(riparian.scenario, ".", time.period ))
save(thedata, file = paste0("data.out/", riparian.scenario, ".", time.period , ".RData"))
rm(thedata)


# Gather future climate scenario data
time.period  <- "future"
for(scenario in scenario.list){
  scenario.data <- NULL
  years <- 2089:2099
  for(yy in years){
    outdir <- paste0(scenario, ".A.", yy)
    load(file = paste0(data.out, "_", riparian.scenario, "/", outdir, "/salmon.finalstep.", yy, ".1.RData"))
    scenario.data <- rbind(scenario.data, salmon.finalstep)
  }
  scenario.data <- as.data.frame(scenario.data)
  scenario.data <- cbind(scenario = scenario, scenario.data)
  assign(paste0(scenario, ".data"), scenario.data); rm(scenario.data)
  rm(salmon.finalstep)
}

# combine scenarios into one dataframe
fut.scenarios.data <- NULL
for(scenario in scenario.list){
  sd <- get(paste0(scenario,".data"))
  fut.scenarios.data <- rbind(fut.scenarios.data, sd)
  rm(sd)
}
fut.scenarios.data$dateSc <- fut.scenarios.data$dateDi; fut.scenarios.data$dateSc[fut.scenarios.data$survive != -2] <- NA
fut.scenarios.data$dateMo <- fut.scenarios.data$dateDi; fut.scenarios.data$dateMo[fut.scenarios.data$survive != 0] <- NA

# prep data for ggplot2
fut.scenarios.data <- fut.scenarios.data %>% 
  select(scenario, survive, weight, dateSp, dateEm, dateOm, dateSc, dateMo) %>% 
  transmute(Scenario = as.factor(scenario),
            FinalState = as.factor(survive),
            Weight = as.numeric(weight),
            DateSpawn = lubridate::date(lubridate::as_datetime(dateSp, origin = "1970-01-01")),
            DateEmerge = lubridate::date(lubridate::as_datetime(dateEm, origin = "1970-01-01")),
            DateOutmigrate = lubridate::date(lubridate::as_datetime(dateOm, origin = "1970-01-01")),
            DateScour = lubridate::date(lubridate::as_datetime(dateSc, origin = "1970-01-01")),
            DateMort = lubridate::date(lubridate::as_datetime(dateMo, origin = "1970-01-01")))
levels(fut.scenarios.data$FinalState) <- c("Scoured", "Stochastic", "Yearling", "Subyearling")
fut.scenarios.data$YearSpawn <- lubridate::year(fut.scenarios.data$DateSpawn)

# Save
assign(paste0(riparian.scenario, ".", time.period ), fut.scenarios.data)
rm(fut.scenarios.data, `bcc-csm1-1-m.data`, CanESM2.data, CCSM4.data, `CNRM-CM5.data`, `CSIRO-Mk3-6-0.data`, `HadGEM2-CC365.data`, `HadGEM2-ES365.data`, `IPSL-CM5A-MR.data`, MIROC5.data, `NorESM1-M.data`)
thedata <- get(paste0(riparian.scenario, ".", time.period ))
save(thedata, file = paste0("data.out/", riparian.scenario, ".", time.period , ".RData"))
rm(thedata)
}

# for Yearling growth plot:
for(time.period in c("historical", "future")){
  scenarios.data <- NULL
  for(scenario in climate.scenario.list){
    scenario.data <- NULL
    if(time.period == "historical") years <- 1995:2005
    if(time.period == "future") years <- 2089:2099
    
    for(yy in years){
      outdir <- paste0(scenario, ".A.", yy)
      load(file = paste0(data.out,"_riparian0/", outdir, "/salmon.array.", yy, ".1.RData"))
      
      # Post-emergence filter
      emgd <- salmon.array[,"emrg",]
      emgd[emgd != 1] <- NA
      
      # Potential yearling survival filter
      srv.age1 <- salmon.array[,"survive", dim(salmon.array)[3]]
      srv.age1[srv.age1 != 1] <- NA
      s.age1 <- array(srv.age1, dim = c(dim(salmon.array)[1], dim(salmon.array)[3]))
      
      # Get growth for potential yearlings only once emerged and only for fish that survived to the end
      dat <- salmon.array[,"growth",] * s.age1 * emgd
      dat <- dat[,seq(2, dim(salmon.array)[3], 2)] + dat[,seq(1, dim(salmon.array)[3], 2)]
      
      # Take mean of growth across these fish
      dat <- apply(dat, 2, mean, na.rm = T) 
      
      # Remove leap year's extra day:
      if(yy %in% c(1996, 2000, 2004, 2092, 2096)){
        dat <- dat[-60]
      }
      scenario.data <- cbind(scenario.data, dat)
    }
    scenario.data[is.nan(scenario.data)] <- NA
    scenarios.data <- abind::abind(scenarios.data, scenario.data, along = 3)
  }
  assign(paste0("growth.", time.period), scenarios.data)
  save(scenarios.data, file = paste0("data.out/growth.", time.period, ".RData"))
}
  
# riparian full restoration scenario:
  scenarios.data <- NULL
  for(scenario in climate.scenario.list){
    scenario.data <- NULL
    years <- 2089:2099
    
    for(yy in years){
      outdir <- paste0(scenario, ".A.", yy)
      load(file = paste0(data.out,"_riparian1/", outdir, "/salmon.array.", yy, ".1.RData"))
      
      # Post-emergence filter
      emgd <- salmon.array[,"emrg",]
      emgd[emgd != 1] <- NA
      
      # Potential yearling survival filter
      srv.age1 <- salmon.array[,"survive", dim(salmon.array)[3]]
      srv.age1[srv.age1 != 1] <- NA
      s.age1 <- array(srv.age1, dim = c(dim(salmon.array)[1], dim(salmon.array)[3]))
      
      # Get growth for potential yearlings only once emerged and only for fish that survived to the end
      dat <- salmon.array[,"growth",] * s.age1 * emgd
      dat <- dat[,seq(2, dim(salmon.array)[3], 2)] + dat[,seq(1, dim(salmon.array)[3], 2)]
      
      # Take mean of growth across these fish
      dat <- apply(dat, 2, mean, na.rm = T) 
      
      # Remove leap year's extra day:
      if(yy %in% c(1996, 2000, 2004, 2092, 2096)){
        dat <- dat[-60]
      }
      scenario.data <- cbind(scenario.data, dat)
    }
    scenario.data[is.nan(scenario.data)] <- NA
    scenarios.data <- abind::abind(scenarios.data, scenario.data, along = 3)
  }
  assign(paste0("growth.riparian1.future"), scenarios.data)
  save(scenarios.data, file = paste0("data.out/growth.riparian1.future", ".RData"))


#  -- For DHSVM-RBM figures (warning: takes a long time!) ----

# Load data for Figure 5:
# Compute seasonal means of flow & water temperature (historical and future climate, baseline riparian)
  themetric <- "mean"
for(time.period  in c("historical", "future")){
  riparian.scenario <- "riparian0"; suffix <- ""
  climate.scenario <- climate.scenario.list[1]
  
  # FLOW
  # Import and collect all raw data
  Q.df <- fncImportQ(paste0(data.in, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Outflow.Only.", time.period , suffix, ".csv"))
  for(climate.scenario in climate.scenario.list[2:length(climate.scenario.list)]){
    Qq <- fncImportQ(paste0(data.in, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Outflow.Only.", time.period , suffix, ".csv"))
    Q.df <- rbind(Q.df, Qq)  
  }
  
  # Get dates into consistent year
  if(time.period  == "historical") {idx <- which(Q.df[,"Date"] < as.Date("1994-10-01") | Q.df[,"Date"] > as.Date("2005-09-30")); years <- 1995:2005}
  if(time.period  == "future") {idx <- which(Q.df[,"Date"] < as.Date("2089-10-01") | Q.df[,"Date"] > as.Date("2099-09-30")); years <- 2089:2099}
  Q.df <- Q.df[-idx,]; Q.df<- Q.df[!is.na(Q.df$Date),]
  Q.df$Year <- lubridate::year(Q.df$Date)
  for(yy in years){
    foo <- Q.df[Q.df[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & Q.df[, "Date"] <= as.Date(paste0(yy, "-09-30")) & !is.na(Q.df[, "Date"]),]
    lubridate::year(foo[foo[, "Year"] == yy - 1, "Date"]) <- 1900
    lubridate::year(foo[foo[, "Year"] == yy, "Date"]) <- 1901
    Q.df[Q.df[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & Q.df[, "Date"] <= as.Date(paste0(yy, "-09-30")) & !is.na(Q.df[, "Date"]),] <- foo
  }
  
  # Add season column & summarize by season 
  Q.df$Season <- NA
  Q.df$Season[Q.df$Date >= as.Date("1900-10-01") & Q.df$Date <= as.Date("1900-11-30") | Q.df$Date >= as.Date("1901-09-01")] <- "fall"
  Q.df$Season[Q.df$Date >= as.Date("1900-12-01") & Q.df$Date <= as.Date("1901-02-28")] <- "winter"
  Q.df$Season[Q.df$Date >= as.Date("1901-03-01") & Q.df$Date <= as.Date("1901-05-31")] <- "spring"
  Q.df$Season[Q.df$Date >= as.Date("1901-06-01") & Q.df$Date <= as.Date("1901-08-31")] <- "summer"
  
  Q.seasons <- Q.df %>% 
    group_by(Season) %>%
    summarise_if(is.numeric, themetric, na.rm = TRUE) #older dplyr
  #summarise(across(where(is.numeric), median, na.rm = TRUE)) #newer dplyr
  Q.seasons <- Q.seasons[!is.na(Q.seasons$Season), -2] #remove NAs and Time column
  write.csv(Q.seasons, file = paste0("data.in/rbm.data/Q.seasonal.",themetric, "s.", time.period , ".csv"))
  rm(Q.df, Qq, foo)
  
  assign(paste0("Q.seasons.", time.period ), Q.seasons); rm(Q.seasons)
  
  # TEMPERATURE
  # Import and collect all raw data
  T.df <- fncImportWT(paste0(data.in, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Tw_output.", time.period , suffix, ".csv"))
  for(climate.scenario in climate.scenario.list[2:length(climate.scenario.list)]){
    Tt <- fncImportWT(paste0(data.in, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Tw_output.", time.period , suffix, ".csv"))
    T.df <- rbind(T.df, Tt)  
  }
  
  # Get dates into consistent year
  if(time.period  == "historical") {idx <- which(T.df[,"Date"] < as.Date("1994-10-01") | T.df[,"Date"] > as.Date("2005-09-30")); years <- 1995:2005}
  if(time.period  == "future") {idx <- which(T.df[,"Date"] < as.Date("2089-10-01") | T.df[,"Date"] > as.Date("2099-09-30")); years <- 2089:2099}
  T.df <- T.df[-idx,]; T.df<- T.df[!is.na(T.df$Date),]
  T.df$Year <- lubridate::year(T.df$Date)
  for(yy in years){
    foo <- T.df[T.df[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & T.df[, "Date"] <= as.Date(paste0(yy, "-09-30")) & !is.na(T.df[, "Date"]),]
    lubridate::year(foo[foo[, "Year"] == yy - 1, "Date"]) <- 1900
    lubridate::year(foo[foo[, "Year"] == yy, "Date"]) <- 1901
    T.df[T.df[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & T.df[, "Date"] <= as.Date(paste0(yy, "-09-30")) & !is.na(T.df[, "Date"]),] <- foo
  }
  
  # Add season column & summarize by season 
  T.df$Season <- NA
  T.df$Season[T.df$Date >= as.Date("1900-10-01") & T.df$Date <= as.Date("1900-11-30") | T.df$Date >= as.Date("1901-09-01")] <- "fall"
  T.df$Season[T.df$Date >= as.Date("1900-12-01") & T.df$Date <= as.Date("1901-02-28")] <- "winter"
  T.df$Season[T.df$Date >= as.Date("1901-03-01") & T.df$Date <= as.Date("1901-05-31")] <- "spring"
  T.df$Season[T.df$Date >= as.Date("1901-06-01") & T.df$Date <= as.Date("1901-08-31")] <- "summer"
  
  T.seasons <- T.df %>% 
    group_by(Season) %>%
    summarise_if(is.numeric, themetric, na.rm = TRUE) #older dplyr
  #summarise(across(where(is.numeric), median, na.rm = TRUE)) #newer dplyr
  T.seasons <- T.seasons[!is.na(T.seasons$Season), -2] #remove NAs and Time column
  write.csv(T.seasons, file = paste0("data.in/rbm.data/T.seasonal.", themetric, "s.", time.period , ".csv"))
  rm(T.df, Tt, foo)
  
  assign(paste0("T.seasons.", time.period ), T.seasons); rm(T.seasons)
}

# Load data for Figure 6 maps:
# Compute seasonal means of water temperature (future climate)
  time.period <- "future"
  themetric <- "mean"
for(riparian.scenario in c("riparian1", "riparian2", "riparian3")){
    suffix <- paste0("_s", substr(riparian.scenario,9,9))
    climate.scenario <- climate.scenario.list[1]
    
    # TEMPERATURE only
    # Import and collect all raw data
    T.df <- fncImportWT(paste0(data.in, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Tw_output.", time.period , suffix, ".csv"))
    for(climate.scenario in climate.scenario.list[2:length(climate.scenario.list)]){
      Tt <- fncImportQ(paste0(data.in, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Tw_output.", time.period , suffix, ".csv"))
      T.df <- rbind(T.df, Tt)  
    }
    
    # Get dates into consistent year
    idx <- which(T.df[,"Date"] < as.Date("2089-10-01") | T.df[,"Date"] > as.Date("2099-09-30")); years <- 2089:2099
    T.df <- T.df[-idx,]; T.df<- T.df[!is.na(T.df$Date),]
    T.df$Year <- lubridate::year(T.df$Date)
    for(yy in years){
      foo <- T.df[T.df[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & T.df[, "Date"] <= as.Date(paste0(yy, "-09-30")) & !is.na(T.df[, "Date"]),]
      lubridate::year(foo[foo[, "Year"] == yy - 1, "Date"]) <- 1900
      lubridate::year(foo[foo[, "Year"] == yy, "Date"]) <- 1901
      T.df[T.df[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & T.df[, "Date"] <= as.Date(paste0(yy, "-09-30")) & !is.na(T.df[, "Date"]),] <- foo
    }
    
    # Add season column & summarize by season 
    T.df$Season <- NA
    T.df$Season[T.df$Date >= as.Date("1900-10-01") & T.df$Date <= as.Date("1900-11-30") | T.df$Date >= as.Date("1901-09-01")] <- "fall"
    T.df$Season[T.df$Date >= as.Date("1900-12-01") & T.df$Date <= as.Date("1901-02-28")] <- "winter"
    T.df$Season[T.df$Date >= as.Date("1901-03-01") & T.df$Date <= as.Date("1901-05-31")] <- "spring"
    T.df$Season[T.df$Date >= as.Date("1901-06-01") & T.df$Date <= as.Date("1901-08-31")] <- "summer"
    
    T.seasons <- T.df %>% 
      group_by(Season) %>%
      summarise_if(is.numeric, themetric, na.rm = TRUE) #older dplyr
    #summarise(across(where(is.numeric), median, na.rm = TRUE)) #newer dplyr
    T.seasons <- T.seasons[!is.na(T.seasons$Season), -2] #remove NAs and Time column
    write.csv(T.seasons, file = paste0("data.in/rbm.data/T.seasonal.", themetric, "s.", riparian.scenario , ".csv"))
    rm(T.df, Tt, foo)
    
    assign(paste0("T.seasons.", riparian.scenario), T.seasons); rm(T.seasons)
}

# Load data for Figure 6:
# Gather data for lowest mainstem reach across all years and GCMs for each riparian scenario
for(time.period  in c("historical", "future")){
  for(riparian.scenario in riparian.scenario.list){
    
    suffix <- paste0("_s", substr(riparian.scenario,9,9))
    if(riparian.scenario == "riparian0") suffix <- ""
    
    Q.df <- fncImportQ(paste0(data.in, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Outflow.Only.", time.period , suffix, ".csv"))[, 1:3]
    T.df <- fncImportWT(paste0(data.in, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Tw_output.", time.period , suffix, ".csv"))[, 1:3]
    
    for(climate.scenario in climate.scenario.list[2:length(climate.scenario.list)]){
      Qq <- fncImportQ(paste0(data.in, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Outflow.Only.", time.period , suffix, ".csv"))[,3]
      Tt <- fncImportWT(paste0(data.in, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Tw_output.", time.period , suffix, ".csv"))[,3]
      Q.df <- cbind(Q.df, Qq)
      T.df <- cbind(T.df, Tt)
    }
    colnames(Q.df) <- c("Date", "Time", climate.scenario.list)
    colnames(T.df) <- c("Date", "Time", climate.scenario.list)
    if(time.period  == "historical"){
      Q.df <- Q.df[Q.df$Date >= as.Date("1994-10-01"),]
      T.df <- T.df[T.df$Date >= as.Date("1994-10-01"),]
    } else if(time.period  == "future"){
      Q.df <- Q.df[Q.df$Date >= as.Date("2088-10-01"),]
      T.df <- T.df[T.df$Date >= as.Date("2088-10-01"),]
    }
    assign(paste0(riparian.scenario, ".", time.period , ".Q"), Q.df)
    assign(paste0(riparian.scenario, ".", time.period , ".T"), T.df)
    write.csv(Q.df, paste0("data.in/rbm.data/", riparian.scenario, ".", time.period , ".Q.csv"), row.names = F)
    write.csv(T.df, paste0("data.in/rbm.data/", riparian.scenario, ".", time.period , ".T.csv"), row.names = F)
  }
}


#--- Re-load processed data & continue data setup for figures (skip gather data step if previously done) -----

# IBM data:

load("data.out/growth.historical.RData")
load("data.out/growth.future.RData")
load("data.out/growth.riparian1.future.RData")

for(riparian.scenario in c("riparian0", "riparian1", "riparian2", "riparian3")){
  for(time.period  in c("historical", "future")){
    load(paste0("data.out/", riparian.scenario, ".", time.period , ".RData"))
    assign(paste0(riparian.scenario, ".", time.period ), thedata); rm(thedata)
  }
}

# Create container to hold results
container <- matrix(NA, nrow = 8, ncol = 9)
container <- as.data.frame(container)
colnames(container) <- c("Period", "Riparian", "ScenNo", "SY_Survival", "Y_Survival", "DateEmerge", "DateOutmigrate", "SY_Mass", "Y_Mass")
scenario.names <- c("Baseline", "FullRestoration", "PartialRestoration", "LeastProtective")
scenario.numbers <- c(0, 1, 3, 2)
container$Riparian <- rep(scenario.names, 2)
container$ScenNo <- rep(scenario.numbers, 2)
container$Period <- c(rep("Historical", 4), rep("Future", 4))
container$DateEmerge <- as.POSIXct(container$DateEmerge)
container$DateOutmigrate <- as.POSIXct(container$DateOutmigrate)


#Get numbers of spawners, yearlings, & subyearlings across scenarios
for(riparian.scenario in riparian.scenario.list){
  his.scenarios.data <- get(paste0(riparian.scenario, ".historical"))
  fut.scenarios.data <- get(paste0(riparian.scenario, ".future"))
  
  # historical
  spawners.by.scenario_year.h <- table(his.scenarios.data$YearSpawn, his.scenarios.data$Scenario)
  spawners.h <- apply(spawners.by.scenario_year.h, 2, sum)
  juveniles.by.scenario.h <- round(table(his.scenarios.data$FinalState, his.scenarios.data$Scenario)[3:4,]/ spawners.h * mean(spawners.h) / 11)
  
  # future
  spawners.by.scenario_year.f <- table(fut.scenarios.data$YearSpawn, fut.scenarios.data$Scenario)
  spawners.f <- apply(spawners.by.scenario_year.f, 2, sum)
  juveniles.by.scenario.f <- round(table(fut.scenarios.data$FinalState, fut.scenarios.data$Scenario)[3:4,] / spawners.f * mean(spawners.f) / 11)
  
  # Combine all data into one frame
  his.scenarios.data$Period <- "Historical"
  fut.scenarios.data$Period <- "Future"
  scenarios.data <- rbind(his.scenarios.data, fut.scenarios.data)
  scenarios.data$Period <- as.factor(scenarios.data$Period)
  levels(scenarios.data$Period) <- c("Future", "Historical")
  assign(paste0(riparian.scenario, ".scenarios.data"), scenarios.data)
  rm(his.scenarios.data, fut.scenarios.data)
}

# DHSVM-RBM data:
# Figure 5 maps
themetric <- "mean"
for(time.period  in c("historical", "future")){
  foo <- read.csv(paste0("data.in/rbm.data/T.seasonal.", themetric, "s.", time.period , ".csv"), header = T, stringsAsFactors = F, row.names = 1)
  assign(paste0("T.seasons.", time.period ), foo); rm(foo)
  foo <- read.csv(paste0("data.in/rbm.data/Q.seasonal.", themetric, "s.", time.period , ".csv"), header = T, stringsAsFactors = F, row.names = 1)
  assign(paste0("Q.seasons.", time.period ), foo); rm(foo)
}

# Figure 6  & S2 time series
for(riparian.scenario in riparian.scenario.list){
  for(time.period  in c("historical", "future")){
    Qdat <- read.csv(paste0("data.in/rbm.data/", riparian.scenario, ".", time.period , ".Q.csv"), header = T, stringsAsFactors = F)
    Qdat$Date <- as.Date(Qdat$Date)
    assign(paste0(riparian.scenario, ".", time.period , ".Q"), Qdat)
    Tdat <- read.csv(paste0("data.in/rbm.data/", riparian.scenario, ".", time.period , ".T.csv"), header = T, stringsAsFactors = F)
    Tdat$Date <- as.Date(Tdat$Date)
    assign(paste0(riparian.scenario, ".", time.period , ".T"), Tdat)
    rm(Qdat, Tdat)
  }
}

#--- Table 1: SUMMARY ACROSS SCENARIOS ---------------------

# Prep data
# get quantiles that pool across 10 GCMs and 11 years (but year is standardized to 1901)
for(riparian.scenario in riparian.scenario.list){
  td <- get(paste0(riparian.scenario, ".scenarios.data"))
  td$YearSpawn = lubridate::year(td$DateSpawn)
  td$YearEmerge = lubridate::year(td$DateEmerge)
  td$YearOutmigrate = lubridate::year(td$DateOutmigrate)
  for(var in c("Spawn", "Emerge", "Outmigrate")){
    for(yy in c(1995:2005, 2089:2099)){
      foo <- td[td[, paste0("Date", var)] >= as.Date(paste0(yy - 1, "-09-01")) & td[, paste0("Date", var)] < as.Date(paste0(yy, "-08-31")) & !is.na(td[, paste0("Date", var)]),]
      lubridate::year(foo[foo[, paste0("Year", var)] == yy - 1, paste0("Date", var)]) <- 1900
      lubridate::year(foo[foo[, paste0("Year", var)] == yy, paste0("Date", var)]) <- 1901
      td[td[, paste0("Date", var)] >= as.Date(paste0(yy - 1, "-09-01")) & td[, paste0("Date", var)] < as.Date(paste0(yy, "-08-31")) & !is.na(td[, paste0("Date", var)]),] <- foo
    }
  }
  
  # Date emerged or smolted
  for(time.period  in c("Historical", "Future")){
    for(var in c("DateEmerge", "DateOutmigrate")){
      result <- td %>% 
      mutate(PhenObj = eval(parse(text = var))) %>% 
      # filter data to surviving fish
      filter(FinalState == "Subyearling" | FinalState == "Yearling") %>% 
      # filter to correct time.period 
      filter(Period == !!(time.period)) %>%
      # select relevant columns
      select(Scenario, PhenObj) %>% 
      # filter data to fish that experienced each event
      filter(!is.na(PhenObj)) 
      
      result$Date <- as.POSIXct(result$PhenObj)
      thestatistic <- quantile(result$Date, probs = c(0.05, 0.5, 0.95), na.rm = T)
      print(thestatistic)
      container[container$Period == time.period  & container$ScenNo == as.numeric(substr(riparian.scenario, 9, 9)), var] <- thestatistic["50%"]
    }
  }
  
  # Final mass subyearling or yearling
  for(time.period  in c("Historical", "Future")){
    for(var in c("Subyearling", "Yearling")){
      result <- td %>% 
        # filter data to surviving fish
        filter(FinalState == !!(var)) %>% 
        # filter to correct time.period 
        filter(Period == !!(time.period)) %>%
        # select relevant columns
        select(Scenario, Weight)
      
        thestatistic <- quantile(result$Weight, probs = c(0.05, 0.5, 0.95))
        print(thestatistic)
        ifelse(substr(var, 1, 1) == "S", column <- "SY_Mass", column <- "Y_Mass")
        container[container$Period == time.period  & container$ScenNo == as.numeric(substr(riparian.scenario, 9, 9)), column] <- thestatistic["50%"]
    }
  }
  
  # No. subyearlings or yearlings
  for(time.period  in c("Historical", "Future")){
    for(var in c("Subyearling", "Yearling")){
      result <- td %>% 
        # filter data to surviving fish
        filter(FinalState == !!(var)) %>% 
        # filter to correct time.period 
        filter(Period == !!(time.period)) %>%
        # select relevant columns
        select(Scenario, FinalState, YearSpawn)
      totals <- td %>% 
        # filter to correct time.period 
        filter(Period == !!(time.period)) %>%
        # select relevant columns
        select(Scenario, FinalState, YearSpawn)
      
      result <- tapply(result$FinalState, list(result$YearSpawn, result$Scenario), length)
      totals <- tapply(totals$FinalState, list(totals$YearSpawn, totals$Scenario), length)
      #quantile(result, probs = c(0.05, 0.5, 0.95), na.rm = T)
      survival <- result / totals
      thestatistic <- quantile(survival, probs = c(0.05, 0.5, 0.95), na.rm = T)
      print(thestatistic)
      ifelse(substr(var, 1, 1) == "S", column <- "SY_Survival", column <- "Y_Survival")
      container[container$Period == time.period  & container$ScenNo == as.numeric(substr(riparian.scenario, 9, 9)), column] <- thestatistic["50%"]
    }
  }
  rm(result, totals, thestatistic)
}
container
write.csv(container, "data.out/Table1.csv", row.names = F)

#--- Figure 5: DHSVM-RBM CLIMATE CHANGE EFFECT MAPS -----

# Read in basin outline & streams
basin <- read_sf(paste0("data.in/shapefiles"), "Basin_snq2")
basin2 <- basin %>% summarise(area = sum(AreaSqKm)) # dissolve on area to just get the outline
ex <- raster::extent(basin2)
streams <- read_sf("data.in/shapefiles", "streamline_original")
idx <- which(streams$downarc == -1)[2:7] #isolated reaches that need to be removed
streams <- streams[-idx,]
streams$Q <- NA
streams$WT <- NA
streams$col.class.q <- NA
streams$col.class.t <- NA

seasons <- c("Fall", "Winter", "Spring", "Summer", "Blank")
Qletters <- c("(a)", "(b)", "(c)", "(d)")
Tletters <- c("(e)", "(f)", "(g)", "(h)")

# Change (future minus historical)
Q.seasons <- Q.seasons.future[2:ncol(Q.seasons.future)] - Q.seasons.historical[2:ncol(Q.seasons.historical)]
rownames(Q.seasons) <- Q.seasons.historical$Season
T.seasons <- T.seasons.future[2:ncol(T.seasons.future)] - T.seasons.historical[2:ncol(T.seasons.historical)]
rownames(T.seasons) <- T.seasons.historical$Season
Q.seasons <- Q.seasons[,-ncol(Q.seasons)]; T.seasons <- T.seasons[,-ncol(T.seasons)] #remove Year column

T.seasons <- T.seasons[c(1,4,2,3),] #reorder seasons for plotting
Q.seasons <- Q.seasons[c(1,4,2,3),] #reorder seasons for plotting

# water temperature
cb <- RColorBrewer::brewer.pal(7, "Reds")
cb <- c("#fcfafa", cb)
theseq <- quantile(t(T.seasons), probs = c(0, 0.05, 0.15, 0.3, 0.45, 0.6, 0.75, 0.95, 1))
left <- theseq[1:(length(theseq) - 1)]
rght <- theseq[2:length(theseq)]

# flow
cb2 <- RColorBrewer::brewer.pal(7, "RdBu")
cb2 <- c("#5e0401", cb2[c(1:3,5:7)], "#014270")
theseq2 <- quantile(t(Q.seasons), probs = c(0, 0.05, 0.15, 0.3, 0.45, 0.6, 0.75, 0.95, 1))
left2 <- theseq2[1:(length(theseq2) - 1)]
rght2 <- theseq2[2:length(theseq2)]

# Make seasonal flow change maps
png(paste0(plot.directory, "/Figure5_DHSVM-RBM_Qmaps_mean2.png"), width = 3.5, height = 11, units = "in", res = 300)
par(mfrow = c(4, 1), mar = c(1, 0, 4, 0), oma = rep(0.5,4), las = 1)

for(ss in 1:length(seasons)){
  season <- seasons[ss]
  letter <- Qletters[ss]

  # plot background
  plot(basin2, col = "gray40", border = 1, lwd = 2, main = "", reset = FALSE)

  qq <- t(Q.seasons[ss,]) #4 = summer temperature
  streams$Q <- qq
  for(n in 1:length(cb2)) {streams$col.class.q[streams$Q >= left2[n] & streams$Q <= rght2[n]] <- n}
  
  # plot colored stream lines
  plot(streams, add = T, lwd = (streams$segorder * 0.1) + 0.5, col = cb2[streams$col.class.q])

  # add descriptive label
  text(x = ex[1], y = ex[3] + 3500, paste(letter, season), cex = 1.5, adj = 0)
  
  # # Add flow legend
  #   plot(1:10, 1:10, type = 'n', axes = F, xlab = 'n', ylab = 'n')
  #   leglabs <- paste(round(left2, 2), "to", round(rght2, 2))
  #   legend("center", legend = leglabs, title = expression(Delta~"Flow (m s"^-1*")"), bty = "n", pch = 19, col = cb2, cex = 1.5)
}

dev.off()

# Make seasonal water temperature change maps
png(paste0(plot.directory, "/Figure5_DHSVM-RBM_Tmaps_mean2.png"), width = 3.5, height = 11, units = "in", res = 300)
par(mfrow = c(4, 1), mar = c(1, 0, 4, 0), oma = rep(0.5,4), las = 1)

for(ss in 1:length(seasons)){
  season <- seasons[ss]
  letter <- Tletters[ss]
    
  # plot background
  plot(basin2, col = "gray40", border = 1, lwd = 2, main = "", reset = FALSE)
    
  wt <- t(T.seasons[ss,]) #4 = summer temperature
  streams$WT <- wt
  for(n in 1:length(cb)) {streams$col.class.t[streams$WT >= left[n] & streams$WT <= rght[n]] <- n}
    
  # plot colored stream lines
  plot(streams, add = T, lwd = (streams$segorder * 0.1) + 0.5, col = cb[streams$col.class.t])
  
  # add descriptive label
  text(x = ex[1], y = ex[3] + 3500, paste(letter, season), cex = 1.5, adj = 0)
  
  #   # Add temperature legend
  #     plot(1:10, 1:10, type = 'n', axes = F, xlab = 'n', ylab = 'n')
  #     leglabs <- paste(round(left, 1), "to", round(rght , 1))
  #     legend("center", legend = leglabs, title = expression(Delta~"Water temperature ("*degree*C*")"), bty = "n", pch = 19, col = cb, cex = 1.5)

}

dev.off()

#--- Figure 5: DHSVM-RBM CLIMATE CHANGE EFFECT TIME SERIES -----
# Get all data into same year for plotting
# change year so that day-month is the comparable across scenarios.
riparian.scenario <- "riparian0"
for(time.period  in c("historical", "future")){
  if(time.period  == "historical") theyears <- 1995:2005
  if(time.period  == "future") theyears <- 2089:2099
  Q.data <- get(paste0(riparian.scenario, ".", time.period , ".Q"))
  T.data <- get(paste0(riparian.scenario, ".", time.period , ".T"))
  Q.data$Year = lubridate::year(Q.data$Date)
  T.data$Year = lubridate::year(T.data$Date)
  
  for(yy in theyears){
    
    # Remove leap year's extra day:
    if(yy %in% c(1996, 2000, 2004, 2092, 2096)){
      idx <- which(Q.data$Date == as.Date(paste0(yy, "-02-29")))
      Q.data <- Q.data[-idx,]
      T.data <- T.data[-idx,]
      rm(idx)
    }
    
    #Flow
    foo = Q.data[Q.data[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & Q.data[, "Date"] < as.Date(paste0(yy, "-10-01")) & !is.na(Q.data[, "Date"]),]
    lubridate::year(foo[foo[, "Year"] == yy - 1, "Date"]) <- 1900
    lubridate::year(foo[foo[, "Year"] == yy, "Date"]) <- 1901
    Q.data[Q.data[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & Q.data[, "Date"] < as.Date(paste0(yy, "-10-01")) & !is.na(Q.data[, "Date"]),] <- foo
    
    #Temp
    foo = T.data[T.data[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & T.data[, "Date"] < as.Date(paste0(yy, "-10-01")) & !is.na(T.data[, "Date"]),]
    lubridate::year(foo[foo[, "Year"] == yy - 1, "Date"]) <- 1900
    lubridate::year(foo[foo[, "Year"] == yy, "Date"]) <- 1901
    T.data[T.data[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & T.data[, "Date"] < as.Date(paste0(yy, "-10-01")) & !is.na(T.data[, "Date"]),] <- foo
  }
  
  # remove extra erroneous records
  idx <- which(Q.data$Date > as.Date("1901-09-30"))
  if(length(idx) > 0) {Q.data <- Q.data[-idx,]; rm(idx)}
  idx <- which(T.data$Date > as.Date("1901-09-30"))
  if(length(idx) > 0) {T.data <- T.data[-idx,]; rm(idx)}
  
  
  assign(paste0(riparian.scenario, ".", time.period , ".Qdat"), Q.data)
  assign(paste0(riparian.scenario, ".", time.period , ".Tdat"), T.data)
}

png(paste0(plot.directory,"/Figure5_QT_timeseries.png"), width = 9, height = 2.3, units = "in", res = 300)
par(las = 1, mfrow = c(1,2), mar = c(2, 5, 0.5, 3), oma = c(0.5, 3, 0.5, 0))

themetric <- "mean"

# Top row: Riparian baseline future minus historical

# Flow
Qdat.h <- riparian0.historical.Qdat
Qdat.f <- riparian0.future.Qdat
Qdat <- Qdat.f - Qdat.h; Qdat <- Qdat[-c(1,2,ncol(Qdat))]; Qdat$Date <- Qdat.h$Date
themin <- aggregate(Qdat[,1:10], by = list(Qdat$Date), quantile, probs = 0.05); colnames(themin)[1] <- "Date"
themin <- apply(themin[,2:11], 1, themetric)
themedian <- aggregate(Qdat[,1:10], by = list(Qdat$Date), quantile, probs = 0.5); colnames(themedian)[1] <- "Date"
themedian <- apply(themedian[,2:11], 1, themetric)
themax <- aggregate(Qdat[,1:10], by = list(Qdat$Date), quantile, probs = 0.95); colnames(themax)[1] <- "Date"
themax <- apply(themax[,2:11], 1, themetric)
xvals <- c(unique(Qdat.h$Date), rev(unique(Qdat.h$Date)))

plot(unique(Qdat.h$Date), themedian, type = 'n', ylab = "", xlab = "", ylim = c(-600, 1200), cex.axis = 0.9, las = 1)
#axis(1, at = c(as.Date("1900-11-01"), as.Date("1901-01-01"), as.Date("1901-03-01"), as.Date("1901-05-01"), as.Date("1901-07-01"), as.Date("1901-09-01")), labels = F)
mtext("\u0394 Flow \n(m \u2219", side = 2, line = 6, adj = 0, cex = 0.9)
mtext(expression(s^-1*")"), side = 2, line = 5, adj = -0.6, padj = 0.88, cex = 1)
#mtext("\u0394 Flow \n(m \u2219 s\u207B\u00B9)", side = 2, line = 6, adj = 0.5, cex = 0.9)
polygon(xvals, c(themin, rev(themax)), border = NA, col = "darkgray")
lines(unique(Qdat.h$Date), themedian, lwd = 1.5)
abline(h = 1, lty = 2)
#legend("topleft", legend = "(a) Climate effect, Baseline riparian", bty = 'n', cex = 1.4)

# Temperature
Tdat.h <- riparian0.historical.Tdat
Tdat.f <- riparian0.future.Tdat
Tdat <- Tdat.f - Tdat.h; Tdat <- Tdat[-c(1,2,ncol(Tdat))]; Tdat$Date <- Tdat.h$Date
themin <- aggregate(Tdat[,1:10], by = list(Tdat$Date), quantile, probs = 0.05); colnames(themin)[1] <- "Date"
themin <- apply(themin[,2:11], 1, themetric)
themedian <- aggregate(Tdat[,1:10], by = list(Tdat$Date), quantile, probs = 0.5); colnames(themedian)[1] <- "Date"
themedian <- apply(themedian[,2:11], 1, themetric)
themax <- aggregate(Tdat[,1:10], by = list(Tdat$Date), quantile, probs = 0.95); colnames(themax)[1] <- "Date"
themax <- apply(themax[,2:11], 1, themetric)
xvals <- c(unique(Tdat.h$Date), rev(unique(Tdat.h$Date)))

plot(unique(Tdat.h$Date), themedian, type = 'n', ylab = "", xlab = "", ylim = c(-5, 16), cex.axis = 0.9)
#axis(1, at = c(as.Date("1900-11-01"), as.Date("1901-01-01"), as.Date("1901-03-01"), as.Date("1901-05-01"), as.Date("1901-07-01"), as.Date("1901-09-01")), labels = F)
mtext("\u0394 Water\ntemperature\n(\u00B0C)", side = 2, line = 4, adj = 0.5, cex = 0.9)
polygon(xvals, c(themin, rev(themax)), border = NA, col = "#FC9272")
lines(unique(Tdat.h$Date), themedian, col = "#99000D", lwd = 1.5)
abline(h = 1, lty = 2)
#legend("topleft", legend = "(b) Climate effect, Baseline riparian", bty = 'n', cex = 1.4)

dev.off()

#--- Figure 6: DHSVM-RBM RIPARIAN EFFECT MAPS -----

# Read in basin outline & streams
basin <- read_sf(paste0("data.in/shapefiles"), "Basin_snq2")
basin2 <- basin %>% summarise(area = sum(AreaSqKm)) # dissolve on area to just get the outline
add.labels <- F #add descriptive labels & legends to each panel?
if(add.labels == T) ex <- raster::extent(basin2)
streams <- read_sf("data.in/shapefiles", "streamline_original")
idx <- which(streams$downarc == -1)[2:7] #isolated reaches that need to be removed
streams <- streams[-idx,]
streams$WT <- NA
streams$col.class <- NA

descriptions <- c("(a) Climate effect, \nBaseline riparian", "(b) Full restoration \neffect, Future climate", 
                  "(c) Partial restoration \neffect, Future climate", "(d) Least protective \neffect, Future climate")
riparian.scenario.list <- c("riparian1", "riparian3", "riparian2")

# Make maps
png(paste0(plot.directory, "/Figure6_DHSVM-RBM_Maps_mean.png"), width = 5, height = 14, units = "in", res = 300)
par(mfrow = c(4, 1), oma = rep(0.5,4), las = 1)

# Future minus historical
  T.seasons <- T.seasons.future[2:ncol(T.seasons.future)] - T.seasons.historical[2:ncol(T.seasons.historical)]
  rownames(T.seasons) <- T.seasons.historical$Season
  T.seasons <- T.seasons[,-ncol(T.seasons)] #remove Year column
  T.seasons <- T.seasons[c(1,4,2,3),] #reorder seasons for plotting
  
  cb <- RColorBrewer::brewer.pal(7, "Reds")
  cb <- c("#fcfafa", cb)
  theseq <- quantile(t(T.seasons), probs = c(0, 0.05, 0.15, 0.3, 0.45, 0.6, 0.75, 0.95, 1))
  left <- theseq[1:(length(theseq) - 1)]
  rght <- theseq[2:length(theseq)]

  wt <- t(T.seasons[4,]) #4 = summer temperature
  streams$WT <- wt
  for(n in 1:length(cb)) {streams$col.class[streams$WT >= left[n] & streams$WT <= rght[n]] <- n}
  
  # plot background
  plot(basin2, col = "gray40", border = 1, lwd = 2, main = "", reset = FALSE)
  # plot colored stream lines
  plot(streams, add = T, lwd = (streams$segorder * 0.1) + 0.5, col = cb[streams$col.class])
  # add descriptive label
  if(add.labels == T) text(x = ex[1] - 100, y = ex[3] + 2500, descriptions[1], cex = 1.5, adj = 0)
  # Add  legend
  if(add.labels == T){
    leglabs = paste(round(left, 2), "to", round(rght , 2))
    legend("right", legend = leglabs, title = expression(Delta~"Water temperature ("*degree*C*")"), bty = "n", pch = 19, col = cb)
  }
  #box()

# Change (riparian scenario minus future baseline)
for(i in 1:length(riparian.scenario.list)){
  descr <- descriptions[i + 1]
  riparian.scenario <- riparian.scenario.list[i]

    T.scenario <- get(paste0("T.seasons.", riparian.scenario))
    T.seasons <- T.scenario[2:ncol(T.scenario)] - T.seasons.future[2:ncol(T.seasons.future)]
    rownames(T.seasons) <- T.seasons.future$Season
    T.seasons <- T.seasons[,-ncol(T.seasons)] #remove Year column
    T.seasons <- T.seasons[c(1,4,2,3),] #reorder seasons for plotting
    
    # set colors
    wt <- t(T.seasons)
    if(riparian.scenario %in% c("riparian1", "riparian3")){
      cb <- RColorBrewer::brewer.pal(8, "Blues")
      cb <- c("#fcfafa", cb) #add white
      cb <- cb[9:1]
      theseq <- c(quantile(wt[wt < 0], probs = c(0, 0.05, 0.15, 0.3, 0.45, 0.6, 0.75, 0.95, 1)), max(wt))
     }
    if(riparian.scenario == "riparian2"){
      cb <- RColorBrewer::brewer.pal(8, "Reds")
      cb <- c("#fcfafa", cb) #add white
      theseq <- c(min(wt), quantile(wt[wt > 0], probs = c(0, 0.05, 0.15, 0.3, 0.45, 0.6, 0.75, 0.95, 1)))
    }
    left <- theseq[1:(length(theseq) - 1)]
    rght <- theseq[2:length(theseq)]
    
    wt <- t(T.seasons[4,]) #4 = summer temperature
    streams$WT <- wt
    for(n in 1:length(cb)) {streams$col.class[streams$WT >= left[n] & streams$WT <= rght[n]] <- n}
    
    # plot background
    plot(basin2, col = "gray40", border = 1, lwd = 2, main = "", reset = FALSE)
    # plot colored stream lines
    plot(streams, add = T, lwd = (streams$segorder * 0.1) + 0.5, col = cb[streams$col.class])
    # add descriptive label
    if(add.labels == T) text(x = ex[1] - 100, y = ex[3] + 2500, descr, cex = 1.5, adj = 0)
    # Add  legend
    if(add.labels == T){
      leglabs = paste(round(left, 2), "to", round(rght , 2))
      legend("right", legend = leglabs, title = expression(Delta~"Water temperature ("*degree*C*")"), bty = "n", pch = 19, col = cb)
    }
    #box()
}

dev.off()
# reset riparian scenario list
riparian.scenario.list <- c("riparian0", "riparian1", "riparian2", "riparian3") #Baseline, Full restoration, Least protective, Partial restoration

#--- Figure 6: DHSVM-RBM RIPARIAN EFFECT TIME SERIES -----
# Get all data into same year for plotting
# change year so that day-month is the comparable across scenarios.
for(riparian.scenario in riparian.scenario.list){
  for(time.period  in c("historical", "future")){
    if(time.period  == "historical") theyears <- 1995:2005
    if(time.period  == "future") theyears <- 2089:2099
    T.data <- get(paste0(riparian.scenario, ".", time.period , ".T"))
    T.data$Year <- lubridate::year(T.data$Date)
    
    for(yy in theyears){
      
      # Remove leap year's extra day:
      if(yy %in% c(1996, 2000, 2004, 2092, 2096)){
        idx <- which(T.data$Date == as.Date(paste0(yy, "-02-29")))
        T.data <- T.data[-idx,]
        rm(idx)
      }
      
      #Temp
      foo <- T.data[T.data[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & T.data[, "Date"] < as.Date(paste0(yy, "-10-01")) & !is.na(T.data[, "Date"]),]
      lubridate::year(foo[foo[, "Year"] == yy - 1, "Date"]) <- 1900
      lubridate::year(foo[foo[, "Year"] == yy, "Date"]) <- 1901
      T.data[T.data[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & T.data[, "Date"] < as.Date(paste0(yy, "-10-01")) & !is.na(T.data[, "Date"]),] <- foo
    }
    
    # remove extra erroneous records
    idx <- which(T.data$Date > as.Date("1901-09-30"))
    if(length(idx) > 0) {T.data <- T.data[-idx,]; rm(idx)}
    
    assign(paste0(riparian.scenario, ".", time.period , ".Tdat"), T.data)
  }
}

png(paste0(plot.directory,"/Figure6_QT_timeseries.png"), width = 5.3, height = 9.3, units = "in", res = 300)
par(las = 1, mfrow = c(4,1), mar = c(2.2, 8.5, 0, 0.5), oma = c(0.5, 3, 0.5, 0.5))

themetric <- "mean"

# Top row Riparian baseline future minus historical
# Subsequent rows: Riparian scenario in future climate minus baseline riparian in future climate

# Temperature
Tdat.h <- riparian0.historical.Tdat
Tdat.f <- riparian0.future.Tdat
Tdat <- Tdat.f - Tdat.h; Tdat <- Tdat[-c(1,2,ncol(Tdat))]; Tdat$Date <- Tdat.h$Date
themin <- aggregate(Tdat[,1:10], by = list(Tdat$Date), quantile, probs = 0.05); colnames(themin)[1] <- "Date"
themin <- apply(themin[,2:11], 1, themetric)
themedian <- aggregate(Tdat[,1:10], by = list(Tdat$Date), quantile, probs = 0.5); colnames(themedian)[1] <- "Date"
themedian <- apply(themedian[,2:11], 1, themetric)
themax <- aggregate(Tdat[,1:10], by = list(Tdat$Date), quantile, probs = 0.95); colnames(themax)[1] <- "Date"
themax <- apply(themax[,2:11], 1, themetric)
xvals <- c(unique(Tdat.h$Date), rev(unique(Tdat.h$Date)))

plot(unique(Tdat.h$Date), themedian, type = 'n', ylab = "", xlab = "", ylim = c(-5, 17), xaxt = 'n', cex.axis = 1.5)
axis(1, at = c(as.Date("1900-11-01"), as.Date("1901-01-01"), as.Date("1901-03-01"), as.Date("1901-05-01"), as.Date("1901-07-01"), as.Date("1901-09-01")), labels = F)
mtext("\u0394 Water\ntemperature\n(\u00B0C)", side = 2, line = 7.5, adj = 0.5, cex = 0.9)
polygon(xvals, c(themin, rev(themax)), border = NA, col = "#FC9272")
lines(unique(Tdat.h$Date), themedian, col = "#99000D", lwd = 1.5)
abline(h = 1, lty = 2)
legend("topleft", legend = "(a) Climate effect, Baseline riparian", bty = 'n', cex = 1.4)

# Next 3 rows: Riparian scenario future X minus riparian baseline scenario future
for(riparian.scenario in riparian.scenario.list[c(2,4,3)]){
  
  # Temperature
  Tdat.0 <- riparian0.future.Tdat
  Tdat.x <- get(paste0(riparian.scenario, ".future.Tdat"))
  Tdat <- Tdat.x - Tdat.0; Tdat <- Tdat[-c(1,2,ncol(Tdat))]; Tdat$Date <- Tdat.0$Date
  themin <- aggregate(Tdat[,1:10], by = list(Tdat$Date), quantile, probs = 0.05); colnames(themin)[1] <- "Date"
  themin <- apply(themin[,2:11], 1, mean)
  themedian <- aggregate(Tdat[,1:10], by = list(Tdat$Date), quantile, probs = 0.5); colnames(themedian)[1] <- "Date"
  themedian <- apply(themedian[,2:11], 1, mean)
  themax <- aggregate(Tdat[,1:10], by = list(Tdat$Date), quantile, probs = 0.95); colnames(themax)[1] <- "Date"
  themax <- apply(themax[,2:11], 1, mean)
  xvals <- c(unique(Tdat.0$Date), rev(unique(Tdat.0$Date)))
  
  if(riparian.scenario == riparian.scenario.list[3]){
    plot(unique(Tdat.0$Date), themedian, type = 'n', ylab = "", xlab = "", ylim = c(-1.25, 1.25), xaxt = 'n', cex.axis = 1.5)
    axis(1, cex.axis = 1.5, at = c(as.Date("1900-11-01"), as.Date("1901-01-01"), as.Date("1901-03-01"), as.Date("1901-05-01"), as.Date("1901-07-01"), as.Date("1901-09-01")), 
         labels = c("Nov", "Jan", "Mar", "May", "Jul", "Sep"))
    mtext("\u0394 Water\ntemperature\n(\u00B0C)", side = 2, line = 7.5, adj = 0.5, cex = 0.9)
  
  }else{
    plot(unique(Tdat.0$Date), themedian, type = 'n', ylab = "", xlab = "", ylim = c(-1.25, 1.25), xaxt = 'n', cex.axis = 1.5)
    axis(1, at = c(as.Date("1900-11-01"), as.Date("1901-01-01"), as.Date("1901-03-01"), as.Date("1901-05-01"), as.Date("1901-07-01"), as.Date("1901-09-01")), labels = F)
    mtext("\u0394 Water\ntemperature\n(\u00B0C)", side = 2, line = 7.5, adj = 0.5, cex = 0.9)
  }
  colors2use <- c("#6BAED6", "#084594")
  if(riparian.scenario == riparian.scenario.list[3]) colors2use <- c("#FC9272", "#99000D")
  polygon(xvals, c(themin, rev(themax)), border = NA, col = colors2use[1])
  lines(unique(Tdat.0$Date), themedian, col = colors2use[2], lwd = 1.5)
  abline(h = 0, lty = 2)
  lg <- c("(b) Full restoration effect, Future climate", "(d) Least protective effect, Future climate", "(c) Partial restoration effect, Future climate")[as.numeric(substr(riparian.scenario, 9, 9))]
  legend("topleft", legend = lg, bty = 'n', cex = 1.4)
  
}

dev.off()

#--- Figure 7: PHENOLOGY -----------------------------------------------------------------

# change year so that day-month is the comparable across scenarios.
riparian.scenario <- "riparian0"
phenology.data <- get(paste0(riparian.scenario, ".scenarios.data"))
phenology.data$YearSpawn = lubridate::year(phenology.data$DateSpawn)
phenology.data$YearEmerge = lubridate::year(phenology.data$DateEmerge)
phenology.data$YearOutmigrate = lubridate::year(phenology.data$DateOutmigrate)

for(var in c("Spawn", "Emerge", "Outmigrate")){
  for(yy in c(1995:2005, 2089:2099)){
    foo = phenology.data[phenology.data[, paste0("Date", var)] >= as.Date(paste0(yy - 1, "-09-01")) & phenology.data[, paste0("Date", var)] < as.Date(paste0(yy, "-08-31")) & !is.na(phenology.data[, paste0("Date", var)]),]
    lubridate::year(foo[foo[, paste0("Year", var)] == yy - 1, paste0("Date", var)]) <- 1900
    lubridate::year(foo[foo[, paste0("Year", var)] == yy, paste0("Date", var)]) <- 1901
    phenology.data[phenology.data[, paste0("Date", var)] >= as.Date(paste0(yy - 1, "-09-01")) & phenology.data[, paste0("Date", var)] < as.Date(paste0(yy, "-08-31")) & !is.na(phenology.data[, paste0("Date", var)]),] <- foo
  }
}
yy = 1901
month.min = lubridate::month(min(phenology.data$DateEmerge, na.rm = T))

# EMERGENCE / OUTMIGRATION TIMING
for(var in c("DateEmerge", "DateOutmigrate")){
  if(var == "DateEmerge"){nm <- "Emergence"; yl <- 125; xlb <- ""; ylb <- "Simulated\nsalmon\nemerged\n(1000s)"}
  if(var == "DateOutmigrate"){nm <- "Outmigration"; xlb <- "Date"; yl <- 60; ylb <- "Simulated\nsubyearling\nmigrants\n(1000s)"}
  
plot.object <- phenology.data %>% 
  mutate(PhenObj = eval(parse(text = var))) %>% 
  # filter data to surviving fish
  filter(FinalState == "Subyearling" | FinalState == "Yearling") %>% 
  # select relevant columns
  select(Scenario, Period, PhenObj) %>% 
  # combine emergence into a single column
  gather(key = "Event", value = "Date", PhenObj) %>% 
  # filter data to fish that experienced each event
  filter(!is.na(Date)) %>% 
  # plot event vs. day-month
  ggplot(aes(x = Date, fill = Period, color = Period)) + 
  # add histogram
  geom_histogram(alpha = 0.5, position = "identity", aes(y = (..count..)/11/10), binwidth = 5) +
  # supply x and y limits
  coord_cartesian(ylim = c(0, yl)) +
  # set theme
  theme_classic() +
  # adjust y-axis label position
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  # remove legend title
  theme(legend.title = element_blank()) +
  # remove bottom axis line and ticks
  theme(axis.line.x.bottom = element_blank(), axis.ticks.x.bottom = element_blank()) +
  # remove subplot label background
  theme(strip.background = element_blank()) +
  # manually set color
  scale_color_manual(values=c("#FF8C00", "#00008B")) +
  # manually set fill
  scale_fill_manual(values=c("#FF8C00", "#00008B")) +
  # set x-axis labels to day-month
  scale_x_date(limits = c(as.Date(paste0(yy - 1, "-", month.min, "-01")), as.Date(paste0(yy, "-07-15"))), date_labels = "%b", date_breaks = "1 month") +
  # adjust y-axis label text
  labs(y = ylb, x = xlb) +
  # add x-axis to each plot
  geom_hline(yintercept = 0)
assign(paste0(nm, ".plot.object"), plot.object)
}

#Combine plots for manuscript figure
figure <- ggpubr::ggarrange(Emergence.plot.object, Outmigration.plot.object,
                    labels = c("(a)", "(b)"), hjust = -6.5, vjust = 1.8, font.label = list(size = 12, face = "plain"),
                    ncol = 1, nrow = 2, align = "hv", common.legend = TRUE)
figure
if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("Figure7_phenology_", riparian.scenario,".png"), plot = last_plot(), 
         width = 6, height = 6, units = "in", dpi = 300)
}


#--- Figure 8: FINAL MASS ----------------------------------------------------------------

# SUBYEARLING / YEARLING SIZES
riparian.scenario <- "riparian0"
scenarios.data <- get(paste0(riparian.scenario, ".scenarios.data"))

for(var in c("Subyearling", "Yearling")){
  if(var == "Subyearling"){nm <- "Subyearling"; yl <- 165; xl <- 5; ylb <- "Simulated\nsubyearling\nmigrants\n(1000s)"; xlb <- ""; bw <- 0.08}
  if(var == "Yearling"){nm <- "Yearling"; yl <- 15; xl<- 30; ylb <- "Simulated\npotential\nyearlings\n(1000s)"; xlb <- "Fish mass (g)" ;bw<- 0.5}
  
  plot.object <- scenarios.data %>% 
    # filter data to survivors
    filter(FinalState == !!(var)) %>%
    # plot weight
    ggplot(aes(x = Weight, color = Period, fill = Period)) + 
    # add histogram plot
    geom_histogram(alpha = 0.5, aes(y = (..count..)/11/10), binwidth = bw) +
    # supply x and y limits
    coord_cartesian(xlim = c(0, xl), ylim = c(0, yl)) +
    # set theme
    theme_classic() +
    # remove legend title
    #theme(legend.position = "none") +
    theme(legend.title = element_blank()) +
    # remove bottom axis line and ticks
    theme(axis.line.x.bottom = element_blank(), axis.ticks.x.bottom = element_blank()) +
    # remove subplot label background
    theme(strip.background = element_blank()) +
    # adjust y-axis label position
    theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
    # add axis and title text
    labs(x = xlb, y = ylb) +
    # manually set color
    scale_color_manual(values=c("#FF8C00", "#00008B")) +
    # manually set fill
    scale_fill_manual(values=c("#FF8C00", "#00008B")) +
    # add x-axis to each plot
    geom_hline(yintercept = 0)
  
  assign(paste0(nm, ".plot.object"), plot.object)
}

#Combine plots for manuscript figure
figure <- ggpubr::ggarrange(Subyearling.plot.object, Yearling.plot.object,
                    labels = c("(a)", "(b)"), hjust = -6.5, vjust = 1.8, font.label = list(size = 12, face = "plain"),
                    ncol = 1, nrow = 2, align = "hv", common.legend = TRUE)
figure
if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("Figure8_size_", riparian.scenario, ".png"), plot = last_plot(), 
         width = 6, height = 6, units = "in", dpi = 300)
}


#--- Figure 9: YEARLING GROWTH -----
png(paste0(plot.directory, "/Figure9_growth_over_time.png"), width = 6, height = 9, units = "in", res = 300)
par(mfrow = c(3, 1), las = 1, mar = c(2,7,0.5,1), oma = rep(0.5, 4), cex = 1.1)

for(timeperiod in c("historical", "future")){
  td <- get(paste0("growth.", timeperiod))
  dat <- apply(td, 1, quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = T)
  dat<- t(dat)

  # Set up date axis
  first.date <- as.Date("1994-09-01") #starting date for simulation and for spawning
  last.date <- as.Date("1995-08-31") #last date of the simulation
  months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  #minX <- trunc(which(dat[,3] > 0))[1]
  #val1 <- as.numeric(format(as.Date(minX + 1, origin = first.date), "%m"))
  minX <- 67; val1 <- 11
  val2 <- as.numeric(format(last.date, "%m"))
  if(val1 >= 9 & val1 <= 12) months <- months[c(val1:length(months), 1:val2)]
  if(val1 >= 1 & val1 <= 8) months <- months[val1:val2]
  lng.out<- length(months)
  maxX <- length(dat[,3])
  
  ylm <- c(-0.0263, 0.04)
  
  if(timeperiod == "historical") {mycols <- c("#B0BFFC96", "#6262bf", "#00008B"); leglab = "(a) Historical"}
  if(timeperiod == "future") {mycols <- c("#fad189", "#fca553", "#db7902"); leglab = "(b) Future"}

  plot(dat.hf[,3], ylim = ylm, ylab = "", type = 'n', xaxt = 'n', xlab = "", xlim = c(minX, maxX), cex.axis = 1)
  axis(1, at = seq(minX, maxX, length.out = lng.out), labels = rep("",10))
  #ylab = expression(paste("Growth (g g ",d^-1,")"))
  mtext("Growth \n(g \u2219", side = 2, line = 7, adj = 0, cex = 1.1)
  mtext(expression(g^-1*" "), side = 2, line = 7, adj = -1, padj = 0.98, cex = 1.1)
  mtext(expression(d^-1*")"), side = 2, line = 7, adj = -1.9, padj = 0.98, cex = 1.1)
  
  #Min/Max
  polygon(c(minX: maxX, rev(minX: maxX)), c(dat.hf[minX: maxX, 1], rev(dat.hf[minX: maxX, 5])), border = NA, col = mycols[1])
  
  #Q1/Q3
  polygon(c(minX: maxX, rev(minX: maxX)), c(dat.hf[minX: maxX, 2], rev(dat.hf[minX: maxX, 4])), border = NA, col = mycols[2])
  
  #Median
  lines(dat.hf[,3], lwd = 2, col = mycols[3])
  
  abline(h = 0, lty = 2)
  
  legend("topleft", legend = leglab, bty = 'n')
}

# Add riparian restoration scenario panel:
td <- get("growth.riparian1.future")
dat <- apply(td, 1, quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = T)
dat<- t(dat)

# Set up date axis
first.date <- as.Date("1994-09-01") #starting date for simulation and for spawning
last.date <- as.Date("1995-08-31") #last date of the simulation
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#minX <- trunc(which(dat[,3] > 0))[1]
#val1 <- as.numeric(format(as.Date(minX + 1, origin = first.date), "%m"))
minX <- 67; val1 <- 11
val2 <- as.numeric(format(last.date, "%m"))
if(val1 >= 9 & val1 <= 12) months <- months[c(val1:length(months), 1:val2)]
if(val1 >= 1 & val1 <= 8) months <- months[val1:val2]
lng.out<- length(months)
maxX <- length(dat[,3])

mycols <- c("#d3c3e0", "#775f8a", "#572282"); leglab = "(c) Future + full riparian restoration"

plot(dat[,3], ylim = ylm, ylab = "", type = 'n', xaxt = 'n', xlab = "", xlim = c(minX, maxX), cex.axis = 1)
axis(1, at = seq(minX, maxX, length.out = lng.out), labels = months)
#ylab = expression(paste("Growth (g g ",d^-1,")"))
mtext("Growth \n(g \u2219", side = 2, line = 7, adj = 0, cex = 1.1)
mtext(expression(g^-1*" "), side = 2, line = 7, adj = -1, padj = 0.98, cex = 1.1)
mtext(expression(d^-1*")"), side = 2, line = 7, adj = -1.9, padj = 0.98, cex = 1.1)

#Min/Max
polygon(c(minX: maxX, rev(minX: maxX)), c(dat[minX: maxX, 1], rev(dat[minX: maxX, 5])), border = NA, col = mycols[1])

#Q1/Q3
polygon(c(minX: maxX, rev(minX: maxX)), c(dat[minX: maxX, 2], rev(dat[minX: maxX, 4])), border = NA, col = mycols[2])

#Median
lines(dat[,3], lwd = 2, col = mycols[3])

abline(h = 0, lty = 2)

legend("topleft", legend = leglab, bty = 'n')

dev.off()


#--- Figure 10: RIPARIAN COMPARISON -------------------------------------------------------
# Note: Need to run Table 1 summary first - this uses the data compiled there.
base1 <- container[container$Period == "Historical" & container$Riparian == "Baseline", 4:ncol(container)]
base2 <- container[container$Period == "Future" & container$Riparian == "Baseline", 4:ncol(container)]
base1$DateEmerge <- base1$DateOutmigrate <- 365; base2$DateEmerge <- base2$DateOutmigrate <- 365
row1 <- (container[container$Period == "Future" & container$Riparian == "Baseline", 4:ncol(container)] - container[container$Period == "Historical" & container$Riparian == "Baseline", 4:ncol(container)]) / base1
row2 <- (container[container$Period == "Future" & container$Riparian == "FullRestoration", 4:ncol(container)] - container[container$Period == "Future" & container$Riparian == "Baseline", 4:ncol(container)]) / base2
row3 <- (container[container$Period == "Future" & container$Riparian == "PartialRestoration", 4:ncol(container)] - container[container$Period == "Future" & container$Riparian == "Baseline", 4:ncol(container)]) / base2
row4 <- (container[container$Period == "Future" & container$Riparian == "LeastProtective", 4:ncol(container)] - container[container$Period == "Future" & container$Riparian == "Baseline", 4:ncol(container)]) / base2
balloon.data <- rbind(row1, row2, row3, row4)
balloon.data$DateEmerge <- as.numeric(balloon.data$DateEmerge); balloon.data$DateOutmigrate <- as.numeric(balloon.data$DateOutmigrate)
balloon.data
row.names(balloon.data) <- c("Climate effect, Baseline riparian", "Full restoration effect, Future climate", "Partial restoration effect, Future climate", "Least protective effect, Future climate")
colnames(balloon.data) <- c("Subyearling survival", "Potential yearling survival", "Date emerged", "Date outmigrated", "Subyearling mass", "Potential yearling mass")

mycolrs <- sign(balloon.data)
mycolrs[mycolrs == -1] <- "#030303" #"#c73926"
mycolrs[mycolrs >=0] <- "#ffffff" #"#4fc3e3"
mycolrs <- unlist(mycolrs)

ggpubr::ggballoonplot(abs(balloon.data), fill = mycolrs, size.range = c(1, 20)) +
  theme(axis.ticks = element_blank())

if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("Figure10_scenario_comparison.png"), plot = last_plot(), 
         width = 7.5, height = 3.5, units = "in", dpi = 300)  
}


#--- Figure S1: GCM PRECIP & AIR TEMP ------------------------------------------------------------
# read in data and prep for ggplot2
td <- read.csv(paste0("data.in/rbm.data/GCM_P_T.csv"), header = T)
td <- td %>% 
  transmute(Variable = as.factor(Variable),
            Period = as.factor(Period),
            GCM = as.factor(GCM),
            Month = as.factor(Month),
            Value = as.numeric(Value)) %>%
  mutate(Period2 = fct_relevel(Period, "H", "F"))
  levels(td$Period2) <- c("Historical", "Future")
  #levels(td$Month) <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D") #can't do this because it'll group all the 'J's etc.
  levels(td$Month) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

for(var in c("P", "T")){
  if(var == "P"){ylm = c(0, 600); ylb = "Precipitation\n(mm)"}
  if(var == "T"){ylm = c(-2, 30); ylb = "Air\ntemperature\n(\u00B0C)"}  # this weird syntax gives degree symbol
  
plot.object <- td %>% 
  # filter data to survivors
  filter(Variable == !!(var)) %>%
  # plot weight
  ggplot(aes(x = Month, y = Value, color = Period2, fill = Period2)) + 
  # add box plot
  geom_boxplot(alpha = 0.5) +
#  # supply x and y limits
  coord_cartesian(ylim = ylm) +
#  # split plot by life history stage
#  facet_wrap( ~ Scenario, nrow = 4, ncol = 1) +
  # set theme
  theme_classic() +
  # remove legend title
  theme(legend.title = element_blank()) +
#  # remove legend
#  theme(legend.position = leg) + leg = "none", "right", etc.
  # remove subplot label background
  theme(strip.background = element_blank()) +
  # adjust y-axis label position
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  # add axis and title text
  labs(x = "", y = ylb) +
  # manually set color
  scale_color_manual(values=c("#00008B", "#FF8C00")) +
  # manually set fill
  scale_fill_manual(values=c("#00008B", "#FF8C00")) 

assign(paste0(var, ".plot.object"), plot.object); rm(plot.object)
}

#Combine plots for manuscript figure
figure <- ggpubr::ggarrange(P.plot.object, T.plot.object,
                  labels = c("(a)", "(b)"), hjust = -6.5, vjust = 1.8, font.label = list(size = 12, face = "plain"),
                  ncol = 1, nrow = 2, align = "hv", common.legend = TRUE)
figure
if (save.figures) {
  ggsave(path = plot.directory, filename = "FigureS1_GCM_P_T.png", plot = last_plot(), 
         width = 6, height = 6, units = "in", dpi = 300)
}


#--- Figure S2: DHSVM-RBM Annual flow & water temperature time series ----
# Plot time series of GCM ensemble medians and quantiles with transparent shading
# Warning: slow processing time!

riparian.scenario <- "riparian0"
# Temperature
for(time.period  in c("historical", "future")){
  Tdat <- get(paste0(riparian.scenario, ".", time.period , ".T"))
  themedian <- apply(Tdat[3:ncol(Tdat)], 1, median)
  themin <- apply(Tdat[3:ncol(Tdat)], 1, quantile, probs = 0.05)
  themax <- apply(Tdat[3:ncol(Tdat)], 1, quantile, probs = 0.95)
  thedate <- Tdat$Date
  temper <- cbind.data.frame(thedate, themedian, themin, themax)
  assign(paste0("Tt.",time.period), temper)
}
# Flow
for(time.period  in c("historical", "future")){
  Qdat <- get(paste0(riparian.scenario, ".", time.period , ".Q"))
  themedian <- apply(Qdat[3:ncol(Qdat)], 1, median)
  themin <- apply(Qdat[3:ncol(Qdat)], 1, quantile, probs = 0.05)
  themax <- apply(Qdat[3:ncol(Qdat)], 1, quantile, probs = 0.95)
  thedate <- Qdat$Date
  flow <- cbind.data.frame(thedate, themedian, themin, themax)
  assign(paste0("Qq.",time.period), flow)
}


png(paste0(plot.directory, "/FigureS2_QT_timeseries.png"), width = 10, height = 7.5, units = "in", res = 300)
par(las = 1, mfcol = c(2, 2), mar = c(3, 4, 1, 0.52), oma = c(0.5, 4, 2, 0.5), cex = 1)

for(time.period  in c("historical", "future")){
  
  Tt <- get(paste0("Tt.", time.period))
  Qq <- get(paste0("Qq.", time.period))
  if(time.period  == "historical"){col2use <- 4; labls <- c("(a)","(b)"); main <- "Historical"}
  if(time.period  == "future"){col2use <- 2; labls <- c("(c)","(d)"); main <- "Future"}
  
  # Temperature
  plot(Tt$thedate, Tt$themedian, type = 'n', ylab = "", xlab = "", ylim = c(0, 34)) # ylab = expression("Water temperature ("*degree*C*")")
  if(time.period == "historical") mtext("Water\ntemperature\n(\u00B0C)", side = 2, line = 5, adj = 0.5, cex = 1.1, oma = TRUE)
  polygon(c(Tt$thedate, rev(Tt$thedate)), c(Tt$themin, rev(Tt$themax)), border = NA, col = mycolors[[1]][col2use])
  lines(Tt$thedate, Tt$themedian, col = mycolors[[3]][col2use])
  legend("topleft", legend = labls[1], bty = 'n')

  mtext(main, side = 3, line = 1, cex = 1.3)
  
  # Flow
  plot(Qq$thedate, Qq$themedian, type = 'n', ylab = "", xlab = "", ylim = c(0,1200)) #ylab = expression("Flow (m s"^-1*")")
  if(time.period == "historical") mtext("Flow \n(m \u2219", side = 2, line = 6.3, adj = 0, cex = 1.1, oma = TRUE)
  if(time.period == "historical") mtext(expression(s^-1*")"), side = 2, line = 5.3, adj = -0.6, padj = 0.94, cex = 1.1, oma = TRUE)
  polygon(c(Qq$thedate, rev(Qq$thedate)), c(Qq$themin, rev(Qq$themax)), border = NA, col = mycolors[[1]][col2use])
  lines(Qq$thedate, Qq$themedian, col = mycolors[[3]][col2use])
  legend("topleft", legend = labls[2], bty = 'n')
  
}

dev.off()

#--- Figure S3: MORTALITY ----------------------------------------------------------------
riparian.scenario <- "riparian0"
scenarios.data <- get(paste0(riparian.scenario, ".scenarios.data"))

for(time.period in c("Historical", "Future")){
  if(time.period == "Historical") {thecolors <- c("#87a1e8", "#00008B"); ylb <- "Simulated\nsalmon\nmortalities\n(1000s)"}
  if(time.period == "Future") {thecolors <- c("#fabc70", "#FF8C00"); ylb <- ""}
  # old colors blue/purple: c("#426e82", "#611268")
  
plot.object1 <- scenarios.data %>% 
    # filter data to mortalities and time period
    filter(FinalState == "Scoured" & Period == !!(time.period) | FinalState == "Stochastic" & Period == !!(time.period)) %>%
    # plot weight
    ggplot(aes(x = Weight, color = FinalState, fill = FinalState)) + 
    # add histogram plot
    geom_histogram(alpha = 0.5, aes(y = (..count..)/11/10), binwidth = 0.05) +
    # supply x and y limits
    coord_cartesian(xlim = c(0.25, 1.5), ylim = c(0,3000)) +
    # set theme
    theme_classic() +
    # remove legend title
    theme(legend.title = element_blank()) +
    # adjust legend position
    theme(legend.position = "right") +
    # remove bottom axis line and ticks
    theme(axis.line.x.bottom = element_blank(), axis.ticks.x.bottom = element_blank()) +
    # remove subplot label background
    theme(strip.background = element_blank()) +
    # adjust y-axis label position
    theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
    # add axis and title text
    labs(x = "Salmon mass at death (g)", y = ylb) +
    # manually set color
    scale_color_manual(values = thecolors) +
    # manually set fill
    scale_fill_manual(values = thecolors) +
    # add x-axis to each plot
    geom_hline(yintercept = 0)

if(time.period == "Historical") {assign("plot2", plot.object1); leg.h <- ggpubr::get_legend(plot.object1)}
if(time.period == "Future") {assign("plot4", plot.object1); leg.f <- ggpubr::get_legend(plot.object1)}
rm(plot.object1)

# change year so that day-month is the comparable across scenarios.
phenology.data <- scenarios.data
phenology.data$YearScour <- lubridate::year(phenology.data$DateScour)
phenology.data$YearMort <- lubridate::year(phenology.data$DateMort)

for(var in c("Scour", "Mort")){
  for(yy in c(1995:2005, 2089:2099)){
    foo <- phenology.data[phenology.data[, paste0("Date", var)] >= as.Date(paste0(yy - 1, "-09-01")) & phenology.data[, paste0("Date", var)] < as.Date(paste0(yy, "-08-31")) & !is.na(phenology.data[, paste0("Date", var)]),]
    lubridate::year(foo[foo[, paste0("Year", var)] == yy - 1, paste0("Date", var)]) <- 1900
    lubridate::year(foo[foo[, paste0("Year", var)] == yy, paste0("Date", var)]) <- 1901
    phenology.data[phenology.data[, paste0("Date", var)] >= as.Date(paste0(yy - 1, "-09-01")) & phenology.data[, paste0("Date", var)] < as.Date(paste0(yy, "-08-31")) & !is.na(phenology.data[, paste0("Date", var)]),] <- foo
  }
}
yy <- 1901
month.min <- lubridate::month(min(phenology.data$DateScour, na.rm = T))


  plot.object2 <- phenology.data %>% 
    mutate(Scoured = DateScour, Stochastic = DateMort) %>% 
    # filter data to mortalities and time period
    filter(FinalState == "Scoured" & Period == !!(time.period) | FinalState == "Stochastic" & Period == !!(time.period)) %>%
    # select relevant columns
    select(Scenario, Scoured, Stochastic) %>% 
    # combine emergence and outmigration into a single column
    gather(key = "Event", value = "Date", c(Scoured, Stochastic)) %>% 
    # # filter data to fish that experienced each event
    # filter(!is.na(Date)) %>% 
    # plot event vs. day-month
    ggplot(aes(x = Date, fill = Event, color = Event)) + 
    # add histogram
    geom_histogram(alpha = 0.5, position = "identity", aes(y = (..count..)/11/10), binwidth = 5) +
    # supply x and y limits
    coord_cartesian(ylim = c(0, 200)) +
    # set theme
    theme_classic() +
#    # adjust y-axis label position
    theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
#    # remove legend title
    theme(legend.title = element_blank()) +
#    theme(legend.position = "none") +
    # remove bottom axis line and ticks
    theme(axis.line.x.bottom = element_blank(), axis.ticks.x.bottom = element_blank()) +
    # remove subplot label background
    theme(strip.background = element_blank()) +
    # manually set color
    scale_color_manual(values = thecolors) +
    # manually set fill
    scale_fill_manual(values = thecolors) +
    # set x-axis labels to day-month
    scale_x_date(limits = c(as.Date(paste0(yy - 1, "-", month.min, "-01")), as.Date(paste0(yy, "-07-15"))), date_labels = "%b", date_breaks = "1 month") +
    # adjust y-axis label text
    labs(y = ylb, x = "Date of death") +
    # add x-axis to each plot
    geom_hline(yintercept = 0)
  if(time.period == "Historical") assign("plot1", plot.object2)
  if(time.period == "Future") assign("plot3", plot.object2)
  rm(plot.object2)
  
} #end Hist/Fut

  #Combine plots for manuscript figure
  figure <- ggpubr::ggarrange(plot1, plot3, plot2, plot4,
                      labels = c("(a)", "(b)", "(c)", "(d)"), hjust = -6.5, vjust = 1.8, font.label = list(size = 12, face = "plain"),
                      ncol = 2, nrow = 2, align = "hv", legend.grob = leg.f, common.legend = TRUE)
  figure
  if (save.figures) {
    ggsave(path = plot.directory, filename = paste0("FigureS3_mortality_", riparian.scenario, ".png"), plot = figure, 
           width = 8.5, height = 7, units = "in", dpi = 300)
  }
  #save legends for assembling in photo editing software
  ggsave(path = plot.directory, filename = "legend.his.png", plot = ggpubr::as_ggplot(leg.h),width = 2, height = 2, units = "in", dpi = 300)
  ggsave(path = plot.directory, filename = "legend.fut.png", plot = ggpubr::as_ggplot(leg.f),width = 2, height = 2, units = "in", dpi = 300)


#--- Figure S4: FINAL STATE ---------------------------------------------------------------

the.data <- riparian0.scenarios.data
no_spawners <- spawners.h #equal to spawners.f
  
for(var in c("Subyearling", "Yearling")){
if(var == "Subyearling"){labl <- " (a) Subyearling migrants"; ylm <- c(0,815); xticks <- element_blank()}
if(var == "Yearling"){labl <- " (b) Potential yearlings"; ylm <- c(0,105); xticks <- element_text(angle = 45, hjust = 1)}

# Final state barplot (historical or future)
p1 <- the.data %>% 
  mutate(Period2 = fct_relevel(Period, "Historical", "Future")) %>%
  # filter data to life history type
  filter(FinalState == !!(var)) %>%
  # plot survivors vs. scenario
  ggplot(aes(x = Scenario, y = (..count..)/c(no_spawners, no_spawners)*mean(no_spawners)/11, color = Period2, fill = Period2)) + 
  # add barplot
  geom_bar(alpha = 0.5, position = "dodge") +
  # set theme
  theme_classic() +
  # set y-axis limit
  ylim(ylm) +
  # remove legend title
  theme(legend.title = element_blank()) +
  # add axis and title text
  labs(x = "", y = "Simulated\nsalmon\nsurvivors\n(1000s)") +
  # adjust y-axis label position
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  # adjust x-axis label rotation
  theme(axis.text.x = xticks) +
  # manually set color
  scale_color_manual(values=c("#00008B", "#FF8C00")) +
  # manually set fill
  scale_fill_manual(values=c("#00008B", "#FF8C00")) +
  # add plot title
  ggtitle(labl)
#  # center plot title
#  theme(plot.title = element_text(hjust = 0.5))

assign(paste0("plot.",var), p1); rm(p1)
}
  
#Combine plots for manuscript figure
figure <- ggpubr::ggarrange(plot.Subyearling, plot.Yearling,
                            ncol = 1, nrow = 2, align = "hv", common.legend = TRUE)
figure
if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("FigureS4_survival_", riparian.scenario, ".png"), plot = figure, 
         width = 7, height = 10, units = "in", dpi = 300)
}



# End of file ====