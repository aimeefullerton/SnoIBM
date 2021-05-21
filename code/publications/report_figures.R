# Figures used for final project report describing results from climate and riparian scenario effects on Chinook salmon
# Last updated 23 Oct 2020

#--- SETUP #####
# Load packages
library(tidyverse)
library(sf)
# Depends (packages that must be installed but don't need to be loaded):
# lubridate, ggpubr, SSN, abind, RColorBrewer

source("code/functions.R")

# Set up directory structure
data.in <- "data.in" #/Volumes/BluPassport/SnoIBM/data.in"
data.out<- "data.out" #/Volumes/BluPassport/SnoIBM/data.out"
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

# Load data:
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
    Tt <- fncImportQ(paste0(data.in, "/rbm.data/", riparian.scenario, "/", climate.scenario, "/Tw_output.", time.period , suffix, ".csv"))
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

# Load data:
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
# Maps
themetric <- "mean"
for(time.period  in c("historical", "future")){
  foo <- read.csv(paste0("data.in/rbm.data/T.seasonal.", themetric, "s.", time.period , ".csv"), header = T, stringsAsFactors = F, row.names = 1)
  assign(paste0("T.seasons.", time.period ), foo); rm(foo)
  foo <- read.csv(paste0("data.in/rbm.data/Q.seasonal.", themetric, "s.", time.period , ".csv"), header = T, stringsAsFactors = F, row.names = 1)
  assign(paste0("Q.seasons.", time.period ), foo); rm(foo)
}

# Time series
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

#--- SUMMARY ACROSS SCENARIOS (needed for Summary Comparison Figure) ---------------------

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
#write.csv(container, "data.out/Table1.csv", row.names = F)

#--- DHSVM-RBM CLIMATE CHANGE EFFECT MAPS -----

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
png(paste0(plot.directory, "/Figure_DHSVM-RBM_Qmaps_mean2.png"), width = 7.5, height = 7, units = "in", res = 300)
par(mfrow = c(2, 2), mar = c(1, 0, 4, 0), oma = rep(0.5,4), las = 1)

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
}

dev.off()

# Make seasonal water temperature change maps
png(paste0(plot.directory, "/Figure_DHSVM-RBM_Tmaps_mean2.png"), width = 7.5, height = 7, units = "in", res = 300)
par(mfrow = c(2, 2), mar = c(1, 0, 4, 0), oma = rep(0.5,4), las = 1)

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

}

dev.off()

#--- DHSVM-RBM CLIMATE CHANGE EFFECT TIME SERIES -----
# Get all data into same year for plotting
# change year so that day-month is the comparable across scenarios.
riparian.scenario <- "riparian0"
for(time.period  in c("historical", "future")){
  if(time.period  == "historical") theyears <- 1995:2005
  if(time.period  == "future") theyears <- 2089:2099
  Q.data <- get(paste0(riparian.scenario, ".", time.period , ".Q"))
  T.data <- get(paste0(riparian.scenario, ".", time.period , ".T"))
  Q.data$Year <- lubridate::year(Q.data$Date)
  T.data$Year <- lubridate::year(T.data$Date)
  
  for(yy in theyears){
    
    # Remove leap year's extra day:
    if(yy %in% c(1996, 2000, 2004, 2092, 2096)){
      idx <- which(Q.data$Date == as.Date(paste0(yy, "-02-29")))
      Q.data <- Q.data[-idx,]
      T.data <- T.data[-idx,]
      rm(idx)
    }
    
    #Flow
    foo <- Q.data[Q.data[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & Q.data[, "Date"] < as.Date(paste0(yy, "-10-01")) & !is.na(Q.data[, "Date"]),]
    lubridate::year(foo[foo[, "Year"] == yy - 1, "Date"]) <- 1900
    lubridate::year(foo[foo[, "Year"] == yy, "Date"]) <- 1901
    Q.data[Q.data[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & Q.data[, "Date"] < as.Date(paste0(yy, "-10-01")) & !is.na(Q.data[, "Date"]),] <- foo
    
    #Temp
    foo <- T.data[T.data[, "Date"] >= as.Date(paste0(yy - 1, "-10-01")) & T.data[, "Date"] < as.Date(paste0(yy, "-10-01")) & !is.na(T.data[, "Date"]),]
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

png(paste0(plot.directory,"/Figure_QT_timeseries.png"), width = 9, height = 2.3, units = "in", res = 300)
par(las = 1, mfrow = c(1,2), mar = c(2, 5, 0.5, 3), oma = c(0.5, 3, 0.5, 0))

themetric <- "mean"
# Top row Riparian baseline future minus historical
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

#--- DHSVM-RBM RIPARIAN EFFECT MAPS -----

# Read in basin outline & streams
basin <- read_sf(paste0("data.in/shapefiles"), "Basin_snq2")
basin2 <- basin %>% summarise(area = sum(AreaSqKm)) # dissolve on area to just get the outline
add.labels <- F #add descriptive labels & legends to each panel?
if(add.labels == T) ex <- raster::extent(basin2)
streams <- read_sf("data.in/shapefiles", "streamline_original")
idx <- which(streams$downarc == -1)[2:7] #isolated reaches that need to be removed
streams <- streams[-idx,]
streams$WT <- NA
streams$col.class.t <- NA

seasons <- c("Fall", "Winter", "Spring", "Summer", "Blank")
Tletters <- c("(a)", "(b)", "(c)", "(d)")


# Make seasonal water temperature change maps
png(paste0(plot.directory, "/Figure_DHSVM-RBM_Tmaps_mean_riparian.png"), width = 8, height = 10, units = "in", res = 300)
par(mfcol = c(4, 3), mar = c(1, 0, 4, 0), oma = rep(0.5,4), las = 1)

# Change (riparian scenario minus future baseline)
descriptions <- c("(a) Climate effect, \nBaseline riparian", "(b) Full restoration \neffect, Future climate", "(c) Partial restoration \neffect, Future climate", "(d) Riparian degradation \neffect, Future climate")
riparian.scenario.list <- c("riparian1", "riparian3", "riparian2")
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
  
  
  for(season in 1:4){
  wt <- t(T.seasons[season,]) #4 = summer temperature
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
  }
}
dev.off()



#--- SUMMARY COMPARISON -------------------------------------------------------
# Note: Need to run 'Summary Across Scenarios' first - this uses the data compiled there.
base1 <- container[container$Period == "Historical" & container$Riparian == "Baseline", 4:ncol(container)]
base2 <- container[container$Period == "Future" & container$Riparian == "Baseline", 4:ncol(container)]
base1$DateEmerge <- base1$DateOutmigrate <- 365; base2$DateEmerge <- base2$DateOutmigrate <- 365
row1 <- (container[container$Period == "Future" & container$Riparian == "Baseline", 4:ncol(container)] - container[container$Period == "Historical" & container$Riparian == "Baseline", 4:ncol(container)]) / base1
row2 <- (container[container$Period == "Future" & container$Riparian == "FullRestoration", 4:ncol(container)] - container[container$Period == "Future" & container$Riparian == "Baseline", 4:ncol(container)]) / base2
row3 <- (container[container$Period == "Future" & container$Riparian == "PartialRestoration", 4:ncol(container)] - container[container$Period == "Future" & container$Riparian == "Baseline", 4:ncol(container)]) / base2
row4 <- (container[container$Period == "Future" & container$Riparian == "Degradation", 4:ncol(container)] - container[container$Period == "Future" & container$Riparian == "Baseline", 4:ncol(container)]) / base2
balloon.data <- rbind(row1, row2, row3, row4)
balloon.data$DateEmerge <- as.numeric(balloon.data$DateEmerge); balloon.data$DateOutmigrate <- as.numeric(balloon.data$DateOutmigrate)
balloon.data
row.names(balloon.data) <- c("Climate effect, Baseline riparian", "Full restoration effect, Future climate", "Partial restoration effect, Future climate", "Riparian degradation effect, Future climate")
colnames(balloon.data) <- c("Subyearling survival", "Potential yearling survival", "Date emerged", "Date outmigrated", "Subyearling mass", "Potential yearling mass")

mycolors <- c("#99000D", "#1F78B4",  "#A6CEE3", "#FC9272") # "#B2DF8A" "#99000D"
      
png(paste0(plot.directory,"/Figure_SalmonSummary.png"), width = 8, height = 6, units = "in", res = 300)
par(las = 1)
barplot(c(balloon.data[,1], NA, NA, balloon.data[,2], NA, NA, balloon.data[,3], NA, NA, balloon.data[,4], NA, NA, balloon.data[,5], NA, NA, balloon.data[,6]),
        col = rep(c(mycolors, NA, NA),6))
abline(h = 0)
dev.off()

png(paste0(plot.directory,"/Salmon_LegendColors.png"), width = 2.5, height = 7, units = "in", res = 300)
barplot(rep(1, 4), col = mycolors, horiz = T, axes = F)
dev.off()

# End of file ====