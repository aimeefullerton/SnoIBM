
#----- Prepare -----------------------------------------------------------------

# load packages
library(lubridate)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(dplyr)
library(GLDEX)

# would you like to save these figures?
save.figures <- TRUE
if (save.figures) {
  # specify directory for plot output
  plot.directory <- "plots"
  if (!dir.exists(plot.directory)) {dir.create(plot.directory)}
}

# load data froom scenarios to be compared
#scenario.list = c("current_climate_riparian_0", "current_climate_riparian_1", "current_climate_riparian_2", "current_climate_riparian_3")
  #years = 2003:2013
scenario.list <- c("bcc-csm1-1-m", "CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "HadGEM2-CC365", "HadGEM2-ES365", "IPSL-CM5A-MR", "MIROC5", "NorESM1-M")
riparian.scenario.list <- c("riparian0", "riparian1", "riparian2", "riparian3") #Baseline, Full restoration, Degradation, Partial restoration

for(riparian.scenario in riparian.scenario.list){
  
# Gather historical climate scenario data
timeperiod <- "historical"
for(scenario in scenario.list){
  scenario.data = NULL
  years = 1995:2005
  for(yy in years){
    outdir <- paste0(scenario, ".A.", yy)
    load(file = paste0("/Volumes/BluPassport/SnoIBM/data.out_", riparian.scenario, "/", outdir, "/salmon.finalstep.", yy, ".1.RData"))
    scenario.data = rbind(scenario.data, salmon.finalstep)
  }
  scenario.data = as.data.frame(scenario.data)
  scenario.data = cbind(scenario = scenario, scenario.data)
  assign(paste0(scenario, ".data"), scenario.data); rm(scenario.data)
  rm(salmon.finalstep)
}

# combine scenarios into one dataframe
his.scenarios.data = NULL
for(scenario in scenario.list){
  sd = get(paste0(scenario,".data"))
  his.scenarios.data = rbind(his.scenarios.data, sd)
  rm(sd)
}

his.scenarios.data$dateSc = his.scenarios.data$dateDi; his.scenarios.data$dateSc[his.scenarios.data$survive != -2] = NA
his.scenarios.data$dateMo = his.scenarios.data$dateDi; his.scenarios.data$dateMo[his.scenarios.data$survive != 0] = NA


#Get number of spawners and survivors across scenarios
#spawners <- tapply(his.scenarios.data$pid, list(his.scenarios.data$scenario), length) #Spawners
#srv.sy <- tapply(his.scenarios.data$pid[his.scenarios.data$survive == 2], list(his.scenarios.data$scenario[his.scenarios.data$survive == 2]), length) #Subyearlings
#srv.yl <- tapply(his.scenarios.data$pid[his.scenarios.data$survive == 1], list(his.scenarios.data$scenario[his.scenarios.data$survive == 1]), length) #Yearlings
#quantile(srv.sy/spawners)
#quantile(srv.yl/spawners)

# prep data for ggplot2
his.scenarios.data <- his.scenarios.data %>% 
  select(scenario, survive, weight, dateSp, dateEm, dateOm, dateSc, dateMo) %>% 
  transmute(Scenario = as.factor(scenario),
            FinalState = as.factor(survive),
            Weight = as.numeric(weight),
            DateSpawn = date(as_datetime(dateSp, origin = "1970-01-01")),
            DateEmerge = date(as_datetime(dateEm, origin = "1970-01-01")),
            DateOutmigrate = date(as_datetime(dateOm, origin = "1970-01-01")),
            DateScour = date(as_datetime(dateSc, origin = "1970-01-01")),
            DateMort = date(as_datetime(dateMo, origin = "1970-01-01")))
levels(his.scenarios.data$FinalState) <- c("Scoured", "Stochastic", "Yearling", "Subyearling")
his.scenarios.data$YearSpawn = year(his.scenarios.data$DateSpawn)

# Save
assign(paste0(riparian.scenario, ".", timeperiod), his.scenarios.data)
rm(his.scenarios.data, `bcc-csm1-1-m.data`, CanESM2.data, CCSM4.data, `CNRM-CM5.data`, `CSIRO-Mk3-6-0.data`, `HadGEM2-CC365.data`, `HadGEM2-ES365.data`, `IPSL-CM5A-MR.data`, MIROC5.data, `NorESM1-M.data`)
thedata <- get(paste0(riparian.scenario, ".", timeperiod))
save(thedata, file = paste0("data.out/", riparian.scenario, ".", timeperiod, ".RData"))



# Gather future climate scenario data
timeperiod <- "future"
for(scenario in scenario.list){
  scenario.data = NULL
  years = 2089:2099
  for(yy in years){
    outdir <- paste0(scenario, ".A.", yy)
    load(file = paste0("/Volumes/BluPassport/SnoIBM/data.out_", riparian.scenario, "/", outdir, "/salmon.finalstep.", yy, ".1.RData"))
    scenario.data = rbind(scenario.data, salmon.finalstep)
  }
  scenario.data = as.data.frame(scenario.data)
  scenario.data = cbind(scenario = scenario, scenario.data)
  assign(paste0(scenario, ".data"), scenario.data); rm(scenario.data)
  rm(salmon.finalstep)
}

# combine scenarios into one dataframe
fut.scenarios.data = NULL
for(scenario in scenario.list){
  sd = get(paste0(scenario,".data"))
  fut.scenarios.data = rbind(fut.scenarios.data, sd)
  rm(sd)
}
fut.scenarios.data$dateSc = fut.scenarios.data$dateDi; fut.scenarios.data$dateSc[fut.scenarios.data$survive != -2] = NA
fut.scenarios.data$dateMo = fut.scenarios.data$dateDi; fut.scenarios.data$dateMo[fut.scenarios.data$survive != 0] = NA

# prep data for ggplot2
fut.scenarios.data <- fut.scenarios.data %>% 
  select(scenario, survive, weight, dateSp, dateEm, dateOm, dateSc, dateMo) %>% 
  transmute(Scenario = as.factor(scenario),
            FinalState = as.factor(survive),
            Weight = as.numeric(weight),
            DateSpawn = date(as_datetime(dateSp, origin = "1970-01-01")),
            DateEmerge = date(as_datetime(dateEm, origin = "1970-01-01")),
            DateOutmigrate = date(as_datetime(dateOm, origin = "1970-01-01")),
            DateScour = date(as_datetime(dateSc, origin = "1970-01-01")),
            DateMort = date(as_datetime(dateMo, origin = "1970-01-01")))
levels(fut.scenarios.data$FinalState) <- c("Scoured", "Stochastic", "Yearling", "Subyearling")
fut.scenarios.data$YearSpawn = year(fut.scenarios.data$DateSpawn)

# Save
assign(paste0(riparian.scenario, ".", timeperiod), fut.scenarios.data)
rm(fut.scenarios.data, `bcc-csm1-1-m.data`, CanESM2.data, CCSM4.data, `CNRM-CM5.data`, `CSIRO-Mk3-6-0.data`, `HadGEM2-CC365.data`, `HadGEM2-ES365.data`, `IPSL-CM5A-MR.data`, MIROC5.data, `NorESM1-M.data`)
thedata <- get(paste0(riparian.scenario, ".", timeperiod))
save(thedata, file = paste0("data.out/", riparian.scenario, ".", timeperiod, ".RData"))
rm(thedata)

}



# Re-load
for(riparian.scenario in c("riparian0", "riparian1", "riparian2", "riparian3")){
  for(timeperiod in c("historical", "future")){
    load(paste0("data.out/", riparian.scenario, ".", timeperiod, ".RData"))
    assign(paste0(riparian.scenario, ".", timeperiod), thedata); rm(thedata)
    rm(thedata)
  }
}


# Create container to hold results
container <- matrix(NA, nrow = 8, ncol = 9)
container <- as.data.frame(container)
colnames(container) <- c("Period", "Riparian", "ScenNo", "SY_Survival", "Y_Survival", "DateEmerge", "DateOutmigrate", "SY_Mass", "Y_Mass")
scenario.names <- c("Baseline", "FullRestoration", "PartialRestoration", "Degradation")
scenario.numbers <- c(0, 1, 3, 2)
container$Riparian <- rep(scenario.names, 2)
container$ScenNo <- rep(scenario.numbers, 2)
container$Period <- c(rep("Historical", 4), rep("Future", 4))
container$DateEmerge <- as.POSIXct(container$DateEmerge)
container$DateOutmigrate <- as.POSIXct(container$DateOutmigrate)


#Get numbers of spawners, yearlings, & subyearlings across scenarios
riparian.scenario <- "riparian0"
his.scenarios.data <- get(paste0(riparian.scenario, ".historical"))
fut.scenarios.data <- get(paste0(riparian.scenario, ".future"))

#Historical
spawners.by.scenario_year.h <- table(his.scenarios.data$YearSpawn, his.scenarios.data$Scenario)
spawners.h <- apply(spawners.by.scenario_year.h, 2, sum)
juveniles.by.scenario.h <- round(table(his.scenarios.data$FinalState, his.scenarios.data$Scenario)[3:4,]/ spawners.h * mean(spawners.h) / 11)

#Future
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

#--- TABLE ---------------------

# Prep data
# get quantiles that pool across 10 GCMs and 11 years (but year is standardized to 1901)
td <- get(paste0(riparian.scenario, ".scenarios.data"))
td$YearSpawn = year(td$DateSpawn)
td$YearEmerge = year(td$DateEmerge)
td$YearOutmigrate = year(td$DateOutmigrate)
for(var in c("Spawn", "Emerge", "Outmigrate")){
  for(yy in c(1995:2005, 2089:2099)){
    #for(yy in years){
    foo = td[td[, paste0("Date", var)] >= as.Date(paste0(yy - 1, "-09-01")) & td[, paste0("Date", var)] < as.Date(paste0(yy, "-08-31")) & !is.na(td[, paste0("Date", var)]),]
    year(foo[foo[, paste0("Year", var)] == yy - 1, paste0("Date", var)]) <- 1900
    year(foo[foo[, paste0("Year", var)] == yy, paste0("Date", var)]) <- 1901
    td[td[, paste0("Date", var)] >= as.Date(paste0(yy - 1, "-09-01")) & td[, paste0("Date", var)] < as.Date(paste0(yy, "-08-31")) & !is.na(td[, paste0("Date", var)]),] <- foo
  }
}

# Date emerged or smolted
for(per in c("Historical", "Future")){
  for(var in c("DateEmerge", "DateOutmigrate")){
    result <- td %>% 
    mutate(PhenObj = eval(parse(text = var))) %>% 
    # filter data to surviving fish
    filter(FinalState == "Subyearling" | FinalState == "Yearling") %>% 
    # filter to correct period
    filter(Period == !!(per)) %>%
    # select relevant columns
    select(Scenario, PhenObj) %>% 
    # filter data to fish that experienced each event
    filter(!is.na(Date)) 
    
    result$Date <- as.POSIXct(result$PhenObj)
    thestatistic <- quantile(result$Date, probs = c(0.05, 0.5, 0.95), na.rm = T)
    print(thestatistic)
    container[container$Period == per & container$ScenNo == as.numeric(substr(riparian.scenario, 9, 9)), var] <- thestatistic["50%"]
  }
}

# Final mass subyearling or yearling
for(per in c("Historical", "Future")){
  for(var in c("Subyearling", "Yearling")){
    result <- td %>% 
      # filter data to surviving fish
      filter(FinalState == !!(var)) %>% 
      # filter to correct period
      filter(Period == !!(per)) %>%
      # select relevant columns
      select(Scenario, Weight)
    
      thestatistic <- quantile(result$Weight, probs = c(0.05, 0.5, 0.95))
      print(thestatistic)
      ifelse(substr(var, 1, 1) == "S", column <- "SY_Mass", column <- "Y_Mass")
      container[container$Period == per & container$ScenNo == as.numeric(substr(riparian.scenario, 9, 9)), column] <- thestatistic["50%"]
  }
}

# No. subyearlings or yearlings
for(per in c("Historical", "Future")){
  for(var in c("Subyearling", "Yearling")){
    result <- td %>% 
      # filter data to surviving fish
      filter(FinalState == !!(var)) %>% 
      # filter to correct period
      filter(Period == !!(per)) %>%
      # select relevant columns
      select(Scenario, FinalState, YearSpawn)
    totals <- td %>% 
      # filter to correct period
      filter(Period == !!(per)) %>%
      # select relevant columns
      select(Scenario, FinalState, YearSpawn)
    
    result <- tapply(result$FinalState, list(result$YearSpawn, result$Scenario), length)
    totals <- tapply(totals$FinalState, list(totals$YearSpawn, totals$Scenario), length)
    #quantile(result, probs = c(0.05, 0.5, 0.95), na.rm = T)
    survival <- result / totals
    thestatistic <- quantile(survival, probs = c(0.05, 0.5, 0.95), na.rm = T)
    print(thestatistic)
    ifelse(substr(var, 1, 1) == "S", column <- "SY_Survival", column <- "Y_Survival")
    container[container$Period == per & container$ScenNo == as.numeric(substr(riparian.scenario, 9, 9)), column] <- thestatistic["50%"]
  }
}
rm(result, totals, thestatistic)
container

#years = 1995:2005 #years = 2089:2099


#--- RIPARIAN COMPARISON -------------------------------------------------------
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
colnames(balloon.data) <- c("Subyearling survival", "Yearling survival", "Date emerged", "Date outmigrated", "Subyearling mass", "Yearling mass")

mycolrs <- sign(balloon.data)
mycolrs[mycolrs == -1] <- "#c73926"
mycolrs[mycolrs >=0] <- "#4fc3e3"
mycolrs <- unlist(mycolrs)

ggballoonplot(abs(balloon.data), fill = mycolrs, size.range = c(1, 20))

if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("Figure10_scenario_comparison.png"), plot = last_plot(), 
         width = 7.5, height = 3.5, units = "in", dpi = 300)  
}


#--- FINAL STATE ---------------------------------------------------------------
if(2000 %in% years){the.data <- his.scenarios.data; no_spawners = spawners; nm = "CChis"}
if(2090 %in% years){the.data <- fut.scenarios.data; no_spawners = spawners.f; nm = "CCfut"}

# Final state barplot (historical or future)
the.data %>% 
  # filter data to surviving fish
  filter(FinalState == "Subyearling" | FinalState == "Yearling") %>%
  # plot final state vs. scenario
  ggplot(aes(x = Scenario, color = FinalState, fill = FinalState)) + 
  # add barplot
  #geom_bar(aes(y = (..count..)/sum(..count..), alpha = 0.5)) +
  #geom_bar(aes(y = (..count..)/11), alpha = 0.5) +
  geom_bar(aes(y = (..count..)/c(no_spawners, no_spawners)*mean(no_spawners)/11), alpha = 0.5) +
  # set theme
  theme_classic() +
  # remove legend title
  theme(legend.title = element_blank()) +
  # adjust y-axis label position
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  # manually set color
  scale_color_manual(values=c("#008000", "#808080")) +
  # manually set fill
  scale_fill_manual(values=c("#008000", "#808080")) +
  # adjust y-axis label text
  labs(y = "Simulated\nsalmon\n(1000s)")
if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("FinalState.", nm, ".png"), plot = last_plot(), 
         width = 5.5, height = 4, units = "in", dpi = 300)  
}

#--- FINAL MASS ----------------------------------------------------------------

# SUBYEARLING / YEARLING SIZES
for(var in c("Subyearling", "Yearling")){
  if(var == "Subyearling"){nm <- "Subyearling"; yl <- 165; xl <- 5; ylb <- "Simulated\nSubyearling\nSalmon\n(1000s)"; xlb <- ""; bw <- 0.08}
  if(var == "Yearling"){nm <- "Yearling"; yl <- 15; xl<- 30; ylb <- "Simulated\nPotential\nYearlings\n(1000s)"; xlb <- "Fish mass (g)" ;bw<- 0.5}
  
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
  
  if (save.figures) {
    ggsave(path = plot.directory, filename = paste0("Size_AllGCMs.", nm, ".png"), plot = last_plot(), 
           width = 5, height = 3, units = "in", dpi = 300)
  }
  assign(paste0(nm, ".plot.object"), plot.object)
}

#Combine plots for manuscript figure
figure <- ggarrange(Subyearling.plot.object, Yearling.plot.object,
                    labels = c("(a)", "(b)"), hjust = -6.5, vjust = 1.8, font.label = list(size = 12, face = "plain"),
                    ncol = 1, nrow = 2, align = "hv")
if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("Figure8_size_", riparian.scenario, ".png"), plot = last_plot(), 
         width = 6, height = 6, units = "in", dpi = 300)
}



# Older:
if(2000 %in% years){the.data <- his.scenarios.data; no_spawners = spawners; nm = "CChis"}
if(2090 %in% years){the.data <- fut.scenarios.data; no_spawners = spawners.f; nm = "CCfut"}

# Subyearling weight histogram 
plot.object <- the.data %>% 
  # filter data to surviving yearlings
  filter(FinalState == "Subyearling") %>%
  # plot weight
  ggplot(aes(x = Weight, color = FinalState, fill = FinalState)) + 
  # add histogram plot
  geom_histogram(alpha = 0.5, aes(y = (..count..)/11/10), binwidth = 0.04) +
  # supply x and y limits
  coord_cartesian(xlim = c(0.3, 5), ylim = c(0, 60)) +
  # set theme
  theme_classic() +
  # remove legend title
  theme(legend.position = "none") +
  # remove bottom axis line and ticks
  theme(axis.line.x.bottom = element_blank(), axis.ticks.x.bottom = element_blank()) +
  # remove subplot label background
  theme(strip.background = element_blank()) +
  # adjust y-axis label position
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  # add axis and title text
  labs(x = "Subyearling mass (g)", y = "Simulated\nsalmon\n(1000s)") +
  # manually set color
  scale_color_manual(values="#808080") +
  # manually set fill
  scale_fill_manual(values="#808080") +
  # add x-axis to each plot
  geom_hline(yintercept = 0)
if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("Subyearling_AllGCMs.", nm, ".png"), plot = last_plot(), 
         width = 4, height = 3, units = "in", dpi = 300)
}
if(2000 %in% years) {assign("sub.size.his", plot.object)}
if(2090 %in% years) {assign("sub.size.fut", plot.object)}

# Yearling weight histogram
plot.object <- the.data %>% 
  # filter data to surviving yearlings
  filter(FinalState == "Yearling") %>%
  # plot weight
  ggplot(aes(x = Weight, color = FinalState, fill = FinalState)) + 
  # add histogram plot
  geom_histogram(alpha = 0.5, aes(y = (..count..)/11/10), binwidth = 0.25) +
  # supply x and y limits
  coord_cartesian(xlim = c(0, 30), ylim = c(0, 5)) +
  # set theme
  theme_classic() +
  # remove legend
  theme(legend.position = "none") +
  # remove bottom axis line and ticks
  theme(axis.line.x.bottom = element_blank(), axis.ticks.x.bottom = element_blank()) +
  # remove subplot label background
  theme(strip.background = element_blank()) +
  # adjust y-axis label position
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  # add axis and title text
  labs(x = "Yearling mass (g)", y = "Simulated\nsalmon\n(1000s)") +
  # manually set color
  scale_color_manual(values="#008000") +
  # manually set fill
  scale_fill_manual(values="#008000") +
  # add x-axis to each plot
  geom_hline(yintercept = 0)
if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("Yearling_AllGCMs.", nm, ".png"), plot = last_plot(), 
         width = 4, height = 3, units = "in", dpi = 300)
}
if(2000 %in% years) {assign("yrl.size.his", plot.object)}
if(2090 %in% years) {assign("yrl.size.fut", plot.object)}



# Subyearling weight histogram Each GCM
the.data %>% 
  # filter data to surviving yearlings
  filter(FinalState == "Subyearling") %>%
  # plot weight
  ggplot(aes(x = Weight, color = FinalState, fill = FinalState)) + 
  # add histogram plot
  geom_histogram(alpha = 0.5, aes(y = (..count..)/11), binwidth = 0.04) +
  # supply xlim()
  xlim(0.3, 1.4) +
  # split plot by life history stage
  facet_wrap( ~ Scenario, nrow = 10, ncol = 1) +
  # set theme
  theme_classic() +
  # remove legend title
  theme(legend.position = "none") +
  # remove bottom axis line and ticks
  theme(axis.line.x.bottom = element_blank(), axis.ticks.x.bottom = element_blank()) +
  # remove subplot label background
  theme(strip.background = element_blank()) +
  # adjust y-axis label position
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  # add axis and title text
  labs(x = "Subyearling mass (g)", y = "Simulated\nsalmon\n(1000s)") +
  # manually set color
  scale_color_manual(values="#808080") +
  # manually set fill
  scale_fill_manual(values="#808080") +
  # add x-axis to each plot
  geom_hline(yintercept = 0)
if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("Subyearling.", nm, ".png"), plot = last_plot(), 
         width = 3, height = 7, units = "in", dpi = 300)
}

# Yearling weight histogram Each GCM
the.data %>% 
  # filter data to surviving yearlings
  filter(FinalState == "Yearling") %>%
  # plot weight
  ggplot(aes(x = Weight, color = FinalState, fill = FinalState)) + 
  # add histogram plot
  geom_histogram(alpha = 0.5, aes(y = (..count..)/11), binwidth = 1) +
  # supply xlim()
  xlim(0, 30) +
  # split plot by life history stage
  facet_wrap( ~ Scenario, nrow = 10, ncol = 1) +
  # set theme
  theme_classic() +
  # remove legend
  theme(legend.position = "none") +
  # remove bottom axis line and ticks
  theme(axis.line.x.bottom = element_blank(), axis.ticks.x.bottom = element_blank()) +
  # remove subplot label background
  theme(strip.background = element_blank()) +
  # adjust y-axis label position
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  # add axis and title text
  labs(x = "Yearling mass (g)", y = "Simulated\nsalmon\n(1000s)") +
  # manually set color
  scale_color_manual(values="#008000") +
  # manually set fill
  scale_fill_manual(values="#008000") +
  # add x-axis to each plot
  geom_hline(yintercept = 0)
if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("Yearling.", nm, ".png"), plot = last_plot(), 
         width = 3, height = 7, units = "in", dpi = 300)
}



# BOXPLOTS of Subyearling Size (variance is across Scenarios)
foo = histsu(the.data$Weight[the.data$FinalState == "Subyearling"], breaks = 300, plot = FALSE)
foo$counts
foo$mids

new.data <- NULL
for(s in unique(the.data$Scenario)){
  new.data <- cbind(new.data, histsu(the.data$Weight[the.data$FinalState == "Subyearling" & the.data$Scenario == s], breaks = foo$breaks, plot = FALSE)$counts)
}
new.data <- cbind(foo$mids, new.data)
new.data <- new.data[1:20,] # cut off the very long tails
colnames(new.data) <- c("Weight", paste0("CC",seq(1,10)))

new.df <- as.data.frame(new.data) %>% pivot_longer(cols = starts_with("CC"))
new.df$Weight.f <- as.factor(new.df$Weight)

# Plot
new.df %>% 
  # plot weight
  ggplot(aes(y = value/11, x = Weight.f)) + 
  # add histogram plot
  geom_boxplot() +
  # set theme
  theme_classic() +
  # remove legend
  theme(legend.position = "none") +
  # remove bottom axis line and ticks
  theme(axis.line.x.bottom = element_blank(), axis.ticks.x.bottom = element_blank()) +
  # remove subplot label background
  theme(strip.background = element_blank()) +
  # adjust y-axis label position
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  # add axis and title text
  labs(x = "Yearling mass (g)", y = "") +
  # manually set x-axis labels
  scale_x_discrete(labels = round(as.numeric(levels(new.df$Weight.f)),2)) +
  # manually set color
  scale_color_manual(values="#008000") +
  # manually set fill
  scale_fill_manual(values="#008000") +
  # add x-axis to each plot
  geom_hline(yintercept = 0)
if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("Subyearling_Boxplot_AllGCMs.", nm, ".png"), plot = last_plot(), 
         width = 6, height = 5, units = "in", dpi = 300)
}


# BOXPLOTS of Yearling Size (variance is across Scenarios)
foo = histsu(the.data$Weight[the.data$FinalState == "Yearling"], breaks = 50, plot = FALSE)
foo$counts
foo$mids

new.data <- NULL
for(s in unique(the.data$Scenario)){
  new.data <- cbind(new.data, histsu(the.data$Weight[the.data$FinalState == "Yearling" & the.data$Scenario == s], breaks = foo$breaks, plot = FALSE)$counts)
}
new.data <- cbind(foo$mids, new.data)
new.data <- new.data[1:20,] # cut off the very long tails
colnames(new.data) <- c("Weight", paste0("CC",seq(1,10)))

new.df <- as.data.frame(new.data) %>% pivot_longer(cols = starts_with("CC"))
new.df$Weight.f <- as.factor(new.df$Weight)

# Plot
new.df %>% 
  # plot weight
  ggplot(aes(y = value/11, x = Weight.f)) + 
  # add histogram plot
  geom_boxplot() +
  # set theme
  theme_classic() +
  # remove legend
  theme(legend.position = "none") +
  # remove bottom axis line and ticks
  theme(axis.line.x.bottom = element_blank(), axis.ticks.x.bottom = element_blank()) +
  # remove subplot label background
  theme(strip.background = element_blank()) +
  # adjust y-axis label position
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  # add axis and title text
  labs(x = "Yearling mass (g)", y = "") +
  # manually set x-axis labels
  scale_x_discrete(labels = round(as.numeric(levels(new.df$Weight.f)),2)) +
  # manually set color
  scale_color_manual(values="#008000") +
  # manually set fill
  scale_fill_manual(values="#008000") +
  # add x-axis to each plot
  geom_hline(yintercept = 0)
if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("Yearling_Boxplot_AllGCMs.", nm, ".png"), plot = last_plot(), 
         width = 6, height = 5, units = "in", dpi = 300)
}


#--- PHENOLOGY -----------------------------------------------------------------

# change year so that day-month is the comparable across scenarios.
phenology.data <- scenarios.data
phenology.data$YearSpawn = year(phenology.data$DateSpawn)
phenology.data$YearEmerge = year(phenology.data$DateEmerge)
phenology.data$YearOutmigrate = year(phenology.data$DateOutmigrate)

for(var in c("Spawn", "Emerge", "Outmigrate")){
  for(yy in c(1995:2005, 2089:2099)){
  #for(yy in years){
    foo = phenology.data[phenology.data[, paste0("Date", var)] >= as.Date(paste0(yy - 1, "-09-01")) & phenology.data[, paste0("Date", var)] < as.Date(paste0(yy, "-08-31")) & !is.na(phenology.data[, paste0("Date", var)]),]
    year(foo[foo[, paste0("Year", var)] == yy - 1, paste0("Date", var)]) <- 1900
    year(foo[foo[, paste0("Year", var)] == yy, paste0("Date", var)]) <- 1901
    phenology.data[phenology.data[, paste0("Date", var)] >= as.Date(paste0(yy - 1, "-09-01")) & phenology.data[, paste0("Date", var)] < as.Date(paste0(yy, "-08-31")) & !is.na(phenology.data[, paste0("Date", var)]),] <- foo
  }
}
yy = 1901
month.min = month(min(phenology.data$DateEmerge, na.rm = T))

# EMERGENCE / OUTMIGRATION TIMING
for(var in c("DateEmerge", "DateOutmigrate")){
  if(var == "DateEmerge"){nm <- "Emergence"; yl <- 125; xlb <- ""; ylb <- "Simulated\nSalmon\nEmerged\n(1000s)"}
  if(var == "DateOutmigrate"){nm <- "Outmigration"; xlb <- "Date"; yl <- 60; ylb <- "Simulated\nSalmon\nOutmigrants\n(1000s)"}
  
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
if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("Phenology_AllGCMs.", nm, ".png"), plot = last_plot(), 
         width = 5, height = 3, units = "in", dpi = 300)
}
assign(paste0(nm, ".plot.object"), plot.object)
}

#Combine plots for manuscript figure
figure <- ggarrange(Emergence.plot.object, Outmigration.plot.object,
                    labels = c("(a)", "(b)"), hjust = -6.5, vjust = 1.8, font.label = list(size = 12, face = "plain"),
                    ncol = 1, nrow = 2, align = "hv")
if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("Figure7_phenology_", riparian.scenario,".png"), plot = last_plot(), 
         width = 6, height = 6, units = "in", dpi = 300)
}


# Older versions:

if(2000 %in% years){the.data <- his.scenarios.data; no_spawners = spawners; nm = "CChis"}
if(2090 %in% years){the.data <- fut.scenarios.data; no_spawners = spawners.f; nm = "CCfut"}
the.data <- scenarios.data

# plot emergence and outmigration for COMBINED SCENARIOS
phenology.data %>% 
  mutate(Emergence = DateEmerge, Outmigration = DateOutmigrate) %>% 
  # filter data to surviving fish
  filter(FinalState == "Subyearling" | FinalState == "Yearling") %>% 
  # select relevant columns
  select(Scenario, Emergence, Outmigration) %>% 
  # combine emergence and outmigration into a single column
  gather(key = "Event", value = "Date", c(Emergence, Outmigration)) %>% 
  # filter data to fish that experienced each event
  filter(!is.na(Date)) %>% 
  # plot event vs. day-month
  ggplot(aes(x = Date, fill = Event, color = Event)) + 
  # add histogram
  geom_histogram(alpha = 0.5, position = "identity", aes(y = (..count..)/11/10), binwidth = 5) +
  # supply x and y limits
  coord_cartesian(ylim = c(0, 120)) +
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
  scale_color_manual(values=c("#00008B", "#FF8C00")) +
  # manually set fill
  scale_fill_manual(values=c("#00008B", "#FF8C00")) +
  # set x-axis labels to day-month
  scale_x_date(limits = c(as.Date(paste0(yy - 1, "-", month.min, "-01")), as.Date(paste0(yy, "-07-15"))), date_labels = "%b", date_breaks = "1 month") +
  # adjust y-axis label text
  labs(y = "Simulated\nsalmon\n(1000s)") +
  # add x-axis to each plot
  geom_hline(yintercept = 0)
if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("Phenology_AllGCMs.", nm, ".png"), plot = last_plot(), 
         width = 5, height = 3, units = "in", dpi = 300)
}


# plot emergence and outmigration across scenarios for each GCM
phenology.data %>% 
  mutate(Emergence = DateEmerge, Outmigration = DateOutmigrate) %>% 
  # filter data to surviving fish
  filter(FinalState == "Subyearling" | FinalState == "Yearling") %>% 
  # select relevant columns
  select(Scenario, Emergence, Outmigration) %>% 
  # combine emergence and outmigration into a single column
  gather(key = "Event", value = "Date", c(Emergence, Outmigration)) %>% 
  # filter data to fish that experienced each event
  filter(!is.na(Date)) %>% 
  # plot event vs. day-month
  ggplot(aes(x = Date, fill = Event, color = Event)) + 
  # add histogram
  geom_histogram(alpha = 0.5, position = "identity", aes(y = (..count..)/11), binwidth = 5) +
  # split plot by scenario
  facet_wrap(~ Scenario, nrow = 10, ncol = 1) +
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
  scale_color_manual(values=c("#00008B", "#FF8C00")) +
  # manually set fill
  scale_fill_manual(values=c("#00008B", "#FF8C00")) +
  # set x-axis labels to day-month
  scale_x_date(limits = c(as.Date(paste0(yy - 1, "-", month.min, "-01")), as.Date(paste0(yy, "-07-01"))), date_labels = "%b", date_breaks = "1 month") +
  # adjust y-axis label text
  labs(y = "Simulated\nsalmon\n(1000s)") +
  # add x-axis to each plot
  geom_hline(yintercept = 0)
if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("Phenology.", nm, ".png"), plot = last_plot(), 
         width = 5, height = 7, units = "in", dpi = 300)
}




#--- MORTALITY ----------------------------------------------------------------

plot.object1 <- scenarios.data %>% 
    # filter data to survivors
    filter(FinalState == "Scoured" | FinalState == "Stochastic") %>%
    # plot weight
    ggplot(aes(x = Weight, color = FinalState, fill = FinalState)) + 
    # add histogram plot
    geom_histogram(alpha = 0.5, aes(y = (..count..)/11/10), binwidth = 0.05) +
    # supply x and y limits
    coord_cartesian(xlim = c(0.25, 1.5)) +
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
    labs(x = "Fish mass (g)", y = "Simulated\nSalmon\nMortalities\n(1000s)") +
    # manually set color
    scale_color_manual(values=c("#426e82", "#611268")) +
    # manually set fill
    scale_fill_manual(values=c("#426e82", "#611268")) +
    # add x-axis to each plot
    geom_hline(yintercept = 0)
  
  if (save.figures) {
    ggsave(path = plot.directory, filename = paste0("Mortality_by_Size", riparian.scenario, ".png"), plot = last_plot(), 
           width = 5, height = 3, units = "in", dpi = 300)
  }



# change year so that day-month is the comparable across scenarios.
phenology.data <- scenarios.data
phenology.data$YearScour = year(phenology.data$DateScour)
phenology.data$YearMort = year(phenology.data$DateMort)

for(var in c("Scour", "Mort")){
  for(yy in c(1995:2005, 2089:2099)){
    foo = phenology.data[phenology.data[, paste0("Date", var)] >= as.Date(paste0(yy - 1, "-09-01")) & phenology.data[, paste0("Date", var)] < as.Date(paste0(yy, "-08-31")) & !is.na(phenology.data[, paste0("Date", var)]),]
    year(foo[foo[, paste0("Year", var)] == yy - 1, paste0("Date", var)]) <- 1900
    year(foo[foo[, paste0("Year", var)] == yy, paste0("Date", var)]) <- 1901
    phenology.data[phenology.data[, paste0("Date", var)] >= as.Date(paste0(yy - 1, "-09-01")) & phenology.data[, paste0("Date", var)] < as.Date(paste0(yy, "-08-31")) & !is.na(phenology.data[, paste0("Date", var)]),] <- foo
  }
}
yy = 1901
month.min = month(min(phenology.data$DateScour, na.rm = T))


  plot.object2 <- phenology.data %>% 
    mutate(Scoured = DateScour, Stochastic = DateMort) %>% 
    # filter data mortalities
    filter(FinalState == "Scoured" | FinalState == "Stochastic") %>% 
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
    #coord_cartesian(ylim = c(0, yl)) +
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
    scale_color_manual(values=c("#426e82", "#611268")) +
    # manually set fill
    scale_fill_manual(values=c("#426e82", "#611268")) +
    # set x-axis labels to day-month
    scale_x_date(limits = c(as.Date(paste0(yy - 1, "-", month.min, "-01")), as.Date(paste0(yy, "-07-15"))), date_labels = "%b", date_breaks = "1 month") +
    # adjust y-axis label text
    labs(y = "Simulated\nSalmon\nMortalities\n(1000s)") +
    # add x-axis to each plot
    geom_hline(yintercept = 0)
  if (save.figures) {
    ggsave(path = plot.directory, filename = paste0("Mortality_by_Date", riparian.scenario, ".png"), plot = last_plot(), 
           width = 5, height = 3, units = "in", dpi = 300)
  }

  #Combine plots for manuscript figure
  figure <- ggarrange(plot.object2, plot.object1,
                      labels = c("(a)", "(b)"), hjust = -6.5, vjust = 1.8, font.label = list(size = 12, face = "plain"),
                      ncol = 1, nrow = 2, align = "hv")
  if (save.figures) {
    ggsave(path = plot.directory, filename = paste0("FigureS3_mortality_", riparian.scenario, ".png"), plot = last_plot(), 
           width = 6, height = 6, units = "in", dpi = 300)
  }
  


