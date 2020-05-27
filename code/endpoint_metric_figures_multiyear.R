
#----- Prepare -----------------------------------------------------------------

# load packages
library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)

# would you like to save these figures?
save.figures <- TRUE
if (save.figures) {
  # specify directory for plot output
  plot.directory <- paste0(getwd(), "/data.out/Figures")
  if (!dir.exists(plot.directory)) {dir.create(plot.directory)}
}

# load data froom scenarios to be compared
#scenario.list = c("current_climate_riparian_0", "current_climate_riparian_1", "current_climate_riparian_2", "current_climate_riparian_3")
  #years = 2003:2013
scenario.list = c("bcc-csm1-1-m", "CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "HadGEM2-CC365", "HadGEM2-ES365", "IPSL-CM5A-MR", "MIROC5", "NorESM1-M")
iter = 1

# Gather historical climate scenario data
for(scenario in scenario.list){
  scenario.data = NULL
  years = 1995:2005
  for(yy in years){
    outdir <- paste0(scenario, ".A.", yy)
    load(file = paste0("data.out/", outdir, "/salmon.finalstep.", yy, ".", iter, ".RData"))
    scenario.data = rbind(scenario.data, salmon.finalstep)
  }
  scenario.data = as.data.frame(scenario.data)
  scenario.data = cbind(scenario = scenario, scenario.data)
  assign(paste0(scenario, ".data"), scenario.data); rm(scenario.data)
}

# combine four scenarios into one dataframe
his.scenarios.data = NULL
for(scenario in scenario.list){
  sd = get(paste0(scenario,".data"))
  his.scenarios.data = rbind(his.scenarios.data, sd)
  rm(sd)
}

#Get number of spawners and survivors across scenarios
#spawners <- tapply(his.scenarios.data$pid, list(his.scenarios.data$scenario), length) #Spawners
#srv.sy <- tapply(his.scenarios.data$pid[his.scenarios.data$survive == 2], list(his.scenarios.data$scenario[his.scenarios.data$survive == 2]), length) #Subyearlings
#srv.yl <- tapply(his.scenarios.data$pid[his.scenarios.data$survive == 1], list(his.scenarios.data$scenario[his.scenarios.data$survive == 1]), length) #Yearlings
#quantile(srv.sy/spawners)
#quantile(srv.yl/spawners)

# prep data for ggplot2
his.scenarios.data <- his.scenarios.data %>% 
  select(scenario, survive, weight, dateSp, dateEm, dateOm) %>% 
  transmute(Scenario = as.factor(scenario),
            FinalState = as.factor(survive),
            Weight = as.numeric(weight),
            DateSpawn = date(as_datetime(dateSp, origin = "1970-01-01")),
            DateEmerge = date(as_datetime(dateEm, origin = "1970-01-01")),
            DateOutmigrate = date(as_datetime(dateOm, origin = "1970-01-01")))
levels(his.scenarios.data$FinalState) <- c("Scoured", "Stochastic", "Yearling", "Subyearling")
his.scenarios.data$YearSpawn = year(his.scenarios.data$DateSpawn)

#Get numbers of spawners, yearlings, & subyearlings across scenarios
spawners.by.scenario_year <- table(his.scenarios.data$YearSpawn, his.scenarios.data$Scenario)
spawners <- apply(spawners.by.scenario_year, 2, sum)
juveniles.by.scenario <- round(table(his.scenarios.data$FinalState, his.scenarios.data$Scenario)[3:4,]/ spawners.f * mean(spawners.f) / 11)

# Gather future climate scenario data
for(scenario in scenario.list){
  scenario.data = NULL
  years = 2089:2099
  for(yy in years){
    outdir <- paste0(scenario, ".A.", yy)
    load(file = paste0("data.out/", outdir, "/salmon.finalstep.", yy, ".", iter, ".RData"))
    scenario.data = rbind(scenario.data, salmon.finalstep)
  }
  scenario.data = as.data.frame(scenario.data)
  scenario.data = cbind(scenario = scenario, scenario.data)
  assign(paste0(scenario, ".data"), scenario.data); rm(scenario.data)
}

# combine four scenarios into one dataframe
fut.scenarios.data = NULL
for(scenario in scenario.list){
  sd = get(paste0(scenario,".data"))
  fut.scenarios.data = rbind(fut.scenarios.data, sd)
  rm(sd)
}

# prep data for ggplot2
fut.scenarios.data <- fut.scenarios.data %>% 
  select(scenario, survive, weight, dateSp, dateEm, dateOm) %>% 
  transmute(Scenario = as.factor(scenario),
            FinalState = as.factor(survive),
            Weight = as.numeric(weight),
            DateSpawn = date(as_datetime(dateSp, origin = "1970-01-01")),
            DateEmerge = date(as_datetime(dateEm, origin = "1970-01-01")),
            DateOutmigrate = date(as_datetime(dateOm, origin = "1970-01-01")))
levels(fut.scenarios.data$FinalState) <- c("Scoured", "Stochastic", "Yearling", "Subyearling")
fut.scenarios.data$YearSpawn = year(fut.scenarios.data$DateSpawn)

#Get numbers of spawners, yearlings, & subyearlings across scenarios
spawners.by.scenario_year.f <- table(fut.scenarios.data$YearSpawn, fut.scenarios.data$Scenario)
spawners.f <- apply(spawners.by.scenario_year.f, 2, sum)
juveniles.by.scenario.f <- round(table(fut.scenarios.data$FinalState, fut.scenarios.data$Scenario)[3:4,] / spawners.f * mean(spawners.f) / 11)

apply(juveniles.by.scenario.f - juveniles.by.scenario, 1, mean)




years = 1995:2005 #years = 2089:2099

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
  #geom_bar(aes(y = (..count..)/length(years)), alpha = 0.5) +
  geom_bar(aes(y = (..count..)/c(no_spawners, no_spawners)*mean(no_spawners)/length(years)), alpha = 0.5) +
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
  labs(y = "Simulated\nsalmon\n(count)")
if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("FinalState.", nm, ".png"), plot = last_plot(), 
         width = 5.5, height = 4, units = "in", dpi = 300)  
}

# Final state barplot (future minus historical)
the.data <- as.data.frame(juveniles.by.scenario.f - juveniles.by.scenario)
colnames(the.data) <- c("FinalState", "Scenario", "Value")

the.data %>% 
  # plot final state vs. scenario
  ggplot(aes(x = Scenario, color = FinalState, fill = FinalState)) + 
  # add barplot
  geom_col(aes(y = Value), alpha = 0.5) +
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
  labs(y = "Simulated\nsalmon\n(count)")
if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("FinalStateDiff.", nm, ".png"), plot = last_plot(), 
         width = 5.5, height = 4, units = "in", dpi = 300)  
}

#--- FINAL MASS ----------------------------------------------------------------
if(2000 %in% years){the.data <- his.scenarios.data; no_spawners = spawners; nm = "CChis"}
if(2090 %in% years){the.data <- fut.scenarios.data; no_spawners = spawners.f; nm = "CCfut"}

# Subyearling weight histogram
the.data %>% 
  # filter data to surviving yearlings
  filter(FinalState == "Subyearling") %>%
  # plot weight
  ggplot(aes(x = Weight, color = FinalState, fill = FinalState)) + 
  # add histogram plot
  geom_histogram(alpha = 0.5, aes(y = (..count..)/length(years)), binwidth = 0.04) +
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
  labs(x = "Subyearling mass (g)", y = "Simulated\nsalmon\n(count)") +
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

# Yearling weight histogram
the.data %>% 
  # filter data to surviving yearlings
  filter(FinalState == "Yearling") %>%
  # plot weight
  ggplot(aes(x = Weight, color = FinalState, fill = FinalState)) + 
  # add histogram plot
  geom_histogram(alpha = 0.5, aes(y = (..count..)/length(years)), binwidth = 1) +
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
  labs(x = "Yearling mass (g)", y = "Simulated\nsalmon\n(count)") +
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


# Subyearling weight histogram COMBINED SCENARIOS
the.data %>% 
  # filter data to surviving yearlings
  filter(FinalState == "Subyearling") %>%
  # plot weight
  ggplot(aes(x = Weight, color = FinalState, fill = FinalState)) + 
  # add histogram plot
  geom_histogram(alpha = 0.5, aes(y = (..count..)/length(years)/10), binwidth = 0.04) +
  # supply x and y limits
  coord_cartesian(xlim = c(0.3, 1.4), ylim = c(0, 110)) +
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
  labs(x = "Subyearling mass (g)", y = "Simulated\nsalmon\n(count)") +
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

# Yearling weight histogram COMBINED SCENARIOS
the.data %>% 
  # filter data to surviving yearlings
  filter(FinalState == "Yearling") %>%
  # plot weight
  ggplot(aes(x = Weight, color = FinalState, fill = FinalState)) + 
  # add histogram plot
  geom_histogram(alpha = 0.5, aes(y = (..count..)/length(years)/10), binwidth = 1) +
  # supply x and y limits
  coord_cartesian(xlim = c(0, 30), ylim = c(0, 20)) +
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
  labs(x = "Yearling mass (g)", y = "Simulated\nsalmon\n(count)") +
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
if(2000 %in% years){the.data <- his.scenarios.data; no_spawners = spawners; nm = "CChis"}
if(2090 %in% years){the.data <- fut.scenarios.data; no_spawners = spawners.f; nm = "CCfut"}

# change year so that day-month is the comparable across scenarios.
phenology.data <- the.data
phenology.data$YearSpawn = year(phenology.data$DateSpawn)
phenology.data$YearEmerge = year(phenology.data$DateEmerge)
phenology.data$YearOutmigrate = year(phenology.data$DateOutmigrate)

for(var in c("Spawn", "Emerge", "Outmigrate")){
  for(yy in years){
    foo = phenology.data[phenology.data[, paste0("Date", var)] >= as.Date(paste0(yy - 1, "-09-01")) & phenology.data[, paste0("Date", var)] < as.Date(paste0(yy, "-08-31")) & !is.na(phenology.data[, paste0("Date", var)]),]
    year(foo[foo[, paste0("Year", var)] == yy - 1, paste0("Date", var)]) <- 1900
    year(foo[foo[, paste0("Year", var)] == yy, paste0("Date", var)]) <- 1901
    phenology.data[phenology.data[, paste0("Date", var)] >= as.Date(paste0(yy - 1, "-09-01")) & phenology.data[, paste0("Date", var)] < as.Date(paste0(yy, "-08-31")) & !is.na(phenology.data[, paste0("Date", var)]),] <- foo
  }
}
phenology.data <- phenology.data[, 1:(ncol(phenology.data) - 3)]
yy = 1901
month.min = month(min(phenology.data$DateEmerge, na.rm = T))

# plot emergence and outmigration across scenarios
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
  geom_histogram(alpha = 0.5, position = "identity", aes(y = (..count..)/length(years)), binwidth = 5) +
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
  labs(y = "Simulated\nsalmon\n(count)") +
  # add x-axis to each plot
  geom_hline(yintercept = 0)
if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("Phenology.", nm, ".png"), plot = last_plot(), 
         width = 5, height = 7, units = "in", dpi = 300)
}

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
  geom_histogram(alpha = 0.5, position = "identity", aes(y = (..count..)/length(years)/10), binwidth = 5) +
  # supply x and y limits
  coord_cartesian(ylim = c(0, 90)) +
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
  labs(y = "Simulated\nsalmon\n(count)") +
  # add x-axis to each plot
  geom_hline(yintercept = 0)
if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("Phenology_AllGCMs.", nm, ".png"), plot = last_plot(), 
         width = 5, height = 3, units = "in", dpi = 300)
}

