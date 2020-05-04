
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
  if (!dir.exists(plot.directory)) {
    dir.create(plot.directory)
  }
}

# load data froom scenarios to be compared
#scenario.list = c("current_climate_riparian_0", "current_climate_riparian_1", "current_climate_riparian_2", "current_climate_riparian_3")
scenario.list = c("bcc-csm1-1-m", "CanESM2", "CCSM4", "CNRM-CM5")
iter = 1

for(scenario in scenario.list){
  scenario.data = NULL
  #years = 2003:2013
  years = 1995:2005
  #years = 2089:2099
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
all.scenarios.data = NULL
for(scenario in scenario.list){
  sd = get(paste0(scenario,".data"))
  all.scenarios.data = rbind(all.scenarios.data, sd)
  rm(sd)
}

# prep data for ggplot2
all.scenarios.data <- all.scenarios.data %>% 
  select(scenario, survive, weight, dateSp, dateEm, dateOm) %>% 
  transmute(Scenario = as.factor(scenario),
            FinalState = as.factor(survive),
            Weight = as.numeric(weight),
            DateSpawn = date(as_datetime(dateSp, origin = "1970-01-01")),
            DateEmerge = date(as_datetime(dateEm, origin = "1970-01-01")),
            DateOutmigrate = date(as_datetime(dateOm, origin = "1970-01-01")))
levels(all.scenarios.data$FinalState) <- c("Scoured", "Stochastic", "Yearling", "Subyearling")

#--- FINAL STATE ---------------------------------------------------------------

# Final state barplot
all.scenarios.data %>% 
  # filter data to surviving fish
  filter(FinalState == "Subyearling" | FinalState == "Yearling") %>%
  # plot final state vs. scenario
  ggplot(aes(x = Scenario, color = FinalState, fill = FinalState)) + 
  # add barplot
  #geom_bar(alpha = 0.5) +
  #geom_bar(aes(y = (..count..)/sum(..count..), alpha = 0.5)) +
  geom_bar(alpha = 0.5, aes(y = (..count..)/length(years))) +
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
  ggsave(path = plot.directory, filename = paste0("FinalState.multiyears.png"), plot = last_plot(), 
         width = 5.5, height = 4, units = "in", dpi = 300)  
}

#--- FINAL MASS ----------------------------------------------------------------

# Subyearling weight histogram
all.scenarios.data %>% 
  # filter data to surviving yearlings
  filter(FinalState == "Subyearling") %>%
  # plot weight
  ggplot(aes(x = Weight, color = FinalState, fill = FinalState)) + 
  # add histogram plot
  geom_histogram(alpha = 0.5, aes(y = (..count..)/length(years))) +
  # split plot by life history stage
  facet_wrap( ~ Scenario, nrow = 4, ncol = 1) +
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
  ggsave(path = plot.directory, filename = paste0("Subyearling.multiyears.png"), plot = last_plot(), 
         width = 4, height = 6, units = "in", dpi = 300)
}

# Yearling weight histogram
all.scenarios.data %>% 
  # filter data to surviving yearlings
  filter(FinalState == "Yearling") %>%
  # plot weight
  ggplot(aes(x = Weight, color = FinalState, fill = FinalState)) + 
  # add histogram plot
  geom_histogram(alpha = 0.5, aes(y = (..count..)/length(years))) +
  # split plot by life history stage
  facet_wrap( ~ Scenario, nrow = 4, ncol = 1) +
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
  ggsave(path = plot.directory, filename = paste0("Yearling.multiyears.png"), plot = last_plot(), 
         width = 4, height = 6, units = "in", dpi = 300)
}


#--- PHENOLOGY -----------------------------------------------------------------

# change year so that day-month is the comparable across scenarios.
phenology.data <- all.scenarios.data
phenology.data$YearSpawn = year(phenology.data$DateSpawn)
phenology.data$YearEmerge = year(phenology.data$DateEmerge)
phenology.data$YearOutmigrate = year(phenology.data$DateOutmigrate)

for(yy in years){
  for(var in c("Spawn", "Emerge", "Outmigrate")){
    year(phenology.data[phenology.data[, paste0("Year",var)] == yy & !is.na(phenology.data[, paste0("Year",var)]), paste0("Date",var)]) <-
      year(phenology.data[phenology.data[, paste0("Year",var)] == yy & !is.na(phenology.data[, paste0("Year",var)]), paste0("Date",var)]) - (yy - years[1])
  }
}
phenology.data <- phenology.data[, 1:(ncol(phenology.data) - 3)]

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
  geom_histogram(alpha = 0.5, position = "identity", aes(y = (..count..)/length(years))) +
  # split plot by scenario
  facet_wrap(~ Scenario, nrow = 4, ncol = 1) +
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
  scale_x_date(date_labels = "%d-%b") +
  # adjust y-axis label text
  labs(y = "Simulated\nsalmon\n(count)") +
  # add x-axis to each plot
  geom_hline(yintercept = 0)
if (save.figures) {
  ggsave(path = plot.directory, filename = paste0("Phenology.multiyears.png"), plot = last_plot(), 
         width = 6.5, height = 5.5, units = "in", dpi = 300)
}

