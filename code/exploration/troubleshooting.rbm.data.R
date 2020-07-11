# Checking that RBM flows and temperatures are different among scenarios

cs = 2003

scenario = "current_climate_riparian_0"
Q0.df = read.csv(paste0("data.in/rbm.data/", scenario, "/Q.", cs, ".df"), header = TRUE, stringsAsFactors = FALSE)
WT0.df = read.csv(paste0("data.in/rbm.data/", scenario, "/WT.", cs, ".df"), header = TRUE, stringsAsFactors = FALSE)

scenario = "current_climate_riparian_1"
Q1.df = read.csv(paste0("data.in/rbm.data/", scenario, "/Q.", cs, ".df"), header = TRUE, stringsAsFactors = FALSE)
WT1.df = read.csv(paste0("data.in/rbm.data/", scenario, "/WT.", cs, ".df"), header = TRUE, stringsAsFactors = FALSE)

scenario = "current_climate_riparian_2"
Q2.df = read.csv(paste0("data.in/rbm.data/", scenario, "/Q.", cs, ".df"), header = TRUE, stringsAsFactors = FALSE)
WT2.df = read.csv(paste0("data.in/rbm.data/", scenario, "/WT.", cs, ".df"), header = TRUE, stringsAsFactors = FALSE)

scenario = "current_climate_riparian_3"
Q3.df = read.csv(paste0("data.in/rbm.data/", scenario, "/Q.", cs, ".df"), header = TRUE, stringsAsFactors = FALSE)
WT3.df = read.csv(paste0("data.in/rbm.data/", scenario, "/WT.", cs, ".df"), header = TRUE, stringsAsFactors = FALSE)

plot(Q0.df[,5], type = 'l')
lines(Q1.df[,5], col = 2)
lines(Q2.df[,5], col = 3)
lines(Q3.df[,5], col = 4)

plot(WT0.df[,5], type = 'l')
lines(WT1.df[,5], col = 2)
lines(WT2.df[,5], col = 3)
lines(WT3.df[,5], col = 4)
