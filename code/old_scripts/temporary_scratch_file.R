# scrapped due to incoming data being formatted correctly.

# @@@-WIP: TEMPORARY! Because DHSVM-RBM output are water years but Chinook start spawning 1 September,
# move September from end to beginning of year (and deal with gap btw 9/30 and 10/2)
Q2.df = Q.df
Q2.df = Q2.df[-which(Q2.df$Date == as.Date("2008-09-30")),]
add.rows1 = Q2.df[Q2.df$Date == as.Date("2008-09-29"),]; add.rows1$Date = as.Date("2007-09-30") #filling in missing data
add.rows2 = Q2.df[Q2.df$Date == as.Date("2008-09-29"),]; add.rows2$Date = as.Date("2007-10-01") #ditto
add.rows = rbind(add.rows1, add.rows2)
idx = which(Q2.df[,"Date"] >= as.Date("2008-09-01"))
Q2.df = rbind(Q2.df[idx,],add.rows,Q2.df[-idx,])
Q2.df$Date[1:length(idx)] = Q2.df$Date[1:length(idx)] - 366
#unique(Q2.df$Date)

#same for WT.df
WT2.df = WT.df
WT2.df = WT2.df[-which(WT2.df$Date == as.Date("2008-09-30")),]
add.rows1 = WT2.df[WT2.df$Date == as.Date("2008-09-29"),]; add.rows1$Date = as.Date("2007-09-30")
add.rows2 = WT2.df[WT2.df$Date == as.Date("2008-09-29"),]; add.rows2$Date = as.Date("2007-10-01")
add.rows = rbind(add.rows1, add.rows2)
idx = which(WT2.df[,"Date"] >= as.Date("2008-09-01"))
WT2.df = rbind(WT2.df[idx,],add.rows,WT2.df[-idx,])
WT2.df$Date[1:length(idx)] = WT2.df$Date[1:length(idx)] - 366

WT.df = WT2.df; Q.df = Q2.df; rm(WT2.df, Q2.df)
