# Load data
WT.df = fncImportWT(paste0(loadDir, "/rbm.data/Tw_output.csv")) #temperature 2001-2013

# Plot all years of water temperature data from DHSVM-RBM
yy = 2003; i = 1
idx.date = WT.df$Date >= as.Date("2003-01-01") & WT.df$Date <= as.Date("2003-12-31")
idx.tmp = WT.df$Date >= as.Date(paste0(yy,"-01-01")) & WT.df$Date <= as.Date(paste0(yy,"-12-31"))
par(las=1)
plot(WT.df$X1[idx.tmp], type = 'l', xlab = "Date", ylab = expression("Temperature "~degree(C)), axes = F)
axis(2)
axis(1, at = seq(1,2920,length.out = 13), labels = c("J","F","M","A","M","J","J","A","S","O","N","D", "J"))
for(yy in 2004:2013){
  i = i + 1
  idx.tmp = WT.df$Date >= as.Date(paste0(yy,"-01-01")) & WT.df$Date <= as.Date(paste0(yy,"-12-31"))
  lines(WT.df$X1[idx.tmp], col = i)
}
for(h in c(0,5,10,15,20)) abline(h=h,lty=2)

# Emphasize one year
yy = 2011
idx.tmp = WT.df$Date >= as.Date(paste0(yy,"-01-01")) & WT.df$Date <= as.Date(paste0(yy,"-12-31"))
lines(WT.df$X1[idx.tmp], col = 1, lwd = 3)
