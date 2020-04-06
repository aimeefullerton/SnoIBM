# Plot range maps
# Need to pre-load SSN, streams and basin shapefiles, and color palette 'cb' and load SSN library

# plot accessible streams
plot(basin,col="gray80",border=NA, main = "Below Falls & Reservoir")
plot(streams, col="darkgray", add = TRUE)
for (i in 1:length(sno.ssn@lines)) {
     for (j in 1:length(sno.ssn@lines[[i]])) {
           lines(sno.ssn@lines[[i]]@Lines[[j]]@coords, col = cb[sno.ssn@data[i,"accessible"]])
       }
}

plot(basin,col="gray80",border=NA, main = "Chinook salmon")
plot(streams, col="darkgray", add = TRUE)
for (i in 1:length(sno.ssn@lines)) {
  for (j in 1:length(sno.ssn@lines[[i]])) {
    lines(sno.ssn@lines[[i]]@Lines[[j]]@coords, col = cb[sno.ssn@data[i,"Chinook_rg"]])
  }
}

plot(basin,col="gray80",border=NA, main = "Largemouth bass")
plot(streams, col="darkgray", add = TRUE)
for (i in 1:length(sno.ssn@lines)) {
  for (j in 1:length(sno.ssn@lines[[i]])) {
    lines(sno.ssn@lines[[i]]@Lines[[j]]@coords, col = cb[sno.ssn@data[i,"LMB_range"]])
  }
}
