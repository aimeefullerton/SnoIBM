# Generate lists (stored as .RData) of all segments upstream of a given segment, downstream of a given segment, and all segments participating in a confluence
# Pre-calculating this makes the model run faster because they can be looked up instead of caluclated on the fly

if(!exists("ssn.folder")) ssn.folder <- "sno.rbm.ssn"

fncDnSegs(ssn, path = paste0("data.in/", ssn.folder))

fncUpSegs(ssn, path = paste0("data.in/", ssn.folder))

fncJctLst(ssn,path = paste0("data.in/", ssn.folder))

if(!exists("basin") | !exists("streams")) plotit <- F else plotit <- T
if(plotit == T){
  plot(basin, col = "gray80", border = NA); plot(streams, col = "darkgray", add = TRUE)
  seg <- 53
  for(x in dnsegs[[seg]]) fncHighlightSeg(x, col = "yellow")
  for(x in upsegs[[seg]]) fncHighlightSeg(x)
  fncHighlightSeg(seg, col = 2)
}