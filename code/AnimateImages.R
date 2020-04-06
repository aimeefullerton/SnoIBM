# Animate IBM results images

# Mac Version
  # Requires ffmpeg to be installed already (developer tool)
  ani.path<- paste0(getwd(),"/",outputDir, "/", "images")
  dir(ani.path)
  setwd(ani.path)
  system("ffmpeg -framerate 5 -pattern_type glob -i '*.png' -c:v libxvid -b:v 2400k 1Fish.ani.mp4") #other libraries: libx264, libx265, postproc
  setwd("../../../")
