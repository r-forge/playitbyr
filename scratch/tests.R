source("sonify.R")
source("scaling.R")
source("Rrender.R")

x <- sonify(data=iris, 
      sonaes(time="Petal.Width",
      pitch = "Petal.Length", 
      timbre=1, dur=5, vol=0.75), 
      rendering="audio")
x <- (x + shape_notes() 
   + scale_time_linear(0, 3) 
   + scale_pitch_linear(3, 13))
x
