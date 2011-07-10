source("aaa.R")
source("sonify.R")
source("sonscaling.R")
source("Rrender.R")
require(audio)

x <- sonify(data=iris, 
      sonaes(time="Petal.Width",
      pitch = "Petal.Length", 
      timbre=1, dur=5, vol=0.75))

x <- (x + scale_time_linear(0, 3))
   + scale_pitch_linear(3, 13))

x
