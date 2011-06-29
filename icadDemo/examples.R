source("R/aaa.R")
source("R/sonify.R")
source("R/scaling.R")
source("R/Rrender.R")
source("rendermethods/MIDI/MIDI.R")

setMIDIPlayer("timidity")

soundset <- c(1, 25)

## Initialize sonification object
x <- sonify(data=iris, 
      sonaes(time="Petal.Width",
      pitch = "Petal.Length", 
      timbre=25, dur=1, vol=0.3), 
      rendering="MIDI")
## Add on scaling parameters
x <- (x + shape_notes() 
   + scale_time_linear(0, 5) 
   + scale_pitch_linear(6,10))
## Invoking the object causes it to play
x

##################################################
source("R/benford.R")
source("rendermethods/csound/csound.R")

x <- sonify(data=Benford(rivers)
      sonaes(time="cumPerc",
      pitch = "Digit", 
      pan ="Ideal", timbre= 1, dur= "Perc", vol= 0.5), 
      rendering="csound")

x <- (x + shape_notes() 
   + scale_time_linear(0, 10) 
   + scale_pitch_linear(8.01, 8.09)
      + scale_pan_linear(0, 1)
      + scale_dur_linear(0, 3))

x





