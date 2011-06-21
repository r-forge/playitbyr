source("R/aaa.R")
source("R/sonify.R")
source("R/scaling.R")
source("R/Rrender.R")
source("rendermethods/MIDI/MIDI.R")

setMIDIPlayer("timidity")


x <- sonify(data=iris, 
      sonaes(time="Petal.Width",
      pitch = "Petal.Length", 
      timbre=1, dur=1, vol=0.3), 
      rendering="MIDI")

x <- (x + shape_notes() 
   + scale_time_linear(0, 3) 
   + scale_pitch_linear(6,10))

x
