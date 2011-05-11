sonify <- function(data=NA, mapping=NA, scales=NA, engine=NA) {
  ## This just puts the items in a list
  s <- list(data, mapping, scales)
  names(s) <- c("data", "mapping", "scales")
  class(s) <- c("sonify", "list")
  s
}

##Need to generate mappings. Let's start with just pitch and tone.
##Pitch can be default stored in csound's "oct" notation, with
##before decimal being octaves of middle 

##From manual at Csounds: the fraction is preceded by a whole number octave index such that 8.00 represents Middle C, 9.00 the C above, etc. Midi note number values range between 0 and 127 (inclusively) with 60 representing Middle C, and are usually whole numbers.

sonaes <- function(pitch=NA, start=NA, dur=NA, vol=NA, timbre=NA) {
  son <- c(pitch, start, dur, vol, timbre)
  names(son) <- c("pitch", "start", "dur", "vol", "timbre")
  son
}

package.skeleton(name="playitbyr", path = "./pkg")
