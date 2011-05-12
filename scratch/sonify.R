sonify <- function(data=NA, mapping=sonaes(), rendering=NA, scales=scale()) {
  ## This just puts the items in a list
  s <- list(data, mapping, rendering, scales, NA)
  names(s) <- c("data", "mapping", "rendering", "scales", "layers") #Theres' got to be an easier way to do this
  class(s) <- c("sonify", "list")
  s
}

##Need to generate mappings. Let's start with just pitch and tone.
##Pitch can be default stored in csound's "oct" notation, with
##before decimal being octaves of middle 

##From manual at Csounds.com: the fraction is preceded by a whole number octave index such that 8.00 represents Middle C, 9.00 the C above, etc. Midi note number values range between 0 and 127 (inclusively) with 60 representing Middle C, and are usually whole numbers.

sonaes <- function(pitch=NA, start=NA, dur=NA, vol=NA, timbre=NA) {
  ##Similar to ggplot2 "aes". In fact, this should BE "aes", so we need some sort
  ##of namespace
  
  son <- c(pitch, start, dur, vol, timbre)
  names(son) <- c("pitch", "start", "dur", "vol", "timbre")
  class(son) <- c("sonifyAes", "character")
  son
}

scale <- function(length=10, pitch=NA, start=NA, dur=NA, vol=NA, timbre=NA) {
  sc <- list(length, pitch, start, dur, vol, timbre)
  names(sc) <- c("length", "pitch", "start", "dur", "vol", "timbre")
  class(sc) <- c("sonifyScale", "list")
  sc
}

layer <- function(shape=NA, shape_params=NA, stat=NA, stat_params=NA, data=NA, mapping=NA) {
  l <- list(list(shape, shape_params), list(stat, stat_params), data, mapping)
  names(l) <- c("shape", "stat", "data", "mapping")
  names(l$stat) <- c("stat", "stat_params")
  names(l$shape) <- c("shape", "shape_params")
  class(l) <- c("sonifyLayer", "list")
  l
}

"+.sonify" <- function(x, y) {
  if("sonifyLayer" %in% class(y)) {
    if(is.na(x$layers)) {
      x$layers <- y
    } else {x$layers <- list(x$layers, y)}
  } else {stop("'+' operator not supported for this operation.")}
  x
}

## ##################################################
## BASIC STRUCTURE DESCRIPTION
## ################################################

## The idea here is to create a sonification class, named "sonify". A "sonify" object is a list containing
##     $data: a default data.frame with the original, unmodified data (all columns)
##     $mapping: a named vector(?) containing default name-value pairs. Can be static values or related to data.frame()
##     	      "pitch", duh
## 	      "start", like in Gini
## 	      "dur", like for inequality
## 	      "vol"
## 	      "timbre"
##     $layers: a list with further elements, similar to the overall one
##     	     $data: data for each individual layer, overrides default
## 	     $mapping: same as top-level mapping, but overrides 
## 	     	       ..type, parameters
## 	     $shape
## 			..type, parameters
## 	     $stat
## 			..type, parameters
##     $shape: right now, "notes" for something like musical notes, & the parameters that go with it
##     	    	       "percussion" for a percussive sound
## 		       "noise" for white, etc. noise (csound/R only)
## 		       "portamento" when there's interpolation (csound only)
##     	    something to cover "tempo" plots? Both 
##     $stat: "Benford", some kind of aggregation related to 
##     $scaling: includes presence of guide tones or beats, also function to scale. 
##     	      $length (scaling of start, dur, &c is relative to this.)
## 	      all the same possible values...should store this as a separate value, a "global" variable of possible mappings.
## 	      Each can have slots for range(can be empty for raw output of function) and scaling function (default linear, then recentered on range either truncated or shrunk to it)

##     $engine: For right now one of c("csound","R", "MIDI"), maybe one day SuperCollider, Chuck, whatever.
##     	     Can include sub parameters specific to that method, e.g. for the craziness of the names plot, real-time
## 	     vs. non-real-time, etc. Should leave in possibility to leave blank and choose method dynamically?




  
    

