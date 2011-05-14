sonify <- function(data=NA, mapping=sonaes(), rendering=NA, scales=scaling()) {
  ## This just puts the items in a list
  s <- list(data, mapping, rendering, scales, NA)
  names(s) <- c("data", "mapping", "rendering", "scales", "layers") #Theres' got to be an easier way to do this
  class(s) <- c(rendering, "sonify", "list")
  s
}

##Need to generate mappings. Let's start with just pitch and tone.
##Pitch can be default stored in csound's "oct" notation, with
##before decimal being octaves of middle 

##From manual at Csounds.com: the fraction is preceded by a whole number octave index such that 8.00 represents Middle C, 9.00 the C above, etc. Midi note number values range between 0 and 127 (inclusively) with 60 representing Middle C, and are usually whole numbers.

sonaes <- function(pitch=NA, start=NA, dur=NA, vol=NA, timbre=NA) {
  ##Similar to ggplot2 "aes". In fact, this should BE "aes", so we need some sort
  ##of namespace
  
  son <- list(pitch, start, dur, vol, timbre)
  names(son) <- c("pitch", "start", "dur", "vol", "timbre")
  class(son) <- c("sonifyAes", "character")
  son
}

scaling <- function(total.length=10, pitch=8, start=NA, dur=NA, vol=0.75, timbre=13, stretch.to.length = TRUE) {
  ##The sonifyScale for a sound parameter is a list with four elements: min, max, polarity, and function
  ##
  ##pitch: specified in csound oct notation, with 8.00 as middle C
  ##start: specified in proportional relation to total length=1 then multiplied, by default
  ##dur: also specified in proportion to total length=1...some room for improvement here
  ##vol: specified in relation to loudest sound = 1
  ##timbre: this argument is rendering-specific; there are different ranges of timbre available for
  ##        different renderings. For MIDI notes, just the general MIDI specification
  ##stretch.to.length: ignore "length" argument and treat start, dur as seconds values
  
  if(!stretch.to.length) total.length <- start$dur
  sc <- list(total.length, pitch, start, dur, vol, timbre)
  sc <- lapply(sc, function(x) {
               if(length(x) == 3)
                 names(x) <- c("min", "max", "scaling.function")
               return(x)})
  names(sc) <- c("total.length", "pitch", "start", "dur", "vol", "timbre")
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
    if(all(is.na(x$layers))) {
      x$layers <- y
    } else {x$layers <- list(x$layers, y)}
  } else {stop("'+' operator not supported for this operation.")}
  x
}

print.sonify <- function(x) {
  ## This currently ONLY works for "MIDI" and for JUST ONE layer!
  x <- addTrack(midi(), track(df.notes(x)))
  render.midi(x)
}

linear.scale <- function(x, min, max) {
  ## Linearly rescales vector x so that "lower" is the minimum
  ## and "upper" the maximum
  
  nrange <- max-min
  out <- ((x-min(x))*nrange/(max(x)-min(x)) + nrange/2)
  out
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




  
    

