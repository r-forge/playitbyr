sonify <- function(data=NULL, mapping=sonaes(), rendering=NULL, scales=scaling(total.length=10, pitch=8, tempo=NULL, dur=NULL, vol=0.75, timbre=13, stretch.to.length = TRUE)) {
  ## This just puts the items in a list
  ##TODO: check if dataset names clash with set aesthetics
  
  s <- list(data, mapping, rendering, scales, NULL)
  names(s) <- c("data", "mapping", "rendering", "scales", "layers") #Theres' got to be an easier way to do this
  class(s) <- c(rendering, "sonify", "list")
  s
}

##Need to generate mappings. Let's tempo with just pitch and tone.
##Pitch can be default stored in csound's "oct" notation, with
##before decimal being octaves of middle 

##From manual at Csounds.com: the fraction is preceded by a whole number octave index such that 8.00 represents Middle C, 9.00 the C above, etc. Midi note number values range between 0 and 127 (inclusively) with 60 representing Middle C, and are usually whole numbers.

sonaes <- function(pitch=60, tempo=120, dur=1, vol=0.5, timbre=NULL) {
  ##Similar to ggplot2 "aes". In fact, this should BE "aes", so we need some sort
  ##of namespace


  
  son <- list(pitch, tempo, dur, vol, timbre)
  names(son) <- c("pitch", "tempo", "dur", "vol", "timbre")
  class(son) <- c("sonifyAes", "character")
  son
}


layer <- function(shape="notes", shape_params=NULL, stat=NULL, stat_params=NULL, data=NULL, mapping=NULL) {
  l <- list(list(shape, shape_params), list(stat, stat_params), data, mapping)
  names(l) <- c("shape", "stat", "data", "mapping")
  names(l$stat) <- c("stat", "stat_params")
  names(l$shape) <- c("shape", "shape_params")
  class(l) <- c("sonifyLayer", "list")
  l
}

"+.sonify" <- function(x, y) {
  if("sonifyLayer" %in% class(y)) {
    ## adds layer
    if(is.null(x$layers)) {
      x$layers[[1]] <- y
    } else {x$layers <- c(x$layers, list(y))}
  } else if("sonifyScale" %in% class(y)) {
    ## adds to or overrides scale
    for(i in names(x$scales)) {
      if(is.null(y[[i]])) x$scales[[i]] <- y[[i]]
    }
  } else {stop("'+' operator not supported for this operation.")}
  x
}          

render <- function(x) UseMethod("render")

print.sonify <- function(x) {
  ## This currently ONLY works for "MIDI" and for JUST ONE layer!
  render(x)
}



"%+%" <- function(x, y) {
  ##Another possible ggplot2 confusion/conflict
  ##Replace data.frame in x (a sonify object)
  ##with y (data.frame)
  ##If mappings are already set up, the input data.frame should have the
  ##same names, which we explicitly should check with sonthing like:
  ##  if(!all(x$mapping[!is.null(x$mapping)] %in% names(y)))
  ##    stop("New data.frame does not match mapping in sonify object")
  ## THe preceding is not quite correct since it doesn't deal with the fact that some
  ## mappings are set
  x$data <- y
  x
}

df.notes <- function(x) {
  ## x is a "sonify" object containing all notes layers
  ## This function renders the "notes" shape

  if(is.null(x$layers)) stop("Cannot render sound without any layers.")
  
  allnotes <- list()

  for(i in 1:length(x$layers)) {
    n <- nrow(x$data) # for notes shape, every row of df is a note
    layernum <- rep(i, n)
    curnotes <- data.frame(pitch)
    
    map <- getMappings(x, i)
    ## run through entire procedure for each layer
    for(j in names(map)) {
      if(map[[j]] %in% names(x$data)) {
        notes[[j]] <- x$scales[[i]]$scaling.function(x$data[[j]], x$scales[[j]]$min, x$scales[[j]]$max)
      } else {notes[[j]] <- bleh}
      ## 2. generate values for variable
      ##    a. If set, generated that value
      ##    b. If mapped, apply scaling function to data.frame
      ## 3. 
    }
  }
  if(!x$mapping$pitch %in% names(x$data))
    stop("'", paste(x$mapping$pitch, "' is not in given data.frame.", sep=""))
  notes <- data.frame(pitch = x$data[x$mapping$pitch])
  names(notes) <- "pitch"
  ## thought...could probably do a clever lapply or somesuch to iterate the following procedure
  ## over all the parameters
  notes$pitch <- x$scales$pitch$scaling.function(notes$pitch, x$scales$pitch$min, x$scales$pitch$max)
  n <- length(notes$pitch)
  
  ## these are only currently set up to be static
  notes$tempo <- (0:(n-1))*x$scales$total.length/n
  notes$dur <- notes$tempo[2]/2 ## need to find a more intelligent way of thinking about duration
  notes$vol <- x$scales$vol
  notes$timbre <- x$scales$timbre
  notes
}

getMappings <- function(x, layernum) {
  ## x: a sonify object, returns the current mappings as a named list
  ## 1. assign mapping based on layer, and on default if layer mapping not present
  if(layernum > length(x$layers)) stop(paste("There is no layer", layernum))

  layermap <- x$layers[[layernum]]$mapping
  if(!is.null(layermap)) {
    for(i in names(layermap)) {
      if(!is.null(layermap[[i]]))
        x$mapping[[i]] <- layermap[[i]]
    }
  }
  return(x$mapping)
}

getData <- function(x, layernum) {
  ## x: a sonify object, returns the current data as a data.frame

  
  
}
  

## ##################################################
## BASIC STRUCTURE DESCRIPTION
## ################################################

## The idea here is to create a sonification class, named "sonify". A "sonify" object is a list containing
##     $data: a default data.frame with the original, unmodified data (all columns)
##     $mapping: a named vector(?) containing default name-value pairs. Can be static values or related to data.frame()
##     	      "pitch", duh
## 	      "tempo", like in Gini
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
##     	      $length (scaling of tempo, dur, &c is relative to this.)
## 	      all the same possible values...should store this as a separate value, a "global" variable of possible mappings.
## 	      Each can have slots for range(can be empty for raw output of function) and scaling function (default linear, then recentered on range either truncated or shrunk to it)

##     $engine: For right now one of c("csound","R", "MIDI"), maybe one day SuperCollider, Chuck, whatever.
##     	     Can include sub parameters specific to that method, e.g. for the craziness of the names plot, real-time
## 	     vs. non-real-time, etc. Should leave in possibility to leave blank and choose method dynamically?







