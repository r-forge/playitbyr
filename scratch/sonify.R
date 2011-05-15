sonify <- function(data=NULL, mapping=sonaes(pitch=8, tempo=120, dur=1, vol=0.5, timbre=1), rendering="midi", scales=scaling()) {
  ## This just puts the items in a list
  
  s <- list(data, mapping, rendering, scales, NULL)
  names(s) <- c("data", "mapping", "rendering", "scales", "layers") #Theres' got to be an easier way to do this
  class(s) <- c(rendering, "sonify", "list")
  s
}

##Need to generate mappings. Let's tempo with just pitch and tone.
##Pitch can be default stored in csound's "oct" notation, with
##before decimal being octaves of middle 

##From manual at Csounds.com: the fraction is preceded by a whole number octave index such that 8.00 represents Middle C, 9.00 the C above, etc. Midi note number values range between 0 and 127 (inclusively) with 60 representing Middle C, and are usually whole numbers.

sonaes <- function(pitch=NULL, tempo=NULL, dur=NULL, vol=NULL, timbre=NULL) {
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
      if(!is.null(y[[i]])) x$scales[[i]] <- y[[i]]
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

df.notes <- function(s) {
  ## s is a "sonify" object containing all notes layers
  ## This function renders the "notes" shape

  if(is.null(s$layers)) stop("Cannot render sound without any layers.")

  notes <- do.call(rbind, lapply(1:length(s$layers), function(x) getNotes(s, x)))
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
  if(layernum > length(x$layers)) stop(paste("There is no layer", layernum))

  if(!is.null(x$layers[[layernum]]$data)){
    return(x$layers[[layernum]]$data)} else {return(x$data)}
} 


getNotes <- function(x, layernum) {
  ## Create the notes for the given layer into Csound score style format
  n <- nrow(x$data) # for notes shape, every row of df is a note
  layer <- rep(layernum, n)
  curnotes <- data.frame(layer)
  
  map <- getMappings(x, layernum)
  data <- getData(x, layernum)

  for(j in names(map)) {
    if(map[[j]] %in% names(data)) {

      ##TODO: some sort of check here before simply assigning variables
      curnotes[[j]] <- x$scales[[j]]$scaling.function(data[[ (map[[j]]) ]], x$scales[[j]]$min, x$scales[[j]]$max)
    } else {curnotes[[j]] <- map[[j]]}
  }
  curnotes

  ## convert tempo data into start times and scale durations in relation to beat
  beatlength <- 60/curnotes$tempo
  curnotes$start <- c(0, cumsum(beatlength[-n]))
  curnotes$dur <- curnotes$dur*beatlength
  curnotes$layer <- factor(curnotes$layer)
  curnotes[,c("layer", "start", "dur", "pitch", "vol", "timbre")]
}
