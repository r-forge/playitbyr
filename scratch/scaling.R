scaling <- function(pitch = NULL, tempo = NULL, dur = NULL, vol = NULL, timbre = NULL, total.length = NULL) {
  ##
  ##The sonifyScale for a sound parameter is a list with three elements: min, max, and function
  ##
  ##pitch: specified in csound oct notation, with 8.00 as middle C
  ##tempo: specified in proportional relation to total length=1 then multiplied, by default
  ##dur: also specified in proportion to total length=1...some room for improvement here
  ##vol: specified in relation to loudest sound = 1
  ##timbre: this argument is rendering-specific; there are different ranges of timbre available for
  ##        different renderings. For MIDI notes, just the general MIDI specification

  ##TODO: add check for temp vs. total.length
  if(!stretch.to.length) total.length <- tempo$dur
  sc <- list(total.length, pitch, tempo, dur, vol, timbre)
  sc <- lapply(sc, function(x) {
               if(length(x) == 3)
                 names(x) <- c("min", "max", "scaling.function")
               return(x)})
  names(sc) <- c("total.length", "pitch", "tempo", "dur", "vol", "timbre")
  class(sc) <- c("sonifyScale", "list")
  sc
}

linear.scale <- function(x, min, max) {
  ## Linearly rescales vector x so that "lower" is the minimum
  ## and "upper" the maximum

  if(min>max) x <- -x
  
  nrange <- abs(max-min)
  out <- ((x-min(x))*nrange/(max(x)-min(x)) + min)
  out
}

