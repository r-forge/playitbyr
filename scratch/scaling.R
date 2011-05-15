scaling <- function(total.length = NA, pitch = NA, start = NA, dur = NA, vol = NA, timbre = NA, stretch.to.length=TRUE) {
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

linear.scale <- function(x, min, max) {
  ## Linearly rescales vector x so that "lower" is the minimum
  ## and "upper" the maximum
  
  nrange <- abs(max-min)
  out <- ((x-min(x))*nrange/(max(x)-min(x)) + min)
  out
}

rev.linear.scale <- function(x, min, max) {
  ## Reverse of linear scale of  vector x so that "lower" is the minimum
  ## and "upper" the maximum
  x <- -x
  nrange <- abs(max-min)
  out <- ((x-min(x))*nrange/(max(x)-min(x)) + min)
  out
}
