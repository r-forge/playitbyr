sonify <- function(data=NA, mapping=NA, rendering=NA, scales=NA, stat=NA) {
  ## This just puts the items in a list
  s <- list(data, mapping, rendering, scales, stat)
  names(s) <- c("data", "mapping", "rendering", "scales", "stat") #Theres' got to be an easier way to do this
  class(s) <- c("sonify", "list")
  s
}

##Need to generate mappings. Let's start with just pitch and tone.
##Pitch can be default stored in csound's "oct" notation, with
##before decimal being octaves of middle 

##From manual at Csounds.com: the fraction is preceded by a whole number octave index such that 8.00 represents Middle C, 9.00 the C above, etc. Midi note number values range between 0 and 127 (inclusively) with 60 representing Middle C, and are usually whole numbers.

sonaes <- function(pitch=NA, start=NA, dur=NA, vol=NA, timbre=NA) {
  son <- c(pitch, start, dur, vol, timbre)
  names(son) <- c("pitch", "start", "dur", "vol", "timbre")
  son
}

scale <- function(length=10, pitch=NA, start=NA, dur=NA, vol=NA, timbre=NA) {
  sc <- list(length, pitch, start, dur, vol, timbre)
  names(sc) <- c("length", "pitch", "start", "dur", "vol", "timbre")
  sc
}


# Benford code below is based on Drew Conway's Wikileaks_Analysis
# Available at http://github.com/drewconway/WikiLeaks_Analysis/blob/master/wikileaks_analysis.R

# Given an integer and a base, returns the leading "digit" in a base.
leading.dig <- function(x, base = 10) {
  highest.power <- floor(log(x, base = base))
  largest.multiple <- floor(x / base^highest.power)
  return(largest.multiple)
}

CountDigits <- function(x, base = 10) {
  ## Count digits and store as data frame
  ## Elements equal to zero or NA are not counted.

  if(all(x==0)) stop("All elements of input are equal to zero, so leading digits are not defined.")
  dig.count <- as.data.frame(table(sapply(as.vector(x),leading.dig, base = base)))
  names(dig.count) <- c("Digit", "DigitCount")
  dig.count$Digit <- as.numeric(levels(dig.count$Digit))

  ## Check to see whether dig.count has all digits and
  ## if not, add the missing ones to the data frame:

  if(length(dig.count$Digit) == (base - 1)) return(dig.count) else {
    dig.missing <- (1:(base - 1))[-dig.count$Digit]
    dig.count[(length(dig.count$Digit) + 1):(base -1), "Digit"] <- dig.missing
    dig.count$DigitCount[is.na(dig.count$DigitCount)] <- 0
    reordered <- dig.count[order(dig.count$Digit),]  
    rownames(reordered) <- NULL
    return(reordered)
  }
}



# Benford's distribution
dbenford<-function(d, base) {
    return(log(1+(1/d),base=base))
}

DTMFBenford <- function(x = NA, total.length = 10) {
  ## Takes an input numeric vector, calculates the
  ## leading digit (in base 10) and returns
  ## a Wave object corresponding to the DTMF realization
  ## of those proportions, where each leading digit is played
  ## for the proportion of *total.length* that it appears in the
  ## dataset.

  ## If no input vector, or input vector all NAs,
  ## return mono DTMF Benford distribution realization
  
  if(all(is.na(x))) {
    DTMFBenf <- DTMFtone(1, duration = dbenford(1, 10)*10)
    for(i in 2:9) {
      DTMFBenf <- bind(DTMFBenf,
                          DTMFtone(i, dbenford(i,10)*10))
    }
    return(DTMFBenf)
  }
      
  lengths <- (CountDigits(x)[,2]/sum(CountDigits(x)[,2])
                                    ) * total.length

  ## Some tasty inefficient looping to generate
  ## Wave objects corresponding to the sample
  ## and Benford's distribution

  
  DTMFsamp <- DTMFtone(1, duration = lengths[1])
  DTMFBenf <- DTMFtone(1, duration = dbenford(1, 10)*10)
  
  for(i in 2:9) {
    DTMFsamp <- bind(DTMFsamp, DTMFtone(i, lengths[i]))
    DTMFBenf <- bind(DTMFBenf,
                     DTMFtone(i, dbenford(i,10)*10))
  }

  ## The lengths may be slightly off due to numerical
  ## imprecision; so we need to add silence at the very end
  ## (probably a tiny fraction of a second) to make the two
  ## files exactly equal
  
  diff.length <- length(DTMFsamp) - length(DTMFBenf)
  if(diff.length > 0 ) {
    DTMFBenf <- bind(DTMFBenf, silence(duration = diff.length,
                                             bit=16, samp.rate = 44100))
  } else if(diff.length < 0) {
    DTMFsamp <- bind(DTMFBenf, silence(duration = diff.length,
                                          bit=16, samp.rate = 44100))
  }

  ## Return a stereo Wave object with the sample proportions
  ## in the left channel, and the ideal Benford proportions
  ## on the right
  
  return(stereo(DTMFsamp, DTMFBenf))
}


  

  

  
  
