source("tclTest.R")
source("scaling.R")
source("sonify.R")
source("MIDI.R")



## when mapping is absent for whatever reason, we get the unintuitive error message:
## 
## Error in if (map[[j]] %in% names(data)) { : argument is of length zero

## Not putting "scalings" on leads to thi error:
## Error: attempt to apply non-function
## Should throw warning for omitted notes and exclude them before rendering
##, and document what the actual range of MIDI is.







##################################################
## EXAMPLES IN REPORT
##################################################

##1a
x <- sonify(data=iris, 
      sonaes(time="Petal.Width", pitch = "Petal.Length", 
      dur=3, vol=0.75, timbre="drum"), 
      rendering="MIDI")
y <- x + shape_notes() + scale_time_linear(0, 3.5) + scale_pitch_linear(3, 12)

## c##1b
## x + shape_notes() + scale_time_linear(0, 3.5) + scale_pitch_linear(3, 12)

## ##1c
## require(ggplot2)
## ggplot(data=iris, aes(x=Petal.Width, y=Petal.Length)) + geom_point()

## ##Need to create figure!

