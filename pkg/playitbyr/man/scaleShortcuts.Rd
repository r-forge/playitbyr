\name{scale_time_linear}
\alias{scale_dur_linear}
\alias{scale_pan_linear}
\alias{scale_pitch_linear}
\alias{scaleShortcuts}
\alias{scale_tempo_linear}
\alias{scale_time_linear}
\alias{scale_vol_linear}
\title{Shortcut functions to set linear scales for \code{sonify} objects}
\usage{
  scale_time_linear(min, max)

  scale_pitch_linear(min, max)

  scale_dur_linear(min, max)

  scale_vol_linear(min, max)

  scale_pan_linear(min, max)

  scale_tempo_linear(min, max)
}
\arguments{
  \item{min}{The desired minimum value, a \code{numeric} of
  length 1}

  \item{max}{The desired maximum value, a \code{numeric} of
  length 1}
}
\value{
  A \code{sonscaling} object, to be used in \code{sonify}
  or added onto a \code{sonify} object.
}
\description{
  These shortcut functions (with the notation
  \code{scale_}\kbd{[data parameter]}\code{_}\kbd{[scaling
  function]}) are intended to be added onto a \code{sonify}
  object to specify how \kbd{[data parameter]} from the
  data (a column of the data) is to be mapped to sonic
  parameters.
}
\details{
  The syntax of specifying a full \code{sonify} object can
  be rather verbose. Inspired by the \pkg{ggplot2} package,
  \pkg{playitbyr} provides shortcut functions to make the
  process easier and more intuitive.

  These functions are intended to be added onto a
  pre-existing sonify object, as shown in the examples.
}
\examples{
## A verbose way of specifying a sonify object,
## without using the shortcuts
x <- sonify(data=iris,
sonaes(time=Petal.Width, pitch=Petal.Length, dur=5, vol=0.75),
scales = sonscaling(
time = list(min=0, max=3,
scaling.function = linear.scale),
pitch = list(min=3, max=13,
scaling.function = linear.scale)),
sonlayers = shape_notes())
summary(x)
\dontrun{x}

## An equivalent and much more readable way, with
## the shortcut functions
y <- sonify(iris, sonaes(time=Petal.Width, pitch=Petal.Length, dur=5, vol=0.75))
y <- y + shape_notes()
y <- y + scale_time_linear(0, 3)
y <- y + scale_pitch_linear(3, 13)
summary(y)
\dontrun{y}

## You can also change a mapping incrementally.
## New parameters overwrite old ones, so now
## the sonification is stretched to 12 seconds
## long instead of 3, and the pitch is mapped to
## a much narrower range, the octave below tuning A (440 Hz)
y <- y + scale_time_linear(0, 12) + scale_pitch_linear(7, 8)
summary(y)
\dontrun{y}
}
\seealso{
  \code{\link{sonscaling}} for the full syntax and range of
  possibilities for specifying scaling of \code{sonify}
  objects; \code{\link{linear.scale}} for the pre-defined
  linear scaling function which this incorporates;
  \code{\link{+.sonify}} for the addition onto
  \code{sonify} objects.
}

