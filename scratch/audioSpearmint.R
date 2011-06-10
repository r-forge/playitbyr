require(audio)

x <- audioSample(sin(1:88200), 44100)
play(x)

library(tuneR)
