x <- sonify(data=iris[1:10,], mapping=sonaes(pitch = "Sepal.Width"), rendering="R", scales=scaling()) + layer('notes') 

## clumsy, but it works!!! (for ONE layer of MIDI)

y <- scaling(total.length=3, pitch=list(4, 12, linear.scale))
