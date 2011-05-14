sonify(data=iris[1:10,], mapping=sonaes(pitch = "Sepal.Width"), rendering="R", scales=scaling(total.length=3, pitch=list(4, 12, linear.scale))) + layer('notes')

## clumsy, but it works!!! (for ONE layer of MIDI)


