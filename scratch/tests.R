

s <- sonify(data=iris[1:10,], mapping=sonaes(pitch = "Sepal.Width"), rendering="R", scales=scale())
s <- s + layer(shape="notes", shape_params = list("short"))
s <- s + layer(shape="noise", shape_params = list("assy"))


## All this is looking like the structure we want.

