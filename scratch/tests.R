## Funny!

x <- sonify(data=iris, mapping=sonaes(pitch = "Sepal.Width", timbre=8, dur=0.75, tempo="Sepal.Length", vol=0.75), rendering="midi", scales=scaling(pitch=list(13.5, 3, linear.scale), tempo=list(400, 800, linear.scale)))+ layer("notes")
x + layer("notes", mapping=sonaes(pitch="Petal.Width", timbre=2, temp="Petal.Length"))

## Wow is this syntax verbose. More defaults here will make my paper prettier...





## when mapping is absent for whatever reason, we get the unintuitive error message:
## 
## Error in if (map[[j]] %in% names(data)) { : argument is of length zero

## Not putting "scalings" on leads to thi error:
## Error: attempt to apply non-function

## Should throw warning for omitted notes and exclude them before rendering
##, and document what the actual range of MIDI is.
