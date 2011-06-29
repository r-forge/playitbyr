render.csound <- function(x) {
  ##option setting should be made more portable, and moved to a separate file!
  ##.Tcl calls should be changed to (safer) tcl calls and passing of tcl objects
  require(tcltk)
  sco <- .dfNotes(x)
  sco$inst <- match(sco$timbre, c("drum", "sine"))
  sleeptime <- ceiling(sco$start[nrow(sco)] + sco$dur[nrow(sco)] + 0.5)

  sco <- sco[c("inst", setdiff(names(sco), c("sonlayer", "timbre", "inst")))] ## not currently supported here

  out <- paste("csNote",do.call(paste, sco)) 

  orcfile <- "/home/fortis/Sonification/playitbyr/icadDemo/rendermenhods/csound/inst/clarinet.orc"

  .Tcl("load /usr/lib/tclcsound/tclcsound.so")
  if(!(.csCompiled)) {
    .Tcl(paste("csCompile -odac", orcfile))
    .csCompiled <<- TRUE
  } else {.Tcl("csRewind")}
  .Tcl("csTable 1 0 4096  10    1")
  sapply(out, .Tcl)

  .Tcl("csPlay")

  print("\n ############## \n Note: Csound is still running.\n Type 'csStop()' to stop running Csound. \n")
}


csStop <- function() .Tcl("csStop")

