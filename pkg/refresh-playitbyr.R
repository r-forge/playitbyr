## This script refreshes your current installation of playitbyr after
## making changes to the source and reloads it into R. It assumes that your
## working directory is the parent directory of playitbyr
##
## Warning: ?detach says 'detaching and re-attaching a package may not
## refresh some or all components of the package, and is inadvisable.'
## In practice I've found that this works just fine with playitbyr;
## the problems listed in ?detach mostly have to do with graphics devices.
## 
## If wanted for quick testing of .R files and not wanting to do a
## formal package load, try either:
##
## lapply(dir("playitbyr/R/", full.name=T), source)
##
## or, from the R.utils package,
##
## require(R.utils)
## sourceDirectory("playitbyr/R/", modifiedOnly=TRUE)
##
## (courtesy Hadley Wickam and Henrik Bengtsson, respectively)
##
## However, this will not load any changes to the shapeDef files
## themselves.


if("package:playitbyr" %in% search())
  detach("package:playitbyr", unload=TRUE)
remove.packages("playitbyr")
system("R CMD build playitbyr") # may not work on Windows
install.packages("playitbyr_0.1-2.tar.gz")
require(playitbyr)

