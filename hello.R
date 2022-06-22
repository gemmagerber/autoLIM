# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#https://r-pkgs.org/whole-game.html
library(devtools)

#use_mit_license() # pick a licence
use_git()

# start adding functions
use_r("multi")
use_r("prepack")
use_r("fmat")
use_r("input")

## autoLIMR Gemma's version begin package building
# error_print
use_r("error_print")

# call "load_all" to make plotpack available for experimentation
load_all()

# Check to see if all moving parts of package work
check()

# Edit decription (Ctrl + .) and start typing "Description"

# Pick a licence
use_mit_license()

# Go to function. Insert roxugen skeleton. Trigger conversion of roxygen comment into
# man/plotpack.Rd with document()
devtools::document()
