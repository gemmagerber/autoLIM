# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

create_package("D:/Desktop/autoLIM")
#https://r-pkgs.org/whole-game.html

library(devtools)
library(roxygen2)
library(testthat)
library(knitr)
library(usethis)
library(rmarkdown)
#library(readxl)

# packageVersion("devtools")

# Vignettes
usethis::use_vignette("autolim_vignette")

use_mit_license() # pick a licence
use_git()
use_github()
# Edit decription (Ctrl + .) and start typing "Description"

# start adding functions
usethis::use_version()
## autoLIMR Gemma's version begin package building
# error_print
use_r("error_print")
use_r("read_all_sheets")
use_r("sci_notation_off")
use_r("NLNode_mat")
use_r("search_cols")
use_r("matrix_def")
use_r("net_data_tidy")
use_r("adj_mat_tidy")
use_r("net_data_node")
use_r("net_data_node_list")
use_r("net_data_external_list")
use_r("pvar_wo_ex")
use_r("pvar_w_ex")
use_r("uvar_wo_ex")
use_r("uvar_w_ex")
use_r("pp_true")
use_r("qvar")
use_r("uvar")
use_r("pvar")
use_r("aevar")
use_r("variable_def")
use_r("net_data_resp_flows")
use_r("net_data_inex_flows")
use_r("adj_mat_flows")
use_r("net_data_ineq")
use_r("adj_mat_ineq")
use_r("meta1")
use_r("meta2")
use_r("merge")
use_r("autoLIMR")

# then add function and save
check()
devtools::document()
check()

?autoLIMR
# call "load_all" to make available for experimentation
load_all()
exists("error_print", where = globalenv(), inherits = FALSE)
exists("read_all_sheets", where = globalenv(), inherits = FALSE)
exists("sci_notation_off", where = globalenv(), inherits = FALSE)
exists("NLNode_mat", where = globalenv(), inherits = FALSE)
exists("search_cols", where = globalenv(), inherits = FALSE)
exists("matrix_def", where = globalenv(), inherits = FALSE)
exists("net_data_tidy", where = globalenv(), inherits = FALSE)
exists("adj_mat_tidy", where = globalenv(), inherits = FALSE)
exists("net_data_node", where = globalenv(), inherits = FALSE)
exists("net_data_node_list", where = globalenv(), inherits = FALSE)
exists("net_data_external_list", where = globalenv(), inherits = FALSE)
exists("pvar_wo_ex", where = globalenv(), inherits = FALSE)
exists("pvar_w_ex", where = globalenv(), inherits = FALSE)
exists("uvar_wo_ex", where = globalenv(), inherits = FALSE)
exists("uvar_w_ex", where = globalenv(), inherits = FALSE)
exists("pp_true", where = globalenv(), inherits = FALSE)
exists("qvar", where = globalenv(), inherits = FALSE)
exists("uvar", where = globalenv(), inherits = FALSE)
exists("pvar", where = globalenv(), inherits = FALSE)
exists("aevar", where = globalenv(), inherits = FALSE)
exists("variable_def", where = globalenv(), inherits = FALSE)
exists("net_data_resp_flows", where = globalenv(), inherits = FALSE)
exists("net_data_inex_flows", where = globalenv(), inherits = FALSE)
exists("adj_mat_flows", where = globalenv(), inherits = FALSE)
exists("net_data_ineq", where = globalenv(), inherits = FALSE)
exists("adj_mat_ineq", where = globalenv(), inherits = FALSE)
exists("meta1", where = globalenv(), inherits = FALSE)
exists("meta2", where = globalenv(), inherits = FALSE)
exists("merge", where = globalenv(), inherits = FALSE)
exists("autoLIMR", where = globalenv(), inherits = FALSE)
# Go to function. Insert roxugen skeleton.
# Trigger conversion of roxygen comment into
# man/plotpack.Rd with document()
devtools::document()
check()
use_git()
install()
Sys.getenv("R_LIBS_USER")
install.packages("pacman")
library(pacman)
