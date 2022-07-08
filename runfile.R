# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

version
library(LIM)
library(network)

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
use_r("fetch_autolimexcel")
use_test("fetch_autolimexcel")

# for multinets
use_r("check_build")
use_r("defaultx0")
use_r("centralx0")
use_r("fmat_fun")
use_r("input_fun")
use_r("resp_fun")
use_r("export_fun")
use_r("output_fun")
use_r("living_fun")
use_r("prepack_fun")
use_r("as_extended")
use_r("ssCheck")

# then add function and save
check()
devtools::document()
check()
use_git()
usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)
use_github()
# testing

# # ? "%v%"
# ?network:::get.vertex.attribute
# # ?network.extraction
# # library(network)
# ?network:::network.extraction()
# getMethod("network::network.extraction")
# ?network
# download.packages(pkgs = "network",
#                   destdir = ".",
#                   type = "source")
#
# untar(download.packages(pkgs = "network",
#                         destdir = ".",
#                         type = "source")[,2])

download.packages(pkgs = "enaR",
                  destdir = ".",
                  type = "source")
?download.packages

untar(download.packages(pkgs = "network",
                        destdir = ".",
                        type = "source")[,2])


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
exists("fetch_autolimexcel", where = globalenv(), inherits = FALSE)
exists("check_build", where = globalenv(), inherits = FALSE)

# Go to function. Insert roxygen skeleton.
# Trigger conversion of roxygen comment into
# man/plotpack.Rd with document()
# fetch_autolimexcel()

check()
devtools::document()

use_git()
#use_github()
install()
Sys.getenv("R_LIBS_USER")
#install.packages("pacman")
#library(pacman)

## Testing

# Testing: near completion of a new feature of bug fix
# Run the entire test suite
devtools::test()

# As part of CMD check
devtools::check()


## GitHub things


gitcreds::gitcreds_get()

install.packages("credentials")
library(credentials)

gitcreds::gitcreds_get(url = "https://github.com", use_cache = TRUE, set_cache = TRUE)
gitcreds::gitcreds_set()
gitcreds::gitcreds_delete()
gitcreds::gitcreds_delete()


usethis::create_github_token()
gitcreds::gitcreds_set()
usethis::use_git()
usethis::use_github()

