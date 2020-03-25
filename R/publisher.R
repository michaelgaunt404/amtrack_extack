#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This script remotely knits RMarkdown files.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: Changes to relative paths or folder structure need to be reflected
#-------- in script write function.
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#package install and load~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(devtools)
install_version("ezknitr", 
                version = "0.6", 
                repos = "http://cran.us.r-project.org")
library(ezknitr)

#path and data set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/")
wd = as.character(rstudioapi::getSourceEditorContext()$path)
wd = gsub("R/.*","\\1",wd)
setwd(wd)

#path and data set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#sets suffix for file name
#generally best if this is 'system date/time'
suffix = format(Sys.time(), '%Y%m%d_%H%M%S')

#should be in current wd
ezknit(file = "amtrak_extract_MD.rmd", 
       out_suffix = suffix,
       out_dir = "./output",
       fig_dir = "./output/myfigs", 
       verbose = TRUE)


print("R script successfully ran from terminal.")
