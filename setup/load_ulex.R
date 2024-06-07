# ULEx Installer

# This is a temporary solution until this is made into a package via devtools

## Install/Load Package Dependencies
if (!require("pacman")) install.packages("pacman")
pacman::p_load(magrittr, lubridate, dplyr, tidyr, readr, purrr, tidytext,
               stringr, stringi, ngram, hunspell, stringdist, tm, raster,
               parallel, jsonlite, sf)

# maptools

## Load ULEx Functions
source("https://raw.githubusercontent.com/ramarty/ulex/master/R/helper_functions.R")
source("https://raw.githubusercontent.com/ramarty/Unique-Location-Extractor/master/R/augment_gazetteer.R")
source("https://raw.githubusercontent.com/ramarty/Unique-Location-Extractor/master/R/locate_event.R")
