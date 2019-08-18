shiny::enableBookmarking(store = "url")

library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(jsonlite)
library(knitr)
library(kableExtra)
library(lubridate)
library(shinythemes)

# setwd("~/sovellukset/tellinkiappi/")
# library(jsonlite)
tellingit <- readRDS("./data/tellingit.RDS")
distanssi <- readRDS("./data/distanssi.RDS")
points <- readRDS("./data/points.RDS")


