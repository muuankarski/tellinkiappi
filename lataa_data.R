#!/usr/bin/r
setwd("/home/aurelius/sovellukset/tellinkiappi")
library(jsonlite)
library(lubridate)
# hae reaaliaikaiden data rajapinastas
d <- jsonlite::fromJSON(txt = "http://api.digitransit.fi/routing/v1/routers/hsl/bike_rental", simplifyDataFrame = TRUE)[[1]]
d$networks <- NULL
d$time <- Sys.time()
d$day <- yday(d$time)

tmp <- read.csv("paivadata.csv", stringsAsFactors = FALSE)

if (unique(d$day) == tmp[nrow(tmp),"day"]){
  write.table(d, file = "./paivadata.csv", append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
} else {
  write.table(d, file = "./paivadata.csv",  sep = ",", row.names = FALSE)
}

