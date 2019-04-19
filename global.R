shiny::enableBookmarking(store = "url")

library(shiny)
library(shinydashboard)
library(dplyr)
library(sf)
library(leaflet)
library(jsonlite)
library(knitr)
library(kableExtra)
library(lubridate)

# setwd("~/sovellukset/tellinkiappi/")
# library(jsonlite)
# # hae reaaliaikaiden data rajapinastas
# d <- jsonlite::fromJSON(txt = "http://api.digitransit.fi/routing/v1/routers/hsl/bike_rental", simplifyDataFrame = TRUE)[[1]]
# d <- d[,c("id","name","x","y","bikesAvailable","spacesAvailable", "state")]
# d$time <- Sys.time()
# d$timen <- as.numeric(d$time)
# # d$id <- as.integer(d$id)
# tellingit <- d$id
# names(tellingit) <- d$name
# saveRDS(tellingit, "./data/tellingit.RDS")
tellingit <- readRDS("./data/tellingit.RDS")
distanssi <- readRDS("./data/distanssi.RDS")
points <- readRDS("./data/points.RDS")

# create_con <- function(){
#   library(DBI)
#   library(RMariaDB)
#   con <- dbConnect(RMariaDB::MariaDB(), group = "db-tellinki") 
#   return(con)
# }

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Kirjoita kantaan
# create_con <- function(){
#   library(dplyr)
#   library(DBI)
#   library(RMariaDB)
#   con <- dbConnect(RMariaDB::MariaDB(), group = "db-tellinki")
#   return(con)
# }


# con <- create_con()
# dbReadTable(con, "KAUPUNKIFILLARIT2018_SAA") %>%
#   filter(grepl("Kaisaniemi", ASEMA)) %>%
#   arrange(desc(TIME)) %>%
#   slice(1:20)
# dbDisconnect(con)


# laske telineiden väliset etäisyydet

# d <- jsonlite::fromJSON(txt = "http://api.digitransit.fi/routing/v1/routers/hsl/bike_rental", simplifyDataFrame = TRUE)[[1]]
# d <- d[,c("id","name","x","y")]
# d$id <- as.integer(d$id)
# library(sp)
# SpatialPointsDataFrame(coords = d[,c("x", "y")], d[,c("name","id")]) %>%
#   st_as_sf() -> dsf
# saveRDS(dsf, "./data/points.RDS")
# 
# # 
# nc_dist_mat <- st_distance(dsf, dsf, by_element = FALSE)
# library(reshape2)
# dm <- melt(nc_dist_mat)
# # paikka-x
# d$rn <- as.integer(row.names(d))
# d %>%
#   select(-x, -y) %>%
#   rename(id_x = id,
#          name_x = name) -> d1
# d %>%
#   select(-x, -y) %>%
#   rename(id_y = id,
#          name_y = name) -> d2
# 
# df1 <- left_join(dm,d1, by = c("Var1" = "rn")) %>%
#   left_join(.,d2, by = c("Var2" = "rn")) %>%
#   mutate(value = value * 100000)
# # 
# saveRDS(df1, "./data/distanssi.RDS")
# 
# df2 <- readRDS("./data/distanssi.RDS")

# df1 %>%
#   filter(name_x == "Kaivopuisto") %>%
#   arrange(value) %>% slice(1:15) %>%
#   mutate(value = value * 100000)



