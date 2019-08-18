#!/usr/bin/r
setwd("/home/aurelius/sovellukset/tellinkiappi")

# laske telineiden väliset etäisyydet

# # hae reaaliaikaiden data rajapinastas
d <- jsonlite::fromJSON(txt = "http://api.digitransit.fi/routing/v1/routers/hsl/bike_rental", simplifyDataFrame = TRUE)[[1]]
d <- d[,c("id","name","x","y","bikesAvailable","spacesAvailable", "state")]
d$time <- Sys.time()
d$timen <- as.numeric(d$time)
# d$id <- as.integer(d$id)
tellingit <- d$id
names(tellingit) <- glue::glue("{d$name} ({d$id})")
saveRDS(tellingit, "./data/tellingit.RDS")

# d <- jsonlite::fromJSON(txt = "http://api.digitransit.fi/routing/v1/routers/hsl/bike_rental", simplifyDataFrame = TRUE)[[1]]
d <- d[,c("id","name","x","y")]
d$id <- as.integer(d$id)
library(sp)
library(sf)
library(dplyr)
SpatialPointsDataFrame(coords = d[,c("x", "y")], d[,c("name","id")]) %>%
  st_as_sf() -> dsf

saveRDS(dsf, "./data/points.RDS")

#
nc_dist_mat <- st_distance(dsf, dsf, by_element = FALSE)
library(reshape2)
dm <- melt(nc_dist_mat)
# paikka-x
d$rn <- as.integer(row.names(d))
d %>%
  select(-x, -y) %>%
  rename(id_x = id,
         name_x = name) -> d1
d %>%
  select(-x, -y) %>%
  rename(id_y = id,
         name_y = name) -> d2

df1 <- left_join(dm,d1, by = c("Var1" = "rn")) %>%
  left_join(.,d2, by = c("Var2" = "rn")) %>%
  mutate(value = value * 100000)

# #
saveRDS(df1, "./data/distanssi.RDS")
#
df2 <- readRDS("./data/distanssi.RDS")

# df1 %>%
#   filter(name_x == "Ruskeasuon varikko") %>%
#   arrange(value) %>% slice(1:15) %>%
#   mutate(value = value * 100000)
