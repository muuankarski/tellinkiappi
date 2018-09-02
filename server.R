library(lubridate)
library(ggplot2)
library(hrbrthemes)
library(ggsci)
Sys.setlocale("LC_ALL" ,"fi_FI.UTF-8")
## server.R ##
function(input, output, session) {
  
  data_input_realtime <- reactive({
    
    # hae reaaliaikaiden data rajapinastas
    d <- jsonlite::fromJSON(txt = "http://api.digitransit.fi/routing/v1/routers/hsl/bike_rental", simplifyDataFrame = TRUE)[[1]]
    dd <- d[,c("id","name","x","y","bikesAvailable","spacesAvailable", "state")]
    dd$id <- as.integer(dd$id)
    return(dd)

  })
  
  
  data_input_db <- reactive({
    
    readRDS("./data/distanssi.RDS") %>% 
    # istanssi %>%
      filter(id_x %in% input$tellinki) %>%
      filter(value <= input$distance) %>% 
      arrange(value) -> dist
    
    uniqs <- dist$id_y
    
    con <- create_con()
    
    time_now <- Sys.time()
    yday_now <- yday(time_now)
    hour_now <- hour(time_now)
    weekday_now <- weekdays(time_now)
    minute_now <- minute(time_now)/60
    aika_nyt <-  hour_now + minute_now
    
    tbl(con, "KAUPUNKIFILLARIT2018") %>% 
      mutate(id = as.integer(id)) %>% 
      # filter(id == input$tellinki) %>%
      filter(id %in% uniqs) %>%
      # filter(bikesAvailable == 2) %>%
      # filter(id == unique(dist$id_x)) %>% 
      filter(hour >=  (hour_now-2),
             hour <= (hour_now+4),
             weekdays == weekday_now) %>%
      collect() %>% 
      group_by(id,name) %>%
      arrange(time) %>%
      mutate(freq = abs(bikesAvailable-lag(bikesAvailable)),
             name = factor(name, levels = dist$id_y)) %>%
      ungroup() -> d12
    
    dbDisconnect(con)
    return(d12)

  })
  
  data_input_saa <- reactive({
    
    library(rvest)
    library(httr)
    read_html("http://suja.kapsi.fi/fmi-suomi.php") %>%
      html_table() %>%
      .[[1]] %>%
      as_tibble() %>%
      filter(grepl("Tapiola|Kaisaniemi", Asema)) -> saa
    return(saa)
  })

  
  data_input_saaennuste <- reactive({
    
    read_html("http://suja.kapsi.fi/ennuste.php?paikka=Helsinki") %>%
      html_table() %>%
      .[[1]] %>%
      as_tibble() -> ennuste
    return(ennuste)

  })  

  output$saa_realtime <- renderTable({
    saa <- data_input_saa()
    saa[c(1,3,7,10,12)]
  })
  
  output$saa_ennuste <- renderTable({
    ennuste <- data_input_saaennuste()
    ennuste[1:10, c(1,3,5,8)]
  })
  
# output$box_realtime <- renderValueBox({
#   
#   data_input_realtime() %>% 
#     filter(id %in% input$tellinki) %>% 
#     select(name,bikesAvailable) -> tbl
#   
#   valueBox(
#     value = tbl$bikesAvailable,
#     subtitle = tbl$name,
#     icon = icon("bicycle"),
#     color = if (tbl$bikesAvailable > 3) "green" else if (tbl$bikesAvailable %in% 1:3) "yellow" else "red")
# })

  output$test <- renderText({
    data_input_realtime() %>% pull(name) 
  })
  
output$tbl_realtime <- renderTable({
  
  # readRDS("./data/distanssi.RDS") %>%
    distanssi %>%
    filter(id_x %in% input$tellinki) %>%
    filter(value <= input$distance) -> dist

  # dist
  #
  library(kableExtra)
  data_input_realtime() %>%
    left_join(., dist, by = c("id" = "id_y")) %>%
    filter(!is.na(value)) %>% 
    mutate(name = paste0("<a href='https://maps.google.fi/?q=",y,",",x,"'>",name,"</a>")) %>%
    select(name,bikesAvailable, value) %>%
    arrange(value) %>% 
    mutate(bikesAvailable = cell_spec(bikesAvailable,
                                      background = ifelse(bikesAvailable >= 5, "#33ff99",
                                                          ifelse(bikesAvailable %in% 1:4, "yellow", "#ff8080"))),
           value = round(value, 0)) %>%
    rename(tellinki = name,
           vapaana = bikesAvailable,
           `matkaa (m)` = value)
   }, sanitize.text.function = function(x) x)
  
  
  # valueBox(
  #   value = tbl$bikesAvailable,
  #   subtitle = tbl$name,
  #   icon = icon("bicycle"),
  #   color = if (tbl$bikesAvailable > 3) "green" else if (tbl$bikesAvailable %in% 1:3) "yellow" else "red")
# })


output$map_realtime <- renderLeaflet({
  
  # readRDS("./data/distanssi.RDS") %>%
    distanssi %>%
    filter(id_x %in% input$tellinki) %>%
    filter(value <= input$distance) -> dist

  data_input_realtime() %>%
    left_join(., dist, by = c("id" = "id_y")) %>%
    filter(!is.na(value)) %>% 
    select(name,bikesAvailable, value) %>% 
    right_join(points,.) -> points2
  
  pal <- colorNumeric(
    palette = "RdYlGn",
    domain = points2$bikesAvailable)
    
  leaflet(points2) %>% 
    leaflet::addTiles() %>% 
    addCircleMarkers(color = ~pal(bikesAvailable), 
                     label = ~paste(name,bikesAvailable), 
                     labelOptions = labelOptions(noHide = T, direction = "auto",  
                                                 style = list(
                                                   "color" = "grey",
                                                   "font-family" = "Open Sans",
                                                   "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                   "font-size" = "10px",
                                                   "border-color" = "rgba(0,0,0,0.5)",
                                                   "background" = "rgba(235, 235, 235, 0.61)"
                                                 )
                                                 ))


})

# output$plot_forecast <- renderTable({
output$plot_forecast <- renderPlot({
  
  d12 <- data_input_db()
  
  time_now <- Sys.time()
  yday_now <- yday(time_now)
  hour_now <- hour(time_now)
  minute_now <- minute(time_now)/60
  aika_nyt <-  hour_now + minute_now
  
  # head(d12)
  
  ggplot(data = d12,
         aes(x=aika,y=bikesAvailable,group=yday)) +
    geom_line(alpha = .2) +
    geom_point(data = d12 %>% filter(yday == yday_now),
               aes(x=aika,y=bikesAvailable,group=yday), fill = "orange", color = "white", shape = 21) +
    geom_line(data = d12 %>% filter(yday == yday_now),
              aes(x=aika,y=bikesAvailable,group=yday), color = "orange") +
    geom_smooth(aes(group = 1), show.legend = FALSE) +
    # geom_text(color = "white", family = "Roboto Condensed", size = 2.5) +
    theme_ipsum_ps(plot_title_face = NULL, plot_title_size = 12,
                   subtitle_size = 10, subtitle_face = "italic",
                   plot_title_margin = 3, subtitle_margin = 1) +
    # scale_color_startrek() +
    # scale_fill_startrek() +
    # scale_color_viridis(option = "plasma", discrete = TRUE) +
    theme(legend.position = "none",
          legend.text = element_text(size = 10, lineheight = .8),
          legend.title = element_text(size = 12)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(position = "left") +
    geom_vline(aes(xintercept = aika_nyt), color = "dim grey", alpha = .7) +
    # scale_x_reverse() +
    labs(x = "tunti", y = "lainauksia/palautuksia per tunti",
         color = NULL,
         title = paste0("Ennuste lähitellingeille: ", format(time_now, format = "%Ana %e.%m"), " kello ",format(time_now, format = "%H:%M")),
         subtitle = "Oranssi viiva on tämän päivän tilanne!",
         caption = paste0("Data: HSL
         (C) Tellinkibotti\n",
                          Sys.time())) +
    theme(plot.margin = unit(c(5, 2, 5, 2), "mm")) +
    facet_wrap(~name, ncol = 1, scales = "free")
})

  
  
}