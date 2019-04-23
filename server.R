library(lubridate)
library(ggplot2)
library(hrbrthemes)
library(ggsci)
library(sparkline)
library(formattable)
library(kableExtra)
library(htmltools)

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
    saa[c(1,3,7,10,12)] %>% 
      mutate(Asema = sub("Helsinki |Espoo ", "", Asema))
    
  })
  
  output$saa_ennuste <- renderTable({
    ennuste <- data_input_saaennuste()
    ennuste <- ennuste[1:10, c(1,3,5,8)]
    ennuste %>% 
      mutate(Ajankohta = sub("\\.2018", "", Ajankohta))
  })
  
  output$test <- renderText({
    data_input_realtime() %>% pull(name) 
  })
  
output$tbl_realtime <- renderUI({
  
  distanssi %>%
    filter(id_x %in% as.integer(input$tellinki)) %>%
    filter(value <= input$distance) -> dist
  
  # readRDS("./data/distanssi.RDS") %>%
  # distanssi %>%
  #   dplyr::filter(id_x %in% c("591")) %>%
  #   dplyr::filter(value <= 800) -> dist
  
  
  data_nyt <- read.csv("/home/aurelius/sovellukset/tellinkiappi/paivadata.csv", stringsAsFactors = FALSE) %>% 
    tibble::as_tibble() %>% 
    dplyr::filter(id %in% unique(c(dist$id_y,dist$id_x))) %>% 
    group_by(id) %>% 
    arrange(desc(time)) %>% 
    slice(1:24) %>% 
    ungroup() #%>% select(id,bikesAvailable,time)
  # data_nyt <- data_nyt %>% filter(id == input$tellinki)
  data_nyt$time2 <- as.POSIXct(data_nyt$time)
  data_nyt$aika <- hour(data_nyt$time2) + minute(data_nyt$time2)/60

  # dist
  #
  dat_tbl <- data_nyt %>% 
    filter(time == max(time2, na.rm = TRUE)) %>% 
    select(id,name,bikesAvailable,x,y) %>% 
    left_join(.,dist, by = c("id" = "id_y")) %>% 
    select(-name_x,-name_y,-id_x,-Var2,-Var1)

  
  
  
  dat_tbl %>% 
    mutate(
           name = glue::glue("<a href='http://82.181.178.246/tellinkiappi/?_inputs_&distance=750&sidebarCollapsed=false&sidebarItemExpanded=null&tellinki=\"{stringr::str_pad(id, 3, pad = '0')}\"'>{name}</a>"),
           value_hidden = value,
value = glue::glue("<a target = '_blank' href='https://maps.google.fi/?q={y},{x}'>{round(value, 0)}</i>
</a>")) %>%
    select(id,name,bikesAvailable,value,value_hidden) %>%
    arrange(value) %>% 
    mutate(bikesAvailable = cell_spec(bikesAvailable,
                                      background = ifelse(bikesAvailable >= 5, "#33ff99",
                                                          ifelse(bikesAvailable %in% 1:4, "yellow", "#ff8080")))) %>%
    rename(tellinki = name,
           vapaana = bikesAvailable,
           `matkaa (m)` = value) -> df
  
  
  # output$Table <- renderUI({
    res <- data_nyt %>% 
      group_by(id) %>% 
      arrange(time) %>% 
      # use new sparkline helper methods
      summarise(`viimeinen 2h`=spk_chr(c(bikesAvailable))) %>%
      left_join(df) %>% 
      arrange(value_hidden) %>% 
      select(tellinki,vapaana,`viimeinen 2h`,`matkaa (m)`) %>% 
      format_table() %>%
      htmltools::HTML() %>%
      div() %>%
      # use new sparkline helper for adding dependency
      spk_add_deps() %>%
      # use column for bootstrap sizing control
      # but could also just wrap in any tag or tagList
      {column(width=6, .)}
    
    res
  
})
  # }, sanitize.text.function = function(x) x)
  
  
  # valueBox(
  #   value = tbl$bikesAvailable,
  #   subtitle = tbl$name,
  #   icon = icon("bicycle"),
  #   color = if (tbl$bikesAvailable > 3) "green" else if (tbl$bikesAvailable %in% 1:3) "yellow" else "red")
# })


output$map_realtime <- renderLeaflet({
  
  # readRDS("./data/distanssi.RDS") %>%
    distanssi %>%
    filter(id_x %in% as.integer(input$tellinki)) %>%
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
    leaflet::addProviderTiles(providers$CartoDB.Positron) %>% 
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
  
  withProgress(message = 'Odota hetki', value = 0, {
  incProgress(1/3, detail = "Louhitaan dataa")
  d12 <- readRDS(glue::glue("/home/aurelius/local_data/kaupunkifillari_data/ennustedatat/{input$tellinki}.RDS"))
  # d12 <- readRDS(glue::glue("~/local_data/kaupunkifillari_data/ennustedatat/107.RDS"))
  d12$group <- glue::glue("{d12$yday}{d12$year}")
  
  time_now <- Sys.time()
  # yday_now <- yday(time_now)
  hour_now <- hour(time_now)
  # year_now <- year(time_now)
  minute_now <- minute(time_now)/60
  aika_nyt <-  hour_now + minute_now
  # Valitaan vaan sama viikonpäivä!
  Sys.setlocale("LC_ALL" ,"fi_FI.UTF-8")
  d12 <- d12[d12$weekdays == weekdays(time_now), ]

  dd <- data_input_realtime()
  
  
  # head(d12)
  
  incProgress(2/2, detail = "Piirretään kuvaa")
  
  data_nyt <- read.csv("/home/aurelius/sovellukset/tellinkiappi/paivadata.csv", stringsAsFactors = FALSE)
  # data_nyt <- data_nyt %>% filter(id == input$tellinki)
  data_nyt <- data_nyt %>% filter(id %in% as.integer(input$tellinki))
  data_nyt$time2 <- as.POSIXlt(data_nyt$time)
  data_nyt$aika <- hour(data_nyt$time2) + minute(data_nyt$time2)/60
  # year_now <- year(time_now)
  
  # data_nyt <- dd %>% filter(id == 107) %>% mutate(aika = aika_nyt) %>% as_tibble()
  
    ggplot(data = d12,
         aes(x=aika,y=bikesAvailable,group=group)) +
    geom_line(alpha = .3) +
      geom_line(data = data_nyt,
                aes(x=aika,y=bikesAvailable,group=1), color = "orange") +
      geom_smooth(aes(group = 1), show.legend = FALSE) +
    geom_point(data = data_nyt[nrow(data_nyt),],
               aes(x = aika, y=bikesAvailable, group = 1), fill = "orange", size = 3, color = "white", shape = 21, stroke = 2, alpha = .6) +
      geom_label(data = data_nyt[nrow(data_nyt),],
                 aes(x = aika, y=bikesAvailable, group = 1, label = glue::glue("{bikesAvailable} vapaana")), fill = "orange", size = 3, color = "white", shape = 21,  family = "Roboto Condensed", nudge_y = 1, alpha = .6) +

    # geom_text(color = "white", family = "Roboto Condensed", size = 2.5) +
    theme_ft_rc(plot_title_face = NULL, plot_title_size = 12,
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
         title = glue::glue("Ennuste tellingille {names(tellingit[tellingit == input$tellinki])} kello {format(time_now, format = '%H:%M')}"),
         # title = glue::glue("{input$tellinki)"),
         subtitle = "Tämä päivä oranssilla!",
         caption = paste0("Data: HSL
         (C) Tellinkibotti\n",
                          Sys.time())) +
    theme(plot.margin = unit(c(5, 2, 5, 2), "mm"))# +
    # facet_wrap(~name, ncol = 1, scales = "free")
  })
})

  
  
}