#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import leaflet
#' @import ggplot2
#' @import hrbrthemes
#' @import lubridate
#' @import stringr
#' @import patchwork
#' @import sf
#'
#'
#' @noRd
app_server <- function(input, output, session) {

  extrafont::loadfonts(quiet = TRUE)

  output$lat <- renderPrint({
    input$lat
  })

  output$long <- renderPrint({
    input$long
  })

  output$geolocation <- renderPrint({
    input$geolocation
  })



  data_input_realtime <- reactive({

    # hae reaaliaikaiden data rajapinastas
    d <- jsonlite::fromJSON(txt = "http://api.digitransit.fi/routing/v1/routers/hsl/bike_rental", simplifyDataFrame = TRUE)[[1]]
    dd <- d[,c("id","name","x","y","bikesAvailable","spacesAvailable", "state")]
    dd <- dd[dd$state == "Station on",]
    dd$id <- as.integer(dd$id)
    data_just_nyt <- dd
    return(data_just_nyt)
  })

  get_tellingit <- reactive({
    tellingit <- readRDS(url("http://data.markuskainu.fi/kaupunkifillari/ennustedatat_tellingit/tellingit.RDS"))
    return(tellingit)
  })

  get_sparkdata <- reactive({
    sparkdata <- readRDS(url("http://data.markuskainu.fi/kaupunkifillari/appidatat/sparkdata.RDS"))
    return(sparkdata)
  })

  get_pvdata <- reactive({
    pvdata <- readRDS(url("http://data.markuskainu.fi/kaupunkifillari/appidatat/paivadata.RDS"))
    return(pvdata)
  })

  get_distanssi <- reactive({
    distanssi <- readRDS(url("http://data.markuskainu.fi/kaupunkifillari/ennustedatat_tellingit/distanssi.RDS"))
    return(distanssi)
  })

  get_points <- reactive({
    points <- readRDS(url("http://data.markuskainu.fi/kaupunkifillari/ennustedatat_tellingit/points.RDS"))
    return(points)
  })


  output$ui_view_mode <- renderUI({

    Sys.sleep(.3)

    if (is.null(input$geolocation)){

    tagi <- selectInput(inputId = "value_list_type",
                                label = "Valitse näkymä",
                                choices = c("Hae tellinkiä",
                                            "Eniten vapaita pyöriä",
                                            "Eniten saapuneita pyöriä tunnin aikana",
                                            "Eniten lähteneitä pyöriä tunnin aikana"
                                ),
                                selected = "Eniten vapaita pyöriä")

    } else { #if (!is.null(input$geolocation)){
      # if (input$geolocation){

        tagi <-  selectInput(inputId = "value_list_type",
                              label = "Valitse näkymä",
                              choices = c("Näytä lähimmät telineet",
                                          "Hae tellinkiä",
                                          "Eniten vapaita pyöriä",
                                          "Eniten saapuneita pyöriä tunnin aikana",
                                          "Eniten lähteneitä pyöriä tunnin aikana"
                              ),
                              selected = "Näytä lähimmät telineet")
    } #else {
      #tagi <- oletustagit
    #}
    tagList(
      tagi
    )
  })


  output$ui_location_distance <- renderUI({

    req(input$value_list_type)

    if (input$value_list_type == "Näytä lähimmät telineet"){
      # if (!is.null(input$geolocation) & input$value_list_type %in% "Näytä lähimmät telineet"){

      # tellingit <- readRDS("./data/tellingit.RDS")

      tellingit <- get_tellingit()
      nms <- names(tellingit)
      nsm <- sub("Hylkeenpyytäjänkatu","Hernesaarenranta", nms)
      tellingit <- as.integer(tellingit)
      names(tellingit) <- nms
      tellingit <- sort(tellingit)

      tagi <- list(sliderInput("value_tellinki_distance_geoloc",
                               label = "näytä lähitellingit säteellä (m)",
                               min = 0,
                               max = 2000,
                               width = "100%",
                               step = 500,
                               value = 500)
      )

    } else tagi <- NULL
    tagList(
      tagi
    )

  })




  output$ui_tellinkihaku <- renderUI({

    req(input$value_list_type)

    # tellingit <- readRDS("./data/tellingit.RDS")

    tellingit <- get_tellingit()
    nms <- names(tellingit)
    nsm <- sub("Hylkeenpyytäjänkatu","Hernesaarenranta", nms)
    tellingit <- as.integer(tellingit)
    names(tellingit) <- nms
    tellingit <- sort(tellingit)

    if (input$value_list_type == "Hae tellinkiä"){

      tagi <- list(selectInput(inputId = "value_tellinki_haku",
                               label = "kirjoita nimi/id",
                               #openIn = "popup",
                               selected = tellingit[24],
                               choices = tellingit),
                   sliderInput("value_tellinki_distance",
                               label = "näytä lähitellingit säteellä (m)",
                               min = 0,
                               max = 2000,
                               width = "100%",
                               step = 500,
                               value = 500)
      )

    } else if (input$value_list_type == "Näytä lähimmät telineet"){

      tagi <- list(sliderInput("value_tellinki_distance_geoloc",
                               label = "näytä lähitellingit säteellä (m)",
                               min = 0,
                               max = 2000,
                               width = "100%",
                               step = 500,
                               value = 500))

    } else tagi <- NULL
    tagList(
      tagi
    )
  })


  process_tellinki_list <-  reactive({

    req(input$value_list_type)

    data_just_nyt <- data_input_realtime()
    # sparkdata <- readRDS("~/sovellukset/tellinkiappi/sparkdata.RDS")
    # sparkdata <- readRDS(url("http://data.markuskainu.fi/kaupunkifillari/appidatat/sparkdata.RDS"))
    # sparkdata <- get_sparkdata()

    if (input$value_list_type == "Eniten vapaita pyöriä"){

      dat_tbl <- data_just_nyt %>%
        select(id,name,bikesAvailable) %>%
        arrange(desc(bikesAvailable)) %>%
        slice(1:10) %>%
        # left_join(sparkdata) %>%
        setNames(c("id","nimi","vapaana"#,"viimeinen 2h"
                   ))
    } else if (input$value_list_type == "Hae tellinkiä"){

      req(input$value_tellinki_haku)
      distanssi <- get_distanssi()
      distanssi[distanssi$id_x == as.integer(input$value_tellinki_haku),] %>%
        filter(value <= input$value_tellinki_distance) %>%
        select(id_y,value) -> lahitellingit

      dat_tbl <- data_just_nyt %>%
        right_join(lahitellingit, by = c("id" = "id_y")) %>%
        select(value,id,name,bikesAvailable) %>%
        mutate(value = round(value)) %>%
        arrange(value) %>%
        # left_join(sparkdata) %>%
        setNames(c("etäisyys (m)","id","nimi","vapaana"#,"viimeinen 2h"
                   ))
    } else if (input$value_list_type == "Eniten saapuneita pyöriä tunnin aikana") {

      # pvdata <- read.csv("~/sovellukset/tellinkiappi/paivadata.csv", stringsAsFactors = FALSE)
      pvdata <- readRDS(url("http://data.markuskainu.fi/kaupunkifillari/appidatat/paivadata.RDS"))
      pvdata$aika <- as.POSIXct(pvdata$time)
      uniikit_ajat <- sort(unique(pvdata$aika))

      left_join(
        pvdata %>% filter(aika == uniikit_ajat[length(uniikit_ajat)]) %>%
          select(id,bikesAvailable) %>%
          rename(nyt = bikesAvailable),
        pvdata %>% filter(aika == uniikit_ajat[length(uniikit_ajat)-12]) %>%
          select(id,bikesAvailable) %>%
          rename(tunti_sitten = bikesAvailable)
      ) %>%
        as_tibble() %>%
        mutate(erotus = nyt - tunti_sitten) %>%
        arrange(desc(erotus)) %>%
        slice(1:10) %>% select(id,erotus) -> kuhina_tellingit


      dat_tbl <- data_just_nyt %>%
        select(id,name,bikesAvailable) %>%
        right_join(kuhina_tellingit) %>%
        # left_join(sparkdata) %>%
        arrange(desc(erotus)) %>%
        setNames(c("id","nimi","vapaana","muutos 1h"#,"viimeinen 2h"
                   ))
    } else if (input$value_list_type == "Eniten lähteneitä pyöriä tunnin aikana") {

      # pvdata <- read.csv("~/sovellukset/tellinkiappi/paivadata.csv", stringsAsFactors = FALSE)
      pvdata <- get_pvdata()
      pvdata$aika <- as.POSIXct(pvdata$time)
      uniikit_ajat <- sort(unique(pvdata$aika))

      left_join(
        pvdata %>% filter(aika == uniikit_ajat[length(uniikit_ajat)]) %>%
          select(id,bikesAvailable) %>%
          rename(nyt = bikesAvailable),
        pvdata %>% filter(aika == uniikit_ajat[length(uniikit_ajat)-12]) %>%
          select(id,bikesAvailable) %>%
          rename(tunti_sitten = bikesAvailable)
      ) %>%
        as_tibble() %>%
        mutate(erotus = nyt - tunti_sitten) %>%
        arrange(erotus) %>%
        slice(1:10) %>% select(id,erotus) -> kuhina_tellingit


      dat_tbl <- data_just_nyt %>%
        select(id,name,bikesAvailable) %>%
        right_join(kuhina_tellingit) %>%
        # left_join(sparkdata) %>%
        arrange(erotus) %>%
        setNames(c("id","nimi","vapaana","muutos 1h"#,"viimeinen 2h"
                   ))
    } else if (input$value_list_type == "Näytä lähimmät telineet"){

      req(input$geolocation)
      req(input$value_tellinki_distance_geoloc)

      points <- get_points()
      oma_sf <- sf::st_as_sf(x = data.frame(x = input$long, y = input$lat),
                         coords = c("x", "y"),
                         crs = "EPSG:4326")

      # oma_sf <- sf::st_as_sf(x = data.frame(x = 24.91678, y = 60.18724),
      #                        coords = c("x", "y"),
      #                        crs = "EPSG:4326")

      input_value_tellinki_distance_geoloc <- input$value_tellinki_distance_geoloc
      # input_value_tellinki_distance_geoloc <- 400

      res <- sf::st_is_within_distance(x = oma_sf, y = points, dist = input_value_tellinki_distance_geoloc)

      respo <- points[res[[1]],] |> sf::st_set_crs("EPSG:4326")
      respo$value <- as.numeric(sf::st_distance(x = oma_sf, y = respo)[1,])

      dat_tbl <- data_just_nyt %>%
        right_join(sf::st_drop_geometry(respo) %>% select(-name)) %>%
        # filter(id %in% lahitellingit)
        select(value,id,name,bikesAvailable) %>%
        mutate(value = round(value)) %>%
        arrange(value) %>%
        # left_join(sparkdata) %>%
        setNames(c("etäisyys (m)","id","nimi","vapaana"))

    }
    dat_tbl$nimi <- paste0(dat_tbl$nimi, " (", dat_tbl$id, ")")
    return(dat_tbl)
  })

  output$tbl_select2 <- renderTable({

    req(input$value_list_type)

    dat_tbl <- process_tellinki_list()
    dat_tbl$id <- NULL
    dat_tbl #%>% select(-`viimeinen 2h`)
  }, digits = 0)

  # output$tbl_select <- DT::renderDataTable({
  #
  #   req(input$value_list_type)
  #
  #   dat_tbl <- process_tellinki_list()
  #   dat_tbl$id <- NULL
  #
  #
  #   datatable(dat_tbl,
  #             rownames = FALSE,
  #             escape = FALSE,
  #             selection = list(mode = "multiple", selected = NULL), #rownames(dat_tbl)[1]),
  #             options = list(#pageLength = 25,
  #               scrollY = "100%",
  #               paging = FALSE,
  #               dom = "",
  #               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Finnish.json')#,
  #               #                   fnDrawCallback = htmlwidgets::JS(
  #               #                     '
  #               # function(){
  #               #   HTMLWidgets.staticRender();
  #               # }
  #               # ')
  #             )
  #   ) %>%
  #     spk_add_deps() %>%
  #     DT::formatStyle(columns = colnames(.), fontSize = '30%') %>%
  #     formatStyle(
  #       'vapaana',
  #       color = styleInterval(c(0, 5, 10), c('red', 'orange', 'green2', 'green4')),
  #       fontWeight = "bold"
  #       # backgroundColor = styleInterval(10, c('gray', 'yellow'))
  #     )
  #
  # }, server = FALSE)

  # get_selected_tellinki_id <- reactive({
  #   data_just_nyt <- data_input_realtime()
  #
  #   dat_tbl <- process_tellinki_list()
  #   tellinki_id <- dat_tbl[input$tbl_select_rows_selected, ]$id
  #
  #   return(tellinki_id)
  # })



  output$map_realtime <- leaflet::renderLeaflet({

    points <- get_points()

    req(input$value_list_type)
    dat_tbl <- process_tellinki_list()
    tellinki_id <- dat_tbl$id

    if (length(tellinki_id) == 0){

      content <- paste(sep = "<br/>",
                       "<b>Valitse ensin yksi, <br/>kaksi tai kaikki tellingit<br/>etusivun taulukosta</b>"
      )

      leaflet() %>%
        # leaflet::addProviderTiles(providers$CartoDB.Positron) %>%
        addTiles(urlTemplate = "http://tiles.hel.ninja/styles/hel-osm-light/{z}/{x}/{y}@2x@fi.png",
                 options = tileOptions(opacity = .3),
                 group = "peruskartta") %>%
        addPopups(24.95, 60.16, content,
                  options = popupOptions(closeButton = FALSE)
        ) %>%
        setView(lng = 24.95, lat = 60.16, zoom = 12) -> leaf


    } else {

      # readRDS("./data/distanssi.RDS") %>%
      # distanssi %>%
      # filter(id_x %in% as.integer(tellinki_id)) %>%
      # filter(value <= 1000) -> dist
      # filter(value <= input$distance) -> dist

      data_just_nyt <- data_input_realtime()
      data_just_nyt %>%
        filter(id %in% tellinki_id) %>%
        # arrange(desc(bikesAvailable)) %>%
        # slice(1:20) %>%
        # left_join(., dist, by = c("id" = "id_y")) %>%
        # filter(!is.na(value)) %>%

        select(id,name,bikesAvailable) %>%
        right_join(points %>% select(-name),.) -> points2

      pal <- colorNumeric(
        palette = "RdYlGn",
        domain = NULL)

      pal.bins <- c(1, 3, 7, 10, 40, 300)
      pal <- colorBin( "RdYlGn", bins=pal.bins, na.color = "#e52527")

      # labels <- sprintf(
      #   "<a href='http://82.181.150.141/tellinkiappi/?_inputs_&distance=750&sidebarCollapsed=false&sidebarItemExpanded=null&tellinki=\"%s\"'>%s</a><br />%s",
      #   stringr::str_pad(as.character(points2$id), 3, pad = '0'),
      #   points2$name,
      #   as.character(points2$bikesAvailable)
      # ) %>% lapply(htmltools::HTML)

      # points[points$id == as.integer(tellinki_id),] %>%
      #   st_coordinates() -> setview_coords


      leaflet(points2) %>%
        #leaflet::addProviderTiles(providers$CartoDB.Positron) %>%
        addTiles(urlTemplate = "http://tiles.hel.ninja/styles/hel-osm-light/{z}/{x}/{y}@2x@fi.png",
                 options = tileOptions(opacity = .6),
                 group = "peruskartta") %>%
        # setView(lng = setview_coords[1], lat = setview_coords[2], zoom = 15) %>%
        addCircleMarkers(color = ~pal(bikesAvailable),
                         label = ~paste(name,bikesAvailable),
                         labelOptions = labelOptions(noHide = T, direction = "auto",
                                                     style = list("font-family" = "Roboto Mono",
                                                                  "font-size" = "0.9em",
                                                                  "line-height" = "1",
                                                                  # "font-weight" = "700",
                                                                  "background-color" = "rgba(0,0,0,0)",
                                                                  "border-color" = "rgba(0,0,0,0)")
                         )
        ) %>% leaflet.extras::addFullscreenControl() -> leaf
      if (input$value_list_type == "Näytä lähimmät telineet"){
      # if (!is.null(input$geolocation)){
        # if (input$geolocation){

          lon <- input$long
          lat <- input$lat

          oma <- sf::st_sfc(sf::st_point(c(lon,lat)))
          # omapoint <- st_set_crs(oma, value = crs(points2))

          leaf %>%  addMarkers(data = oma)  -> leaf


      # }
      }
    }
    return(leaf)
  })




  # output$plot_forecast <- renderTable({
  output$plot_forecast <- renderPlot({

    # req(input$value_list_type)

    # tellinki_id <- get_selected_tellinki_id()
    dat_tbl <- process_tellinki_list()
    tellinki_id <- dat_tbl$id


    time_now <- Sys.time()
    hour_now <- hour(time_now)
    minute_now <- minute(time_now)/60
    aika_nyt <-  hour_now + minute_now
    # Valitaan vaan sama viikonpäivä!
    Sys.setlocale("LC_ALL" ,"fi_FI.UTF-8")
    if (weekdays(time_now) %in% c("lauantai","sunnuntai")){
      viikonpaivat <- c("lauantai","sunnuntai")
      otsikon_alku <- "Viikonloppujen"
    } else {
      viikonpaivat <- c("maanantai","tiistai","keskiviikko","torstai","perjantai")
      otsikon_alku <- "Arkipäivien"
    }

    data_just_nyt <- data_input_realtime()
    dd <- data_just_nyt

    # data_nyt <- readRDS(url("http://data.markuskainu.fi/kaupunkifillari/appidatat/paivadata.RDS"))
    data_nyt <- get_pvdata()
    data_nyt$bikesAvailable <- as.integer(data_nyt$bikesAvailable)
    data_nyt$time2 <- as.POSIXlt(data_nyt$time)
    data_nyt$aika <- hour(data_nyt$time2) + minute(data_nyt$time2)/60
    data_nyt1 <- data_nyt[data_nyt$id %in% tellinki_id,]
    max_y <- max(as.integer(data_nyt1$bikesAvailable), na.rm = TRUE)
    if (max_y < 40) max_y <- 40



    plotlist <- list()
    for (i in 1:length(tellinki_id)){
      gg_smooth <- readr::read_rds(glue::glue("https://data.markuskainu.fi/kaupunkifillari/ennustedatat/gg/{str_pad(tellinki_id[i], width = 3, pad = '0')}.RDS"))
      data_nyt2 <- data_nyt %>% filter(id %in% tellinki_id[i])

      ggplot() +
        geom_smooth(aes_all(names(gg_smooth)), data=gg_smooth, stat="identity", color = "#f89406") +
        geom_line(data = data_nyt2,
                  aes(x=aika,y=bikesAvailable,group=1), color = "#007bff") +
        # geom_point(data = data_nyt2[nrow(data_nyt2),],
        #            aes(x = aika, y=bikesAvailable, group = 1),
        #            fill = "#f89406",
        #            size = 3,
        #            color = "white",
        #            shape = 21,
        #            stroke = 2) +
        geom_label(data = data_nyt2[nrow(data_nyt2),],
                   aes(x = aika,
                       y=bikesAvailable,
                       group = 1,
                       label = glue::glue("{bikesAvailable}")),
                   fill = "#007bff",
                   size = 3.5,
                   fontface = "bold",
                   color = "white",
                   family = "Roboto Mono",
                   nudge_y = 2,
                   alpha = .8) +
        labs(title = glue::glue("{unique(data_nyt2$name)} ({tellinki_id[i]})"),
             y = NULL) +

        # ggplot(cars, aes(x = speed, y = dist)) +
        # geom_point() + geom_smooth() +
        theme_ipsum_rc() +
        theme(
          plot.title = element_text(face = "bold", size = 13),
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.spacing = unit(2, "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = '#e9ecef', color = '#e9ecef'),
          plot.background = element_rect(fill = '#e9ecef', color = '#e9ecef'),
          plot.margin = margin(1, 1, 1, 1)) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,24), breaks = seq.int(from = 1, to = 24, 4)) +
        scale_y_continuous(position = "left", limits = c(0,max_y)) -> plot
      plot -> plotlist[[i]]
    }
    wrap_plots(plotlist, ncol = 2) +
      plot_annotation(
        title = glue::glue("Vapaiden pyörien määrä tänään\n aamusta kello {format(time_now, format = '%H:%M')}"),
        subtitle = glue::glue("{otsikon_alku} trendi on laskettu \nkausilta 2017-2022 kerätystä datasta\n
          "),
        theme = theme_ipsum_rc() +
          theme(panel.background = element_rect(fill = '#e9ecef'),
                plot.subtitle = element_text(color = "#f89406", size = 20),
                plot.title = element_text(color = "#007bff", size = 20, face = "plain"),
                plot.background = element_rect(fill = '#e9ecef'))
      )  -> p
    p
  })

  output$plot_output <- renderUI({

    req(input$value_list_type)

    # tellinki_id <- get_selected_tellinki_id()
    dat_tbl <- process_tellinki_list()
    tellinki_id <- dat_tbl$id


    if (length(tellinki_id) == 0){

      tagit <- '
<div class="card">
<div class="card-content card-content-padding">
<div id="saa_ennuste1" class="shiny-html-output"><b>Valitse ensin yksi, <br/>kaksi tai kaikki tellingit<br/>etusivun taulukosta</b></div>
</div>
</div>
</div>' %>% HTML()


    } else {

      height = paste0(100 + ceiling(length(tellinki_id)/2) * 150, "px")
      tagit <- shinycssloaders::withSpinner(plotOutput("plot_forecast", height = height, width = "auto"),
                                            type = 4,
                                            color = "#7a8288")
    }
    tagList(
      tagit
    )

  })

}
