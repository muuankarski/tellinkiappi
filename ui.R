## ui.R ##
function(request) {
  
  dashboardPage(
    dashboardHeader(
      title = "Tellinkiappi"),
      dashboardSidebar(
      selectInput("tellinki", "Valitse tellinki", choices = tellingit, selected = 85),
      sliderInput("distance", "Montako metriä jaksat kävellä", min = 0, max = 1500,
      step = 100, value = 700)
      # ,selectInput("ajankohta", "Valitse ajankohta", choices = c("nyt", "mennyt"))
      ,shiny::bookmarkButton(label = "Jaa valinnat")

  ),
    dashboardBody(
      fluidRow(
      #   ),
      # fluidRow(
      #   selectInput("tellinki", "Valitse tellinki", choices = tellingit, selected = 85),
      #   sliderInput("distance", "Montako metriä jaksat kävellä", min = 0, max = 1500, 
      #               step = 100, value = 700)
      #   # ,selectInput("ajankohta", "Valitse ajankohta", choices = c("nyt", "mennyt"))
      #   ,shiny::bookmarkButton(label = "Jaa valinnat"),
        tabsetPanel(
          tabPanel("Vapaana"
                   # ,tags$h4("Pyöriä vapaana:")
                   ,tableOutput("tbl_realtime")

          ),
          tabPanel("Kartta"
                   # ,tags$h4("Pyöriä vapaana:")
                   # ,valueBoxOutput("box_realtime")
                   ,leafletOutput("map_realtime")
          ),
          tabPanel("Ennuste"
                   # ,tags$h4("Lineraarinen regressio:")
                   # ,valueBoxOutput("box_realtime")
                   # ,tableOutput("plot_forecast")
                   ,plotOutput("plot_forecast", height = "400", width = "auto")
          ),
          tabPanel("Sää",
                   tags$h4("Tällä hetkellä:")
                   ,tableOutput("saa_realtime")
                   ,tags$h4("Ennuste Helsinki:")
                   ,tableOutput("saa_ennuste")
          )
        )
        
      )

      )
  )
}

