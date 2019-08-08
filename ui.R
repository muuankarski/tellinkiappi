## ui.R ##
function(request) {
# shinyUI(  
  fluidPage(
    # titlePanel("Tellinkiappi"),
    theme = shinythemes::shinytheme(theme = "cosmo"),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
  sidebarLayout(
    sidebarPanel(
      selectInput("tellinki", 
                  # label = NULL,
                  label = "Valitse tellinki",
                  choices = tellingit, 
                  selected = "082"),
      sliderInput("distance", "Etäisyys (m)", min = 0, max = 25000,
      step = 500, value = 1000)
      ,shiny::bookmarkButton(label = "Tallenna valinnat")

  ),
    mainPanel(
      fluidRow(
        tabsetPanel(
          tabPanel("Listaus"
                   ,uiOutput("tbl_realtime")

          ),
          tabPanel("Kartta"
                   ,leafletOutput("map_realtime")
          ),
          tabPanel("Trendi"
                   ,shinycssloaders::withSpinner(plotOutput("plot_forecast", height = "400", width = "auto"))
          ),
          tabPanel("Sää",
                   tags$h4("Tällä hetkellä:")
                   ,shinycssloaders::withSpinner(tableOutput("saa_realtime"))
                   ,tags$h4("Ennuste Helsinki:")
                   ,shinycssloaders::withSpinner(tableOutput("saa_ennuste"))
          ),
          tabPanel("Info"
                   ,tags$br()
                   ,tags$p("Tellinkiappi on tarkoitettu 'vakitellinkien' seuraamiseen. Valitse tellinki ja tallenna urli suosikkeihin.")
                   ,tags$a("Lähetä sähköpostia kehittäjälle!", href = 'mailto:markus.kainu@kapsi.fi?subject=Tellinkiappi')
          )
        )
        
      )

      )
  ))
}
# )
