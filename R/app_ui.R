#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    fluidPage(
      theme = bslib::bs_theme(version = 5,
                              base_font = bslib::font_google("Roboto Mono"),
                              heading_font = bslib::font_google("Sedgwick Ave"),
                              bootswatch = "slate"),
      tags$div(class = "container", style = "margin:auto;",
               tags$div(class = "row",
                        tags$div(class = "col-lg-3"),
                        tags$div(class = "col-lg-6",
                 h1("Tellinkiäppi",
                    tags$img(src = "https://kaupunkifillarit.fi/img/kaupunkifillarit-logo.svg", height = "40px", class = "filter-green", style = "filter: drop-shadow(2px 3px 1px rgb(0 0 0 / 0.4));")),
                 tags$p(class = "lead", style = "font-family: Sedgwick Ave;", "Älä ole se joka kävelee!"),
                 tags$br(),
                 # tags$div(style = "background-color: #3a3f44; padding-bottom: 10px;",
                 fluidRow(
                   column(6,
                          uiOutput("ui_view_mode")
                   ),
                   column(6,
                          # uiOutput("ui_location_distance"),
                          uiOutput("ui_tellinkihaku")
                   )
                 # )
                 ),
                 # verbatimTextOutput("lat"),
                 # verbatimTextOutput("long"),
                 # verbatimTextOutput("location"),
                 tags$div(style = "padding-bottom: 20px;"),
                 tabsetPanel(type = "tabs", footer = tags$div(style = "padding-bottom: 50px;"),
                             tabPanel("Taulukko", tableOutput("tbl_select2")), #DT::dataTableOutput("tbl_select")),
                             tabPanel("Kartta", leaflet::leafletOutput("map_realtime", height = "600px")),
                             tabPanel("Trendi",  uiOutput("plot_output"))
                 ),
                 bookmarkButton(label = "Tallenna valinnat!"),
                 tags$hr(),
                 tags$h2("Tervetuloa Tellinkiappiin!"),
                 tags$html(HTML("<div class='container'>
                           <p>Sovelluksen avulla voi selata pääkaupunkiseudun kaupunkifillarijärjestelmän telineiden reaaliaikaisia tietoja ja ennusteita.</p>
                           <p>Telinehaussa voit määrätä etäisyyden, kuinka kaukaa telineet otetaan huomioon</p>
                           <ul>
                           <li><a href = 'https://github.com/muuankarski/tellinkiappi'>lähdekoodi Githubissa</a></li>
                           <li><a href = 'https://markuskainu.fi'>markuskainu.fi</a></li>
                           </ul>
                           </div>")),
                 tags$div(style = "padding-bottom: 50px;")
    ),
    tags$div(class = "col-lg-3")
    )))
  )
}
#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$head(
    tags$style('
.selectize-dropdown, .selectize-input, .selectize-input input {
    color: #FFF!important;
    font-family: inherit;
    font-size: inherit;
    line-height: 1.5;
    -webkit-font-smoothing: inherit;
}

    .filter-green{
        filter: invert(48%) sepia(79%) saturate(2476%) hue-rotate(86deg) brightness(118%) contrast(119%);
    }

               '),
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "tellinkiappi"
    )
  )
}
