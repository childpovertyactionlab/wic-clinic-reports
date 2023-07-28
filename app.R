#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##### Libraries #####
library(shiny)
library(bslib)
library(cpaltemplates)
library(leaflet)
library(shinyWidgets)
library(googlesheets4)
library(tidyverse)
library(tidycensus)
library(sf)
library(dotenv)

load_dot_env()
gs4_auth(email = "taylor@childpovertyactionlab.org")
census_api_key("7b32db9a1faf6b92a567a778f7763e70285eba97")

files <- list.files(path = "_WICdashboardClinic/data/scriptData/", pattern = "\\.csv$")

for (file in files) {
  read.csv(paste0("data/scriptData/", file))
}

clinics <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1pCkVgjP4VLtNDwIHYbCm7wcYOtFqaAZu2znO-N7eXK8/",
  range = "Clinic Attributes") %>%
  rename(Manager = Quadrant, lat = Latitude, long = Longitude) %>%
  mutate(Clinic = str_replace(Clinic, c("Hwy"), c("Highway"))) %>%
  select(1,3,4,8,9) %>%
  select(-Manager, Manager)

dallas <- get_acs(geography = "county",
                  variables = "B01003_001",
                  state = "TX",
                  county = "Dallas",
                  geometry = TRUE) %>%
  st_transform(., 4326)

##### Set Shiny Options #####
options(shiny.sanitize.errors = TRUE)
options(scipen = 999)

##### App Title #####
cpaltitle <- tags$a(tags$img(src = "www/images/CPAL_Logo_White.png", height = "50"),
                    strong("WIC Administrator"))


##### Javascript functions #####
jsButtonColorChange <- function(inputId, color){
  shinyjs::runjs(
    sprintf('$("#%s").css("background-color", "%s");', inputId, color)
  )
}


#### UI #####
ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = cpal_shiny(),
  navbarPage(
    title = cpaltitle,
    tabsetPanel(
      tabPanel(
        "Home",
        sidebarLayout(
          position = "left",
          sidebarPanel(
            lapply(1:nrow(clinics), function(i) {
              actionButton(inputId = paste0("btn", i), label = clinics$Clinic[i],
                           style = "background-color: lightgray;")
            })
          ),
          mainPanel(
            leafletOutput("map")
          )
        )
      ),
      tabPanel(
        "Clinic",
        fluidRow(
          column(4, selectInput("clinic_dropdown", "Select a clinic:", clinics$Clinic)),
          column(1, actionButton("clear_selection", label = icon("times-circle"),
                                 style = "background-color:transparent; border-color:transparent; color:grey;"))
        ),
        fluidRow(
          column(4, textOutput("cell11")),
          column(4, textOutput("cell12")),
          column(4, textOutput("cell13"))
        ),
        fluidRow(
          column(4, textOutput("cell21")),
          column(4, textOutput("cell22")),
          column(4, textOutput("cell23"))
        ),
        fluidRow(
          column(4, textOutput("cell31")),
          column(4, textOutput("cell32")),
          column(4, textOutput("cell33"))
        )
      )
    )
  )
)

##### Server #####
server <- function(input, output, session) {

  # ggplot2::theme_set(cpal_plot())
  # thematic::thematic_shiny(font = "auto")

  selected_clinic <- reactiveValues(clinic = NULL)

  output$map <- renderLeaflet({
      leaflet(clinics) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addAwesomeMarkers(~long, ~lat,
                          icon = awesomeIcons(
                            icon = 'medkit',
                            iconColor = "black",
                            markerColor = "lightgray"
                          ),
                          popup = ~Clinic) %>%
        addPolygons(data = dallas,
                    fillColor = ~palette_cpal_main[1],
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.3) # %>%
        # setMaxBounds(leaflet::fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)))
    })

  add_markers <- function(i) {
    leafletProxy("map") %>%
      clearMarkers() %>%
      # re-add inactive markers in grey
      addAwesomeMarkers(data = clinics[-i, ], # all but selected
                        ~long, ~lat,
                        icon = awesomeIcons(
                          icon = 'medkit',
                          iconColor = 'black',
                          markerColor = 'lightgray'
                        ),
                        popup = ~Clinic) %>%
      # add selected marker in orange
      addAwesomeMarkers(data = clinics[i, ],
                        ~long, ~lat,
                        icon = awesomeIcons(
                          icon = 'medkit',
                          iconColor = 'black',
                          markerColor = 'orange'
                        ),
                        popup = ~Clinic)
  }

  lapply(1:nrow(clinics), function(i) {
    observeEvent(input[[paste0("btn", i)]], {
      if (is.null(selected_clinic$clinic) || selected_clinic$clinic != clinics$Clinic[i]) {
        selected_clinic$clinic <- clinics$Clinic[i]
        add_markers(i)
        jsButtonColorChange(paste0("btn", i), "orange")
      } else {
        selected_clinic$clinic <- NULL
        jsButtonColorChange(paste0("btn", i), "lightgray")
      }
    })
  })

  observeEvent(input$clinic_dropdown, {
    selected_clinic$clinic <- input$clinic_dropdown
    i <- which(clinics$Clinic == input$clinic_dropdown)
    add_markers(i)

    lapply(1:nrow(clinics), function(j) {
      jsButtonColorChange(paste0("btn", j), "lightgray")
    })

    jsButtonColorChange(paste0("btn", i), "orange")
  })

  observeEvent(input$clear_selection, {
    selected_clinic$clinic <- NULL
    updateSelectInput(session, "clinic_dropdown", selected = "")
    lapply(1:nrow(clinics), function(j) {
      jsButtonColorChange(paste0("btn", j), "lightgray")
    })
  })


  observe({
    if (is.null(selected_clinic$clinic)) {
      updateSelectInput(session, "clinic_dropdown", selected = "")
      output$cell11 <- renderText({"No clinic selected"})
    } else {
      updateSelectInput(session, "clinic_dropdown", selected = selected_clinic$clinic)
    }
  })

}

# Run the application
shinyApp(ui = ui,
         server = server)
