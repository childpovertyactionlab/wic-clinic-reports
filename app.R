#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##### Libraries & Setup #####
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
library(plotly)
library(scales)

readRenviron(".env")

g_auth <- Sys.getenv("g_auth")
gs4_auth(email = g_auth)

census_api <- Sys.getenv("census_api")
census_api_key(census_api)

files <- list.files(path = "data/scriptData/", pattern = "\\.csv$") %>%
  setdiff("ptcps.csv") # ignore

for (file in files) {
  file_name <- tools::file_path_sans_ext(file)
  if (file_name == "ptcp") {
    assign(file_name, read_csv(file = paste0("data/scriptData/", file)) %>%
             rename(Date = colnames(.)[1]) %>%
             mutate(Date = as.Date(Date)))
  } else {
    assign(file_name, read_csv(file = paste0("data/scriptData/", file)))
  }
}

time_gradient_palette <- colorRampPalette(c(palette_cpal_main[2], palette_cpal_main[1]))

ptcp <- ptcp %>%
  mutate(Manager = clinic_map_detail$Manager[match(Site, clinic_map_detail$Site)])

dallas <- get_acs(geography = "county",
                  variables = "B01003_001",
                  state = "TX",
                  county = "Dallas",
                  geometry = TRUE) %>%
  st_transform(., 4326)

bounds <- st_bbox(dallas) %>% as.vector()


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


### UI FUNCTIONS
homeTab <- function(clinics) {
  manager_groups <- split(clinic_map_detail, clinic_map_detail$Manager)
  select_all_button <- actionButton(inputId = "select_all", label = HTML(paste("Select All", icon("check-circle"))))
  deselect_all_button <- actionButton(inputId = "deselect_all", label = HTML(paste("Deselect All", icon("times-circle"))))

  buttons <- lapply(manager_groups, function(group) {
    manager_button <- div(actionButton(inputId = paste0("mgr", gsub("[^a-zA-Z0-9]", "", unique(group$Manager))),
                                       label = unique(group$Manager),
                                       class = "manager-btn"))
    clinic_buttons <- lapply(1:nrow(group), function(i) {
      row <- group[i,]
      div(actionButton(inputId = paste0("btn", row$Site),
                       label = row$Clinic,
                       style = "background-color: lightgray; margin-left: 15px;"))
    })
    manager_div <- div(id = paste0("div", gsub("[^a-zA-Z0-9]", "", unique(group$Manager))), class = "manager-div", manager_button, clinic_buttons)
    column(2, div(style = "margin: 5px; padding: 2px; font-size: 0.8em;", manager_div))
  })

  fluidPage(
    fluidRow(
      column(4, ""), # empty column to the left
      column(4,
             do.call(tagList, c(list(select_all_button, deselect_all_button)))
      ),
      column(4, "") # empty column to the right
    ),
    fluidRow(
      column(1, ""), # empty column to the left
      do.call(tagList, buttons),
      column(1, "") # empty column to the right
    ),
    div(class = "map-text-container",
        style = "margin-bottom: 20px;",
        fluidRow(
          column(6,
                 leafletOutput("map")
          ),
          column(6,
                 div(class = "text-container",
                     style = "padding: 10px;",
                     h4(textOutput("loading_text")),
                     h4(htmlOutput("selected_text")),
                     uiOutput("participation_output"),
                     uiOutput("vacancies_output"),
                     # p(textOutput()),
                     # p(textOutput()),
                     # p(textOutput()),
                     # p(textOutput()),
                 )
          )
        )
    ),
    uiOutput("chart_rows"),
    tags$style(HTML("
    .map-text-container {
      border: 1px solid #ccc;
      padding: 10px; background-color: #f9f9f9
      }
  "))
  )
}



### UI IMPLEMENT
ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = cpal_shiny(),
  tags$head(
    tags$style(HTML("
  .manager-btn {
    background: none !important;
    border: none !important;
    color: lightgray !important;
    padding: 0 !important;
    text-align: left !important;
    box-shadow: none !important;
  }

  .manager-btn:hover, .manager-btn-selected {
      background: none !important;
      border: none !important;
      padding: 0 !important;
      text-align: left !important;
      box-shadow: none !important;
  }

  .manager-btn:hover {
      color: white !important;
  }

  .manager-btn-selected {
      color: orange !important;
  }

  .manager-div {
      background: none !important;
      border: 1px solid transparent !important;
      padding: 10px !important;
  }

  .manager-div:hover, .manager-div-selected {
      background: none !important;
      padding: 10px !important;
  }

  .manager-div:hover {
      border: 1px solid white !important;
  }

  .manager-div-selected {
      border: 1px solid orange !important;
  }

  .subset-p {
      margin: 0px;
      padding: 0px;
      color: Grey;
      font-size: 0.9em;
  }

                    "
    ))
  ),
  navbarPage(
    title = cpaltitle,
    tabsetPanel(
      tabPanel("Home", icon = icon("table"), homeTab(clinics)),
      tabPanel("Clinic", icon = icon("shop"),
               selectInput("clinic_dropdown", "Select a clinic:", clinic_map_detail$Clinic),
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
initialize_selection <- function() {
  lapply(clinic_map_detail$Site, function(site) {
    jsButtonColorChange(paste0("btn", site), "orange")
  })

  lapply(unique(clinic_map_detail$Manager), function(mgr) {
    mgrID <- paste0("mgr", gsub("[^a-zA-Z0-9]", "", mgr))
    divID <- paste0("div", gsub("[^a-zA-Z0-9]", "", mgr))
    shinyjs::addClass(mgrID, "manager-btn-selected")
    shinyjs::addClass(divID, "manager-div-selected")
    shinyjs::removeClass(mgrID, "manager-btn")
    shinyjs::removeClass(divID, "manager-div")
  })

  all_selected_sites <- clinic_map_detail$Site
  add_markers(all_selected_sites)
}

add_markers <- function(selected_sites) {
  non_selected_sites <- setdiff(clinic_map_detail$Site, selected_sites)

  leafletProxy("map") %>%
    clearMarkers() %>%
    # add inactive markers in grey
    addAwesomeMarkers(data = clinic_map_detail[clinic_map_detail$Site %in% non_selected_sites, ],
                      ~long, ~lat,
                      icon = awesomeIcons(
                        icon = 'medkit',
                        iconColor = 'black',
                        markerColor = 'lightgray'
                      ),
                      popup = ~Clinic,
                      layerId = ~Site) %>%
    # add selected markers in orange
    addAwesomeMarkers(data = clinic_map_detail[clinic_map_detail$Site %in% selected_sites, ],
                      ~long, ~lat,
                      icon = awesomeIcons(
                        icon = 'medkit',
                        iconColor = 'black',
                        markerColor = 'orange'
                      ),
                      popup = ~Clinic,
                      layerId = ~Site)
}



server <- function(input, output, session) {

  # ggplot2::theme_set(cpal_plot())
  # thematic::thematic_shiny(font = "auto")

  loading_status <- reactiveVal(FALSE)

  selected <- reactiveValues(manager = unique(clinic_map_detail$Manager), clinics = unique(clinic_map_detail$Clinic))

  initialize_selection()

  selected_text <- reactive({
    selected_managers <- sapply(unique(clinic_map_detail$Manager), function(mgr) {
      clinics_by_manager <- clinic_map_detail$Clinic[clinic_map_detail$Manager == mgr]
      if (all(clinics_by_manager %in% selected$clinics)) return(mgr)
      return(NULL)
    })

    selected_managers <- selected_managers[!sapply(selected_managers, is.null)]
    selected_clinics <- setdiff(selected$clinics, unlist(lapply(selected_managers, function(mgr) clinic_map_detail$Clinic[clinic_map_detail$Manager == mgr])))

    if (length(selected_managers) == length(unique(clinic_map_detail$Manager))) {
      return(list(icon = icon("check-circle"), text = " All clinics in Dallas County"))
    }

    if (length(selected_clinics) == 0 & length(selected_managers) == 0) {
      return(list(icon = icon("exclamation-circle"), text = " No clinics selected"))
    }

    text <- ""

    if (length(selected_managers) > 0) {
      manager_text <- if (length(selected_managers) > 1) {
        paste(paste0(icon("user"), " ", selected_managers[-length(selected_managers)], "'s clinics"), collapse = ", ") %>%
          paste0(" and ", icon("user"), " ", selected_managers[length(selected_managers)], "'s clinics")
      } else {
        paste0(icon("user"), " ", selected_managers, "'s clinics")
      }

      if (nchar(text) > 0) {
        text <- paste0(text, " and ", manager_text)
      } else {
        text <- manager_text
      }
    }

    if (length(selected_clinics) > 0) {
      clinic_text <- if (length(selected_clinics) > 1) {
        paste(paste(icon("shop"), selected_clinics[-length(selected_clinics)], sep = " "), collapse = ", ") %>%
          paste0(" and ", icon("shop"), " ", selected_clinics[length(selected_clinics)])
      } else {
        paste(icon("shop"), " ", selected_clinics)
      }

      if (nchar(text) > 0) {
        text <- paste0(text, " and ", clinic_text)
      } else {
        text <- clinic_text
      }
    }

    return(list(icon = "", text = text))
  })

  output$participation_output <- renderUI({
    if (length(selected$clinics) > 0) {
      tagList(
        p(
          htmlOutput("participation_text"),
          div(
            style = "display: flex; align-items: center; gap: 10px;",
            tags$i(class="fa fa-chart-line", style = "color: grey;"),
            div(
              htmlOutput("participation_change_text_mo", class = "subset-p"),
              htmlOutput("participation_change_text_yr", class = "subset-p")
            )
          )
        )
      )
    } else {
      return(NULL)
    }
  })

  output$vacancies_output <- renderUI({
    data <- selected_staff_reactive()

    if (length(selected$clinics) > 0) {
      if (sum(data$Vacancies) > 0) {
        icon_element <- tags$i(class="fa fa-clipboard-user", style = "color: grey;")
      } else {
        icon_element <- NULL
      }

      tagList(
        p(
          htmlOutput("vacancies_text.total"),
          div(
            style = "display: flex; align-items: center; gap: 10px;",
            icon_element,
            div(
              htmlOutput("vacancies_text.sup", class = "subset-p"),
              htmlOutput("vacancies_text.cert", class = "subset-p"),
              htmlOutput("vacancies_text.clerk", class = "subset-p")
            )
          )
        )
      )
    } else {
      return(NULL)
    }
  })

  selected_participation <- reactive({

    selected_clinics <- selected$clinics

    if (length(selected_clinics) > 0){

      current_participation <- ptcp %>%
        filter(Clinic %in% selected_clinics, Date == max(Date)) %>%
        summarize(total = sum(Participation),
                  maxDate = max(Date))

      last_year_participation <- ptcp %>%
        filter(Clinic %in% selected_clinics,
               Date == (as.Date(current_participation$maxDate) - months(12))) %>%
        group_by(Date) %>%
        summarize(total = sum(Participation))

      last_month_participation <- ptcp %>%
        filter(Clinic %in% selected_clinics,
               Date == (as.Date(current_participation$maxDate) - months(1))) %>%
        group_by(Date) %>%
        summarize(total = sum(Participation))

      return(list(current = current_participation, lastYear = last_year_participation, lastMonth = last_month_participation))
    } else {
      return(NULL)
    }
  })

  output$selected_text <- renderUI({
    selected <- selected_text()
    HTML(paste0(selected$icon, selected$text))
  })

  output$participation_text <- renderUI({
    data <- selected_participation()

    if (!is.null(data)) {
      current = data$current$total
      month = data$current$maxDate

      return(HTML(paste0("Total ", format(month, "%B %Y"), " participants: <b>", format(current, big.mark = ","), "</b>")))
    } else {
      return(NULL)
    }
  })

  number_green_red <- function(pct_change) {

    if (pct_change >= 0) {
      scaling_factor = pct_change**(1/2) / (abs(pct_change)**(1/2) + 5)
    } else {
      scaling_factor = -((-pct_change)**(1/2) / (abs(-pct_change)**(1/2) + 5))
    }

    if (pct_change == 0) {
      return("#808080")
    } else if (pct_change > 0) {
      r_value <- 128 - (48 * scaling_factor)
      g_value <- 128 + (72 * scaling_factor)
      b_value <- 128 - (48 * scaling_factor)
    } else {
      r_value <- 128 - (127 * scaling_factor)
      g_value <- 128 + (64 * scaling_factor)
      b_value <- 128 + (64 * scaling_factor)
    }

    return(rgb(r_value, g_value, b_value, maxColorValue = 255))
  }

  output$participation_change_text_yr <- renderUI({
    data <- selected_participation()

    if (!is.null(data)) {
      current <- data$current$total
      last_year <- data$lastYear$total
      pct_change_yr <- ((current - last_year) / abs(last_year)) * 100
      color <- number_green_red(pct_change_yr)

      sign_indicator <- ifelse(round(pct_change_yr, digits = 2) > 0, "+", ifelse(round(pct_change_yr, digits = 2) < 0, "-", ""))
      return(HTML(paste0("Change from 12 months ago: <b style='color:", color, "'>", sign_indicator, format(pct_change_yr, digits = 2), "%</b>")))
    } else {
      return(NULL)
    }
  })

  output$participation_change_text_mo <- renderUI({
    data <- selected_participation()

    if (!is.null(data)) {
      current <- data$current$total
      last_month <- data$lastMonth$total
      pct_change_mo <- ((current - last_month) / abs(last_month)) * 100
      color <- number_green_red(pct_change_mo)

      sign_indicator <- ifelse(round(pct_change_mo, digits = 2) > 0, "+", ifelse(round(pct_change_mo, digits = 2) < 0, "-", ""))
      return(HTML(paste0("Change from previous month: <b style='color:", color, "'>", sign_indicator, format(abs(pct_change_mo), digits = 2), "%</b>")))
    } else {
      return(NULL)
    }
  })

  selected_staff_reactive <- reactive({
    loading_status(TRUE)
    selected_clinics <- selected$clinics

    summarize_role <- function(df, role) {
      df %>%
        filter(Location %in% selected_clinics, Role == role) %>%
        summarize(
          Vacancies = sum(Vacant == "TRUE", na.rm = TRUE),
          Temps = sum(`In training` == "TRUE" | `Temp` == "TRUE" | `Part-time` == "TRUE" | `On leave` == "TRUE", na.rm = TRUE),
          `WCS trainees` = sum(`WCS trainee` == "TRUE", na.rm = TRUE),
          Active = n() - Vacancies - Temps - `WCS trainees`,
          Role = role
        )
    }

    if (length(selected_clinics) > 0) {
      roles <- c("clerk", "certifier", "supervisor")
      staff_count <- purrr::map_dfr(roles, function(role) {
        summarize_role(df = staff, role = role)
      })
      staff_count$Role <- tools::toTitleCase(staff_count$Role)
    } else {
      staff_count <- NULL
    }

    loading_status(FALSE)
    return(staff_count)
  })


  output$vacancies_text.total <- renderUI({
    data <- selected_staff_reactive()
    if (!is.null(data)) {
      total <- sum(data$Vacancies)
      color <- number_green_red(-total)
      HTML(paste0("Total vacancies: <b style='color:", color, "'>", total, "</b>"))
    } else {
      return(NULL)
    }
  })

  output$vacancies_text.sup <- renderUI({
    data <- selected_staff_reactive()
    if (!is.null(data) && sum(data$Vacancies) > 0) {
      sup <- data %>% filter(Role == "Supervisor") %>% pull(Vacancies) %>% sum()
      if (!is.null(sup)) {
        return(HTML(paste0("Supervisor vacancies: <b>", sup, "</b>")))
      }
    }
    return(NULL)
  })

  output$vacancies_text.cert <- renderUI({
    data <- selected_staff_reactive()
    if (!is.null(data) && sum(data$Vacancies) > 0) {
      cert <- data %>% filter(Role == "Certifier") %>% pull(Vacancies) %>% sum()
      if (!is.null(cert)) {
        return(HTML(paste0("Certifier vacancies: <b>", cert, "</b>")))
      }
    }
    return(NULL)
  })

  output$vacancies_text.clerk <- renderText({
    data <- selected_staff_reactive()
    if (!is.null(data) && sum(data$Vacancies) > 0) {
      clerk <- data %>% filter(Role == "Clerk") %>% pull(Vacancies) %>% sum()
      if (!is.null(clerk)) {
        return(HTML(paste0("Clerk vacancies: <b>", clerk, "</b>")))
      }
    }
    return(NULL)
  })

  # output$milestones_text <- renderText({
  #   selected_milestones()
  # })
  #
  #
  # output$new_hires_text <- renderText({
  #   selected_new_hires()
  # })

  output$map <- renderLeaflet({
    leaflet(clinic_map_detail) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addAwesomeMarkers(~long, ~lat,
                        icon = awesomeIcons(
                          icon = 'medkit',
                          iconColor = "black",
                          markerColor = "lightgray"
                        ),
                        label = ~Clinic,
                        layerId = ~Clinic) %>%
      addPolygons(data = dallas,
                  fillColor = ~palette_cpal_main[1],
                  weight = 2,
                  opacity = 0,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.3) %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      setMaxBounds(bounds[1] - 0.1, bounds[2] - 0.1, bounds[3] + 0.1, bounds[4] + 0.1)
  })

  observe({
    selected_sites <- clinic_map_detail$Site[clinic_map_detail$Clinic %in% selected$clinics]
    add_markers(selected_sites)
  })

  output$chart_rows <- renderUI({
    if (length(selected$clinics) > 0) {
      tagList(
        fluidRow(
          column(6,
                 tags$h3(style = "text-align: center; font-size: 20px;", "Total Participation Across Clinics"),
                 hr(style = "border-top: 2px solid #333; margin: 5px 0;"),
                 plotlyOutput("ptcp_chart"),
                 checkboxInput("individualView", "Individual clinic view", value = FALSE)
          ),
          column(6,
                 tags$h3(style = "text-align: center; font-size: 20px;", "Demographics"),
                 hr(style = "border-top: 2px solid #333; margin: 5px 0;"),
                 selectInput("demographicSelector", "",
                             choices = c("Client category", "Preferred language", "Years using WIC", "Years family using WIC", "Cert. status", "ZIP code")),
                 plotlyOutput("demographics")
          )
        ),
        fluidRow(
          column(6,
                 tags$h3(style = "text-align: center; font-size: 20px;", "Overall POE Fulfillment by Phase"),
                 hr(style = "border-top: 2px solid #333; margin: 5px 0;"),
                 plotlyOutput("POEarea")
          ),
          column(6,
                 tags$h3(style = "text-align: center; font-size: 20px;", "Staffing and Vacancies Across Clinics"),
                 hr(style = "border-top: 2px solid #333; margin: 5px 0;"),
                 plotlyOutput("staffchart")
          )
        )
      )
    } else {
      return(NULL)
    }
  })

  selected_ptcp_reactive <- reactive({
    if (length(selected$clinics) > 0) {
      ptcp %>%
        filter(Clinic %in% selected$clinics)
    } else {
      ptcp
    }
  })

  output$ptcp_chart <- renderPlotly({
    manager_colors <- setNames(palette_cpal_main, unique(clinic_map_detail$Manager))

    clinic_ptcp <- selected_ptcp_reactive()

    if (input$individualView) {
      clinic_ptcp_select <- clinic_ptcp %>%
        group_by(Clinic, Date) %>%
        summarise(Participation = sum(Participation))

      plot <- plot_ly(data = clinic_ptcp_select, x = ~Date, y = ~Participation, color = ~Clinic, type = "scatter", mode = "lines")
      max_ptcp <- max(clinic_ptcp_select$Participation)

    } else {
      plot <- plot_ly()

      if (length(unique(clinic_map_detail$Clinic)) > length(selected$clinics) && length(selected$clinics) > 0) {
        plot <- add_trace(plot,
                          data = clinic_ptcp,
                          x = ~Date,
                          y = ~Participation,
                          color = ~Manager,
                          colors = manager_colors,
                          type = "scatter",
                          mode = "lines",
                          legendgroup = ~Clinic,
                          name = ~Clinic,
                          line = list(dash = "dash")
                          )
        max_ptcp <- max(clinic_ptcp$Participation)
      }

      if (length(selected$manager) > 0) {
        manager_ptcp <- clinic_ptcp %>%
          group_by(Manager, Date) %>%
          summarise(Participation = sum(Participation))

        plot <- add_trace(plot,
                          data = manager_ptcp,
                          x = ~Date,
                          y = ~Participation,
                          color = ~Manager,
                          colors = manager_colors,
                          type = "scatter",
                          mode = "lines")
        max_ptcp <- max(manager_ptcp$Participation)
      }

      if (length(selected$clinics) == length(unique(clinic_map_detail$Clinic))) {
        overall_ptcp <- clinic_ptcp %>%
          group_by(Date) %>%
          summarise(Participation = sum(Participation))

        plot <- add_trace(plot,
                          name = 'Greater Dallas total',
                          data = overall_ptcp,
                          x = ~Date,
                          y = ~Participation,
                          line = list(color = "#2f2f2f", width = 2, dash = "solid", bold = TRUE),
                          type = "scatter",
                          mode = "lines"
                          )
        max_ptcp <- max(overall_ptcp$Participation)
      }
    }

    if (max_ptcp < 10000) {
      max_ptcp <- 10000
    } else {
      max_ptcp <- max_ptcp + 2000
    }

    plot <- plot %>%
      layout(plot_bgcolor = "rgba(0,0,0,0)",
             paper_bgcolor = "rgba(0,0,0,0)",
             title = "",
             yaxis = list(range = c(0, max_ptcp), title = ""),
             xaxis = list(title = "")
             ) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c(
               'sendDataToCloud', 'autoScale2d', 'resetScale2d', 'toggleSpikelines',
               'hoverClosestCartesian', 'hoverCompareCartesian',
               'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d'
             ))

    if (length(selected$clinics) > 0) {
      return(plot)
    }
  })

  selected_demographics <- reactive({

    process_data <- function(df, name_col) {
      df <- df %>%
        filter(Site %in% selected_sites) %>%
        pivot_longer(cols = -Site,
                     names_to = name_col,
                     values_to = "Num. Participants") %>%
        filter(`Num. Participants` != 0) %>%
        group_by(Site) %>%
        mutate(total = sum(`Num. Participants`)) %>%
        ungroup()

      if(name_col == "Preferred language" || name_col == "ZIP code"){
        df <- df %>%
          mutate_at(vars(name_col),
                    ~ifelse((`Num. Participants` / total < 0.01), "other", .))
      }


      if(name_col == "Years using WIC" || name_col == "Years family using WIC"){
        df <- df %>%
          mutate_at(vars(name_col),
                    ~ case_when(
                      . %in% c("5 years", "6 years", "7 years", "8 years", "9 years", "10 years") ~ "5 to 10 years",
                      as.numeric(gsub(" years", "", .)) > 10 ~ "More than 10 years",
                      TRUE ~ .
                    ))
        year_levels <- c("Less than 1 year", "1 year", "2 years", "3 years", "4 years", "5 to 10 years", "More than 10 years")

        df[[name_col]] <- factor(df[[name_col]], levels = year_levels, ordered = TRUE)
      }

      df <- df %>%
        group_by(Site, !!sym(name_col)) %>%
        summarise(`Num. Participants` = sum(`Num. Participants`))

      return(df)
    }

    selected_clinics <- selected$clinics

    selected_sites <- selected$clinics %>%
      map(~ as.character(clinic_map_detail$Site[clinic_map_detail$Clinic == .]))

    list(
      categories      = process_data(client_categories, "Client category"),
      languages       = process_data(client_languages, "Preferred language"),
      duration        = process_data(client_duration, "Years using WIC"),
      familyduration  = process_data(client_familyduration, "Years family using WIC"),
      status          = process_data(client_status, "Cert. status"),
      zip             = process_data(client_zip, "ZIP code")
    )
  })

  output$demographics <- renderPlotly({
    selected_data <- switch(input$demographicSelector,
                            "Client category" = selected_demographics()$categories,
                            "Preferred language" = selected_demographics()$languages,
                            "Years using WIC" = selected_demographics()$duration,
                            "Years family using WIC" = selected_demographics()$familyduration,
                            "Cert. status" = selected_demographics()$status,
                            "ZIP code" = selected_demographics()$zip)

    if (input$demographicSelector %in% c("Years using WIC", "Years family using WIC")) {
      unique_levels <- unique(selected_data[[input$demographicSelector]])
      color_palette <- time_gradient_palette(length(unique_levels))
      names(color_palette) <- unique_levels
    } else {
      color_palette <- palette_cpal_main
    }

    plot_ly(data = selected_data,
            ids = ~Site,
            labels = ~get(input$demographicSelector),
            values = ~`Num. Participants`,
            type = "pie",
            marker = list(colors = color_palette[as.character(selected_data[[input$demographicSelector]])])) %>%
      layout(title = "",
             plot_bgcolor = "rgba(0,0,0,0)",
             paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c(
               'sendDataToCloud', 'autoScale2d', 'resetScale2d', 'toggleSpikelines',
               'hoverClosestCartesian', 'hoverCompareCartesian',
               'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d'
             ))
  })

  selected_POE_reactive <- reactive({
    loading_status(TRUE)


    selected_clinics <- selected$clinics

    if (length(selected_clinics > 0)) {
      clinic_POE <- df %>%
        mutate(`# of Families Waiting` = 1 / exp(`# of Families Waiting` / 8)) %>%
        group_by(Clinic) %>%
        replace(is.na(.), 0) %>%
        pivot_longer(cols = -c(1:4), names_to = "variable", values_to = "score") %>%
        left_join(POE, by = "variable") %>%
        group_by(Clinic, Date, phase) %>%
        summarise(score = mean(score, na.rm = TRUE)) %>%
        pivot_wider(names_from = phase, values_from = score) %>%
        rename_with(~ifelse(grepl("^\\d+$", .), paste0("phase", .), .), -c(Clinic, Date)) %>%
        filter(Clinic %in% selected_clinics) %>%
        group_by(Clinic) %>%
        complete(Date = full_seq(Date, period = 1), fill = list(score = NA)) %>%
        fill(starts_with("phase"), .direction = "down") %>%
        ungroup() %>%
        group_by(Date) %>%
        summarize(across(starts_with("phase"), \(x) mean(x, na.rm = TRUE)))
    } else {
      clinic_POE <- NULL
    }
    loading_status(FALSE)
    return(clinic_POE)
  })

  output$POEarea <- renderPlotly({
    loading_status(TRUE)


    phase_cols <- grep("^phase", names(selected_POE_reactive()), value = TRUE)

    POEarea <- NULL

    if (length(selected$clinics > 0)) {
      for(i in seq_along(phase_cols)) {
        legend_name = sub("phase", "Phase ", phase_cols[i])
        if (is.null(POEarea)){
          POEarea <- plot_ly(selected_POE_reactive(),
                             x = ~Date,
                             y = as.formula(paste0("~`", phase_cols[i], "`")),
                             name = legend_name,
                             fill = 'tozeroy',
                             fillcolor = palette_cpal_main[phase_cols[i]],
                             type = 'scatter',
                             mode = 'none')
        } else {
          POEarea <- POEarea %>%
            add_trace(y = as.formula(paste0("~`", phase_cols[i], "`")),
                      name = legend_name,
                      fill = 'tozeroy',
                      fillcolor = palette_cpal_main[phase_cols[i]])
        }
      }

      start_date <- min(selected_POE_reactive()$Date)
      end_date <- max(selected_POE_reactive()$Date)

      POEarea <- POEarea %>%
        layout(title = "",
               plot_bgcolor = "rgba(0,0,0,0)",
               paper_bgcolor = "rgba(0,0,0,0)",
               xaxis = list(title = "",
                            tickmode = "array",
                            tickvals = seq(from = as.Date(start_date), to = as.Date(end_date), by = "month"),
                            ticktext = format(seq(from = as.Date(start_date), to = as.Date(end_date), by = "month"), "%b-%Y"),
                            tickcolor = "black",
                            showgrid = TRUE,
                            gridcolor = "lightgrey",
                            gridwidth = 1
               ),
               yaxis = list(title = "",
                            tickformat = "0%",
                            gridcolor = "lightgrey",
                            range = c(0, 1))) %>%
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c(
                 'sendDataToCloud', 'autoScale2d', 'resetScale2d', 'toggleSpikelines',
                 'hoverClosestCartesian', 'hoverCompareCartesian',
                 'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d'
               ))
    }
    loading_status(FALSE)
    return(POEarea)
  })

  output$staffchart <- renderPlotly({
    loading_status(TRUE)

    if (length(selected$clinics > 0)) {
      staffchart <- plot_ly(data = selected_staff_reactive(),
                            x = ~Active,
                            y = ~Role,
                            name = "Active",
                            marker = list(color = cpaltemplates::palette_cpal_teal[6]),
                            type = 'bar',
                            orientation = 'h',
                            hovertemplate = paste("Role: %{y}<br>",
                                                  "Active: %{x}"),
                            text = ~Active,
                            textposition = 'auto') %>%
        add_trace(x = ~Temps,
                  y = ~Role,
                  name = "Temporary/inactive",
                  marker = list(color = cpaltemplates::palette_cpal_teal[4]),
                  hovertemplate = paste("Role: %{y}<br>",
                                        "Temporary/Inactive: %{x}"),
                  text = ~Temps,
                  textposition = 'auto') %>%
        add_trace(x = ~Vacancies,
                  y = ~Role,
                  name = "Vacant",
                  marker = list(color = cpaltemplates::palette_cpal_teal[2]),
                  hovertemplate = paste("Role: %{y}<br>",
                                        "Vacancies: %{x}"),
                  text = ~Vacancies,
                  textposition = 'auto') %>%
        layout(plot_bgcolor = "rgba(0,0,0,0)",
               paper_bgcolor = "rgba(0,0,0,0)",
               title = "",
               yaxis = list(title = "",
                            categoryorder = "array",
                            categoryarray = c("Clerk", "Certifier", "Supervisor"),
                            automargin = TRUE,
                            tickfont = list(size = 12),
                            ticksuffix = "   "),  # add 3 spaces after y-label
               xaxis = list(title = "",
                            tickangle = 0,
                            tickmode = 'auto',
                            tick0 = 0,
                            dtick = 1,
                            gridcolor = "lightgrey",
                            gridwidth = 1),
               barmode = 'stack') %>%
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c(
                 'sendDataToCloud', 'autoScale2d', 'resetScale2d', 'toggleSpikelines',
                 'hoverClosestCartesian', 'hoverCompareCartesian',
                 'zoom2d','pan2d','select2d','lasso2d','zoomIn2d','zoomOut2d'
               ))
    } else {
      staffchart <- NULL
    }

    loading_status(FALSE)
    return(staffchart)
  })


  observeEvent(input$select_all, {
    selected$clinics <- unique(clinic_map_detail$Clinic)

    lapply(clinic_map_detail$Site, function(site) {
      updateActionButton(session, paste0("btn", site))
      jsButtonColorChange(paste0("btn", site), "orange")
    })

    lapply(unique(clinic_map_detail$Manager), function(mgr) {
      mgrID <- paste0("mgr", gsub("[^a-zA-Z0-9]", "", mgr))
      divID <- paste0("div", gsub("[^a-zA-Z0-9]", "", mgr))
      shinyjs::addClass(mgrID, "manager-btn-selected")
      shinyjs::addClass(divID, "manager-div-selected")
      shinyjs::removeClass(mgrID, "manager-btn")
      shinyjs::removeClass(divID, "manager-div")
    })
  })


  observeEvent(input$deselect_all, {
    selected$clinics <- NULL

    lapply(clinic_map_detail$Site, function(site) {
      updateActionButton(session, paste0("btn", site))
      jsButtonColorChange(paste0("btn", site), "lightgray")
    })

    lapply(unique(clinic_map_detail$Manager), function(mgr) {
      mgrID <- paste0("mgr", gsub("[^a-zA-Z0-9]", "", mgr))
      divID <- paste0("div", gsub("[^a-zA-Z0-9]", "", mgr))
      shinyjs::removeClass(mgrID, "manager-btn-selected")
      shinyjs::removeClass(divID, "manager-div-selected")
      shinyjs::addClass(mgrID, "manager-btn")
      shinyjs::addClass(divID, "manager-div")
    })
  })

  update_clinic_button_color <- function(clinic_site) {
    if (clinic_map_detail$Clinic[clinic_map_detail$Site == clinic_site] %in% selected$clinics) {
      jsButtonColorChange(paste0("btn", clinic_site), "orange")
    } else {
      jsButtonColorChange(paste0("btn", clinic_site), "lightgray")
    }
  }

  are_all_clinics_selected_for_manager <- function(manager) {
    clinics <- clinic_map_detail$Clinic[clinic_map_detail$Manager == manager]
    all(clinics %in% selected$clinics)
  }

  update_manager_div_button <- function(manager) {
    mgrID <- paste0("mgr", gsub("[^a-zA-Z0-9]", "", manager))
    divID <- paste0("div", gsub("[^a-zA-Z0-9]", "", manager))
    if (are_all_clinics_selected_for_manager(manager)) {
      shinyjs::addClass(mgrID, "manager-btn-selected")
      shinyjs::addClass(divID, "manager-div-selected")
      shinyjs::removeClass(mgrID, "manager-btn")
      shinyjs::removeClass(divID, "manager-div")
    } else {
      shinyjs::removeClass(mgrID, "manager-btn-selected")
      shinyjs::removeClass(divID, "manager-div-selected")
      shinyjs::addClass(mgrID, "manager-btn")
      shinyjs::addClass(divID, "manager-div")
    }
  }


  toggle_selection <- function(manager = NULL, clinic_site = NULL) {
    if (!is.null(manager)) {
      clinics <- clinic_map_detail$Clinic[clinic_map_detail$Manager == manager]

      if (manager %in% selected$managers) {
        selected$managers <- setdiff(selected$managers, manager)
        selected$clinics <- setdiff(selected$clinics, clinics)
      } else {
        selected$managers <- c(selected$managers, manager)
        selected$clinics <- unique(c(selected$clinics, clinics))
      }
    }

    if (!is.null(clinic_site)) {
      clinic_name <- clinic_map_detail$Clinic[clinic_map_detail$Site == clinic_site]
      if (clinic_name %in% selected$clinics) {
        selected$clinics <- setdiff(selected$clinics, clinic_name)
      } else {
        selected$clinics <- c(selected$clinics, clinic_name)
      }
      current_manager <- clinic_map_detail$Manager[clinic_map_detail$Site == clinic_site]
      update_manager_div_button(current_manager)
      clinics_under_manager <- clinic_map_detail$Site[clinic_map_detail$Manager == manager]
      lapply(clinics_under_manager, function(site) {
        update_clinic_button_color(site)
      })
    }
  }

  lapply(clinic_map_detail$Site, function(site) {
    observeEvent(input[[paste0("btn", site)]], {
      toggle_selection(clinic_site = site)
      selected_sites <- clinic_map_detail$Site[clinic_map_detail$Clinic %in% selected$clinics]
      add_markers(selected_sites)

      update_clinic_button_color(clinic_site = site)
      update_manager_div_button(clinic_map_detail$Manager[clinic_map_detail$Site == site])
    })
  })


  observeEvent(input$map_marker_click, {
    clicked_clinic_site <- as.numeric(input$map_marker_click$id)
    toggle_selection(clinic_site = clicked_clinic_site)
    selected_sites <- clinic_map_detail$Site[clinic_map_detail$Clinic %in% selected$clinics]
    add_markers(selected_sites)

    update_clinic_button_color(clicked_clinic_site)
    update_manager_div_button(clinic_map_detail$Manager[clinic_map_detail$Site == clicked_clinic_site])
  })


  lapply(unique(clinic_map_detail$Manager), function(mgr) {
    mgrID <- paste0("mgr", gsub("[^a-zA-Z0-9]", "", mgr))
    observeEvent(input[[mgrID]], {
      toggle_selection(manager = mgr)
      selected_sites <- clinic_map_detail$Site[clinic_map_detail$Clinic %in% selected$clinics]
      add_markers(selected_sites)

      update_manager_div_button(mgr)
      clinic_sites <- clinic_map_detail$Site[clinic_map_detail$Manager == mgr]
      lapply(clinic_sites, function(site) {
        update_clinic_button_color(site)
      })
    })
  })

}

# Run the application
shinyApp(ui = ui,
         server = server)
