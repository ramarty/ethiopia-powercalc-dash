# Ethiopia Power Calc Dashboard
# https://robmarty.shinyapps.io/ethiopia_powercalc_dash/

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(geodata)
library(htmltools)
library(aws.s3)

ui <- fluidPage(
  titlePanel("Ethiopia Woredas: Dashboard"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("variable_selector"),  # Placeholder for selectInput
      uiOutput("population_threshold_input"),  # Placeholder for numericInput
      verbatimTextOutput("stats"),
      verbatimTextOutput("rural_count"),
      downloadButton("download_data", "Download CSV")
    ),
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  # Load/prep data -------------------------------------------------------------
  
  ## Load AWS access keys
  api_keys <- read.csv("aws_keys.csv")
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = api_keys$AWS_ACCESS_KEY_ID,
    "AWS_SECRET_ACCESS_KEY" = api_keys$AWS_SECRET_ACCESS_KEY,
    "AWS_DEFAULT_REGION" = api_keys$AWS_DEFAULT_REGION
  )
  
  ## Define the AWS bucket and object path
  bucket_name <- "wb-eth-dashboard"
  object_key <- "persistent_data.rds"
  
  ## Load or initialize settings from AWS S3
  s3_object <- aws.s3::s3readRDS(object_key, bucket = bucket_name)
  user_settings <- s3_object
  
  ## Load your shapefile
  woredas <- readRDS("clean_data/woreda.Rds")

  # Initialize reactive values with persisted settings
  selected_ids <- reactiveVal(user_settings$selected_woredas)
  map_view <- reactiveVal(NULL)  # To save and restore map view
  
  selected_woredas <- reactive({
    woredas %>% filter(id %in% selected_ids())
  })
  
  # Dynamic UI for selectInput
  output$variable_selector <- renderUI({
    selectInput(
      "variable",
      HTML("<b>Step 1:</b> Choose variable for threshold"),
      choices = c("Population" = "pop_u", 
                  "Population Density" = "pop_density"),
      selected = user_settings$variable # Default value from user_settings
    )
  })
  
  # Dynamic UI for numericInput
  output$population_threshold_input <- renderUI({
    numericInput(
      "population_threshold",
      HTML(paste0("Step 2: ",ifelse(input$variable == "pop_u", "Population", "Population density")," threshold for urban/rural classification<br><em>Values above this number considered urban and displayed as gray:</em>")),
      value = user_settings$population_threshold, # Default value from user_settings
      min = 0
    )
  })
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    leaflet(data = woredas) %>%
      addTiles() %>%
      addPolygons(
        layerId = ~id,
        fillColor = ~ifelse(
          id %in% selected_ids(),
          "blue",  # Highlight selected woredas in blue
          ifelse(
            woredas[[ifelse(length(input$variable) == 0, "pop_u", input$variable)]] > input$population_threshold,
            "gray",  # Urban clusters in gray
            colorNumeric("YlOrRd", woredas[[input$variable]])(woredas[[input$variable]])
          )
        ),
        fillOpacity = 1,
        color = "black",
        weight = 1,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "blue",
          fillOpacity = 0.7,
          bringToFront = TRUE,
          sendToBack = TRUE
        ),
        label = ~lapply(label, HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "bold", "font-size" = "12px"),
          textOnly = FALSE,
          direction = "auto",
          html = TRUE
        )
      )
  })
  
  # Save the map view when the user moves or zooms the map
  observeEvent(input$map_bounds, {
    map_view(list(
      center = input$map_center,
      zoom = input$map_zoom
    ))
  })
  
  # Update selected woredas on map click
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click$id)) {
      ids <- selected_ids()
      if (click$id %in% ids) {
        ids <- setdiff(ids, click$id)
      } else {
        ids <- c(ids, click$id)
      }
      selected_ids(ids)
      
      # Save updated selections to AWS S3
      user_settings$selected_woredas <- ids
      user_settings$variable <- input$variable
      user_settings$population_threshold <- input$population_threshold
      aws.s3::s3saveRDS(user_settings, object = object_key, bucket = bucket_name)
    }
    
    # Restore map view after click
    if (!is.null(map_view())) {
      leafletProxy("map") %>%
        setView(lng = map_view()$center[[1]], lat = map_view()$center[[2]], zoom = map_view()$zoom)
    }
  })
  
  # Save variable and threshold changes to AWS S3
  observe({
    user_settings$selected_woredas <- selected_ids() # ids
    user_settings$variable <- input$variable
    user_settings$population_threshold <- input$population_threshold
    aws.s3::s3saveRDS(user_settings, object = object_key, bucket = bucket_name)
  })
  
  # Display statistics for selected woredas
  output$stats <- renderText({
    selected <- selected_woredas()
    if (nrow(selected) == 0) {
      return("No woredas selected.")
    }
    
    sum_population <- sum(selected$pop_u, na.rm = TRUE)
    avg_density <- mean(selected$pop_density, na.rm = TRUE)
    
    paste(
      "Selected Woredas:", nrow(selected), "\n",
      "Total Population:", round(sum_population), "\n",
      "Average Population Density:", round(avg_density, 2)
    )
  })
  
  # Display count of rural woredas
  output$rural_count <- renderText({
    threshold <- input$population_threshold
    
    var <- ifelse(length(input$variable) == 0,
                  "pop_u",
                  input$variable)
    
    rural_count <- sum(woredas[[var]] < threshold, na.rm = TRUE)
    paste("Total Rural Woredas:", rural_count)
  })
  
  # Download selected data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("woredas_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      threshold <- input$population_threshold
      ids <- selected_ids()
      
      data_to_download <- woredas %>%
        mutate(
          rural = ifelse(population < threshold, 1, 0),
          selected = ifelse(id %in% ids, 1, 0)
        ) %>%
        st_drop_geometry() %>%
        as.data.frame()
      
      write.csv(data_to_download, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
}

shinyApp(ui, server)
