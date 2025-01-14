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
      div(
        style = "text-align: center; margin-bottom: 10px;",
        actionButton("deselect_all", "Deselect All Woredas", style = "color: red;")
      ),
      #actionButton("deselect_all", "Deselect All Woredas", style = "margin-bottom: 10px; color: red;"),
      uiOutput("variable_selector"),  # Step 1
      uiOutput("population_threshold_input"),  # Step 2
      actionButton("select_rural", "Step 3: Select all rural woredas", style = "font-weight: bold;"),
      uiOutput("zone_exclude_selector"),  # Step 4 (Zones)
      uiOutput("region_exclude_selector"),  # Step 4 (Regions)
      verbatimTextOutput("rural_count"),
      verbatimTextOutput("stats"),
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
  
  # Initialize reactive values
  selected_ids <- reactiveVal(user_settings$selected_woredas)
  #excluded_zones <- reactiveVal(user_settings$excluded_zones %||% c())
  #excluded_regions <- reactiveVal(user_settings$excluded_regions %||% c())
  
  # Dynamic UI for Step 1: Variable selector
  output$variable_selector <- renderUI({
    selectInput(
      "variable",
      HTML("<b>Step 1:</b> Choose variable for threshold"),
      choices = c("Population" = "pop_u", 
                  "Population Density" = "pop_density"),
      selected = user_settings$variable
    )
  })
  
  # Dynamic UI for Step 2: Population threshold input
  output$population_threshold_input <- renderUI({
    numericInput(
      "population_threshold",
      HTML(paste0("Step 2: ", ifelse(input$variable == "pop_u", "Population", "Population density"), 
                  " threshold for urban/rural classification<br><em>Values above this number considered urban and displayed as gray:</em>")),
      value = user_settings$population_threshold,
      min = 0
    )
  })
  
  # Dynamic UI for Step 4: Zone and Region Exclude Selectors
  output$zone_exclude_selector <- renderUI({
    selectizeInput(
      "exclude_zones",
      "Step 4: Exclude Woredas within Zones",
      choices = sort(unique(woredas$zone)),
      selected = user_settings$exclude_zones,
      multiple = TRUE,
      options = list(placeholder = "Select zones to exclude")
    )
  })
  
  output$region_exclude_selector <- renderUI({
    selectizeInput(
      "exclude_regions",
      "Step 5: Exclude Woredas within Regions",
      choices = sort(unique(woredas$region)),
      selected = user_settings$exclude_regions,
      multiple = TRUE,
      options = list(placeholder = "Select regions to exclude")
    )
  })
  
  # Exclude woredas based on selected zones and regions
  observe({
    zones <- input$exclude_zones
    regions <- input$exclude_regions
    
    #excluded_zones(zones)
    #excluded_regions(regions)
    
    ids_to_exclude <- woredas %>%
      filter(zone %in% zones | region %in% regions) %>%
      pull(id)
    
    new_selected_ids <- setdiff(selected_ids(), ids_to_exclude)
    selected_ids(new_selected_ids)
    
    # Save updated exclusions to AWS S3
    # user_settings$excluded_zones <- input$exclude_zones
    # user_settings$excluded_regions <- input$exclude_regions
    # user_settings$selected_woredas <- new_selected_ids
    user_settings$selected_woredas <- selected_ids()
    user_settings$variable <- input$variable
    user_settings$population_threshold <- input$population_threshold
    user_settings$exclude_zones <- input$exclude_zones
    user_settings$exclude_regions <- input$exclude_regions
    aws.s3::s3saveRDS(user_settings, object = object_key, bucket = bucket_name)
  })
  
  # Map rendering logic
  output$map <- renderLeaflet({
    leaflet(data = woredas) %>%
      addTiles() %>%
      addPolygons(
        layerId = ~id,
        fillColor = ~ifelse(
          id %in% selected_ids(),
          "blue",
          ifelse(
            woredas[[ifelse(length(input$variable) == 0, "pop_u", input$variable)]] > ifelse(length(input$population_threshold) == 0, 0, input$population_threshold),
            "gray",
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
  
  # Save selected woredas to AWS S3
  observe({
    user_settings$selected_woredas <- selected_ids()
    user_settings$variable <- input$variable
    user_settings$population_threshold <- input$population_threshold
    user_settings$exclude_zones <- input$exclude_zones
    user_settings$exclude_regions <- input$exclude_regions
    aws.s3::s3saveRDS(user_settings, object = object_key, bucket = bucket_name)
  })
  
  # Statistics for selected woredas
  output$stats <- renderText({
    selected <- woredas %>% filter(id %in% selected_ids())
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
  
  # Observe button click to select all rural woredas
  observeEvent(input$select_rural, {
    # Get user-selected variable and threshold
    var <- input$variable
    threshold <- input$population_threshold
    
    # Identify rural woredas based on threshold
    rural_ids <- woredas %>%
      filter(woredas[[var]] < threshold) %>%
      pull(id)
    
    # Update selected_ids with rural woredas
    selected_ids(rural_ids)
    
    # Save updated settings to AWS S3
    #user_settings$selected_woredas <- rural_ids
    user_settings$selected_woredas <- selected_ids()
    user_settings$variable <- input$variable
    user_settings$population_threshold <- input$population_threshold
    user_settings$exclude_zones <- input$exclude_zones
    user_settings$exclude_regions <- input$exclude_regions
    aws.s3::s3saveRDS(user_settings, object = object_key, bucket = bucket_name)
  })
  
  observeEvent(input$deselect_all, {
    # Set selected_ids to an empty vector
    selected_ids(c())
    
    # Save the updated settings to AWS S3
    user_settings$selected_woredas <- selected_ids()
    user_settings$variable <- input$variable
    user_settings$population_threshold <- input$population_threshold
    user_settings$exclude_zones <- input$exclude_zones
    user_settings$exclude_regions <- input$exclude_regions
    aws.s3::s3saveRDS(user_settings, object = object_key, bucket = bucket_name)
  })
  
  
  # Count of rural woredas
  output$rural_count <- renderText({
    
    #threshold <- input$population_threshold
    
    threshold <- ifelse(length(input$population_threshold) == 0,
                  10,
                  input$population_threshold)
    
    var <- ifelse(length(input$variable) == 0,
                  "pop_u",
                  input$variable)
    
    rural_count <- woredas %>%
      dplyr::filter(woredas[[var]] < threshold) %>%
      nrow()
    
    paste("Total Rural Woredas:", rural_count)
  })
  
  # Download handler for selected data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("woredas_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      threshold <- input$population_threshold
      
      print(selected_ids())
      
      data_to_download <- woredas %>%
        dplyr::mutate(
          selected = as.numeric(id %in% selected_ids()),
          rural = ifelse(woredas[[input$variable]] < threshold, 1, 0)
        ) %>%
        st_drop_geometry() %>%
        as.data.frame()
      
      write.csv(data_to_download, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
}

shinyApp(ui, server)
