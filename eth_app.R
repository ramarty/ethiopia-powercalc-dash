library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(geodata)
library(htmltools)
library(aws.s3)
library(shinyjs)
library(scales)
library(ggplot2)

# library(shinytreeview) # remotes::install_github("dreamRs/shinytreeview")

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Woreda Selection for Fyda Rollout"),
  sidebarLayout(
    sidebarPanel(
      
      HTML("<em>This tool helps identify woredas suitable for the randomised rollout of Fayda IDs. Out goal is to create a final list of woredas where implementation is feasible.</em>"),
      
      div(
        style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #f9f9f9; margin-top: 20px;",
        HTML("<b>Current Selection Summary</b>"),
        verbatimTextOutput("rural_count"),
        verbatimTextOutput("stats"),
        HTML("<b>Statistical Power Analysis</b><br><small>Minimum detectable effects with selected woredas at 80% power</small>"),
        #verbatimTextOutput("mde_stats")
        
        plotOutput("mde_plot_1", height = "100px")
        
      ),
      
      br(),
      tags$hr(style = "border: 2px solid black;"),
      
      div(
        style = "text-align: center; margin-bottom: 10px;",
        actionButton("deselect_all", "Deselect All Woredas", style = "color: red;")
      ),
      
      # # Button to show the parameter selection section
      div(
        style = "text-align: center; margin-bottom: 10px;",
        actionButton("toggle_parameters", HTML("<b>Select parameters for defining rural areas</b>"))
      ),
      
      # Hidden div for variable selector and population threshold input
      div(
        id = "parameters_section",  # ID for toggling visibility
        style = "display: none;",  # Initially hidden
        uiOutput("variable_selector"),  # Step 1
        uiOutput("population_threshold_input")  # Step 2
      ),
      
      #uiOutput("variable_selector"),  # Step 1
      #uiOutput("population_threshold_input"),  # Step 2
      
      div(
        actionButton("select_rural", "Step 3: Select all rural woredas", style = "font-weight: bold;"),
        style = "margin-bottom: 20px;" # Adds 20px vertical space below the button
      ),
      
      uiOutput("region_exclude_selector"),  # Step 4 (Regions)
      
      uiOutput("zone_exclude_selector"), # Step 4 (Zones)
      
      HTML("<b>Step 6: Manually click Woredas on map to exclude/include others</b>"),
      br(),
      br(),
      
      #hr(),
      tags$hr(style = "border: 2px solid black;"),
      
      h5("You can save and load maps and settings used to make the map"),
      
      div(
        style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
        textInput("preset_name", "Name Your Map", width = "300px"), # Use the 'width' argument
        actionButton("save_preset", "Save Map", style = "color: green;")
      ),
      
      HTML('<b>Available Maps. <span style="color:red; font-style:italic;">NOTE: You must click "Load Map" TWICE for the map to properly load.</span></b>'),
      div(
        style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
        uiOutput("preset_selector"),
        actionButton("load_preset", "Load Map", style = "color: blue; margin-bottom: 10px;"),
        actionButton("delete_preset", "Delete Map", style = "color: red;")
      ),
      
      #hr(),
      tags$hr(style = "border: 2px solid black;"),
      
      div(
        style = "text-align: center; margin-bottom: 10px;",
        downloadButton("download_data", "Download CSV")
      )
      
    ),
    mainPanel(
      leafletOutput("map", height = "600px")
    ),
    
  ),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br()
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
  
  ## Load MDEs
  mde_df <- readRDS("clean_data/mdes.Rds")
  
  ## Load your shapefile
  woredas <- readRDS("clean_data/woreda.Rds")
  
  # Initialize reactive values
  selected_ids <- reactiveVal(user_settings$selected_woredas)
  map_view <- reactiveVal(NULL)  # To save and restore map view
  #excluded_zones <- reactiveVal(user_settings$excluded_zones %||% c())
  #excluded_regions <- reactiveVal(user_settings$excluded_regions %||% c())
  
  # Dynamic UI for Step 1: Variable selector
  output$variable_selector <- renderUI({
    selectInput(
      "variable",
      HTML("<b>1.</b> Population Variable for Rural/Urban Classification"),
      choices = c("Population" = "pop_u", 
                  "Population Density" = "pop_density"),
      selected = user_settings$variable
    )
  })
  
  # Dynamic UI for Step 2: Population threshold input
  # output$population_threshold_input <- renderUI({
  #   numericInput(
  #     "population_threshold",
  #     HTML("2. Rural/Urban Classification<br><small>We suggest classifying woredas with population below 100,000 as rural</small>"),
  #     value = user_settings$population_threshold,
  #     min = 0
  #   )
  # })
  
  output$population_threshold_input <- renderUI({
    sliderInput(
      "population_threshold",
      HTML("2. Rural/Urban Classification<br><small>We suggest classifying woredas with population below 100,000 as rural</small>"),
      value = ifelse(is.null(user_settings$population_threshold),
                     100000,
                     user_settings$population_threshold),  # Default value
      min = 0,  # Minimum value for the slider
      max = 665000,  # Maximum value for the slider
      step = 1000  # Optional: Adjust step size for finer control
    )
  })
  
  output$region_exclude_selector <- renderUI({
    selectizeInput(
      "exclude_regions",
      HTML("Step 4: Region Level Exclusion<br><small>Select zones to exclude (e.g., due to security concerns)</small>"),
      choices = sort(unique(woredas$region)),
      selected = user_settings$exclude_regions,
      multiple = TRUE,
      options = list(placeholder = "Select regions to exclude")
    )
  })
  
  # Use shinyjs to toggle visibility of the parameters section
  observeEvent(input$toggle_parameters, {
    toggle("parameters_section")  # Toggles visibility of the div with ID "parameters_section"
  })
  
  # Dynamic UI for Step 4: Zone and Region Exclude Selectors
  output$zone_exclude_selector <- renderUI({
    selectizeInput(
      "exclude_zones",
      HTML("Step 5: Zone Level Exclusion<br><small>Select zones to exclude (e.g., due to security concerns)</small>"),
      choices = sort(unique(woredas$zone)),
      selected = user_settings$exclude_zones,
      multiple = TRUE,
      options = list(placeholder = "Select zones to exclude")
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
          "forestgreen",
          ifelse(
            woredas[[ifelse(length(input$variable) == 0, "pop_u", input$variable)]] > ifelse(length(input$population_threshold) == 0, 0, input$population_threshold),
            "gray",
            "yellow"
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
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("forestgreen", "yellow", "gray"),
        labels = c("Selected Rural", "Excluded Rural", "Urban"),
        title = "Legend"
      )
  })
  
  # output$map <- renderLeaflet({
  #   leaflet(data = woredas) %>%
  #     addTiles() %>%
  #     addPolygons(
  #       layerId = ~id,
  #       fillColor = ~ifelse(
  #         id %in% selected_ids(),
  #         "blue",
  #         ifelse(
  #           woredas[[ifelse(length(input$variable) == 0, "pop_u", input$variable)]] > ifelse(length(input$population_threshold) == 0, 0, input$population_threshold),
  #           "gray",
  #           colorNumeric("YlOrRd", woredas[[input$variable]])(woredas[[input$variable]])
  #         )
  #       ),
  #       fillOpacity = 1,
  #       color = "black",
  #       weight = 1,
  #       highlightOptions = highlightOptions(
  #         weight = 3,
  #         color = "blue",
  #         fillOpacity = 0.7,
  #         bringToFront = TRUE,
  #         sendToBack = TRUE
  #       ),
  #       label = ~lapply(label, HTML),
  #       labelOptions = labelOptions(
  #         style = list("font-weight" = "bold", "font-size" = "12px"),
  #         textOnly = FALSE,
  #         direction = "auto",
  #         html = TRUE
  #       )
  #     )
  # })
  
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
      "Total Population:", format(round(sum_population), big.mark = ",", scientific = FALSE), "\n",
      "Average Population Density:", round(avg_density, 2)
    )
  })
  
  # MDE Text
  output$mde_plot_1 <- renderPlot({
    selected <- woredas %>% filter(id %in% selected_ids())
    if (nrow(selected) == 0) {
      return(ggplot())
    }
    
    n_woreda <- nrow(selected)
    if(n_woreda > 850){
      n_woreda <- 850
    }
    
    mde_sel_df <- mde_df[mde_df$Cluster_Count %in% n_woreda,]
    
    ggplot() +
      geom_col(aes(y = "",
                   x = mde_sel_df$`Borrowed?` * 100),
               fill = "dodgerblue2",
               alpha = 0.3) +
      geom_text(aes(y = "",
                    x = mde_sel_df$`Borrowed?` * 100,
                    label = round(mde_sel_df$`Borrowed?` * 100, 2) %>%
                      paste0("%"))) +
      geom_vline(xintercept = 10) +
      geom_text(aes(x = 11.2,
                    y = 1.4,
                    label = "Ref. MDE")) +
      scale_x_continuous(labels = scales::percent_format(scale = 1),
                         limits = c(0,
                                    max(
                                      mde_sel_df$`Borrowed?`*100,
                                      max(mde_df$`Borrowed?`[100] * 100)
                                    )
                         )) +
      theme_classic() +
      labs(x = NULL,
           y = NULL,
           title = "Access to credit: Did you borrow?") +
      theme(plot.title = element_text(face = "bold"))
  })
  
  # MDE Figure 1
  
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
  
  
  # Define the preset path
  preset_folder <- "presets"
  
  # Save preset to S3
  observeEvent(input$save_preset, {
    if (input$preset_name != "") {
      # Save the current settings as a preset
      preset_data <- list(
        selected_woredas = selected_ids(),
        variable = input$variable,
        population_threshold = input$population_threshold,
        exclude_zones = input$exclude_zones,
        exclude_regions = input$exclude_regions
      )
      
      # Path to save the preset
      preset_path <- file.path(preset_folder, paste0(input$preset_name, ".rds"))
      aws.s3::s3saveRDS(preset_data, object = preset_path, bucket = bucket_name)
      
      # Notify user
      showNotification("Preset saved successfully!", type = "message")
      
      # Refresh the list of presets
      presets <- aws.s3::get_bucket(bucket_name, prefix = preset_folder) %>%
        purrr::map_chr(~ .x$Key) %>%
        purrr::keep(~ grepl("\\.rds$", .x)) %>%
        basename() %>%
        sub("\\.rds$", "", .)
      
      # Update the dropdown with the new list
      updateSelectInput(session, "preset_list", choices = presets)
    } else {
      showNotification("Please enter a name for the preset.", type = "error")
    }
  })
  
  
  # List available presets
  output$preset_selector <- renderUI({
    presets <- aws.s3::get_bucket(bucket_name, prefix = preset_folder) %>%
      purrr::map_chr(~ .x$Key) %>%
      purrr::keep(~ grepl("\\.rds$", .x)) %>%
      basename() %>%
      sub("\\.rds$", "", .)
    
    selectInput("preset_list", NULL, choices = presets, selected = NULL, width = "150px")
  })
  
  # Load preset from S3
  observeEvent(input$load_preset, {
    if (!is.null(input$preset_list)) {
      
      
      preset_path <- file.path(preset_folder, paste0(input$preset_list, ".rds"))
      preset_data <- aws.s3::s3readRDS(preset_path, bucket = bucket_name)
      
      # Update reactive values with loaded settings
      selected_ids(preset_data$selected_woredas)
      
      updateSelectInput(session, "variable", selected = preset_data$variable)
      #updateNumericInput(session, "population_threshold", value = preset_data$population_threshold)
      updateSliderInput(session, "population_threshold", value = preset_data$population_threshold)
      
      # Handle excluded zones and regions: Set to blank if missing
      updateSelectizeInput(
        session,
        "exclude_zones",
        selected = if (is.null(preset_data$exclude_zones)) character(0) else preset_data$exclude_zones
      )
      updateSelectizeInput(
        session,
        "exclude_regions",
        selected = if (is.null(preset_data$exclude_regions)) character(0) else preset_data$exclude_regions
      )
      
      # # Wait until inputs for zones and regions are updated before recalculating exclusions
      # isolate({
      #   observe({
      #     req(input$exclude_zones, input$exclude_regions) # Ensure inputs are updated
      #     # Update map logic here if needed, ensuring correct zones/regions are applied
      #     
      #     ids_to_exclude <- woredas %>%
      #       filter(zone %in% input$exclude_zones | region %in% input$exclude_regions) %>%
      #       pull(id)
      #     
      #     selected_ids(setdiff(selected_ids(), ids_to_exclude))
      #     
      #   })
      # })
      
      # Notify user
      showNotification("Preset loaded successfully!", type = "message")
    } else {
      showNotification("Please select a preset to load.", type = "error")
    }
  })
  
  # Delete preset from S3
  observeEvent(input$delete_preset, {
    if (!is.null(input$preset_list)) {
      # Path to the selected preset in the S3 bucket
      preset_path <- file.path(preset_folder, paste0(input$preset_list, ".rds"))
      
      # Delete the file from the S3 bucket
      delete_success <- aws.s3::delete_object(object = preset_path, bucket = bucket_name)
      
      if (delete_success) {
        # Notify user of success
        showNotification("Preset deleted successfully!", type = "message")
        
        # Refresh the list of presets
        presets <- aws.s3::get_bucket(bucket_name, prefix = preset_folder) %>%
          purrr::map_chr(~ .x$Key) %>%
          purrr::keep(~ grepl("\\.rds$", .x)) %>%
          basename() %>%
          sub("\\.rds$", "", .)
        
        # Update the dropdown with the new list
        updateSelectInput(session, "preset_list", choices = presets)
      } else {
        # Notify user of failure
        showNotification("Failed to delete the preset. Please try again.", type = "error")
      }
    } else {
      showNotification("Please select a preset to delete.", type = "error")
    }
  })
  
  
  
}

shinyApp(ui, server)
