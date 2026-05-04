# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

#for current water rights application notice review in Nevada
#csv entries only

#load relevant packages, and install if applicable
packageLoad <-
  function(x) {
    for (i in 1:length(x)) {
      if (!x[i] %in% installed.packages()) {
        install.packages(x[i])
      }
      library(x[i], character.only = TRUE)
    }
  }

#list of packages to load
packages <- c(
  "shiny",
  "readxl",
  "writexl",
  "dplyr",
  "sf",
  "stringr",
  "tidyverse"
)
packageLoad(packages)


ui <- fluidPage(
  titlePanel("Nevada CSV Data Processor"),
  
  sidebarLayout(
    sidebarPanel(
      # 1. Input: Select a file
      fileInput("upload", "Choose CSV File", accept = ".csv"),
      
      hr(),
      
      # 2. Output: Download button (hidden until data is processed)
      uiOutput("download_ui")
    ),
    
    mainPanel(
      h4("Data Preview"),
      tableOutput("preview")
    )
  )
)

server <- function(input, output) {
  
  #reactive value to store the processed data
  processed_data <- reactive({
    req(input$upload)
    
    #data must be in the state's format, unedited before unload
    
    #load the monthly water applications
    df <- read.csv(input$upload$datapath)
    
    
    #load section centroids as point shapefile
    #this is a shapefile made by using NV BLM PLSS polygon layer and 
    #using "Feature to Point" tool (inside), adding fields for latitude and longitude, and calculating geometry
    NV_Section_centroids <- sf::st_read("./PLSS_forR/NV_Section_centroids.shp") |>
      sf::st_make_valid() 
    
    #set WGS coordinate system
    NV_Section_centroids <- sf::st_transform(NV_Section_centroids, crs = 4326)
    #now check it again
    sf::st_crs(NV_Section_centroids)$epsg
    
    #set point shapefile to data frame so it can be manipulated and joined 
    NV_Section_centroids <- as.data.frame(NV_Section_centroids)
    
    
    #load recent water applications 
    NV_raw <- df
    
    #reformat the date from DD-MM to MM-DD-YYYY
    NV_raw$txtDate <- as.Date(NV_raw$txtDate, "%d-%b")
    NV_raw$txtDate <- format(NV_raw$txtDate, format = "%m/%d/%Y")
    
    #quick filtration, personalize this to project needs
    NV_edited <- NV_raw |>
      dplyr::select(-c(1:22)) |> #remove first set of columns since they don't contain data
      dplyr::filter(txtBasin >= 144) #edit this to filter the basins of interest
    
    #next code is to create a PLSSID that can be joined to the NV_Section_centroids
    
    #make the sections into uniformly double digits 
    NV_edited$txtSection <- sprintf("%02d", as.numeric(NV_edited$txtSection))
    head(NV_edited$txtSection)
    
    #insert zero into Township column
    NV_edited$txtTownship <- paste0(
      stringr::str_extract(NV_edited$txtTownship, "\\d+"), #grabs the numbers
      "0",                                        #inserts the zero
      stringr::str_extract(NV_edited$txtTownship, "[NS]")  #grabs the N or S
    )
    #check the results
    head(NV_edited$txtTownship)
    
    #insert zero into Range column 
    NV_edited$txtRange <- paste0(
      stringr::str_extract(NV_edited$txtRange, "\\d+"), #grabs the numbers
      "0",                                     #inserts the zero
      stringr::str_extract(NV_edited$txtRange, "[EW]")  #grabs the E or W
    )
    #check the results
    head(NV_edited$txtRange)
    
    #create a new column and generate the matching PLSSID
    
    #create PLSSID for SN (subdivision numbered)
    #this assumes that all sections are standard subdivisions (SN) and not protracted block (PB, sections 37+)
    NV_edited$PLSSID <- paste0("NV210", NV_edited$txtTownship, "0", 
                               NV_edited$txtRange, "0SN", NV_edited$txtSection, "0")
    head(NV_edited$PLSSID)
    
    #join water applications to BLM NV section centroids
    joined <- dplyr::left_join(NV_edited, NV_Section_centroids, by = c("PLSSID" = "FRSTDIVID"))
    
    #filter out unmatched and only keep relevant columns
    joined_SN <- joined |>
      dplyr::filter(!is.na(Lat_dd)) |>
      dplyr::select(c(1:15,33:34))
    
    #catch any results that were missed in first SN PLSSID
    #recreate the PLSSID for UP (unprotracted block)
    unmatched <- joined |>
      dplyr::filter(is.na(Lat_dd)) |>
      dplyr::select(c(1:14))
    unmatched$PLSSID <- paste0("NV210", unmatched$txtTownship, "0", 
                               unmatched$txtRange, "0UP", unmatched$txtSection, "0")
    
    #join UP PLSSID to NV_Section_centroids
    joined_UP <- dplyr::left_join(unmatched, NV_Section_centroids, by = c("PLSSID" = "FRSTDIVID"))
    joined_UP <- joined_UP |>
      dplyr::filter(!is.na(Lat_dd)) |>
      dplyr::select(c(1:15,33:34))
    
    dfs <- list(joined_SN, joined_UP)
    
    #bind matched applications (SN and UP) into one dataframe
    results <- dplyr::bind_rows(dfs)
    
    #remove inserted 0 from T and R columns
    results$txtTownship <- gsub("(\\d+)0([NS])", "\\1\\2", results$txtTownship)
    
    results$txtRange <- gsub("(\\d+)0([EW])", "\\1\\2", results$txtRange)
    
    #reformat column names to remove "txt" prefix
    colnames(results) <- gsub("txt", "", colnames(results))
    
    final_df <- results 
    
    return(final_df)
  })
  
  # Render a preview of the data
  output$preview <- renderTable({
    req(processed_data())
    head(processed_data(), 10)
  })
  
  # Only show the download button if a file has been uploaded
  output$download_ui <- renderUI({
    req(processed_data())
    downloadButton("downloadData", "Download Edited Data")
  })
  
  # 3. Handle the download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("NV_noticereview", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(processed_data(), file)
    }
  )
}

shinyApp(ui, server)
