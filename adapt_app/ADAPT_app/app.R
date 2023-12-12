# Shiny app for At-risk species Distribution Analysis and Data T... Tool

library(shiny)
#library(ggplot2)
library(tidyr)
#library(INLA)
#library(inlabru)
#library(flexdashboard)
library(shinydashboard)
library(leaflet)
#library(maps)
library(sf)
#library(stars)
library(scico)
library(terra)
library(tidyterra)
#library(lwgeom)
library(shinyWidgets)
library(shinycssloaders)

adapt_pts <- st_read("dat_short.shp")

#read in raster files
# beth_ssp2_2100 <- rast("beth_ssp2_2100.tif")
# beth_ssp2_2060 <- rast('beth_ssp2_2060.tif')
# beth_ssp5_2100 <- rast("beth_ssp5_2100.tif")
# beth_ssp5_2060 <- rast("beth_ssp5_2060.tif")
# beth_current <- rast("beth_current.tif")
# 
# buow_current <- rast("buow_current.tif")
# buow_ssp2_2060 <- rast("buow_ssp2_2060.tif")
# buow_ssp2_2100 <- rast("buow_ssp2_2100.tif")
# buow_ssp5_2060 <- rast("buow_ssp5_2060.tif")
# buow_ssp5_2100 <- rast("buow_ssp5_2100.tif")
# 
# lewo_current <- rast("lewo_current.tif")
# lewo_ssp2_2060 <- rast("lewo_ssp2_2060.tif")
# lewo_ssp2_2100 <- rast("lewo_ssp2_2100.tif")
# lewo_ssp5_2060 <- rast("lewo_ssp5_2060.tif")
# lewo_ssp5_2100 <- rast("lewo_ssp5_2100.tif")

#need function here to read in raster and make brick - 
 # or just need to upload bricks!

# beth_brick <- c(beth_current, 
#                 beth_ssp2_2060,
#                 beth_ssp5_2060,
#                 beth_ssp2_2100,
#                 beth_ssp5_2100)
# 
# writeRaster(beth_brick,"beth_brick.tif",overwrite=TRUE)
# 
# buow_brick <- list(current = buow_current, 
#                    mid = c(buow_ssp2_2060,
#                            buow_ssp5_2060),
#                    end = c(buow_ssp2_2100,
#                            buow_ssp5_2100))
# 
# lewo_brick <- list(current = lewo_current, 
#                    mid = c(lewo_ssp2_2060,
#                            lewo_ssp5_2060),
#                    end = c(lewo_ssp2_2100,
#                            lewo_ssp5_2100))

beth_brick <- rast("beth_brick.tif")
buow_brick <- rast("buow_brick.tif")
lewo_brick <- rast("lewo_brick.tif")

rasters_to_plot <- list(beth_brick,buow_brick,
                        lewo_brick)

source("R/species_input.R")
#source("R/ssp_input.R")
#source("R/time_input.R")

ui <- fluidPage(
  tabsetPanel(
    # Application title
    tabPanel("Predictions",
    titlePanel("Predictions of Occupancy Probability"),

    fluidRow(  
      column(
        4,
        species_input("species")
      ),
       
      column(4,
          selectizeInput("time","Scenario",
                         choices = list(
                           Current = list(`Current` = 1),
                           SSP2 = list(`2040-2060` = 2,
                                       `2080-2100` = 4),
                           SSP5 = list(`2040-2060` = 3,
                                       `2080-2100` = 5),
                           Comparison = list(
                             `SSP2 at 2060 vs 2100` = 6,
                             `SSP2 at current vs 2060` = 8,
                             `SSP2 at current vs 2100` = 7,
                             `SSP5 at 2060 vs 2100` = 9,
                             `SSP5 at current vs 2060` = 11,
                             `SSP5 at current vs 2100` = 10,
                             `SSP2 vs SSP5 at 2060` = 13,
                             `SSP2 vs SSP5 at 2100` = 12)
                         ))
             ),
              
      # column(4,
      #        conditionalPanel(
      #         condition = "input.time == 2 | input.time == 3",
      #           ssp_input("ssp")
      #         )
      #           
      #  )
      ),
  
    # fluidRow(
    #   column(4, offset = 4,
    #          checkboxInput(inputId = "compare",
    #               label = "Compare Scenarios",
    #               value = FALSE)),
      
      # column(4, 
      # 
      # conditionalPanel(
      #   condition = "input.compare == true",
      #   selectInput(input = "time_diff",
      #               label = "Time Period",
      #               choices = c("2040-2060" = 2060,
      #                           "2080-2100" = 2100)),
      #   ssp_input("ssp_diff")
      # )
      #        )),
    
    # Show a plot of the generated distribution
      fluidRow(
      column(12,
             withSpinner(
               leafletOutput(outputId = "ssp_plot"))),
        ),
      
      fluidRow(
        column(4, downloadButton("download_raster","Download Data"))
      )
    ),

#### Tab 2 ####
  tabPanel("Data Summary",
  titlePanel("Data Summary & Map"),         

    fluidRow(
      column(4,
         sliderInput("year",
                    label = "Year",
                    min = 2017, max = 2021,
                    value = c(2017,2021),
                    sep = "")
                    ),
      
      column(4,
        pickerInput(inputId = "state",
                      label = "State",
                    choices = unique(adapt_pts$ID),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE)
        ),
      column(4,
        species_input("species_plot")
        )
      ),
             
    fluidRow(
         column(12, leafletOutput("map"))
           ),
    #fluidRow(
    #     column(4, tableOutput(outputId = "year_summary")),
    #     column(4, tableOutput(outputId = "state_summary")),
    #       )
             
           ),
  tags$head(tags$style(".leaflet-top {z-index:999!important;}"))
  )
)

server <- function(input, output, session) {
  #tab 1
   
  raster_subset <- reactive({
    rasters_to_plot[[as.numeric(input$species)]][[as.numeric(input$time)]]
    })
  
  # raster_subset <- reactive({
  #   if(input$compare == TRUE){
  #     if(input$time == 1){
  #       rasters_to_plot[[as.numeric(input$species)]][[as.numeric(input$time_diff)]][[as.numeric(input$ssp_diff)]] - 
  #         rasters_to_plot[[as.numeric(input$species)]][[1]]
  #     } else {
  #     rasters_to_plot[[as.numeric(input$species)]][[as.numeric(input$time_diff)]][[as.numeric(input$ssp_diff)]] - 
  #       rasters_to_plot[[as.numeric(input$species)]][[as.numeric(input$time)]][[as.numeric(input$ssp)]]
  #   }
  #     } else {
  #   
  #   if(input$time == 1) {
  #     rasters_to_plot[[as.numeric(input$species)]][[1]]  
  #   } else {
  #     rasters_to_plot[[as.numeric(input$species)]][[as.numeric(input$time)]][[as.numeric(input$ssp)]]
  #   }
  # }})
  # 
  pal <- reactive({
    colorNumeric(
      scico(9, alpha = 0.8, palette = ifelse(as.numeric(input$time) > 5,"broc", "tokyo")),
    values(raster_subset()),
    na.color = "transparent")
  })
  
  output$ssp_plot <- renderLeaflet({
    leaflet() |>  
      addTiles() |>  
      addRasterImage(
        raster_subset(),
        colors = pal(),
        opacity = 0.8) |> 
      addLegend(
        pal = pal(),
        values = values(raster_subset()),
        title = "Probability of Occupancy")
  }) |> 
    bindCache(input$species, input$time)
  
  output$download_raster <- downloadHandler(
    filename = function() {
      paste0("output_preds_",input$species,".tif")
    },
    content = function(file) {
      writeRaster(rasters_to_plot[[as.numeric(input$species)]][[as.numeric(input$time)]], file)
    }
  )
  

#tab 2

#  year_subset <- reactive({
#    adapt_pts |>  
#      filter(year == input$year) |>  
#      filter(ID == input$state) |> 
#      filter(species == unique(species)[as.numeric(input$species_plot)])
#  })
  
  #output$year_summary <- renderTable(
  #  table(year_subset()$effort)
  #)

  only_obs <- reactive({
    req(input$state)
    req(input$year)
    req(input$species_plot)
    adapt_pts |> 
      filter(ID == c(input$state) & year == input$year & species == unique(species)[as.numeric(input$species_plot)]) |> 
      arrange(obs)
    })
  
  #output$state_summary <- renderTable(
  #  year_subset()  |>  
  #    count(obs)
  #  )
  
  output$map <- renderLeaflet({
   leaflet() |> 
      addTiles() |> 
      addCircleMarkers(
        data = only_obs(),
        radius = ~(log(effort)+4),
        color = ~ifelse(obs == 1,scico(4)[1],scico(4)[3]),
        fillOpacity = 0.5
      ) |> 
      addLegend(
        color = c(scico(4)[1],scico(4)[3]),
        #color = ~ifelse(obs ==1, scico(4)[1],scico(4)[3]),
        values = c(1,0), 
        labels = c("Present","Absent"),
        title = "Observation")
  })
  
}

shinyApp(ui = ui, server = server)

#shinyApp(ui = ui_t2, server = server_t2)

#testServer(server,{
#  session$setInputs(ID = "arizona", species = "beth", year = 2019)
#  ggplot(only_obs())
#  })
