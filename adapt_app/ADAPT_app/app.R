# Shiny app for At-risk species Distribution Analysis and Data T... Tool

library(shiny)
#library(ggplot2)
library(tidyr)
#library(INLA)
#library(inlabru)
#library(flexdashboard)
library(shinydashboard)
library(leaflet)
library(maps)
library(sf)
#library(stars)
library(scico)
library(terra)
library(tidyterra)
#library(lwgeom)
library(shinyWidgets)
library(shinycssloaders)
library(cachem)

adapt_pts <- st_read("dat_short.shp")

#read in raster files
beth_ssp2_2100 <- rast("beth_ssp2_2100.tif")
beth_ssp2_2060 <- rast('beth_ssp2_2060.tif')
beth_ssp5_2100 <- rast("beth_ssp5_2100.tif")
beth_ssp5_2060 <- rast("beth_ssp5_2060.tif")
beth_current <- rast("beth_current.tif")

buow_current <- rast("buow_current.tif")
buow_ssp2_2060 <- rast("buow_ssp2_2060.tif")
buow_ssp2_2100 <- rast("buow_ssp2_2100.tif")
buow_ssp5_2060 <- rast("buow_ssp5_2060.tif")
buow_ssp5_2100 <- rast("buow_ssp5_2100.tif")

lewo_current <- rast("lewo_current.tif")
lewo_ssp2_2060 <- rast("lewo_ssp2_2060.tif")
lewo_ssp2_2100 <- rast("lewo_ssp2_2100.tif")
lewo_ssp5_2060 <- rast("lewo_ssp5_2060.tif")
lewo_ssp5_2100 <- rast("lewo_ssp5_2100.tif")

#need function here to read in raster and make brick - 
 # or just need to upload bricks!

beth_brick <- list(current = beth_current, 
                mid = c(beth_ssp2_2060,
                        beth_ssp5_2060),
                end = c(beth_ssp2_2100,
                        beth_ssp5_2100)
)

buow_brick <- list(current = buow_current, 
                   mid = c(buow_ssp2_2060,
                           buow_ssp5_2060),
                   end = c(buow_ssp2_2100,
                           buow_ssp5_2100))

lewo_brick <- list(current = lewo_current, 
                   mid = c(lewo_ssp2_2060,
                           lewo_ssp5_2060),
                   end = c(lewo_ssp2_2100,
                           lewo_ssp5_2100))

rasters_to_plot <- list(beth = beth_brick,
                        buow = buow_brick,
                        lewo = lewo_brick)

source("R/species_input.R")
source("R/ssp_input.R")
source("R/time_input.R")

shinyOptions(cache= cachem::cache_disk("./"))

ui <- fluidPage(
  tabsetPanel(
    tabPanel("About",
      fluidRow(
        column(12,
               includeMarkdown("adapt_documentation.md"))
      )
             ),
    
    # Application title
    tabPanel("Predictions",
    titlePanel("Predictions of Occupancy Probability"),

    fluidRow(  
      column(
        4,
        species_input("species")
      ),
       
      column(4,
          time_input("time")
             ),
              
      column(4,
             conditionalPanel(
              condition = "input.time == 2 | input.time == 3",
                ssp_input("ssp")
              )
                
       )
      ),
  
    fluidRow(
      column(4, offset = 4,
             checkboxInput(inputId = "compare",
                  label = "Compare Scenarios",
                  value = FALSE)),
      
      column(4, 
      
      conditionalPanel(
        condition = "input.compare == true",
        selectInput(input = "time_diff",
                    label = "Time Period",
                    choices = c("2040-2060" = 2,
                                "2080-2100" = 3)),
        ssp_input("ssp_diff")
      )
             )),
    
    # Show a plot of the generated distribution
    fluidRow(
      column(12,
             withSpinner(
               leafletOutput(outputId = "ssp_plot"))),
        ),
      
    fluidRow(
        column(4, downloadButton("download_raster","Download Raster")),
        column(4, downloadButton("download_fig","Download High-Res Figure"))
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
    if(input$compare == TRUE){
      if(input$time == 1){
        rasters_to_plot[[as.numeric(input$species)]][[as.numeric(input$time_diff)]][[as.numeric(input$ssp_diff)]] - 
          rasters_to_plot[[as.numeric(input$species)]][[1]]
      } else {
      rasters_to_plot[[as.numeric(input$species)]][[as.numeric(input$time_diff)]][[as.numeric(input$ssp_diff)]] - 
        rasters_to_plot[[as.numeric(input$species)]][[as.numeric(input$time)]][[as.numeric(input$ssp)]]
    }
      } else {
    
    if(input$time == 1) {
      rasters_to_plot[[as.numeric(input$species)]][[1]]  
    } else {
      rasters_to_plot[[as.numeric(input$species)]][[as.numeric(input$time)]][[as.numeric(input$ssp)]]
    }
  }})
  
  pal <- reactive({
    colorNumeric(
      scico(9, alpha = 0.8, palette = ifelse(as.numeric(input$compare) == TRUE,"broc", "tokyo")),
    values(raster_subset()),
    na.color = "transparent")
  })
  
  output$ssp_plot <- renderLeaflet({
    leaflet() |>  
      addProviderTiles(providers$Esri.WorldImagery) |>  
      addRasterImage(
        raster_subset(),
        colors = pal(),
        opacity = 0.8) |> 
      addLegend(
        pal = pal(),
        values = values(raster_subset()),
        title = "Probability of Occupancy")
  }) |> 
    bindCache(input$compare, input$time,input$species,
              input$time_diff, input$ssp_diff, input$ssp)
  
  raster_plot <- reactive({
    states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE),"SpatialPolygons") %>%
         filter(ID %in% c(
           "new mexico","arizona", "california","nevada","utah",
           "oklahoma","texas","colorado"
         ))
    ggplot() + geom_spatraster(data = raster_subset()) +
      scale_fill_continuous(na.value = "transparent") +
      theme_bw() + 
      geom_sf(data=states,fill=NA) + 
      guides(fill=guide_legend(title = "Prob of \nOccupancy"))
  })
  
  output$download_raster <- downloadHandler(
    filename = function() {
      paste0("output_",names(raster_subset()),".tif")
    },
    content = function(file) {
      #writeRaster(rasters_to_plot[[as.numeric(input$species)]][[as.numeric(input$time)]][[as.numeric(input$ssp)]], file)
      writeRaster(raster_subset(), file)
    }
  )
  
  output$download_fig <- downloadHandler(
    filename = function() {
      paste0("figure_",names(raster_subset()),".png")
    },
    content = function(file){
      ggsave(file, plot = raster_plot(), device = "png",
             width = 9, height = 9, units = "in")
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
      addProviderTiles(providers$Esri.WorldImagery) |> 
      addCircleMarkers(
        data = only_obs(),
        radius = ~(log(effort)+4),
        color = ~ifelse(obs == 1,scico(10,palette = "tokyo")[1],scico(10,palette = "tokyo")[8]),
        fillOpacity = 0.5,
        popup = ~paste("Survey effort =",effort)
      ) |> 
      addLegend(
        color = c(scico(10,palette = "tokyo")[1],scico(10,palette = "tokyo")[8]),
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
