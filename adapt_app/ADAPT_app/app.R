# Shiny app for At-risk species Distribution Analysis and Data T... Tool

library(shiny)
library(ggplot2)
library(tidyr)
#library(INLA)
#library(inlabru)
library(flexdashboard)
library(shinydashboard)
library(leaflet)
library(maps)
library(sf)
library(stars)
library(scico)
library(terra)
library(tidyterra)
library(lwgeom)

#load data
#beth_ras_tmp <- read.csv("beth_ras_year2.csv")
#beth_short <- beth_ras_tmp |> 
#  filter(!is.na(obs))
#write.csv(beth_short,"beth_short.csv")

adapt_pts <- st_read("dat_short.shp")

#read in raster files
beth_rast_ssp2_2100 <- rast("preds_ssp2_2100.tif")
#beth_rast_ssp5_2100 <- rast("preds_ssp5_2100.tif")
beth_rast_current <- rast("preds_2020.tif")

buow_current <- rast("buow_preds_2021.tif")
buow_ssp2_2100 <- rast("buow_preds_ssp2_2100.tif")

lewo_current <- rast("lewo_test_25kmesh_500kmatern.tif")
lewo_ssp2_2100 <- rast("lewo_preds_ssp2_2100.tif")

#need function here to read in raster and make brick - 
 # or just need to upload bricks!

beth_brick <- c(beth_rast_current, beth_rast_ssp2_2100,diff = beth_rast_ssp2_2100-beth_rast_current)
buow_brick <- c(buow_current, buow_ssp2_2100,diff = buow_current-buow_ssp2_2100)
lewo_brick <- c(lewo_current, lewo_ssp2_2100,diff = lewo_current-lewo_ssp2_2100)

rasters_to_plot <- list(beth = beth_brick,
                        buow = buow_brick,
                        lewo = lewo_brick)

#rasters_to_plot[[4]] <- rasters_to_plot$SSP2-rasters_to_plot$current
#names(rasters_to_plot)[4] <- "Diff_SSP2"
#rasters_to_plot[[5]] <- rasters_to_plot$SSP5-rasters_to_plot$current
#names(rasters_to_plot)[5] <- "Diff_SSP5"

# Define UI for application that plots prediction rasters

ui <- fluidPage(
  tabsetPanel(
    # Application title
    tabPanel("Predictions",
    titlePanel("Predictions of Occupancy Probability"),

    fluidRow(  
       column(4,selectInput(inputId = "ssp", label = "SSP:",
                choices = c("Current" = 1,
                            "SSP2-4.5" = 2,
                            "Difference of current and SSP2" = 3
                                 #"Difference of current and SSP5" = 5
                            )
                     )
         )),
    fluidRow(   
      column(12,selectInput(inputId = "species", label = "Species",
                            choices = c("Bendire's Thrasher"= 1,
                                        "Burrowing Owl" = 2,
                                        "Lewis's Woodpecker" = 3)))),
         
        # Show a plot of the generated distribution
      fluidRow(
           column(12,leafletOutput(outputId = "ssp_plot")),
           #column(4,plotOutput(outputId = "current_plot")),
           #column(4,plotOutput(outputId = "difference_plot"))
        ),
      fluidRow(
        column(4, downloadButton("download_raster","Download Data"))
      )
    ),

  tabPanel("Data Summary",
           #### Tab 2 ####
    fluidRow(
      column(6,
        selectInput(inputId = "year",
                    label = "Year",
                    choices = c("2017","2018","2019","2020","2021")),
                    
        selectInput(inputId = "state",
                      label = "State",
                    choices = unique(adapt_pts$ID)),
        selectInput(inputId = "species_plot",
                      label = "Species",
                    choices = c("Bendire's Thrasher" = "beth",
                                "Burrowing Owl" = "buow",
                                "Lewis's Woodpecker" = "lewo"))
               )),
             
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
   
  output$ssp_plot <- renderLeaflet({
  # generate plot based on input$ssp from ui.R

    leaflet() %>% 
      addTiles() %>% 
      addRasterImage(rasters_to_plot[[as.numeric(input$species)]][[as.numeric(input$ssp)]],
          colors = colorNumeric(
            scico(9,
                  alpha=.8,
                  palette = ifelse(input$ssp == 3,"broc", "tokyo")),
            values(rasters_to_plot[[as.numeric(input$species)]][[as.numeric(input$ssp)]]), 
            na.color = "transparent"),
            opacity = 0.8) %>%
      addLegend(
        pal = colorNumeric(
        scico(
          9, 
          alpha=.8, 
          palette = ifelse(input$ssp== 3,"broc", "tokyo")),
        values(rasters_to_plot[[as.numeric(input$species)]][[as.numeric(input$ssp)]]), na.color = "transparent"), 
        values = values(rasters_to_plot[[as.numeric(input$species)]][[as.numeric(input$ssp)]]),
                title = "Probability of Occupancy")
  })
  
  output$download_raster <- downloadHandler(
    filename = function() {
      paste0("output_preds_",input$ssp,"2100.tif")
    },
    content = function(file) {
      writeRaster(rasters_to_plot[[as.numeric(input$species)]][[as.numeric(input$ssp)]], file)
    }
  )
  
  #}

# Run the application 
#shinyApp(ui = ui, server = server)

  #tab 2
#server_t2 <- function(input, output, session){
  year_subset <- reactive({
    adapt_pts |>  
      filter(year == input$year) |>  
      filter(ID == input$state) |> 
      filter(species == input$species_plot)
  })
  
  #output$year_summary <- renderTable(
  #  table(year_subset()$effort)
  #)

  only_obs <- reactive({
    req(input$state)
    req(input$year)
    req(input$species_plot)
    adapt_pts |> 
      filter(ID == input$state & year == input$year & species == input$species_plot) |> 
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
        color = ~ifelse(obs ==1, scico(4)[1],scico(4)[3]),
        values = c("Yes","No"), 
        title = "Species Observed")
  })
  
}

shinyApp(ui = ui, server = server)

#shinyApp(ui = ui_t2, server = server_t2)

#testServer(server,{
#  session$setInputs(ID = "arizona", species = "beth", year = 2019)
#  ggplot(only_obs())
#  })
