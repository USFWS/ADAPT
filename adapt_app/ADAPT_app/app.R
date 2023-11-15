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

beth_short <- read.csv("beth_short.csv")
buow_short <- read.csv("buow_short.csv")

data_transform <- function(dat_short){

  dat_short <- st_as_sf(dat_short,coords = c("POINT_X","POINT_Y"))
  st_crs(dat_short) <- st_crs("EPSG:5070")
  dat_short2 <- st_transform(dat_short, crs = st_crs("EPSG:4326"))
  states = st_as_sf(maps::map("state",plot=FALSE, fill=TRUE))
  states = st_transform(states, crs = st_crs("EPSG:4326"))

#states is not valid
#st_is_valid(states)
  sf_use_s2(FALSE)
  states = st_make_valid(states)
  dat_short2 = st_join(dat_short2,states)

return(dat_short = dat_short2)

}

#read in raster files
beth_rast_ssp2_2100 <- rast("preds_ssp2_2100.tif")
#beth_rast_ssp5_2100 <- rast("preds_ssp5_2100.tif")
beth_rast_current <- rast("preds_2020.tif")

buow_current <- rast("buow_preds_2021.tif")
buow_ssp2_2100 <- rast("buow_preds_ssp2_2100.tif")

lewo_current <- rast("lewo_test_25kmesh_500kmatern.tif")
lewo_ssp2_2100 <- rast("lewo_preds_ssp2_2100.tif")

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
                     choices = c("Current (2021)" = 1,
                                 "SSP2" = 2,
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
           
           #ui_t2 <- fluidPage(
             #titlePanel("Test"),
             #sidebarLayout(
             #sidebarPanel(
             fluidRow(
               column(6,
                      selectInput(inputId = "year",
                                  label = "Year",
                                  choices = c("2017","2018","2019","2020","2021")),
                      
                      selectInput(inputId = "state",
                                  label = "State",
                                  choices = unique(beth_short2$ID))
               )),
             
           fluidRow(
             column(12, leafletOutput("map"))
           ),
           fluidRow(
               column(4, tableOutput(outputId = "year_summary")),
               column(4, tableOutput(outputId = "state_summary")),
             )
             
           )
  )
)

server <- function(input, output, session) {
  #tab 1
   
  #plot_raster <- reactive({
    #req(input$ssp)
    #rast(rasters_to_plot[[as.integer(input$ssp)]])
    #rast(rasters_to_plot[[input$ssp]])
   # rasters_to_plot[[input$species]][[input$ssp]]
   # })
  
  #cols_plot <- colorNumeric(viridis(9, alpha=.8, option = "D"),
  #                          values(plot_raster()), na.color = "transparent")
  
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
                title = paste0(names(rasters_to_plot[input$ssp])," Pred Occ"))
  })
  
  #switch raster for SSP2, SSP5, Difference, or Current
  #observeEvent(input$ssp, 
  #  {
  #  leafletProxy("ssp_plot", data = plot_raster()) %>%
  #    addRasterImage(plot_raster(), colors = cols_plot,opacity = 0.8) %>%
  #    addLegend(pal = cols_plot, values = values(plot_raster()),
  #              title = paste0(names(rasters_to_plot[input$ssp])," Pred Occ"))
  #})
  
  #output$current_plot <- renderPlot({
    #plot(rast(rasters_to_plot$current),main = "Current (2021)")
  #})
  #output$difference_plot <- renderPlot({
  #  plot(plot_raster()-(rast(rasters_to_plot$current)),main = "Difference (2100 - Current)")
  #})
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
    #req(input$year)
    #subset(beth_short2, year == input$year)
    as.data.frame(beth_short2) %>% filter(year == input$year) %>% filter(ID == input$state)
  })
  
  output$year_summary <- renderTable(
    table(year_subset()$effort)
    #year_subset() %>% count(effort)
  )
  
  #state_subset <- reactive({
  #  as.data.frame(beth_short2) %>% filter(ID == input$state)
    #req(input$state)
    #pts_state <- subset(beth_short2, ID == input$state)
  #  })
  
  only_obs <- reactive({
    req(input$state)
    tmp = subset(beth_short2, ID == input$state & year == input$year)
    tmp[order(tmp$obs),]
  })
  
  output$state_summary <- renderTable(
    year_subset() %>% count(obs)
    #table(state_subset()$obs, state_subset()$year,useNA="ifany")
  )
  
  output$map <- renderLeaflet({
    #pop_by_year <- filter(beth_short2,
    #                      year == input$year)
    #leaflet(data = beth_short2) %>%
   leaflet(only_obs()) %>%
      addTiles() %>%
      addCircleMarkers(
        #radius = ~ifelse(obs == 1, 10, 6),
        radius = ~(log(effort)+4),
        color = ~ifelse(obs == 1,scico(4)[1],scico(4)[3]),
        fillOpacity = 0.5
      )
  })
  
}

shinyApp(ui = ui, server = server)

#shinyApp(ui = ui_t2, server = server_t2)
