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
#beth_clim_dat <- read.csv("BETH_clim_dat.csv")[,-1]
#beth_short$ahm <- beth_clim_dat$AHM.ssp245.2001.2020[104000:105000]
#beth_short$lulc <- beth_lulc$l.245.20[1:1001]
beth_short <- st_as_sf(beth_short,coords = c("POINT_X","POINT_Y"))
latlong <- "+proj=longlat +datum=WGS84"
epsg5070 <- paste("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5",
                  "+lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83", 
                  "+units=m +no_defs")
st_crs(beth_short) <- epsg5070
beth_short2 <- st_transform(beth_short, crs=latlong)
states = st_as_sf(map("state",plot=FALSE, fill=TRUE))
states = st_transform(states, crs = latlong)

#states is not valid
#st_is_valid(states)
sf_use_s2(FALSE)
states = st_make_valid(states)
beth_short2 = st_join(beth_short2,states)

#read in raster files
rast_ssp2_2100 <- read_stars("preds_ssp2_2100.tif")
rast_ssp5_2100 <- read_stars("preds_ssp5_2100.tif")
rast_current <- read_stars("preds_2020.tif")

#buow_current <- read_stars("buow_preds_2020.tif")
#buow_ssp2_2100 <- read_stars("buow_preds_ssp2_2100.tif")

rasters_to_plot <- list(SSP2 = rast_ssp2_2100,SSP5 = rast_ssp5_2100,
                        current = rast_current)
rasters_to_plot[[4]] <- rasters_to_plot$SSP2-rasters_to_plot$current
names(rasters_to_plot)[4] <- "Diff_SSP2"
rasters_to_plot[[5]] <- rasters_to_plot$SSP5-rasters_to_plot$current
names(rasters_to_plot)[5] <- "Diff_SSP5"

# Define UI for application that plots prediction rasters

ui <- fluidPage(
  tabsetPanel(
    # Application title
    tabPanel("Predictions",
    titlePanel("Predictions of Occupancy Probability"),

    fluidRow(  
       column(4,selectInput(inputId = "ssp", label = "SSP:",
                     choices = c("SSP2"= 1,
                                 "SSP5"= 2,
                                 "Current (2021)" = 3,
                                 "Difference of current and SSP2" = 4,
                                 "Difference of current and SSP5" = 5))
         )),
       #column(4,selectInput(inputId = "species", label = "Species",
      #                      choices = c("Bendire's Thrasher"=1,
      #                                  "Burrowing Owl" = 2)))),
         
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
   
  plot_raster <- reactive({
    #req(input$ssp)
    rast(rasters_to_plot[[as.integer(input$ssp)]])
    })
  
  #cols_plot <- colorNumeric(viridis(9, alpha=.8, option = "D"),
  #                          values(plot_raster()), na.color = "transparent")
  
  output$ssp_plot <- renderLeaflet({
      # generate plot based on input$ssp from ui.R

    leaflet() %>% addTiles() %>% addRasterImage(plot_raster(),
                                                colors = colorNumeric(scico(9, alpha=.8, palette = ifelse(as.numeric(input$ssp)== 5| as.numeric(input$ssp)== 4,"broc", "tokyo")),
                                                                      values(plot_raster()), na.color = "transparent"),
                                                opacity = 0.8) %>%
      addLegend(pal = colorNumeric(scico(9, alpha=.8, palette = ifelse(as.numeric(input$ssp)== 5| as.numeric(input$ssp)== 4,"broc", "tokyo")),
                                   values(plot_raster()), na.color = "transparent"), 
                values = values(plot_raster()),
                title = paste0(names(rasters_to_plot[as.integer(input$ssp)])," Pred Occ"))
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
      writeRaster(plot_raster(), file)
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
