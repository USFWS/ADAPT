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

adapt_pts <- st_read("dat_short_new.shp")
cov_all_sp <- read.csv("cov_all_sp.csv")[,-1]
colnames(cov_all_sp)[1] = "Covariate"
lulc_all_sp <- read.csv("lulc_all_sp.csv")[,-c(1:2)]

source("R/raster_prep.R")
#read in raster files

beth_brick <- raster_prep("beth")
buow_brick <- raster_prep("buow")
lewo_brick <- raster_prep("lewo")
feha_brick <- raster_prep("feha")
osfl_brick <- raster_prep("osfl")
losh_brick <- raster_prep("losh")
mopl_brick <- raster_prep("mopl")
cclo_brick <- raster_prep("cclo")
btpi_brick <- raster_prep("btpi")

rasters_to_plot <- list(beth = beth_brick,
                        buow = buow_brick,
                        lewo = lewo_brick,
                        feha = feha_brick,
                        osfl = osfl_brick,
                        losh = losh_brick,
                        mopl = mopl_brick,
                        cclo = cclo_brick,
                        btpi = btpi_brick)

source("R/species_input.R")
source("R/ssp_input.R")
source("R/time_input.R")

shinyOptions(cache= cachem::cache_disk("./"))

ui <- fluidPage(
  tabsetPanel(
    
    tabPanel("About",
      fluidRow(
        column(12,
               includeMarkdown("adapt_documentation.md")),
        tags$div(
          "For questions about ADAPT, contact",
          tags$a(href="mailto:beth_ross@fws.gov",
          "Beth Ross"),
          tags$a(href="mailto:matthew_boggie@fws.gov",
          "or Matt Boggie")
        )
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
           )
    #fluidRow(
    #     column(4, tableOutput(outputId = "year_summary")),
    #     column(4, tableOutput(outputId = "state_summary")),
    #       )
             
           ),

#### Tab 3 - Figs and Tables ####

tabPanel("Figures and Tables",
         titlePanel("Figures and Tables from Model Output"),

         fluidRow(
           column(4,
                  selectInput(
                    inputId = "species_fig",
                    label = "Species",
                    choices = c("Bendire's Thrasher" = "beth",
                                "Burrowing Owl" = "buow",
                                "Lewis's Woodpecker" = "lewo",
                                "Ferruginous Hawk" = "feha",
                                "Olive-sided Flycatcher" = "osfl",
                                "Loggerhead Shrike" = "losh",
                                "Mountain Plover" = "mopl",
                                "Chestnut-collared Longspur" = "cclo",
                                "Band-tailed Pigeon" = "btpi")
                  )
           )
         ),
         fluidRow(
           #column(4),
           tags$div(
             "Figure showing the spatially-varying effect of relative humidity on
             occupancy across the study area."
           )
         ),

         fluidRow(
           column(4, imageOutput("result_fig"))),

         tags$div(
           "Table of the effect of elevation, year, and survey effort on
           occupancy across the study area with the mean, standard
           deviation, and 2.5%, 50%, and 97.5% quantiles."
         ),

         fluidRow(
           column(8, tableOutput("cov_vals"))
         ),
         
         tags$div(
           "Table of the effect of land cover type on
           occupancy across the study area with the mean, standard
           deviation, and 2.5%, 50%, and 97.5% quantiles."
         ),
         
         fluidRow(
           column(8, tableOutput("lulc_vals"))
         )
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
  }) #|> 
    # bindCache(input$compare, input$time,input$species,
    #           input$time_diff, input$ssp_diff, input$ssp)
    # 
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
       filter(ID %in% input$state & year %in% input$year & species == unique(species)[as.numeric(input$species_plot)]) |> 
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
    
  #### Server Tab 3 ####
    output$result_fig <- renderImage({

      # load fig 1 file
      width <- session$clientData$output_result_fig_width
      height <- session$clientData$output_result_fig_height

      filename <- paste0("./images/",input$species_fig,"_rh.png")

      list(src = filename,
           width = 400,
           height = 400,
           alt = "Effect of relative humidity")

    }, deleteFile = FALSE)

  selected_cov <- reactive(cov_all_sp |> 
                             filter(species == input$species_fig) |> 
                             select(!species))
  
    output$cov_vals <- renderTable(
      selected_cov()  #see renderDataTable for option to change row colors based on 95% CIs
    )
    
    selected_lulc <- reactive(lulc_all_sp |> 
                                filter(species == input$species_fig) |> 
                                select(!species))
    
    output$lulc_vals <- renderTable(
      selected_lulc()
    )
}

shinyApp(ui = ui, server = server)

#shinyApp(ui = ui_t2, server = server_t2)

#testServer(server,{
#  session$setInputs(ID = "arizona", species = "beth", year = 2019)
#  ggplot(only_obs())
#  })
