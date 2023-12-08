ssp_input <- function(id){
  selectInput(id,
              label = "SSP",
              choices = c("SSP2-4.5" = 1,
                          "SSP5-8.5" = 2))
}