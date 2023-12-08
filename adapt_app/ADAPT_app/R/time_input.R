time_input <- function(id){
  selectInput(input = id,
              label = "Time Period",
              choices = c("Current" = 1,
                          "2040-2060" = 2,
                          "2080-2100" = 3))
}