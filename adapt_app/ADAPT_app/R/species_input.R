# create function for species input

species_input <- function(id){
  
  selectInput(
    inputId = id, 
    label = "Species",
    choices = c("Bendire's Thrasher"= 1,
                "Burrowing Owl" = 2,
                "Lewis's Woodpecker" = 3)
    )
}