# create function for species input

species_input <- function(id){
  
  selectInput(
    inputId = id, 
    label = "Species",
    choices = c("Bendire's Thrasher"= 1,
                "Burrowing Owl" = 2,
                "Lewis's Woodpecker" = 3,
                "Ferruginous Hawk" = 4,
                "Olive-sided Flycatcher" = 5,
                "Loggerhead Shrike" = 6,
                "Mountain Plover" = 7,
                "Chestnut-collared Longspur" = 8,
                "Band-tailed Pigeon" = 9)
  )
}