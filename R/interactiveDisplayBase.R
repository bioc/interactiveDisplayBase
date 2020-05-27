################################################################################
###   Main
################################################################################

## declare the display generic
setGeneric("display", function(object, ...)
  standardGeneric("display")
)

setMethod("display",
signature(object = "ANY"),
function(object){
  message("Wrong object")
})

setMethod("display",
signature(object = "missing"),
function(object){
  message("Missing object")
})

