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

################################################################################
###   Helper Functions
################################################################################

.usePackage <- function(p) {
  if (!requireNamespace("BiocManager", quietly=TRUE))
      install.packages("BiocManager")
  if (!is.element(p, installed.packages()[,1])){
    stop(paste("The required package, '",p,"', is missing.  Please install it by
               typing BiocManager::install('",p,"') in the console", sep=""))
  }
  require(p, character.only = TRUE)
}


## helper for JS library tags

.jstags <- function(){
  list(
  tags$script(src="/js/jquery.min.js"),
  tags$script(src="/js/d3.v2.js"))
}

#tags$script(src="/js/jquery-svgpan.js"),
#tags$script(src="/js/jscolor/jscolor.js"))

#.shiny-output-error { visibility: hidden; }
#.shiny-output-error:before { visibility: hidden; }

.csstags <- function(){

  shiny::tags$head(
    shiny::tags$style(type='text/css', "

    .span4 {
      width: 370px;
      position: absolute;
      z-index: 50;
    }

    .span8 {
      position: absolute;
      left: 400px;
      right: 30px;
      width: auto;
      height: auto;
    }

    ")
  )
}

## The loading gif/panel
.loading_gif <- function(){
  list(
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   div("Loading...", style = "color:blue")),
  conditionalPanel(condition="!($('html').hasClass('shiny-busy'))", br())
  )
}

#selDataTableOutput <- function (outputId){
#  tagList(singleton(tags$head(tags$link(rel = "stylesheet",
#    type = "text/css", href = "shared/datatables/css/DT_bootstrap.css"),
#    tags$style(type="text/css", ".rowsSelected td{background-color: rgba(112,164,255,0.2) !important}"),
#    tags$style(type="text/css", ".selectable div table tbody tr{cursor: hand; cursor: pointer;}"),
#    tags$style(type="text/css",".selectable div table tbody tr td{
#      -webkit-touch-callout: none;
#      -webkit-user-select: none;
#      -khtml-user-select: none;
#      -moz-user-select: none;
#      -ms-user-select: none;
#      user-select: none;}"),
#    tags$script(src = "shared/datatables/js/jquery.dataTables.min.js"),
#    tags$script(src = "shared/datatables/js/DT_bootstrap.js"),
#    tags$script(src = "/js/DTbinding.js"))),
#  div(id = outputId, class = "shiny-datatable-output selectable"))
#}

################################################################################
###   Additional Functions
################################################################################

#grid2jssvg <- function(gp){
#
#  jscode <- "
#              <script type='text/javascript'>
#              $(document).ready(function() {
#                $('svg').svgPan('viewport');
#              });
#              </script>
#            "
#  png(filename = "myplot.png", bg = "transparent",height=1000,width=1000)
#  print(gp)
#
#  mysvg <- gridSVG::grid.export()
#  dev.off()
#  mysvg2 <- saveXML(mysvg$svg[["g"]])
#  mysvg3 <- sub("<g transform=","<g id='viewport' transform=",mysvg2)
#  mysvg4 <- sub(">NA<","><",mysvg3)
#  htmlxml <- HTML(paste("<svg xmlns='http://www.w3.org/2000/svg'
#  xmlns:xlink='http://www.w3.org/1999/xlink' version='1.1' width='100%'
#  height='100%'>",jscode,mysvg4,"</svg>",sep=""))
#  htmlxml
#}

#  This pair of functions can be used in cases where it is desirable to
#  give the user a choice between rendering a plot as svg or to use the default
#  Shiny plot function.

#svgcheckout <- function(contents,sflag){
#  if(sflag==TRUE){
#    uiOutput(contents)
#  }
#  else{
#    plotOutput(contents)
#  }
#}
