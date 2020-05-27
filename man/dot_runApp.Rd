\docType{methods}
\name{.runApp}
\alias{.runApp}

\title{Run a shiny app, capturing results to the R session}
\description{
  This utility function launches a shiny visualization application,
  either in the RStudio viewer pane (if run under RStudio) or in the
  browser.
}
\usage{
.runApp(app, ...)
}
\arguments{
  \item{app}{The shiny application definition, see \code{?shiny::runApp}.}
  \item{...}{additional arguments passed to \code{shiny::runApp()}.}
}
\value{
  The return value of \code{shiny::runApp}.
}
\author{Martin Morgan}
\examples{

if (interactive()) {
    require(shiny)

    app <- list(
        ui = fluidPage(
          title="Who Am I?",
          sidebarLayout(
              position="left",
              sidebarPanel(
                  h1("Your name"),
                  textInput("your_name", "Your name?", "Anonymous"),
                  actionButton("done", "Done")),
              mainPanel(
                  "Hi", textOutput("your_name", inline=TRUE))
              )),

        server = function(input, output) {
            output$your_name <- renderText(input$your_name)
            observe({
                if (input$done > 0)
                    isolate(stopApp(returnValue = input$your_name))
            })

        })

    .runApp(app)
}
}

\keyword{manip, internal}

