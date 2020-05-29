##########################################################################3
## experimental new(er) version of .dataFrame
## helper for rowname wrangling:
.rownamesAreUnique <- function(df) {
    identical(
        length(rownames(df)),
        length(unique(rownames(df)))
    )
}

.dataFrame3 <- function(
    df, ..., summaryMessage = "", serverOptions = list(orderClasses = TRUE)
) {
    rowNames <- rownames(df)
    ## If the rownames are unique then just use the names as idx.
    ## but if not, then also also append supplementary idx
    if (.rownamesAreUnique(df)) {
        dt <- data.frame(idx = rowNames, df)
    } else {
        dt <- data.frame(idx = seq_len(nrow(df)), rownames = rowNames, df)
    }

    ## define the app
    app <- list(
        ui = fluidPage(
            titlePanel("Select rows in the Data Table"),
            sidebarLayout(
                sidebarPanel(
                    actionButton("btnSend", "Send"),
                    width = 1
                ),
                mainPanel(
                    DT::dataTableOutput('tbl')
                )
            )
        ),
        server = function(input, output) {
            output$tbl <- DT::renderDataTable(
                df, server = TRUE, filter = "top",
                options = serverOptions
            )

            if (length(summaryMessage) != 1L) {
                output$summary <- renderUI({
                    HTML(paste0(
                        sprintf(
                            '<span class="shiny-html-output" >%s</span> ',
                            summaryMessage
                        ), "<br>"
                    ))
                })
            }

            observe({
                if (input$btnSend > 0)
                    isolate({
                        idx <- input$tbl_rows_selected
                        stopApp(returnValue = df[idx,])
                    })
            })
        }
    )

    .runApp(app, ...)

}

setMethod("display", signature(object = "data.frame"),
    function(object, ...) {
        .dataFrame3(df=object, ...)
    }
)
