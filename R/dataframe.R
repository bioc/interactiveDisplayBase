##########################################################################3
## experimental new(er) version of .dataFrame
## helper for rowname wrangling:
.rownamesAreUnique <- function(df) {
    identical(
        length(rownames(df)),
        length(unique(rownames(df)))
    )
}

.dataFrame3 <- function(df, ...) {
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
            titlePanel("Select Rows"),
            sidebarLayout(
                sidebarPanel(
                    actionButton("btnSend", "Return rows to R session")
                ),
                mainPanel(
                    DT::dataTableOutput('tbl')
                )
            )
        ),
        server = function(input, output) {
            output$tbl <- DT::renderDataTable(
                df, server = FALSE,
                options = list(orderClasses = TRUE)
            )

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

setMethod("display", signature(object = c("data.frame")),
    function(object, ...) {
        .dataFrame3(df=object, ...)
    }
)
