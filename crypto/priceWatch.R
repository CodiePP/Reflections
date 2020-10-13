#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(httr)
library(jsonlite)
library(plotly)
library(shiny)
library(shinyjs)

# Binance interface

ENDPOINT <- "https://api.binance.com"

# get OHLC (open, high, low, close)
ohlcurl <- function (EP = ENDPOINT) {
    return (paste(EP, "/api/v3/klines", sep=''));
}
ohlcreq <- function (symbol, interval, limit = 10) {
    return (list(symbol = symbol, interval = interval, limit = limit));
}


# Define UI for application that will query and plot price charts
ui <- fluidPage(

    # Application title
    titlePanel("Market Price Watch (binance)"),

    # Sidebar with inputs 
    sidebarLayout(
        sidebarPanel(
            passwordInput("apikey", "Api key:"),
            selectInput("cy", "Currency:",
                        c("BTCUSDT", "ADABTC", "ETHBTC", "ETCBTC",
                          "XTZBTC", "XVGBTC", "XLMBTC", "XMRBTC")
                        ),
            selectInput("tspan", "Timespan:",
                        c("5m", "15m", "30m",
                          "1h", "2h", "3h", "6h", "12h",
                          "1d", "2d", "3d", "1w")),
            selectInput("n", "Count:",
                        c("10", "20", "50", "100", "200")),
            actionButton("recalc", "Go")
        ),

        # Show a plot
        mainPanel(
            useShinyjs(),
            plotlyOutput("distPlot"),
            dataTableOutput("datatable"),
            downloadButton("downloadtable", "Download")
        )
    )
)

# Define server logic
server <- function(input, output, session) {

    disable("downloadtable")
    
    output$distPlot <- renderPlotly({

        if (input$cy != "" && input$apikey != "") {
            APIKEY <- input$apikey
            cy <- input$cy
            tspan <- input$tspan
            n <- input$n
            input$recalc # add dependency
            
            fig <- c()
            withProgress({
                ohlc <- httr::GET(ohlcurl(), query = ohlcreq(cy, tspan, n), httr::add_headers('X-MBX-APIKEY' = APIKEY));
                jOHLC <- jsonlite::toJSON(httr::content(ohlc, as = 'parsed'));
                #print(jsonlite::prettify(jOHLC));
                setProgress(0.8)
    
                df <- as.data.frame(fromJSON(jOHLC))
                colnames(df) <- c('open time', 'open', 'high', 'low', 'close', 'volume', 'close time', 'qav', 'ntrades', 'takbb', 'takbq', 'ign')
                
                df <- cbind(df, data.frame(openTstamp = as.numeric(df$`open time`) / 1000))
                df <- cbind(df, data.frame(closeTstamp = as.numeric(df$`close time`) / 1000))
                df <- cbind(df, data.frame(openTime = strptime(df$openTstamp, "%s")))
                df <- cbind(df, data.frame(closeTime = strptime(df$closeTstamp, "%s")))

                otimes <- df$openTime
                now <- Sys.time()
                datevect <- (otimes - now)
                
                df2 <- select(df, openTime, open, high, low, close, closeTime, volume)
                output$datatable <- renderDataTable(df2)
                output$downloadtable <- downloadHandler(
                    filename = function() { paste0("data-",cy,".csv") },
                    content = function(file) { write.csv(df2, file = file) }
                )
                enable("downloadtable")
                
                setProgress(0.9)
                
                fig <- df %>% plot_ly(x = datevect, type="ohlc",
                                      open = ~open, close = ~close,
                                      high = ~high, low = ~low) 
                fig <- fig %>% layout(title = cy,
                                      xaxis = list(rangeslider = list(visible = T))) # F = no volume display
                setProgress(1.0)
            })
            fig
        }    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
