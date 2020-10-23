#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# current portfolio:
# http://127.0.0.1:8778/?_state_id_=de7fd8b7820cbdf2




library(httr)
library(jsonlite)
library(plotly)
library(shiny)


# Binance interface

ENDPOINT <- "https://api.binance.com"

# get last trades
lasturl <- function (EP = ENDPOINT) {
    return (paste0(EP, "/api/v3/trades"));
}
lastreq <- function (symbol, limit = 1) {
    return (list(symbol = symbol, limit = limit));
}

# Define UI for application that will query and plot price charts
ui <- function(request) {
    fluidPage(
    
        # Application title
        titlePanel("Portfolio Valuation"),
    
        # Sidebar with inputs 
        sidebarLayout(
            sidebarPanel(
                
                radioButtons("controller", NULL,
                             choiceNames = list(
                                 icon("cog", class="fa-3x"),
                                 icon("calculator", class="fa-3x"),
                                 icon("sync-alt", class="fa-3x"),
                                 icon("compass", class="fa-3x")
                             ),
                             choiceValues = list(
                                 "1", "2", "3", "4"
                             )),
                bookmarkButton(label = "save")
              , width = 3), # sidebarPanel
            
            # Show a plot
            mainPanel(
                tabsetPanel(id = "inTabset",
                            tabPanel(title = "Setup", icon = icon("cog", class="fa-sm"), value = "panel1",
                                     numericInput("nBTC", "BTC:", 0, width = '120px'),

                                     numericInput("nUSDT", "USDT:", 0, width = '120px'),
                                     numericInput("nUSD", "USD:", 0, width = '120px'),
                                     numericInput("nCHF", "CHF:", 0, width = '120px'),
                                     numericInput("rUSDCHF", "USDCHF:", 0.92, width = '80px'),
                                     numericInput("nEUR", "EUR:", 0, width = '120px'),
                                     numericInput("rEURUSD", "EURUSD:", 1.18, width = '80px'),
                                     numericInput("nGBP", "GBP:", 0, width = '120px'),
                                     numericInput("rGBPUSD", "GBPUSD:", 1.3, width = '80px'),
                                     
                                     numericInput("nADA", "ADA:", 0, width = '120px'),
                                     numericInput("nAE",  "AE:",  0, width = '120px'),
                                     numericInput("nALGO","ALGO:",0, width = '120px'),
                                     numericInput("nATOM","ATOM:",0, width = '120px'),
                                     numericInput("nBCH", "BCH:", 0, width = '120px'),
                                     numericInput("nBNB", "BNB:", 0, width = '120px'),
                                     numericInput("nEOS", "EOS:", 0, width = '120px'),
                                     numericInput("nETC", "ETC:", 0, width = '120px'),
                                     numericInput("nETH", "ETH:", 0, width = '120px'),
                                     numericInput("nIOTA","IOTA:",0, width = '120px'),
                                     numericInput("nLTC", "LTC:", 0, width = '120px'),
                                     numericInput("nNEO", "NEO:", 0, width = '120px'),
                                     numericInput("nRVN", "RVN:", 0, width = '120px'),
                                     numericInput("nTRX", "TRX:", 0, width = '120px'),
                                     numericInput("nXLM", "XLM:", 0, width = '120px'),
                                     numericInput("nXMR", "XMR:", 0, width = '120px'),
                                     numericInput("nXRP", "XRP:", 0, width = '120px'),
                                     numericInput("nXTZ", "XTZ:", 0, width = '120px'),
                                     numericInput("nXVG", "XVG:", 0, width = '120px'),
                                     numericInput("nZEC", "ZEC:", 0, width = '120px'),
                                     
                                     passwordInput("apikey", "Api key:"),
                                     selectInput("basecy", "Base currency:",
                                                 c("USD", "BTC"))
                            ),
                            tabPanel(title = "Valuation", icon = icon("calculator", class="fa-sm"), value = "panel2",
                                     dataTableOutput("table")
                            ),
                            tabPanel(title = "Transactions", icon = icon("sync-alt", class="fa-sm"), value = "panel3",
                                     "tbd: transaction table"
                            ),
                            tabPanel(title = "Risk", icon = icon("compass", class="fa-sm"), value = "panel4",
                                     "tbd: risk decomposition"
                            )
                ) # tabsetPanel
            ) # mainPanel
        ) # sidebarLayour
    ) # fluidPage
}

tablePfnames <- c("symbol", "px", "$", "n", "amount $", "amount BTC")

getLast <- function(symbol, APIKEY="") {
    px <- httr::GET(lasturl(), query = lastreq(symbol, 1), httr::add_headers('X-MBX-APIKEY' = APIKEY));
    jPx <- jsonlite::toJSON(httr::content(px, as = 'parsed'));
    #print(jsonlite::prettify(jPx));
    
    df <- as.data.frame(fromJSON(jPx))
    df$price <- as.numeric(df$price)
    df$qty <- as.numeric(df$qty)
    print(df)
    df
}

mkRow <- function(symbol, n, pxBTCUSDT, APIKEY="") {
    px <- getLast(paste0(symbol,"BTC"), APIKEY)
    r <- data.frame(symbol, px$price, px$price * pxBTCUSDT, n, n * px$price * pxBTCUSDT, n * px$price)
    colnames(r) <- tablePfnames
    r
}

# Define server logic
server <- function(input, output, session) {

    setBookmarkExclude(names = c("apikey"))
    
    observeEvent(input$controller, {
        updateTabsetPanel(session, "inTabset",
                          selected = paste0("panel", input$controller)
        )
    })
    
    output$table <- renderDataTable({

        if (input$apikey != "") {
            isolate({
                APIKEY <- input$apikey
                nBTC <- as.numeric(input$nBTC)
                
                nUSDT <- as.numeric(input$nUSDT)
                nUSD <- as.numeric(input$nUSD)
                nCHF <- as.numeric(input$nCHF)
                rUSDCHF <- as.numeric(input$rUSDCHF)
                nEUR <- as.numeric(input$nEUR)
                rEURUSD <- as.numeric(input$rEURUSD)
                nGBP <- as.numeric(input$nGBP)
                rGBPUSD <- as.numeric(input$rGBPUSD)
                
                nADA <- as.numeric(input$nADA)
                nAE <- as.numeric(input$nAE)
                nALGO <- as.numeric(input$nALGO)
                nATOM <- as.numeric(input$nATOM)
                nBCH <- as.numeric(input$nBCH)
                nBNB <- as.numeric(input$nBNB)
                nEOS <- as.numeric(input$nEOS)
                nETC <- as.numeric(input$nETC)
                nETH <- as.numeric(input$nETH)
                nIOTA <- as.numeric(input$nIOTA)
                nLTC <- as.numeric(input$nLTC)
                nNEO <- as.numeric(input$nNEO)
                nRVN <- as.numeric(input$nRVN)
                nTRX <- as.numeric(input$nTRX)
                nXLM <- as.numeric(input$nXLM)
                nXMR <- as.numeric(input$nXMR)
                nXTZ <- as.numeric(input$nXTZ)
                nXRP <- as.numeric(input$nXRP)
                nXVG <- as.numeric(input$nXVG)
                nZEC <- as.numeric(input$nZEC)
                
                df <- 0;
                withProgress({
                    pxBTCUSD <- getLast('BTCUSDT', APIKEY)
                    incProgress(1)
                    df <- data.frame("BTC", 1.0, pxBTCUSD$price, nBTC, nBTC * pxBTCUSD$price, nBTC)
                    colnames(df) <- tablePfnames
                    df2 <- data.frame("USDT", 1.0 / pxBTCUSD$price, 1.0, nUSDT, nUSDT, nUSDT / pxBTCUSD$price)
                    colnames(df2) <- tablePfnames
                    df <- rbind(df, df2)
                    df2 <- data.frame("USD", 1.0 / pxBTCUSD$price, 1.0, nUSD, nUSD, nUSD / pxBTCUSD$price)
                    colnames(df2) <- tablePfnames
                    df <- rbind(df, df2)
                    df2 <- data.frame("CHF", 1.0 / rUSDCHF / pxBTCUSD$price, rUSDCHF, nCHF, nCHF / rUSDCHF, nCHF / rUSDCHF / pxBTCUSD$price)
                    colnames(df2) <- tablePfnames
                    df <- rbind(df, df2)
                    df2 <- data.frame("EUR", rEURUSD / pxBTCUSD$price, rEURUSD, nEUR, nEUR * rEURUSD, nEUR * rEURUSD / pxBTCUSD$price)
                    colnames(df2) <- tablePfnames
                    df <- rbind(df, df2)
                    df2 <- data.frame("GBP", rGBPUSD / pxBTCUSD$price, rUSDCHF, nGBP, nGBP * rGBPUSD, nGBP * rGBPUSD / pxBTCUSD$price)
                    colnames(df2) <- tablePfnames
                    df <- rbind(df, df2)
                    df <- rbind(df, mkRow("ADA", nADA, pxBTCUSD$price, APIKEY))
                    incProgress(1)
                    df <- rbind(df, mkRow("AE", nAE, pxBTCUSD$price, APIKEY))
                    incProgress(1)
                    df <- rbind(df, mkRow("ALGO", nALGO, pxBTCUSD$price, APIKEY))
                    incProgress(1)
                    df <- rbind(df, mkRow("ATOM", nATOM, pxBTCUSD$price, APIKEY))
                    incProgress(1)
                    df <- rbind(df, mkRow("BCH", nBCH, pxBTCUSD$price, APIKEY))
                    incProgress(1)
                    df <- rbind(df, mkRow("BNB", nBNB, pxBTCUSD$price, APIKEY))
                    incProgress(1)
                    df <- rbind(df, mkRow("EOS", nEOS, pxBTCUSD$price, APIKEY))
                    incProgress(1)
                    df <- rbind(df, mkRow("ETH", nETH, pxBTCUSD$price, APIKEY))
                    incProgress(1)
                    df <- rbind(df, mkRow("ETC", nETC, pxBTCUSD$price, APIKEY))
                    incProgress(1)
                    df <- rbind(df, mkRow("IOTA", nIOTA, pxBTCUSD$price, APIKEY))
                    incProgress(1)
                    df <- rbind(df, mkRow("LTC", nLTC, pxBTCUSD$price, APIKEY))
                    incProgress(1)
                    df <- rbind(df, mkRow("NEO", nNEO, pxBTCUSD$price, APIKEY))
                    incProgress(1)
                    df <- rbind(df, mkRow("RVN", nRVN, pxBTCUSD$price, APIKEY))
                    incProgress(1)
                    df <- rbind(df, mkRow("TRX", nTRX, pxBTCUSD$price, APIKEY))
                    incProgress(1)
                    df <- rbind(df, mkRow("XLM", nXLM, pxBTCUSD$price, APIKEY))
                    incProgress(1)
                    df <- rbind(df, mkRow("XMR", nXMR, pxBTCUSD$price, APIKEY))
                    incProgress(1)
                    df <- rbind(df, mkRow("XRP", nXRP, pxBTCUSD$price, APIKEY))
                    incProgress(1)
                    df <- rbind(df, mkRow("XTZ", nXTZ, pxBTCUSD$price, APIKEY))
                    incProgress(1)
                    df <- rbind(df, mkRow("XVG", nXVG, pxBTCUSD$price, APIKEY))
                    incProgress(1)
                    df <- rbind(df, mkRow("ZEC", nZEC, pxBTCUSD$price, APIKEY))
                    incProgress(1)
                    sums <- data.frame("sum", 0, 0, sum(df[,4]), sum(df[,5]), sum(df[,6]))
                    incProgress(1)
                    colnames(sums) <- colnames(df)
                    df <- rbind(df, sums)
                    }, min = 0, max = 22)
                df
            })
        } else {
            df <- data.frame("BTC", 1.0, 0.0, 0, 0, 0)
            colnames(df) <- tablePfnames
            df
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server,
         enableBookmarking = "server",
         options = list(port = 8778))
