#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


shinyUI(
  dashboardPage(
    
    skin = "yellow",
    
    dashboardHeader(title = "Cryptograph", titleWidth = 250),
  
    dashboardSidebar(width = 250,
    
      sidebarUserPanel("Ali Uysal", "NYC Data Science Academy",
                       image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"
                       ),
    
      sidebarMenu(
        menuItem("About", tabName = "about", icon = icon("info-circle")),
        menuItem("Data", tabName = "data", icon = icon("database")),
        menuItem("Stores", tabName = "stores", icon = icon("store")),
        menuItem("Popularity", tabName = "popularity", icon = icon("thumbs-up")),
        menuItem("Profit/Loss", tabName = "profit_loss", icon = icon("dollar-sign")),
        menuItem("Volatility", tabName = "volatility", icon = icon("exclamation"))
        )
    ),
  
    dashboardBody(
      
      tabItems(
        
        #About
        tabItem(tabName = "about",
                fluidRow(
                  box(tags$h3("What is Cryptocurrency?"),
                      tags$hr(),
                      tags$p("Cryptocurrency is a digital asset that can be exchanged. The crypto part comes from the fact of use of cryptography for security and verification purposes during transactions. In using cryptocurrency for an exchange instead of fiat currency, crypto owners don't have to rely on banks to facilitate transactions, and can successfully avoid the fees that come with using financial institutions."),
                      tags$p("Please have a look at the Wikipedia page below to learn more about cryptocurrencies."),
                      tags$a(href="https://en.wikipedia.org/wiki/Cryptocurrency", "Click to go to Wikipedia!"),
                      tags$br(),
                      tags$p("Or you can watch this 3 minutes video for quick introduction"),
                      tags$a(href="https://youtu.be/6Gu2QMTAkEU", "Click to watch!"),
                      width = 12
                      ),
                  
                  box(
                    tags$h3("Data"),
                    tags$hr(),
                    tags$p("Main dataset used in this project is from Kaggle. You can view and download it via link below:"),
                    tags$a(href="https://www.kaggle.com/jessevent/all-crypto-currencies/version/17", "Main Data Source"),
                    tags$p("Data consists of daily price, market capitalization and volume (in US dollars) of 2,071 cryptocurrencies with 942,000 observations in total."),
                    tags$br(),
                    tags$p("Stores that accept Bitcoin payments data are taken from coinmap API in the link below."),
                    tags$a(href="https://coinmap.org/api/", "Stores Data Source"),
                    tags$p("Regarding the Bitcoin popularity, Google trends index of countries for Bitcoin interest for the past 5 years is employed."),
                    width = 12),
                  
                  box(
                    tags$h3("Research Questions"),
                    tags$hr(),
                    tags$p("Aim of this Shiny project is to give answers to the below questions:"),
                    tags$ol(
                    tags$li("How did the historical prices / market capitalizations of various currencies change over time?"), 
                    tags$li("Which currencies are more volatile and which ones are more stable?"), 
                    tags$li("How popular is Bitcoin? How different is its popularity across countries?")
                  ),
                  width = 12),
                  
                  box(
                    tags$h3("Further Research"),
                    tags$hr(),
                    tags$p("This project can be enhanced in the future by explaining below subjects"),
                    tags$ol(
                      tags$li("Correlation of market price for different cryptocurrencies."), 
                      tags$li("Correlation of popularity for Bitcoin and other cryptocurrencies."), 
                      tags$li("Correlation of bitcoin popularity and crypto mining across countries.")
                    ),
                    width = 12)
                
                  )
        ),
        
        # Tab1 - Show data
        tabItem(tabName = "data",
                fluidRow(
                  box(selectizeInput("name",
                                     h4("Select Coin"),
                                     names),
                      width = 6,
                      status = "warning",
                      solidHeader = TRUE
                      ),
                
                  box(selectizeInput("attr",
                                     h4("Select Metric"),
                                     attrs),
                      width = 6,
                      status = "warning",
                      solidHeader = TRUE
                      ),
                  
                  box(htmlOutput("annchart"),
                      width = 12, 
                      status = "warning",
                      solidHeader = TRUE),
                         
                  box(DT::dataTableOutput("table"),
                      width = 12,
                      status = "warning",
                      solidHeader = TRUE)
                )
        ),
      
      # Tab2 - Map of Bitcoin Accepting Venues
      tabItem(tabName = "stores",
              
              h3("Stores That Accept Bitcoin Payments"), #title of the leaflet map
              
              fluidRow(leafletOutput(
                "venues_map",
                width = "100%",
                height = 600
              )),
              
              br(),
              
              fluidRow(
                box(
                  htmlOutput("donut_venues"),
                  width = 6,
                  status = "warning",
                  solidHeader = TRUE
                ),
                box(
                  htmlOutput("combo_venues"),
                  width = 6,
                  status = "warning",
                  solidHeader = TRUE
                )
              )
      ),
              
      # Tab3 - Bitcoin Popularity by Country
      tabItem(tabName = "popularity",

              h3("Bitcoin Popularity Index by Country"), #title of the gvisGeoChart popularity map
              
              fluidRow(htmlOutput("bitcoin_pop_map"),
                       width = "100%",
                       height = 600),
              
              br(),
              
              fluidRow(
                box(
                  htmlOutput("barchart_pop"),
                  width = 6,
                  status = "warning",
                  solidHeader = TRUE
                ),
                box(
                  plotOutput("faceted_pie_pop"),
                  width = 6,
                  status = "warning",
                  solidHeader = TRUE,
                  height = "522px"
                )
              )
      ),
              
      # Tab4 - Profit/Loss?
      tabItem(tabName = "profit_loss",
              fluidPage(
                
                fluidRow(
                  #Input: Select Coin to Invest
                  box(selectizeInput("coin",
                                     h4("Which Coin?"),
                                     names),
                      width = 4,
                      status = "warning",
                      solidHeader = TRUE),
                  
                  #Input: Invest money from user
                  box(numericInput("money", 
                                   h4("How Much?"),
                                   value = 100),
                      width = 4,
                      status = "warning",
                      solidHeader = TRUE),
                  
                  #Input: Enter Date Range for the money to be invested
                  box(dateRangeInput("dates", 
                                     h4("How Long?"), 
                                     #min = "2018-11-01", 
                                     start = "2018-11-01", 
                                     end = '2018-11-29', 
                                     max = '2018-11-29'),
                      width = 4,
                      status = "warning",
                      solidHeader = TRUE)
                ),
  
                fluidRow(valueBoxOutput("perc"),
                         valueBoxOutput("youget"),
                         valueBoxOutput("dayspassed"))
       
                )
              ),
      
      # Tab5 - Volatility
      tabItem(tabName = "volatility",
              fluidPage(
                
                h3("Compare Volatility of Different Coins"),
                br(),
                
                checkboxGroupInput(
                  "checkGroup_Coins",
                  label = h4("Add Coins"),
                  choices = names,
                  #selected = c('Bitcoin','Ethereum'),
                  inline = TRUE
                ),
                
                br(),
                
                checkboxGroupInput(
                  "checkGroup_YM",
                  label = h4("Select Month"),
                  choices = unique_ym,
                  inline = TRUE
                ),
                
                br(),
                
                box(plotlyOutput("barplot_vol"), width = 12),
                
                br(),
                
                tags$hr(),
                tags$head(
                  tags$style(HTML("hr {border-top: 1px solid #000000;}"))
                ),
                
                br(),
                
                h3("Finally, Let's See How Market Capitalization and Volatility of Top 20 Coins Change Over Time"),
                
                br(),
                
                box(plotlyOutput("bubble_vol"), width = 12)
              )) # Tab-5 ends
  ) # tabItems ends
  ) # dashboardBody ends
) # dashboardPage
) # shinyUI ends
