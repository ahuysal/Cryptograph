#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#



shinyServer(function(input, output, session) {

  #Create connection to SQLite
  conn <- dbConnector(session, dbname = dbname)
  
  sqlite_venues <- reactive(dbGetVenues(conn = conn, tblname = tbl_venues))
  sqlite_coins  <- reactive(dbGetCoins(conn = conn, tblname = tbl_coins))
  
  
  
  observe({

    #Tab1 - Display Time Series and Data Table Begins
    
    #gVis Annotation chart
    output$annchart = renderGvis({
      gvisAnnotationChart(sqlite_coins() %>% filter(name == input$name), datevar = 'date', numvar = input$attr, 
                          idvar = 'name',
                          #titlevar = "Daily Coin Metrics",
                          #annotationvar = "", date.format = "%Y/%m/%d",
                          options = list(height="300px",
                                         width="1400px",
                                         fill=10, displayExactValues=FALSE,
                                         colors="['#0000ff']",
                                         legendPosition='newRow'))
    })
    
    # Display data using DataTable
    output$table <- DT::renderDataTable({
      datatable(sqlite_coins() %>%
                  select(symbol, name, date, ranknow, open, ln_open, high, ln_high, low, ln_low, close, ln_close, volume, ln_volume, market, ln_market) %>% 
                  filter(name == input$name), rownames = FALSE) %>%
                  formatStyle(input$attr,
                  background = "skyblue",
                  fontWeight = 'bold')
    })
    
    #Tab1 - Display Time Series and Data Table Ends
    
    
    #Tab2 - Bitcoin Accepting Venues Map Begins
    
    #Define Icon Type and Colors
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = markerColors #helper.R
    )
    
    #Bitcoin accepting venues on world map with markers
    output$venues_map <- renderLeaflet({
      leaflet(sqlite_venues()) %>%
        addProviderTiles("Esri.WorldStreetMap") %>%
        addAwesomeMarkers(~lon, ~lat, icon=icons, 
                          label=~as.character(category), 
                          popup = ~as.character(name), 
                          clusterOptions = markerClusterOptions() #Cluster markers
                          )
    })
    
    #Venues by category - gVis Donut Chart 
    output$donut_venues = renderGvis({
      gvisPieChart(
        df_venues_cat_count,
        options = list(
          slices = "{8: {offset: 0.2}, 4: {offset: 0.2}, 0: {offset: 0.2}}",
          title = 'Type of Stores',
          pieSliceText = 'label',
          pieHole = 0.5,
          height = 300
        )
      )
    })
    
    #Venue categories by year - gVis Combo Chart
    output$combo_venues = renderGvis({
      gvisComboChart(
        df_venues_combo_chart,
        xvar = "YEAR",
        yvar = c("ATM", "ATTRACTION", "FOOD", "OTHER", "SHOPPING"),
        options = list(
          seriesType = "bars",
          title = "Store Types by Year",
          theme = 'maximized',
          height = 300
        )
      )
    })
    
    #Bitcoin Popularity by Country - gVis geo chart
    output$bitcoin_pop_map <- renderGvis({
      gvisGeoChart(
        pop_btc,
        locationvar = 'Country',
        colorvar = 'Popularity',
        options = list(
          projection = "kavrayskiy-vii",
          backgroundColor = "lightblue",
          width = "100%",
          height = "600px"
        )
      )
    })

    #Bitcoin Popularity Index by Country - gVis bar chart
    output$barchart_pop = renderGvis({
      gvisBarChart(
        pop_btc_top20,
        xvar = "Country",
        yvar = c("Popularity", "Popularity.style"),
        options = list(
          title = "Bitcoin Popularity Index by Country - Top 20",
          titleTextStyle="{color:'black',fontSize:20, bold:'true'}",
          bar = "{groupWidth:'80%'}",
          height = "500px",
          legend = {position = 'none'}
        )
      )
    })
    
    #Bitcoin Grouped Popularity Index Ratio by Continent - ggplot
    output$faceted_pie_pop = renderPlot({
    ggplot(pop_btc_group_continent_ratio,
           aes(x = 2,y = ratio,fill = as.factor(PopGroup))) + 
      geom_bar(position = 'fill', stat = 'identity') + 
      facet_wrap( ~ continent, nrow =2) +
      coord_polar(theta = "y") + 
      labs(x = NULL, y = NULL) + 
      xlim(0.5, 2.5) + 
      scale_fill_discrete(name = "Popularity Index Group") + 
      theme_bw() + 
      theme(plot.margin = margin(2, 1, 1, 1, "cm"),
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(), 
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
      ggtitle("Country Bitcoin Popularity Index Distribution by Continent")
    })
    
    #Tab2 - Bitcoin Accepting Venues Map Ends

    
    #Tab3 - Profit/Loss Begins
    
    #Calculate min, date for the input coin
    #Note that max. date is same for all coins so that no need to calculate
    cal_min_date = min_date_coins %>% filter(name == input$coin) %>% pull(date)

    #When user selects a different coin, update for the min. date only
    updateDateRangeInput(session, "dates",
                         #start = cal_min_date,
                         min = cal_min_date)

    
    #Calculate start price given start date
    start_price = all_coins %>% filter(name == input$coin & date == input$dates[1]) %>% pull(close)
    #Calculate end price given end date
    end_price = all_coins %>% filter(name == input$coin & date == input$dates[2]) %>% pull(close)

    perc = prettyNum(round(end_price/start_price*100), big.mark = ",")
    you_get = prettyNum(round((input$money*as.numeric(perc))/100), big.mark = ",")
    days_passed = prettyNum(as.integer(input$dates[2]-input$dates[1]), big.mark = ",")
    
    #Valuebox - Percentage
    output$perc <- renderValueBox({
      valueBox(
        subtitle = "Percentage",
        value =  paste0(perc, '%'),
        icon = icon("percentage"),
        color = "maroon",
        width = 4
      )
    })
    
    #Valuebox - You Get
    output$youget <- renderValueBox({
      valueBox(
        subtitle =  "You Get",
        value =  paste0(you_get, '$'),
        icon = icon("hand-holding-usd"),
        color = "aqua",
        width = 4
      )
    })
    
    #Valuebox - Days Passed
    output$dayspassed <- renderValueBox({
      valueBox(
        subtitle = "Days Passed",
        value = paste0(days_passed, ' Days'),
        icon = icon("hourglass-end"),
        color = "olive",
        width = 4
      )
    })
    
    #Tab4 - Profit/Loss Ends
    
    
    #Tab5 - Volatility Begins
    
    #For each new coin selected, find ym shared by all and update ym choices
    if (length(input$checkGroup_Coins) > 0) {
      for (i in 1:length(input$checkGroup_Coins)) {
        L[i] = lapply (i:i, function (x) df_daily_vol %>% filter(coin == input$checkGroup_Coins[i]) %>% select(ym))
      }
      
      reduced_ym = unlist(Reduce(intersect, L))
      names(reduced_ym) = NULL
      
      ## Can also set the label and select items
      updateCheckboxGroupInput(session, "checkGroup_YM",
                               choices = reduced_ym,
                               #selected = reduced_ym,
                               inline = TRUE
      )
    }
    
    #Plot volatility histogram
    output$barplot_vol = renderPlot({
      ggplot(df_daily_vol %>% 
               filter(coin %in% input$checkGroup_Coins & ym %in% input$checkGroup_YM)) +
        geom_col(aes(x=ym, y=daily_vol, fill=coin), position = 'dodge') +
        ggtitle("Compare Volatility of Different Coins") +
        xlab("Month") +
        ylab("Volatility")
               
    })
    
    #Plot Plotly bubble animation
    output$bubble_vol = renderPlotly({
      df_bubble %>%
        plot_ly(
          x = ~avg_market, 
          y = ~monthly_vol, 
          size = ~avg_market, 
          color = ~name, 
          frame = ~yearmonth, 
          text = ~name, 
          hoverinfo = "text",
          type = 'scatter',
          mode = 'markers'
        ) %>%
        layout(
          xaxis = list(
            type = "log"
          )
        ) %>% 
        animation_opts(
          1000, easing = "elastic", redraw = FALSE
        ) %>%
        animation_slider(
          currentvalue = list(prefix = "YEAR/MONTH ", font = list(color="red"))
        )
    })
    
    #Tab5 - Volatility Ends
    
  #End of observe
  })
  
  

  
})




