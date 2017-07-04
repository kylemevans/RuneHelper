library(dplyr)
library(shiny)
library(plotly)
library(jsonlite)
library(purrr)
library(httr)

#Function that sets up runescape data from csv files
initData <- function() {
  #Import Runescape data from split files and turn to tables
  files <-
    list.files(path = "data/",
               pattern = "Runescape_Market_Data",
               full.names = TRUE)
  tables <- lapply(files, read.csv, stringsAsFactors = FALSE)
  
  #Merge 8 data files into one data frame
  runescape.data <- do.call(rbind, tables)
  
  #Fix PriceDate to make it readable and neat
  runescape.data <-
    runescape.data %>% mutate(PriceDate = as.Date(as.POSIXct(
      as.POSIXct(runescape.data$PriceDate, origin = "1970-01-01"),
      origin = "1970-01-01"
    )))
  
  #return data for scope
  return (runescape.data)
}

# Runescape likes to frequently change the item codes that are used for making
# API calls so this gets the most recent item codes from a 3rd party API
initItemCodesData <- function() {
  # Loads Runescape item data from API into a list (API has odd formatting)
  l <- RJSONIO::fromJSON('http://mooshe.pw/files/items_rs3.json')
  
  # Converts the list of API data into a data frame
  item.codes <- l %>% transpose() %>% map_df(simplify)
  
  # Add the item IDs as a column to the item.codes data frame
  item.codes$id <- names(l)
  
  # Gets rid of the unnecessary columns from the API for efficiency
  item.codes <- select(item.codes, name, id)
  
  # Returns the dataframe of Runescape item codes
  return (item.codes)
}

#Initialize Grand Exchange data from 'data' folder
runescape.data <- initData()

#Initialize 3rd party item code data
item.codes <- initItemCodesData()

#Vector containing unique categories of items
unique.category <- sort(as.vector(unique(runescape.data$Category)))

shinyServer(function(input, output) {
  #Set category dataframe to be later modified and scoped
  selected.category <- runescape.data
  
  #Set item dataframe to be later modified and scoped
  selected.item <- runescape.data
  
  #Render itemSelect ui
  output$itemSelect <- renderUI({
    #dataframe with filtered category is the category the user selected
    selected.category <-
      filter(runescape.data, Category == input$category)
    
    #Vector containing unique items for selected category
    unique.item <-
      sort(as.vector(unique(selected.category$ItemName)))
    
    #drop down to select item from category list
    selectInput("item",
                "Item:",
                unique.item,
                selected = unique.item[1],
                multiple = FALSE)
  })
  
  #Render dateSelect ui
  output$dateSelect <- renderUI({
    #set up selected.item dataframe and get min and max values
    selected.item <-
      selected.category %>% filter(ItemName == input$item)
    min.date <- min(selected.item$PriceDate, na.rm = TRUE)
    max.date <- max(selected.item$PriceDate, na.rm = TRUE)
    
    #Load slider for input dates
    sliderInput(
      "dateSelect",
      "Choose Date Range:",
      min = min.date,
      max = max.date,
      value = c(min.date, max.date)
    )
  })
  
  #Render plot of item price over time
  output$graphic <- renderPlotly({
    #Dates, min and max, taken from the above input slider.
    min.date <- min(input$dateSelect)
    max.date <- max(input$dateSelect)
    
    #Using DPLYR functions, creates new column for gold values to be plotted.
    plot.data <- runescape.data %>%
      filter(ItemName == input$item,
             PriceDate > min.date & PriceDate < max.date) %>%
      group_by(PriceDate) %>%
      summarize(Gold = mean(Price))
    
    #Using Plotly, plots data from above created dataframe
    plot_ly(
      plot.data,
      x = ~ PriceDate,
      y = ~ Gold,
      type = "scatter",
      mode = 'lines',
      hoverinfo = 'text',
      text = ~ paste(PriceDate, ",", Gold, 'GP')
    ) %>%
      layout(
        title = "Price (GP) vs. Time",
        plot_bgcolor = 'rgba(193, 205, 205, 0.8)',
        paper_bgcolor = 'rgba(193, 205, 205, 0.8)',
        xaxis = list(title = "Date")
      )
  })
  
  #Renders the image of the item that the user is searching for information about. Gets
  #the image URL from the Runescape API
  output$ItemImage = renderUI({
    # In case there is more than one item with the same item ID, get the first one
    # (the Runescape API says this is the best practice)
    item.id <- head(item.codes %>% filter(name == input$item), 1)
    
    # Get the image URL for the item icon based on its item id
    image.url = paste0(
      "http://services.runescape.com/m=itemdb_rs/obj_big.gif?id=",
      item.id$id
    )
    
    # Set the item icon as an image to be displayed in the app UI
    #Check if URL is valid
    if (url_ok(image.url)) {
      img(src = image.url)
    } else {
      p("Image not available")
    }
    
  })
  
  #Render table under date slider
  output$ItemInfo <- renderTable({
    base <-
      "http://services.runescape.com/m=itemdb_rs/api/catalogue/detail.json?item="
    
    # In case there is more than one item with the same item ID, get the first one
    # (the Runescape API says this is the best practice)
    item.id <- head(item.codes %>% filter(name == input$item), 1)
    
    #Parsing url for JSON
    url <- paste0(base, item.id$id)
    
    #Parsed URL to pull data from Runescape API. Try function to test if URL returns 404 error.
    item.data <- try(fromJSON(url))
    
    #If/Else statement for whether or not URL returns 404 error.
    # If 404 error, this means the item has been removed from the Grand Exchange
    # between the time of taking item data and now.
    if (item.data[1] != "Error in open.connection(con, \"rb\") : HTTP error 404.\n") {
      #make table data from Runescape price history data
      table.data <-
        runescape.data %>% filter(ItemName == input$item)
      Info <-
        c(
          'Description',
          'Current Price (GP)',
          '% Change in Last 30 Days',
          '% Change in Last 90 Days',
          '% Change in Last 180 Days',
          'Members Only',
          'Low Alch',
          'High Alch'
        )
      # Adds all of the Runescape API data about current Grand Exchange prices
      # if the item is available in the GE
      Data <-
        c(
          item.data[[1]]$description[[1]],
          item.data[[1]]$current$price,
          item.data[[1]]$day30$change,
          item.data[[1]]$day90$change,
          item.data[[1]]$day180$change,
          table.data$MembersOnly[[1]],
          table.data$LowAlch[[1]],
          table.data$HighAlch[[1]]
        )
    } else {
      #If URL is 404 error/item is not in Grand Exchange, return message.
      Info <- "Details N/A"
      Data <- "Item no longer in the Grand Exchange"
    }
    
    #Display Both columns
    return(data.frame(Info, Data))
  })
  
  output$perCategory <- renderPlotly({
    #Bunch of if/elses for each radio button. Combines multiple similar categories into one.
    if (input$checkCategory == 1) {
      combined.category <-
        c("Ammo",
          "Arrows",
          "Bolts",
          "Tools and containers",
          "Pocket items")
    } else if (input$checkCategory == 2) {
      combined.category <-
        c(
          "Construction materials",
          "Construction products",
          "Woodcutting product",
          "Mining and Smithing",
          "Familiars",
          "Fletching materials"
        )
    } else if (input$checkCategory == 3) {
      combined.category <-
        c("Farming produce",
          "Cooking ingredients",
          "Food and Drink",
          "Hunting Produce")
    } else if (input$checkCategory == 4) {
      combined.category <-
        c("Herblore materials",
          "Seeds",
          "Jewellery",
          "Prayer materials",
          "Potions")
    } else if (input$checkCategory == 5) {
      combined.category <-
        c(
          "Mage armour",
          "Melee armour - high level",
          "Melee armour - low level",
          "Melee armour - mid level",
          "Range armour",
          "Prayer armour"
        )
    } else if (input$checkCategory == 6) {
      combined.category <-
        c(
          "Range weapons",
          "Mage weapons",
          "Hunting equipment",
          "Melee weapons - high level",
          "Melee weapons - low level",
          "Melee weapons - mid level"
        )
    } else if (input$checkCategory == 7) {
      combined.category <-
        c(
          "Runecrafting",
          "Runes, Spells and Teleports",
          "Summoning scrolls",
          "Crafting materials",
          "Miscellaneous"
        )
    } else if (input$checkCategory == 8) {
      combined.category <- c("Costumes")
    } else {
      combined.category <- c(unique.category)
    }
    
    #Creating new filtered data frame per category
    #Prices dates assigned are the ones that are the max/min for our data set
    categoryFrame <-
      runescape.data %>% filter(
        Category %in% c(combined.category),
        PriceDate > '2011-10-30',
        PriceDate < '2015-06-20'
      ) %>% group_by(PriceDate) %>% summarize(Gold = round(mean(Price)))
    
    #Plotting data for per category line graph
    plot_ly(
      categoryFrame,
      x = ~ PriceDate,
      y = ~ Gold,
      name = 'Ammo',
      type = "scatter",
      mode = 'lines',
      line = list(color = 'rgb(205, 12, 24)'),
      hoverinfo = 'text',
      text = ~ paste(PriceDate, ",", Gold, 'GP')
    ) %>%
      layout(
        title = "Average Price per Category",
        plot_bgcolor = 'rgba(193, 205, 205, 0.8)',
        paper_bgcolor = 'rgba(193, 205, 205, 0.8)'
      )
    
  })
  
  #Render high vs low alch chart
  output$alchChart <- renderPlotly({
    #Creates a new data frame with one high and one low alch for each item
    y <-
      runescape.data[!duplicated(runescape.data$ItemName), c(3:5)]
    
    #Removes outliers
    alchData <-
      y %>% filter(LowAlch < 200000) %>% filter(HighAlch < 200000)
    
    #Using Plotly, plots high vs low alchemy chart
    plot_ly(
      alchData,
      x = ~ LowAlch,
      y = ~ HighAlch,
      text = ~ ItemName,
      type = "scatter",
      mode = "markers"
    ) %>% layout(
      plot_bgcolor = 'rgba(193, 205, 205, 0.8)',
      paper_bgcolor = 'rgba(193, 205, 205, 0.8)',
      xaxis = list(title = "Low Alchemy"),
      yaxis = list(title = "High Alchemy"),
      showlegend = FALSE
    )
  })
  
  #Render the table for the category statistics
  output$categoryTable <- renderTable({
    #Make data frame filtered per category
    stats <- runescape.data %>% filter(Category == input$statCat)
    
    #Assign info and data for max item
    Info <-
      c("Item Name", "Most Expensive Price", "Date Occurrence")
    Data <-
      c(stats$ItemName[stats$Price == max(stats$Price, na.rm = TRUE)][1],
        paste0(unique(stats$Price[stats$Price == max(stats$Price, na.rm = TRUE)]), " GP"),
        as.character(as.Date(stats$PriceDate[stats$Price == max(stats$Price, na.rm = TRUE)][1])))
    
    #Return data frame with format data
    return(data.frame(Info, Data))
  })
})
