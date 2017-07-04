library(shiny)
library(dplyr)
library(plotly)
library(shinythemes)

source('server.R')

shinyUI(
  fluidPage(
    theme = shinytheme('slate'),
    style = "font-family: 'Century Gothic';
    font-size: 9pt;
    background: url('bg.jpg');
    background-size: cover",
    
    #Hide warnings from showing up on page
    tags$style(
      type = "text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    navbarPage(
      tags$b("RuneHelper"),
      
      tabPanel(
        "Overview",
        
        # Header
        titlePanel(img(
          src = 'RunescapeLogo.png',
          width = '300px',
          height = '100px'
        )),
        
        sidebarLayout(# Credits
          sidebarPanel(
            h5("Developed by Kyle Evans"),
            img(
              src = 'GE.png',
              width = '250px',
              height = '220px'
            )
          ),
          
          # Overview
          mainPanel(tabsetPanel(
            #Introduction to project
            tabPanel(
              "Introduction",
              h1("Introduction"),
              p(
                "Runescape is a free to play MMORPG (massively multiplayer online role playing game) that contains its own economy. In the game, players buy and sell virtual items for virtual gold coins through an in-game system called the Grand Exchange. Like a real economy, the price of a given item fluctuates with time and is dependent on a variety of ingame variables. The dataset used here contains approximately three years worth of data that describes the price of all in-game items that are available to players. This dataset was initially from Reddit, which was under a Github repository. Since Runescape has a low-level API, the user who initially compiled the data had to scrape the data from a website that had all of the prices for different items over the past years. This took approximately 20 hours using a scraping applications named BeautifulSoup and Requests. The target audience for this application would be Runescape users who want to get more information on their items, in order to get an edge while playing the game. The users are able to get a plethora of information from the scraped file. The original data set was 300MB, but was cut down to 200MB by removing unnecessary data. The users are now able to ask: \"What would the price of a certain item be in the next month?\", \"How did the price change relative to the past three years?\", \"What items fluctuates more or less relative to other items?\", and \"Are there certain economic trends that can be used to predict the price in the distant future?\""
              ),
              br(), br(), br(), br(), br(), br(), br(), br(),
              br(), br(), br(), br(), br(), br(), br(), br()
            )
          )))
      ),
      
      #Standard Price Chart for a single item
      tabPanel(
        "Simple Item Price Chart",

        sidebarLayout(
          #Price Chart Controls
          sidebarPanel(
            #Category Dropdown
            selectInput(
              "category",
              label = h5("Category:"),
              unique.category,
              selected = unique.category[1],
              multiple = FALSE
            ),
            
            #Item Dropdown
            uiOutput("itemSelect"),
            
            #Date Range Slider
            uiOutput("dateSelect"),
            
            #Table containing additonal item information
            h5("Item Information:"),
            
            #Image of the item (if available)
            htmlOutput('ItemImage'),
            
            #Additional information relevent to the item (if available)
            tableOutput('ItemInfo')
          ),
          #Item Price Chart
          mainPanel(
            h3("Simple Price Chart"),
            p(
              'Interactive line graph with descriptive stats based on gold, month, and year. Gives a basic overview of a chosen item\'s selling performance on the Grand Exchange over the last few years. Use the date slider to the left to reactively change the data based on a range of dates!'
            ),
            plotlyOutput('graphic'),
            img(
              src = 'coins.png',
              width = '500px',
              height = ' 100px'
            ),
            br(), br(), br(), br(), br(), br(), br(), br(),
            br(), br(), br(), br(), br(), br(), br(), br()
          )
        )
      ),
      # New tab for more charts
      tabPanel(
        "Runescape Economy Statistics",
        sidebarLayout(
          # Conditional Panel that changes the sidebar based on which tabset panel is selected.
          sidebarPanel(
            conditionalPanel(
              condition = "input.conditionalPanels == 1",
              h4('Alchemy'),
              p(
                "Alchemy is a magic ability in Runescape. It is used to turn items into gold. As a player levels up their magic skill, they first unlock the Low Level of alchemy, but eventually will unlock a High Level. The High Level alchemy skill gives a higher return on investment; it gives more gold for an item than the Low Level spell."
              ),
              p(
                "The amount of gold a player gets when they perform an alchemy spell on an item is static. It never changes. This chart shows the amount of gold recieved when performing the High Level spell vs the amount recieved when performing the Low Level one."
              ),
              p(
                "The vast majority of the data lies almost exactly on a line. The High Level spell almost always provides 50% more than the Low Level spell. There are a mere handful of examples where this is not the case, but it holds true for the remaining ~2000 observations."
              ),
              img(src = 'alch.gif',
                  width = '150px',
                  height = '200px')
            ),
            conditionalPanel(
              condition = "input.conditionalPanels == 2",
              radioButtons(
                "checkCategory",
                label = "Categories:",
                choices = list(
                  'All' = 9,
                  'Ammunition/Tools' = 1,
                  'Building/Cooking' = 2,
                  'Food/Drink' = 3,
                  'Herblore' = 4,
                  'Armour' = 5,
                  'Weapons' = 6,
                  'Runes' = 7,
                  'Costumes' = 8
                ),
                selected = 9
              ),
              img(src = 'walk.gif',
                  width = '150px',
                  height = '200px')
            ),
            conditionalPanel(
              condition = "input.conditionalPanels == 3",
              p("This tab allows the user to see the statistics of each category."),
              
              selectInput(
                "statCat",
                label = h5("Category:"),
                unique.category,
                selected = unique.category[1],
                multiple = FALSE
              ),
              img(src = 'partyhat.gif',
                  width = '150px',
                  height = '230px')
            )
          ),
          # Tabset panels of a few more charts
          mainPanel(
            h3("Economic Charts"),
            tabsetPanel(
              id = "conditionalPanels",
              
              #Alchemy Tab
              tabPanel("Alchemy Chart", value = 1,
                       plotlyOutput("alchChart")),
              #Per Category Chart
              tabPanel(
                "Per Category Chart",
                value = 2,
                p(
                  'An Interactive chart that shows the overall average gold prices based on category, which can be chosen by the radio buttons to the left.'
                ),
                p(
                  'This chart serves as a great resource to get a general overall sense of how the Runescape global economy looked over a span of a few years, as well as spot any trends that could impact the future of the economy.'
                ),
                plotlyOutput('perCategory'),
                p(
                  'Sudden dips or rises in prices across categories are due to missing data, which can be attributed to real world events'
                ),
                p(
                  '(Notice how similar the charts for All items and Costumes are. This is due to the fact that the Costumes category has within it, the infamous Blue Party Hat, the most expensive item in the entire game!)'
                )
              ),
              
              #Highest Price Chart
              tabPanel("Statistic Per Category", value = 3,
                       
                       #Render table for third panel
                       tableOutput("categoryTable"))
              
            ),
            
            br(), br(), br(), br(), br(), br(), br(), br(),
            br(), br(), br(), br(), br(), br(), br(), br()
          )
        )
      )
    )
  )
)
