library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    navbarPage("Happiness Report",
    tabPanel("Home",
         htmlOutput("overview"),
         htmlOutput("audience"),
         htmlOutput("data"),
         htmlOutput("questions"),
         htmlOutput("conclusion"),
         htmlOutput("names")
    ),
    tabPanel("What Most Influences Happiness",
             # Sidebar with a slider input for number of bins
             sidebarLayout(
                 sidebarPanel(
                     radioButtons("component", label = "components",
                                  choices = list("Gross Domestic Product(GDP)" = "GDP.per.capita",
                                                 "Social Support" = "Social.support",
                                                 "Health" = "Healthy.life.expectancy",
                                                 "Freedom" = "Freedom.to.make.life.choices",
                                                 "Generosity" = "Generosity",
                                                 "Corruption" = "Perceptions.of.corruption"),
                                  selected = "GDP.per.capita")
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     plotOutput("component"),
                     htmlOutput("cor"),
                     plotOutput("corPlot"),
                     textOutput("summary")
                 ))
    ),
    tabPanel("Happiness and Economy",
             # Sidebar with buttons to choose a year
             sidebarLayout(
                 sidebarPanel(
                     radioButtons("year", label = "Year",
                                  choices = list("2015" = 2015, "2016" = 2016, "2017" = 2017, "2018" = 2018, "2019" = 2019), 
                                  selected = 2015)
                 ),
                 
                 # Show the tables
                 mainPanel(
                     textOutput("intro_text"),
                     textOutput("happiest_text"),
                     tableOutput("happiest"),
                     textOutput("unhappiest_text"),
                     tableOutput("unhappiest"),
                     textOutput("gdp_text"),
                     tableOutput("average_GDP"),
                     textOutput("summary1_page4"),
                     textOutput("summary2_page4"),
                     textOutput("increase_text"),
                     tableOutput("increase"),
                     textOutput("summary3_page4")
                 )
             )
    ),
    tabPanel("Happiness Over Time"
    )
))

