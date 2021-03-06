library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    navbarPage("Happiness Report",
    tabPanel("Home",
         imageOutput("logo"),
         htmlOutput("overview"),
         htmlOutput("audience"),
         htmlOutput("data"),
         htmlOutput("questions"),
         htmlOutput("names")
    ),
    tabPanel("Happiness Over Time",
             sidebarLayout(
                 sidebarPanel(
                     #drop down country selection widget
                     uiOutput("country_select")
                 ),
                 
                 mainPanel(
                     # show a plot that graphs happiness of the chosen countries over time
                     plotOutput("happiness_over_time"),
                     textOutput("inc_text"),
                     # shows table with the five countries with the largest increase in happiness
                     tableOutput("largest_increase"),
                     # shows table with the five countries with the largest decrease in happiness
                     textOutput("dec_text"),
                     tableOutput("largest_decrease")
                 )
             )
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
    tabPanel("Conclusion",
             imageOutput("map"),
             htmlOutput("conclusion")
    )
))

