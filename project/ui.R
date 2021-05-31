library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    navbarPage("Happiness Report",
    tabPanel("Home"
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
    tabPanel("Happiness and Economy"
    ),
    tabPanel("Happiest Countries"
    ),
    tabPanel("Analysis")
))

