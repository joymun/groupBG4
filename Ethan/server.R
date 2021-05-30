library(shiny)
library(dplyr)
library(ggplot2)

data <- read.csv('../happiness.csv')
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({
        data <- data %>% 
            mutate(Year = factor(Year))
        ggplot(data, aes_string(x = input$component, y = "Score", color = "Year")) +
            geom_point(size = 3)
    })
    output$cor <- renderText({
        cor.test <- cor(data[,input$component], data$Score)
        paste0("Based on Parametric correlation method, estimate of orrelation between Happiness Score and ", input$component, " was ", cor.test)
    })
    GDP.test <- cor(data$GDP.per.capita, data$Score)
    SS.test <- cor(data$Social.support, data$Score)
    Health.test <- cor(data$Healthy.life.expectancy, data$Score)
    Freedom.test <- cor(data$Freedom.to.make.life.choices, data$Score)
    Generosity.test <- cor(data$Generosity, data$Score)
    Corruption.test <- cor(data$Perceptions.of.corruption, data$Score)
    components <- c("GDP", "Social Security", "Health", "Freedom", "Generosity", "Corruption")
    cor.num <- c(GDP.test, SS.test, Health.test, Freedom.test, Generosity.test, Corruption.test)
    df <- data.frame(components,cor.num)
    
    output$corPlot <- renderPlot({
        ggplot(df, aes(cor.num,components))+
            geom_bar(stat = 'identity',fill = "green")
    })
    output$summary <- renderText({
        highest <- df %>% 
            filter(cor.num == max(cor.num))
        paste0("We can assume that what influences happiness is the most is ", highest$components)
    })
})
