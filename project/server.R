library(shiny)
library(dplyr)
library(ggplot2)

data <- read.csv('../happiness.csv')

shinyServer(function(input, output) {
    
    
    #-------------------------- Start of Page 2 ----------------------------# 
    output$component <- renderPlot({
        data <- data %>% 
            mutate(Year = factor(Year))
        ggplot(data, aes_string(x = input$component, y = "Score", color = "Year")) +
            geom_point(size = 3) +
            labs(title = paste0("Relationship Between Happiness score and ", input$component), y = "Happiness Score")
    })
    output$cor <- renderText({
        cor.test <- cor(data[,input$component], data$Score)
        if(cor.test >= 0.9){
            result <- "<br/>Correlation coefficients whose magnitude are between 0.9 
            and 1.0 indicate variables which can be considered very highly correlated.<br/>"
        } else if(cor.test >= 0.5){
            result <- "<br/>Correlation coefficients whose magnitude are between 0.5 and 0.7 indicate 
            variables which can be considered moderately correlated.<br/>"
        } else if(cor.test >= 0.3){
            result <- "<br/>Correlation coefficients whose magnitude
            are between 0.3 and 0.5 indicate variables which have a low correlation.<br/>"
        } else {
            result <- "<br/>Correlation coefficients whose magnitude are between 0.0 and 0.3 indicate 
                   variables which can be considered very low correlated.<br/>"
        }
        HTML(paste0("Based on Parametric correlation method, estimate of correlation between Happiness Score and ",
               input$component, " was ", cor.test,".", result, sep = "<br/>"))
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
        ggplot(df)+
            geom_col(aes(reorder(components,cor.num),cor.num, fill = components)) +
            labs(title = 'Correlation Coefficient of each component', y = 'Pearson Correlation Coefficient', x = 'Components')
    })
    output$summary <- renderText({
        highest <- df %>% 
            filter(cor.num == max(cor.num))
        paste0("We found that GDP is the most determining factor of happiness. While Heatlh Expectancy, Social Security
        , Freedom, and Corruption showed moderate correlations with happiness, Generosity was considered
        very low correlated.")
    })
    
    #----------------------------------End of Page2-------------------------------#
})
