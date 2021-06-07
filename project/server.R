library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)

data <- read.csv('data/happiness.csv')

shinyServer(function(input, output) {
    #-------------------------- Start of Page 1 ----------------------------# 
    #image of the world happiness report logo for the home page
    output$logo <- renderImage({
        list(src = "pic/happy.png", height = 400,align = "left")}, deleteFile = FALSE)
    
    output$overview <-renderText({
        paste0("<h2> Project Overview</h2>
               The report provides a broad summary of how different factors affect a country's happiness level. 
               With the results, we hope to display what affects happiness levels the most in order to encourage 
               policy changes that can lead to increase in happiness and increase overall awareness of the factors
               that affect happiness. We also hope to test a popular belief of money leading to happiness.")
    })
    output$audience <- renderText({
        paste0("<h3>Audience</h3>
               We beleive that anyone in the general population can benefit from this report as much of the data
               is harvested from polls (Cantril ladder poll) and the results may be more accurate if they have a 
               better understanding of happiness levels prior to taking the poll. The general population can also take 
               actions to encourage changes which may increase happiness levels. However, our target audience is people in government
               positions who have power to make policy changes which can lead to much quicker changes and increase in happiness.")
        
    })
    output$data <- renderText({
        paste0("<h3>Data Set</h3>
               We will be working with the <a href = https://www.kaggle.com/unsdsn/world-happiness?select=2016.csv>world happiness report</a> data set made by the Sustainable Development Solutions Network published
               on kaggle. The dataset includes data from 2015 to 2019 and ranks 155 countries by their happiness levels. The happiness 
               scores are based on answers from a poll (Cantril ladder). The poll mainly focuses on six factors and how they impact happiness. 
               (economics, psychology, survey analysis, national statistics, health, public policy)
               <br>Although the dataset contains more variables, we narrowed down the dataset and only utilized the following variables:
               Country, Year, Happiness Score, GDP.per capita, Social.Support,
               Healthy.life, Freedom.to.make.life.choices, Generosity, and  Percerptions of Corruption</br> By Narrowing down the number
               of variables, we created a more consise report that provides a broad overview of happiness levels")
    })
    output$questions <- renderText({
        paste0("<h3>Questions</h3>
               Some questions we focused on are:
               <ul>
                <li>Which component influences happiness the most?</li>
                <li>How great of an impact does the economy have on the country’s happiness?</li>
                <li>Which country’s happiness increased the most between 2015 and 2019?</li>
               </ul>")
    })
    output$conclusion <-renderText({
        paste0("<ul>
            
            <li>From our data, we found that wealth (GDP) has the greates impact on happiness and generosity had the lowest impact.
            This can be seen in the countriees that had the most increase in happiness in the five years and their
             GDP growth shown in the tables in the Happiness and Economy tab. However, it may be difficult to implement changes that 
               will immmedietly cause a large increase in wealth. So while our original goal was to encourage change 
               that will result in increased happiness, this may not be possible with only the data from the happiness report</li>
            <li>Although the World Happiness Report states that it does its best to remain unbaised, the way that
               the questions are phrased on the poll may show a cultural bias as it can emphasize western preferences
               and values. This may explain why countries that have lower GDP and seem to have a majority of happy citizens
               apppear much lower in happiness rank than what one may expect.</li>
            <li>With additional research and data of the specific country and its sitation, the project may be able to showcase
            possible initiatives that will improve overall happiness. ")
    })
    #image of a map showcasing happiness level in 2015 by vox, appears on conclusion page
    output$map <- renderImage({
        list(src = "pic/map.png", height = 400,align = "left", deleteFile=TRUE)}, deleteFile = FALSE)

    output$names <- renderText({
        paste0("<h3>Creators</h3>
               <ul>
                <li>Ethan Oh</li>
                <li>Christina Kuo</li>
                <li>Joy Mun</li>
                <li>Eli Lockard</li>
               </ul>")
    })
    #--------------------------- End of Page 1 -----------------------------# 
    
    
    #-------------------------- Start of Page 3 ----------------------------# 
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
    
    #----------------------------------End of Page 3-------------------------------#
    
    #----------------------------------Start of Page 4-------------------------------#
    
    ## Text for the page's introduction
    output$intro_text <- renderText({
        paste0("The table on this page can show the strong relationship between happiness and GDP of a country.")
    })
    
    ## Table and text for the three happiest countries with their GDP
    output$happiest_text <- renderText({
        paste0("Table of Happiest Countries in ", input$year)
    })
    output$happiest <- renderTable({
        happiest <- data %>% 
            select(Year, Overall.rank, Country, Score, GDP.per.capita) %>% 
            filter(Year == input$year) %>% 
            arrange(desc(Score)) %>% 
            head(3)
    })
    
    ## Table for the three unhappiest countries with their GDP
    output$unhappiest_text <- renderText({
        paste0("Table of least happy Countries in ", input$year)
    })
    output$unhappiest <- renderTable({
        unhappiest <- data %>% 
            select(Year, Overall.rank, Country, Score, GDP.per.capita) %>% 
            filter(Year == input$year) %>% 
            arrange(Score) %>% 
            head(3)
    })
    
    ## Table and text for the table of average GDP
    output$gdp_text <- renderText({
        paste0("The average GDP each year")
    })
    output$average_GDP <- renderTable({
        averageGDP_df <- data %>% 
            group_by(Year) %>% 
            summarize(Average_GDP = mean(GDP.per.capita))
    })
    
    ## First summary
    output$summary1_page4 <- renderText({
        paste0("Comparing the two tables, we found that the GDP of the three happiest countries are all higher than the average GDP, 
               while the GDP of the least happy countries are lower than the average GDP.")
    })
    output$summary2_page4 <- renderText({
        paste0("Additionally, we summarized the growth of the score and GDP of each country between 2015 and 2019. 
               We found that the five countries that have the most increase on happiness have either the same GDP
               or a rising GDP.")
    })
    
    ## Table and text for the table of countries that increased their happiness the most
    output$increase_text <- renderText({
        paste0("Table of Countries that their happiness increased the most between 2015 and 2019")
    })
    output$increase <- renderTable({
        increase <- data %>% 
            select(Year, Overall.rank, Country, Score, GDP.per.capita) %>% 
            filter(Year == 2015|Year == 2019) %>%
            group_by(Country) %>% 
            summarize(Growth = Score - lag(Score), GDP_growth = GDP.per.capita - lag(GDP.per.capita)) %>% 
            arrange(desc(Growth)) %>% 
            head(5)
    })
    
    ## Last summary
    output$summary3_page4 <- renderText({
        paste0("Looking at the tables, we can conclude that GDP has a strong relationship with a country's happiness score.
                Moreover, the happiness score is proportional to GDP in most cases.")
    })
    
    #----------------------------------End of Page 4-------------------------------#
    
    #----------------------------------Start of Page 2-----------------------------#
    
    output$country_select <- renderUI({
        countries <- data %>% 
            filter(Country == unique(Country)) %>% 
            arrange(by = Country)
        selectInput("Country", label = "Countries", 
                           choices = countries$Country, multiple = TRUE)
    })
    
    countries_to_graph <- reactive({
        data %>% 
            filter(Country %in% input$Country)
            
    })
    
    world_avg <- data %>% 
        group_by(Year) %>% 
        mutate(number = n()) %>% 
        mutate(Score = sum(Score) / number) %>% 
        filter(Year == unique(Year)) %>% 
        summarise(Country = "International", Year = unique(Year), Score) %>% 
        distinct()
    
    output$happiness_over_time <- renderPlot({
        ggplot(countries_to_graph(), aes(Year, Score, col = Country))+
            geom_line(data = world_avg, size = 2)+
            geom_line(size = 2)+
            labs(title = "Happiness Scores Over Time", y = "Happiness Score")
    })
    
    data_wide <- data %>% 
        select(Overall.rank, Country, Year, Score) %>% 
        pivot_wider(names_from = Year, values_from = c(Score, Overall.rank)) %>% 
        mutate(Score_Change = Score_2019 - Score_2015)
    
    
    output$largest_increase <- renderTable({
        increase <- data_wide %>%
            select(Country, Score_2015, Score_2019, Overall.rank_2015, Overall.rank_2019, Score_Change) %>% 
            arrange(desc(Score_Change)) %>% 
            head(5)
    })
    
    output$largest_decrease <- renderTable({
        decrease <- data_wide %>%
            select(Country, Score_2015, Score_2019, Overall.rank_2015, Overall.rank_2019, Score_Change) %>% 
            arrange(Score_Change) %>% 
            head(5)
    })
    
    output$inc_text <- renderText({
        paste0("\n", "The Five Countries With the largest increase in Happiness Score over the Observed Time period")
    })
    
    
    output$dec_text <- renderText({
        paste0("\n", "The Five Countries With the largest decrease in Happiness Score over the Observed Time period")
    })
    
    #----------------------------------End of Page 2-------------------------------#
})
