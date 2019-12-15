#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)

setwd("C:/Users/Cameron/Documents/College/Senior/ds202/Project/ds202_project")
movie = read.csv("IMDB-Movie-Data.csv")
newMovie = movie %>% separate("Actors", sep = ",", into = c("Actor1", "Actor2", "Actor3", "Actor4")) %>%
    separate("Genre", sep = ",", into = c("Genre1", "Genre2", "Genre3"))
names(newMovie)[c(13,16)] = c("Runtime", "Revenue")
newerMovie = newMovie %>% select(-Rank, -Description) %>% filter(!is.na(Revenue), !is.na(Rating),
                                                                 !is.na(Votes), !is.na(Metascore),
                                                                 !is.na(Runtime))
actor1 = newerMovie %>% select(Actor1, Rating, Votes, Revenue, Metascore, Runtime)
actor2 = newerMovie %>% select(Actor2, Rating, Votes, Revenue, Metascore, Runtime)
actor3 = newerMovie %>% select(Actor3, Rating, Votes, Revenue, Metascore, Runtime)
actor4 = newerMovie %>% select(Actor4, Rating, Votes, Revenue, Metascore, Runtime)
names(actor1)[c(1)] = c("Actor")
names(actor2)[c(1)] = c("Actor")
names(actor3)[c(1)] = c("Actor")
names(actor4)[c(1)] = c("Actor")
actorSet = rbind(actor1, actor2, actor3, actor4)
actorSet = actorSet %>% filter(!is.na(Actor))
for(i in 1:length(actorSet$Actor)){
    if(substr(actorSet$Actor[i], 1, 1) == " "){
        actorSet$Actor[i] = substr(actorSet$Actor[i], 2, nchar(actorSet$Actor[i]))
    }
}
actorGrouped = actorSet %>% group_by(Actor) %>% summarize(count = n(),
                                                          Runtime = mean(Runtime),
                                                          Revenue = mean(Revenue),
                                                          Rating = mean(Rating),
                                                          Votes = mean(Votes),
                                                          Metascore = mean(Metascore))
actorFinal = filter(actorGrouped, count>4)

directorGrouped =  newerMovie %>% group_by(Director) %>% summarize(count = n(),
                                                                   Revenue = mean(Revenue),
                                                                   Rating = mean(Rating),
                                                                   Votes = mean(Votes),
                                                                   Runtime = mean(Runtime),
                                                                   Metascore = mean(Metascore))

directorFinal = filter(directorGrouped, count>2)

genre1 = newerMovie %>% select(Genre1, Rating, Votes, Revenue, Metascore, Runtime)
genre2 = newerMovie %>% select(Genre2, Rating, Votes, Revenue, Metascore, Runtime)
genre3 = newerMovie %>% select(Genre3, Rating, Votes, Revenue, Metascore, Runtime)
names(genre1)[c(1)] = c("Genre")
names(genre2)[c(1)] = c("Genre")
names(genre3)[c(1)] = c("Genre")
genreSet = rbind(genre1, genre2, genre3)
genreSet = genreSet %>% filter(!is.na(Genre))
genreGrouped = genreSet %>% group_by(Genre) %>% summarize(count = n(),
                                                          Revenue = mean(Revenue),
                                                          Rating = mean(Rating),
                                                          Votes = mean(Votes),
                                                          Metascore = mean(Metascore),
                                                          Runtime = mean(Runtime))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("IMDB Data"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(3,
            selectInput("selectType", h3("Actor/Director/Genre"),
                        choices = list("Actor", "Director", "Genre"),
                        selected = "Actor")
        ),
        column(3,
            selectInput("selectCriteria", h3("Select Criteria"), 
                        choices = list("Revenue", "Rating",
                                       "Votes", "Metascore",
                                       "Runtime"),
                        selected = "Revenue")
        ),
        column(3,
               sliderInput("num",
                           "Number of values:",
                           min = 1,
                           max = 20,
                           value = 10)
        )
    ),

    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("barPlot")
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$barPlot <- renderPlot({
        if(input$selectType == "Actor"){
            actorFinal = head(actorFinal[order(-actorFinal[,input$selectCriteria]),], input$num)
            frame = as.data.frame(actorFinal)
            frame$Actor = factor(frame$Actor, levels = frame[order(frame[,input$selectCriteria]),1])
            ggplot(frame) + geom_bar(aes_string(x = input$selectType, y = input$selectCriteria,
                                              fill = input$selectCriteria), stat = "identity") + 
                geom_label(aes_string(x=input$selectType,y = input$selectCriteria, label = paste("count")), size = 5) +
                coord_flip()
        }
        else if(input$selectType == "Director"){
            directorFinal = head(directorFinal[order(-directorFinal[,input$selectCriteria]),], input$num)
            frame = as.data.frame(directorFinal)
            frame$Director = factor(frame$Director, levels = frame[order(frame[,input$selectCriteria]),1])
            ggplot(frame) + geom_bar(aes_string(x = input$selectType, y = input$selectCriteria,
                                                       fill = input$selectCriteria), stat = "identity") + 
                geom_label(aes_string(x=input$selectType,y = input$selectCriteria, label = paste("count")), size = 5) +
                coord_flip()
        }
        else{
            genreGrouped = head(genreGrouped[order(-genreGrouped[,input$selectCriteria]),], input$num)
            frame = as.data.frame(genreGrouped)
            frame$Genre = factor(frame$Genre, levels = frame[order(frame[,input$selectCriteria]),1])
            ggplot(frame) + geom_bar(aes_string(x = input$selectType, y = input$selectCriteria,
                                                        fill = input$selectCriteria), stat = "identity") + 
                geom_label(aes_string(x=input$selectType,y = input$selectCriteria, label = paste("count")), size = 5) +
                coord_flip()
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
