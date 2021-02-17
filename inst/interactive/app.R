library(shiny)
library(ggplot2)
library(readxl)
library(stats)
source("functions.R")

# Built in example dataset

four<-read.csv("fourmeasure.csv")

# User interface
ui<- fluidPage(
  # Title Panel
  titlePanel("Interactive Data Cloud Rotation"),

  # Sidebar
  sidebarLayout(

    sidebarPanel(

      selectInput("data",
                  "Select which dataset to use",
                  choices = c("four"))

    ),

    # Main Panel
    mainPanel(
      plotOutput("MainPlot"))

  )
)

# Server

server<-function(input, output, session){

  data<-reactive({
    if(input$data=="four"){
      four
    }else{
      data.frame("v1"=c(1,2,3), "v2"=c(1,2,3), "v3"=c(1,2,3), "v4"=c(1,2,3))
    }
  })

  output$MainPlot<-renderPlot({
    ggplot(data=data, aes(x=data[,1], y=data[,2]))+
      geom_point()
  })
}

# Running the app
shinyApp(ui = ui, server = server)
