library(shiny)
library(ggplot2)
library(readxl)
library(stats)
library(vroom)
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
                  choices = c("four", "Upload"),
                  selected = '"'),


      fileInput("file", NULL, accept = c(".csv", ".XLS", ".xls"))

      # fileInput("file1", "Choose CSV File",
      #           multiple = FALSE,
      #           accept = c("text/csv",
      #                      "text/comma-separated-values,text/plain",
      #                      ".csv")),



    ),

    # Main Panel
    mainPanel(
      plotOutput("MainPlot"),
      tableOutput("test")
      )

  )
)

# Server

server<-function(input, output, session){

  up<-reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    switch(ext,
           csv = read.csv(input$file$datapath),
           XLS = read_excel(input$file$datapath),
           xls = read_excel(input$file$datapath),
           validate("Invalid file; Please upload a .csv or .xls file")
    )
  })

  dat<-reactive({
    if(input$data=="four"){
      four
    }else{
      as.data.frame(up())
      #data.frame("v1"=c(1,2,3), "v2"=c(1,2,3), "v3"=c(1,2,3), "v4"=c(1,2,3))
    }
  })

  output$MainPlot<-renderPlot({
    df<-dat()
    ggplot(data=df, aes(x=df[,1], y=df[,2]))+
      geom_point()
  })



  output$test<-renderTable({
    head(up(), 5)

    # tryCatch(
    #   {
    #     df <- read.csv(input$file$datapath)
    #   },
    #   error = function(e) {
    #     # return a safeError if a parsing error occurs
    #     stop(safeError(e))
    #   }
    # )


  })


}

# Running the app
shinyApp(ui = ui, server = server)
