library(shiny)
library(ggplot2)
library(readxl)
library(stats)
library(vroom)
library(tidyselect)
library(tibble)
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

      #Selecting the type of data (example or uploaded)
      selectInput("data",
                  "Select which dataset to use",
                  choices = c("Example_Four_Measure", "Upload_Your_Own"),
                  selected = '"'),


      #Reading in the file
      fileInput("file", NULL, accept = c(".csv", ".XLS", ".xls")),


      #Choosing x variable
      radioButtons("varsX",
                         "Select x variable ",
                         choices = c("x1", "x2", "x3", "x4", "dsq")),

      #Choosing y variable
      radioButtons("varsY",
                   "Select y variable ",
                   choices = c("x1", "x2", "x3", "x4", "dsq")),

      #Choosing rotation angle
      textInput("angle",
                "Pick an angle (degrees)",
                value="0")


    ),


    # Main Panel
    mainPanel(

      # Rendering data table
      tableOutput("frame"),


      # Main plot
      plotOutput("MainPlot", click="plot_click"),

      #Drop 1 correlation
      verbatimTextOutput("drop"),

      # Rotated plot
      plotOutput("RotPlot")
      )

  )
)

# Server

server<-function(input, output, session){

  # Reading in the data file based on $file input
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

  # Creating data file based on $data input
  dat<-reactive({
    if(input$data=="Example_Four_Measure"){
      four
    }else{
      as.data.frame(up())
    }
  })

  newNames<-reactive({
    names(dat())
  })

  #Updating x variable
  observe({

    temp<-as.vector(newNames())
    updateRadioButtons(session, "varsX", choices=temp)

  })

  #Updating y variable

  observe({

    temp<-as.vector(newNames())
    updateRadioButtons(session, "varsY", choices=temp)

  })


  horizontal<-reactive({
    input$varsX
  })

  vertical<-reactive({
    input$varsY
  })




  # Main plot output
  output$MainPlot<-renderPlot({

    if(input$data=="Upload_Your_Own"){
      req(input$file)
    }


    df<-dat()
    h<-horizontal()
    v<-vertical()


    if(is.element(v, newNames())){

      ggplot(data=df, aes(x=df[,h], y=df[,v]))+
      geom_point()+
      xlab(h)+
      ylab(v)+
      ggtitle("Data Cloud (Original)")
    }else{
      ggplot(data=NULL)
    }

  })


  # Drop 1 correlations
  output$drop<-renderPrint({

    req(input$plot_click)
    df<-dat()
    h<-horizontal()
    v<-vertical()
    if(is.element(v, newNames())){

    pointTest<-nearPoints(df, input$plot_click, xvar=input$varsX, yvar=input$varsY)
    pointTest<-rownames_to_column(pointTest)
    if(is.numeric(df[1,h])& is.numeric(df[1,v])){
      values<-dropCor(df, c(h,v))
      obs<-as.integer(pointTest[1,1])
      cat("Drop", "One", "Correlation", "For", "Obeservation" , obs, "Is", values[obs])}
    else{
      cat("Pick", "Numeric", "Columns")}
    }

  })


# Rotated Plot
  output$RotPlot<-renderPlot({

    req(input$angle)
    if(input$data=="Upload_Your_Own"){
      req(input$file)
    }


    df<-dat()
    h<-horizontal()
    v<-vertical()

    theta<-as.numeric(input$angle)


    if(is.element(v, newNames()) & is.numeric(df[1,h])& is.numeric(df[1,v])){


      df2<-as.data.frame(myTilde(df[,c(h,v)], theta))


      ggplot(data=df2, aes(x=x1t, y=x2t))+
        geom_point()+
        xlab(h)+
        ylab(v)+
        ggtitle("Data Cloud (Rotated)")
    }else{

      blank<-data.frame("1"=c(1:10), "2"=c(1:10))
      ggplot(data=blank, aes(x=1, y=2))+
         ggtitle("Data Cloud (Rotated")+
        annotate(geom="text", x=30, y=30, label="Pick Numeric Columns")+
        theme_void()
    }

  })





  # Rendering the data table
  output$frame<-renderTable({
    head(dat(), 5)
  })


}

# Running the app
shinyApp(ui = ui, server = server)
