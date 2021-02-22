library(shiny)
library(ggplot2)
library(readxl)
library(stats)
library(vroom)
library(tidyselect)
library(tibble)
library(ggpubr)
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
      plotOutput("RotPlot"),


      # Uniroot
      verbatimTextOutput("root")
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
      ggtitle("Data Cloud")
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
    theta<-theta*pi/180


    if(is.element(v, newNames()) & is.numeric(df[1,h])& is.numeric(df[1,v])){

      scale<-sqrt(max(abs(df[,h]))**2+ max(abs(df[,v]))**2 )

      df2<-myTilde(df[,c(h,v)], theta)
      s<-round(cor(df2)[2],5)


      g1<-ggplot(data=df, aes(x=df[,h], y=df[,v]))+
        geom_point()+
        xlab("")+
        ylab("")+
        ggtitle("Data Cloud (Unrotated)")+
        xlim(-scale, scale)+
        ylim(-scale, scale)+
        geom_hline(yintercept=0, color="red")+
        geom_vline(xintercept=0, color="blue")+
        coord_fixed()


      g2<-ggplot(data=df2, aes(x=x1t, y=x2t))+
        geom_point()+
        xlab("")+
        ylab("")+
        xlim(-scale, scale)+
        ylim(-scale, scale)+
        coord_fixed()+
        ggtitle("Data Cloud (Rotated)")+
        labs(caption=(paste("Sample Correlation:",  s, sep=" ")))+
        geom_abline(aes(slope=
                          ifelse(2*theta/pi==round(2*theta/pi) & theta!=pi & theta!=0,
                                 10000000 ,-tan(theta)), intercept = 0), color="red",lty=2)+
        geom_abline(aes(slope=
                          ifelse(theta/pi==round(theta/pi),
                                 10000000 ,1/tan(theta)), intercept = 0),color="blue" ,lty=2)+
        geom_hline(yintercept=0, color="black")+
        geom_vline(xintercept=0, color="black")

      ggarrange(g1,g2)


    }else{

      blank<-data.frame("1"=c(1:10), "2"=c(1:10))
      ggplot(data=blank, aes(x=1, y=2))+
         ggtitle("Data Cloud (Rotated")+
        annotate(geom="text", x=30, y=30, label="Pick Numeric Columns")+
        theme_void()
    }


  })

output$root<-renderPrint({
  req(input$angle)
  if(input$data=="Upload_Your_Own"){
    req(input$file)
  }



df<-dat()
h<-horizontal()
v<-vertical()

  theta<-as.numeric(input$angle)
  theta<-theta*pi/180


  if(is.element(v, newNames()) & is.numeric(df[1,h])& is.numeric(df[1,v])){


    cov12<-cov(df)[2]

    var11<-cov(df)[1]
    var22<-cov(df)[4]

    r<-uniroot(f=function(x){
      covTilde(t=x, s11=var11, s22=var22, s12=cov12)},
      c(0, pi/2), extendInt = "yes")$root
    d<-r*180/pi

  cat("The first quadrant solution to Tilde S12=0 is", r, "radians or", d,"degrees")

  }
})





  # Rendering the data table
  output$frame<-renderTable({
    head(dat(), 5)
  })


}

# Running the app
shinyApp(ui = ui, server = server)
