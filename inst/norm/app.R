#App

#Loading the dependencies
library(shiny)
library(shinyjs)
library(dplyr)
library(stats)
library(ggplot2)
library(PlaneGeometry)
source("functions.R")

# Built in example dataset ----

four<-read.csv("fourmeasure.csv")

#User interface ----

ui<-fluidPage(

  #Aesthetics ----

  inlineCSS(list(
    ".round"="border: solid rgba(255, 99, 71, 0.3);
           border-radius: 50px;
           padding: 15px;",
    ".red"="text-color: red;"
  )),

  style = "
  font-size:20px;
  font-family:times;
  ",



  #Title ----
  titlePanel("Interactive Multivariate Assumptions Checks"
  ),

  #Choose Data ----
  fluidRow(
    column(3, class="round",
           #Selecting the type of data (example or uploaded)
           selectInput("data",
                       "Select which dataset to use",
                       choices = c("Example_Four_Measure", "Upload_Your_Own"),
                       selected = '"'),


           #Reading in the file
           fileInput("file", NULL, accept = c(".csv", ".XLS", ".xls"))
    ),

    column(9,
           "Data",
           uiOutput("row1", class="pastel"),
           tableOutput("dataHead")
    )
  ), #End first row

  #Tabs for different tasks

  tabsetPanel(

    #Task 1 tab ----
    tabPanel( "Task 1",
              fluidRow(
                column(3,class="round",
                       radioButtons("vars1",
                                    "Select variable ",
                                    choices = c("x1", "x2", "x3", "x4", "dsq"))
                ),
                column(9,
                       uiOutput("propExp"),
                       plotOutput("propPlot"),
                       uiOutput("task1")
                )
              ) #End fluid row
    ), #End tab 1


    #Task 2 tab ----
    tabPanel( "Task 2",
              fluidRow(
                column(3,class="round",
                       radioButtons("vars2",
                                    "Select variable ",
                                    choices = c("x1", "x2", "x3", "x4", "dsq"))
                ),
                column(9,
                       uiOutput("task2"),
                       plotOutput("qqPlots"),
                       uiOutput("rQ")
                )
              )#End fluid row
    ), #End tab 2

    #Task 3 tab ----
    tabPanel( "Task 3",
              fluidRow(
                column(3,class="round",
                       radioButtons("vars3x",
                                    "Select x variable ",
                                    choices = c("x1", "x2", "x3", "x4", "dsq")),
                       radioButtons("vars3y",
                                    "Select y variable ",
                                    choices = c("x1", "x2", "x3", "x4", "dsq")),
                       textInput("alpha",
                                 "Pick an alpha value",
                                 value="0.05")

                ),
                column(9,
                       uiOutput("task3"),
                       plotOutput("ellipsePlot"),
                       plotOutput("chiPlots")
                )
              )#End fluid row
    ), #End tab 3

    #Task 4 tab ----
    tabPanel( "Task 4",
              fluidRow(
                column(3,class="round",
                       radioButtons("vars4",
                                    "Select x variable ",
                                    choices = c("x1", "x2", "x3", "x4", "dsq")),

                ),
                column(9,
                       uiOutput("task4"),
                       tableOutput("zScore")

                )
              )#End fluid row
    ) #End tab 4

  ) #End tabset panels
) #End fluid page

#Server ----

server<-function(input, output, session){

  # Read in data ----
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
    if(input$data=="Example_Four_Measure"){
      four
    }else{
      as.data.frame(up())
    }
  })

  datNum <- reactive({
    dat() %>%
      select(where(is.numeric))
  })




  #Data Table ----
  output$dataHead<-renderTable({
    head(dat(),5)
  })

  #Task 1 Buttons Update ----
  newNames<-reactive({
    names(datNum())
  })

  #Updating x variable
  observe({

    temp<-as.vector(newNames())
    updateRadioButtons(session, "vars1", choices=temp)

  })

  #Task 1 Output----

  output$propExp <- renderUI({

    title <- "Proportion Test"

    exp <- paste(
      "The proportion test will give information about the tails of the distribution.
    If significantly more data points than expected under the normal distribution are
    outside the range of 1 or 2 standard deviations from the mean, the tails will be
    too thick and the test will
    fail. Similarly, if significantly fewer data points than expected under the
    normal distribution are outside the range of 1 or 2 standard deviations from the
    mean, the tails will be too skinny and the test will fail.", sep= " "
    )

    HTML( "<span style='font-size:150%'>", title, "</span>","<br>" ,
          "<span style='font-size:80%'>", exp)

  })

  v <- reactive({
    input$vars1
  })

  output$propPlot <- renderPlot({

    df <- datNum()
    v<-v()

    if(input$data=="Upload_Your_Own"){
      req(input$file)
    }

    if(is.element(v, newNames())){


      var <- df[,v]
      propPlot(var)
    }
    else{

      blank<-data.frame("1"=c(1:10), "2"=c(1:10))
      ggplot(data=blank, aes(x=1, y=2))+
        theme_void()
    }

  })

  output$task1 <- renderUI({

    v<-v()

    if(input$data=="Upload_Your_Own"){
      req(input$file)
    }

    if(is.element(v, newNames())){


      df <- datNum()
      var <- df[,input$vars1]

      test <- propTest(var)

      a <- paste(
        paste("The proportion test result for",
              input$vars1, "is",
              test$Result),
        paste("The proportion of values within 1 standard deviation for",
              input$vars1, "is",
              test$PropOne),
        paste("The proportion of values within 2 standard deviations for",
              input$vars1, "is",
              test$PropTwo),
        sep=" <br> "
      )


      HTML("<span style='font-size:80%'>",
           a)

    }

  })


  #Task 2 Buttons Update ----

  #Updating variable
  observe({

    temp<-as.vector(newNames())
    updateRadioButtons(session, "vars2", choices=temp)

  })

  #Task 2 Output ----

  v2 <- reactive({
    input$vars2
  })


  output$task2<-renderUI({

    title <- "QQ-Plot Test"

    exp <- paste(
      "The QQ plot measures the observed distribution of the data against a theoretical
    normal distribution. If the points align along the 45 degree line then the observed
    distribution follows a normal distribution. Deviations from the line indicate
    deviation from normality."
      , sep= " "
    )

    HTML( "<span style='font-size:150%'>", title, "</span>","<br>" ,
          "<span style='font-size:80%'>", exp)

  })


  output$qqPlots <- renderPlot({
    df <- datNum()
    v2<-v2()

    if(input$data=="Upload_Your_Own"){
      req(input$file)
    }

    if(is.element(v2, newNames())){


      var <- df[,v2]
      qqplot(var)
    }
    else{

      blank<-data.frame("1"=c(1:10), "2"=c(1:10))
      ggplot(data=blank, aes(x=1, y=2))+
        theme_void()
    }

  })



  output$rQ <- renderUI({

    df <- datNum()
    v2<-v2()

    if(input$data=="Upload_Your_Own"){
      req(input$file)
    }

    if(is.element(v2, newNames())){

      rq <- rQ(df[,v2])

      val <- paste(
        "The correlation coefficient of the QQ plot is", rq, sep=" "
      )

      HTML( "<span style='font-size:80%'>", val)


    }
  })

  #Task 3 Buttons Update ----

  #Updating x variable
  observe({

    temp<-as.vector(newNames())
    updateRadioButtons(session, "vars3x", choices=temp)

  })



  observe({

    temp<-as.vector(newNames())
    updateRadioButtons(session, "vars3y", choices=temp)

  })

  v3x <- reactive({
    input$vars3x
  })

  v3y <- reactive({
    input$vars3y
  })

  # Task 3 Output ----

  output$task3 <- renderUI({

    title <- "Visualizing Bivariate Normality"

    exp <- paste(
      "We can look at the spread of two seperate variables to visually assess
    bivariate normality. Choose an alpha value to see the ellipse that
    encloses 1-alpha % of the data. If the data is normally distributed, we
    expect the points to be equally scattered within the ellipse.", sep= " "
    )

    HTML( "<span style='font-size:150%'>", title, "</span>","<br>" ,
          "<span style='font-size:80%'>", exp)

  })

  alpha <- reactive({
    input$alpha
  })

  output$ellipsePlot <- renderPlot({
    df <- datNum()
    v3x<-v3x()
    v3y <- v3y()
    alpha <- as.numeric(alpha())

    if(input$data=="Upload_Your_Own"){
      req(input$file)
    }

    req(input$alpha)

    if(is.element(v3x, newNames()) & !identical(v3x, v3y)){


      varX <- df[,v3x]
      varY <- df[,v3y]
      ellipsePlot(varX, varY, alpha)
    }
    else{

      blank<-data.frame("1"=c(1:10), "2"=c(1:10))
      ggplot(data=blank, aes(x=1, y=2))+
        theme_void()+
        ggtitle("Pick 2 Different Variables")
    }

  })

  output$chiPlots <- renderPlot({
    df <- datNum()
    v3x<-v3x()
    v3y <- v3y()

    if(input$data=="Upload_Your_Own"){
      req(input$file)
    }

    req(input$alpha)

    if(is.element(v3x, newNames()) & !identical(v3x, v3y)){


      varX <- df[,v3x]
      varY <- df[,v3y]
      chiPlot(varX, varY)
    }


  })

  # Task 4 ----
  output$task4 <- renderUI({

    title <- "Look for Abnormal Values"

    exp <- paste(
      "Alongside the original data frame we have the standardized scores and the
    chi-square distance. We can look for abnormal values (for distance think large,
    for standardized scores think large absolute value) to detect outliers.", sep= " "
    )

    HTML( "<span style='font-size:150%'>", title, "</span>","<br>" ,
          "<span style='font-size:80%'>", exp)

  })

  output$zScore <- renderTable({
    df <- datNum()
    table <- outDet(df)

  })

}

#Run the app ----

shinyApp(ui=ui, server=server)
