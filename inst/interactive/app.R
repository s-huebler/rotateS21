library(shiny)
library(ggplot2)
library(readxl)
library(stats)
library(vroom)
library(tidyselect)
library(tibble)
library(ggpubr)
library(ggExtra)
source("functions.R")

# Built in example dataset

four<-read.csv("fourmeasure.csv")

# User interface
ui<- fluidPage(


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

      #Marginal plots yes or no
      checkboxInput("marg",
                    label="Show marginal densities?",
                    value=FALSE),

      checkboxInput("outs",
                    label="Show points with 5 largest Mahalanobis Distances in Red?",
                    value=FALSE),

      #Choosing rotation angle
      textInput("angle",
                "Pick an angle (degrees)",
                value="0")


    ),


    # Main Panel
    mainPanel(

      # Rendering data table

      uiOutput("title"),

      uiOutput("intro"),

      tableOutput("frame"),

      uiOutput("mainPlotExplanation"),


      # Main plot
      plotOutput("MainPlot", click="plot_click"),


      #Drop 1 correlation
      verbatimTextOutput("drop"),

      uiOutput("rotPlotExplanation"),

      # Rotated plot
      plotOutput("RotPlot"),


      # Uniroot
      uiOutput("root")
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


  output$title<-renderUI({
    title<-"Interactive Data Cloud Rotation"

    HTML("<span style='font-size:300%'>",
         title)
  })

  output$intro<-renderUI({
    intro<-paste("Use the side panels to explore your dataset (or use the built in example).",
    "You can upload any csv or excel file as long as it has at least 2 continuous variables",
    "and column headers. You have the option to plot categorical variables, but you",
    "won't be able to use the rotation feature." ,sep="\n")

    HTML("<span style='font-size:100%'>",
      intro
    )
  })

  output$mainPlotExplanation<-renderUI({

    exM<-paste(
      "Visualize your data cloud here. You can play around with the axes by selecting ",
      "different x or y variables. Would you like to see the the marginal densities for the",
      "variables you selected? Just check the marginal densities box. Do you suspect that",
      "some of the points might be a outliers? You can change the color of the points with",
      "the 5 largest Mahalanobis distances to red. Note, this will only work if you are plotting",
      "two different variables. If you click on a point, you'll be able to see what observation",
      "number the point represents as well as the correlation between the two variables",
      "with that observation being dropped.", sep="\n"
    )

    HTML("<span style='font-size:100%'>",
         exM
    )
  })


  output$rotPlotExplanation<-renderUI({
    exR<-paste(
      "Here you can see what happens when you rotate you data clockwise. The graph ",
      "on the left is the original data cloud (there for reference). The graph on ",
      "the right is where you'll see your cloud rotate. To help keep you oriented,",
      "the orignal axes will rotate along with the data cloud. Control the angle of",
      "rotation by inputing an angle on the side panel. Careful, make sure you're in ",
      "degrees. At the bottom of the graph, you'll find the sample correlation between",
      "the two new variables. Check the very bottom of the page to see which angle will",
      "end up making the sample correlation between the rotated points 0. If you copy",
      "and paste that angle into the input, you'll be able to check for yourself that",
      "the sample correlation goes to 0.",
      sep="\n"
    )

    HTML("<span style='font-size:100%'>",
         exR
    )
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

      if(input$outs==TRUE){
      if(v!=h ){

      dist<-mahalanobis(df[,c(h,v)], c(mean(df[,h]), mean(df[,v])), cov(df[,c(h,v)]))
      df<-mutate(df, "Dist"=dist)
      df<-mutate(df, "Out"=ifelse(is.element(dist,head(dist[order(-dist)], 5)),"yes","no"))

      }else{df<-mutate(df, "Out"=rep("no",nrow(df)))}

        }
      else{
        df<-mutate(df, "Out"=rep("no",nrow(df)))
      }

      p<-ggplot(data=df, aes(x=df[,h], y=df[,v], color=Out))+
      geom_point()+
      xlab(h)+
      ylab(v)+
      ggtitle("Data Cloud")+
        coord_fixed()+
        scale_color_manual(labels=c("yes", "no"),
                          values=c("black", "red"))+
        theme(legend.position = "none")

      p1<-ggMarginal(p, type="density")

      if(input$marg==TRUE){
        p1
      }else{p}

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

  angle<-reactive({
    as.numeric(input$angle)
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

    theta<-angle()
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
        coord_fixed()+
        theme_minimal()+
        annotate(geom="label", x=scale, y=0, label=h, color="red")+
        annotate(geom="label", x=0, y=scale, label=v, color="blue")


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
        geom_vline(xintercept=0, color="black")+
        theme_minimal()+
        annotate(geom="label", x=scale*cos(-theta), y=scale*sin(-theta), label=h, color="red")+
        annotate(geom="label", x=scale*sin(theta), y=scale*cos(theta), label=v, color="blue")+
        theme(
          plot.caption = element_text(hjust=0.5, size=15)
        )

      ggarrange(g1,g2)


    }else{

      blank<-data.frame("1"=c(1:10), "2"=c(1:10))
      ggplot(data=blank, aes(x=1, y=2))+
         ggtitle("Data Cloud (Rotated")+
        annotate(geom="text", x=30, y=30, label="Pick Numeric Columns")+
        theme_void()
    }


  })


output$root<-renderUI({
  req(input$angle)
  if(input$data=="Upload_Your_Own"){
    req(input$file)
  }



df<-dat()
h<-horizontal()
v<-vertical()

  theta<-angle()
  theta<-theta*pi/180


  if(is.element(v, newNames()) & is.numeric(df[1,h])& is.numeric(df[1,v])){

    df2<-df[,c(h,v)]
    cov12<-cov(df2)[2]

    var11<-cov(df2)[1]
    var22<-cov(df2)[4]

    r<-uniroot(f=function(x){
      covTilde(t=x, s11=var11, s22=var22, s12=cov12)},
      c(0, pi/2), extendInt = "yes")$root
    d<-r*180/pi

    HTML("<span style='font-size:150%'>",
         "The first quadrant solution to",
         "s&#771 <sub> 12</sub>",
         "is", r, "radians", "or", d, "degrees")
    }
})





  # Rendering the data table
  output$frame<-renderTable({
    head(dat(), 5)
  })

}

# Running the app
shinyApp(ui = ui, server = server)
