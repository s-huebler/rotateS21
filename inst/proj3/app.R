#App

#Loading the dependencies
library(shinyjs)
library(dplyr)
library(stats)
library(ggplot2)
library(CCA)
library(RCurl)

# Built in example dataset ----

covid <- read.csv("covidWrangled.csv")

#User interface ----

ui<-fluidPage(


  #Aesthetics ----
  tags$head(
    tags$style(HTML("
  body {
    background-color: grey;
    color: white;
    font-family: times;
    font-size:20px;
    margin: 70px;

  }
  ")),
    tags$style(HTML("
  a{
    color: white;
    padding: 20px;
    margin: 20px;
  }
   ")),

    tags$style(HTML('table.dataTable tr:nth-child(even) {background-color: grey !important;}')),
    tags$style(HTML('table.dataTable tr:nth-child(odd) {background-color: grey !important;}')),
    tags$style(HTML('table.dataTable th {background-color: grey !important;}')),





  inlineCSS(list(
    ".round"="border: solid rgba(255, 255, 255, 0.3);
           border-radius: 50px;
           padding: 30px;
    ",
    ".round2"="border: solid rgba(255, 255, 255, 0.3);
           border-radius: 100px;
           padding: 70px;
           text-align:center;
           margin: 70px;
           margin-right: 70px;
    ",
    ".red"="text-color: red;
    ",
    ".pad" = "padding:100px
    "
  )),

  #Title----
  h1(id="big-heading", "Using CCA to Describe"),
  h1(id="big-heading","Trends in Covid Severity by"),
  h1(id="big-heading","Patient Demographics"),
  h1(id="small-heading", "By Sophie Huebler"),
  tags$style(HTML("#big-heading{color: white;
                  text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;
                  font-size:50px;
                  text-align: center;
                  }")),
  tags$style(HTML("#small-heading{color: white;
                  font-size:30px;
                  text-align: center;
                  }")),






  #Choose Data ----
  fluidRow( class= "pad",

    column(12,
           uiOutput("Problem"),

    ), #End first row

  #Tabs for different tasks

  tabsetPanel(

    #Tab 1: Data Table  ----
    tabPanel( "Data",
                       column(12, class="round2",
                        uiOutput("UI1.1"),
                        dataTableOutput("Dataframe1.1"))
    ),   #End Tab 1

    #Tab 1.5: Profile Analysis ----
    tabPanel("Profile",
             fluidRow(
               column(2, class="round",
                      radioButtons("demVars",
                                   "Select Demographic Variable",
                                   choices = c("sex", "race", "ethnicity"))

               ),

               column(10,
                      uiOutput("UI1.5.1"),
                      plotOutput("Plot1.5.1")
                      )
             ) # End Fluid Row
             ), #End tab panel 1.5



    #Tab 2: CCA ----
    tabPanel( "CCA",
              fluidRow(
                # column(2,class="round",
                #        radioButtons("vars2",
                #                     "Select variable ",
                #                     choices = c("x1", "x2", "x3", "x4", "dsq"))
                # ),
                column(11,
                       uiOutput("UI2.1"),
                       plotOutput("Plot2.1"),
                       uiOutput("UI2.2")
                )
              )#End fluid row
    ), #End tab 2

    #Tab 3: Plots ----
    tabPanel( "Plots",
              fluidRow(
                column(2,class="round",
                       radioButtons("plotType",
                                    "Select Type of Plot to View",
                                    choiceNames = c("Observations", "Variable Biplot", "Triplot"),
                                    choiceValues=c("obs", "bi", "tri")),
                       radioButtons("component",
                                    "Select Component Space ",
                                    choices = c(1, 2, 3, 4))

                ),
                column(10,
                       uiOutput("UI3.1"),
                       plotOutput("Plot3.1"),
                       plotOutput("Plot3.2")
                )
              )#End fluid row
    ) #End tab 3

  ), #End tabset panels

  #Conclusion

      column(12,
            uiOutput("Conclusion"),

            ) #End conclusion

  ) # End Aesthetics
),
padding = 100) #End fluid page

#Server ----

server<-function(input, output, session){

  covid <- covid
  #dummies <- covid[,9:19]
  demographics <- covid[,9:15]
  severity <- covid[,16:19]

  ccaList <- cc(demographics, severity)


  #Data Table ----
  output$Dataframe1.1<-renderDataTable({
    dat <- data.frame(covid)
    dat <- dat[,c("sex","race","ethnicity","symptom_status", "hosp_yn" ,"icu_yn", "death_yn")]
    dat
  },
  options = list(
    pageLength = 5)
  )

 # Problem Output ----
  output$Problem <- renderUI({
    Goal <- "Goal"

    exp <- paste(
      "The corona virus affects people differently. Some people are asymptomatic through the whole infection while others
      develop symtoms anywhere between 2-14 days. For some, the symptoms are comparable to the common flu. For others,
      the symptoms are much more severe. We will seek to identify the correlation between demographic factors and
      markers of symptom severity in different patients. The demographic factors are sex, race, and ethnicity. The markers
      of symptom severity are whether symptoms are present, whether someone was hospitalized prior to ICU, whether somone was entered
      into the ICU, and whether the outcome of the case was death. Although we might expect everyone who has the outcome of
      death to have been in the ICU and hospitalized and symptomatic, there are some rows within the data frame where this
      is not the case. Since the escalation of the disease is not always linear, some cases escalate straight to the ICU and
      others from hospitalization straight to death. For this reason, we consider each severity marker as a seperate variable.

      "
    )

    exp3 <- paste("The CCA will give us 4 different pairs of linear combinations. Each pair will have a canonical correlation
                  value that will tell us the correlation between the pair. The first pair will maximize correlation, so if
                  there is high correlation between the two data sets we will see it displayed there most prominently. If there
                  is correlation, then we expect to see a high canonical correlation value between the first pair. Convesely, if
                  there is no correlation, we expect to see a low value. ")

    Process <- "Process"

    exp2 <- paste(
      "For this analysis we split a data set that contains both demographics
      and symptom severity variables into two sub data sets. We then run a canonical correlation analysis (CCA) on the two. We can
      observe each factor as well as each observation in CCA component space. Since the data is categorical in both data
      sets, we will need to dummy code each variable to run the CCA. Two of the variables in the demographic data set are
      binary, so the base levels will be Female for sex and Hispanic/Latino for ethnicity. The race variable has 6 possible
      options, so the base level will be American Indian/Alaska Native and there will be five variables added to the linear
      combination, one each for Asian, Black, Multiple/Other, White, Native Hawaiian//Other Pacific Islander. All of the
      variables in the symptom severity are binary, so the base levels will be Asymptomatic for symptom status, No for hospitalization,
      No for ICU, and No for death."

    )

    HTML("<span style='font-size:200%'>", Goal, "</span>", "<br>",
         "<span style='font-size:100%'>", exp,  "</span>", "<br>", "</span>","<br>",
         "<span style='font-size:100%'>", exp3,  "</span>", "<br>",
         "<span style='font-size:200%'>", Process, "</span>", "<br>",
         "<span style='font-size:100%'>", exp2,"</span>", "<br>",
         "<span style='font-size:200%'>", "Results", "</span>")
  })

  #Tab 1 Output ----
  output$UI1.1 <- renderUI({
    title <- "Raw Data"

    exp <- paste(
      "exp"
    )

    HTML("<span style='font-size:200%'>", title, "</span>", "<br>",
         "<span style='font-size:80%'>", exp)
  })

  # Tab 1.5 Output ----
  output$UI1.5.1 <- renderUI({

    exp <- paste(" We can take a look at how each individual demographic variable affects (or doesn't affect)  3 of the
    severity measures. The graph below shows the demographic variable on the x axis. The y axis is the proportion
    of each level of the demographic variable that demonstrates each of the 3 severity measures being charted. This visualization
    will be helpful in letting us anticipate general trends, but it won't capture the whole picture. For that, we'll need to take
    a multivariate approach.
                 ")

       HTML( "<span style='font-size:150%'>", "Visualize Profiles", "</span>","<br>" ,
             "<span style='font-size:100%'>", exp)
  })

  x <- reactive({
    input$demVars
  })

  output$Plot1.5.1 <-renderPlot({

    x <- x()
     rotateS21::profiles(covid, x, "hosp_yn_Yes", "icu_yn_Yes", "death_yn_Yes")+
      ggplot2::scale_color_manual(
        name="Severity Measure",
        values=c("1"="blue", "2"="green", "3"="black"),
        labels=c("Hospitalization", "ICU", "Death")
      )

  })


  # Tab 2 Output ----

  output$UI2.1 <- renderUI({

    varsX <- paste("We can set the different variables for our linear combination. Let X1 through X7 represent the variables
    from the data set that describes demographics.Then we have the following:")

    x1 <- paste("X1-Sex: 1 male, 0 female")
    x2 <- paste("X2-Race Asian")
    x3 <- paste("X3-Race Black")
    x4 <- paste ("X4-Race Multiple/Other")
    x5 <- paste("X5-Race Native Hawaiian/Other Pacific Islander")
    x6 <- paste("X6-Race White")
    x7 <- paste("X7-Ethnicity: 1 Non-hispanic/latino, 0 Hispanic/latino")

    varsY <- paste("Let Y1 through Y4 represent the variables from the data set that describes symptom severity. We have the
                   following:")
    y1 <- paste("Y1-Symptom Status: 1 Symptomatic, 0 Asymtomatic")
    y2 <- paste("Y2-Hospitalization: 1 Yes, 0 No")
    y3 <- paste("Y3-ICU: 1 Yes, 0 No")
    y4 <- paste("Y4-Death: 1 Yes, 0 No")

    exp1 <- paste("After performing the canonical correlation analysis, we find the following pairs of linear combinations, with
                  canonical correlations of 0.1942, 0.06587, 0.05337, and 0.02613 respectively.")

    lc1x <- paste("Demographics 1: - 0.501 * X1 - 0.810 * X2 - 0.929 * X3 - 0.329 * X4 + 2.329 * X5 + 1.771 * X6 - 0.721 * X7")
    lc1y <- paste ("Severity 1: 1.506 * Y1 - 2.603 * Y2 - 0.909 * Y3 + 0.457 * Y4")

    lc2x <- paste("Demographics 2: 0.955 * X1 + 7.013 * X2 + 2.040 * X3 + 0.816 * X4 + 2.425 * X5 + 3.439 * X6 + 0.524 * X7")
    lc2y <- paste ("Severity 2: 1.282 * Y1 - 1.480 * Y2 + 2.501 * Y3 + 4.657 * Y4")

    lc3x <- paste("Demographics 3:0.135 * X1 + 2.294 * X2 + 5.634 * X3 + 2.745 * X4 + 1.265 * X5 + 5.327 * X6 + 1.969 * X7")
    lc3y <- paste ("Severity 3: - 5.228 * Y1 - 0.234 * Y2 - 2.610 * Y3 + 3.027 * Y4")

    lc4x <- paste("Demographics 4: 1.376 * X1 - 3.953 * X2 - 0.748 * X3 - 3.558 * X4 + 0.538 * X5 - 1.728 * X6 - 1.367 * X7")
    lc4y <- paste ("Severity 4: - 3.248 * Y1 - 1.387 * Y2 + 5.009 * Y3 - 3.039 * Y4")


      HTML(  "<span style='font-size:150%'>", "CCA Results", "</span>","<br>", "<span style='font-size:80%'>",
             varsX,  "</span>","<br>", x1, "</span>","<br>", x2, "</span>","<br>", x3, "</span>","<br>", x4, "</span>","<br>",
           x5, "</span>","<br>", x6, "</span>","<br>", x7 , "</span>","<br>", "</span>","<br>",varsY, "</span>","<br>", y1, "</span>","<br>",
           y2, "</span>","<br>", y3, "</span>","<br>", y4, "</span>","<br>", "</span>","<br>", exp1, "</span>","<br>", lc1x,
           "</span>","<br>", lc1y, "</span>","<br>", "</span>","<br>", lc2x, "</span>","<br>", lc2y, "</span>","<br>", "</span>","<br>",
           lc3x, "</span>","<br>", lc3y, "</span>","<br>", "</span>","<br>", lc4x, "</span>","<br>", lc4y
           )
  })


  # output$UI2.2<-renderUI({
  #
  #   title <- "Title"
  #
  #   exp <- paste(
  # "exp")
  #
  #   HTML( "<span style='font-size:150%'>", title, "</span>","<br>" ,
  #         "<span style='font-size:80%'>", exp)
  #
  # })


  # output$Plot2.1 <- renderPlot({
  #
  #     blank<-data.frame("1"=c(1:10), "2"=c(1:10))
  #     ggplot(data=blank, aes(x=1, y=2))+
  #     theme_void()+
  #     ggtitle("title")
  #
  # })





  # Tab 3 Output ----

  output$UI3.1 <- renderUI({

    title <- "Observations and Variables in Canonical Correlation Component Space"

    exp <- paste(
     "Here you can use obs to see the different observations in the cannonical correlation component space. You can use
     bi to see a biplot for the variables from each of the 2 different datasets in the correlation component space. You can
     use tri to see a triplot that overlays the previous 2 graphs. You can also select which component space you want to be in,
     ie. which pair of linear combinations to view as the axes."
    )

    HTML( "<span style='font-size:150%'>", title, "</span>","<br>" ,
          "<span style='font-size:80%'>", exp)

  })


pType <- reactive({
  input$plotType
})

compNum <- reactive({
  input$component
})

  output$Plot3.1 <- renderPlot({

    pType <- pType()
    compNum <- as.integer(compNum())

    varlabels <- c("Male", "Asian", "Black", "MultiRace", "NativePI", "White", "Ethnicity",
                   "Symptomatic", "Hospital", "ICU", "Death")

    canCorrPlot(demographics, severity, pair=compNum, plotType=pType, labels=varlabels)+
      ggplot2::scale_color_manual(
        name="Variable Type",
        values=c("blue","green"),
        labels=c("Demographics", "Severity"))+
      ggplot2::xlab("Demographic Component")+
      ggplot2::ylab("Severity Component")


  })

  # output$Plot3.2 <- renderPlot({
  #
  #   blank<-data.frame("1"=c(1:10), "2"=c(1:10))
  #   ggplot(data=blank, aes(x=1, y=2))+
  #   theme_void()+
  #   ggtitle("P3")
  #
  # })

  # Conclusion Output ----

  output$Conclusion<-renderUI({

    title <- "Conclusion"

    exp <- paste(
      "We can see by the canonical correlation values that the correlation between demographics and severity of case are not
      highly correlated. In the univariate profile visualizations we can see that each of the individual demographic variables may
      have some effect on the severity of the case, but the overall canonical correlation analysis shows that these effects
      are not strong enough to make the overall data sets highly correlated.")
    exp2 <- paste(
      "Going forward, it would be a good idea to expand the data set. Adding more demographic variables would be a good idea. Age
      was not added because it is already well established that age plays a large role in the outcome of the disease. One variable
      that is not as well explored is income. Income might be a good demographic variable to look at because people without health
      insurance may be less likely to go to the hospital. Location might be another good one because some areas, like New York in
      the beginning of the pandemic, had very high mortality rates due to the hospitals being overwhelmed. As for the severity
      variables, days in the hospital and days in the icu might be good variables to add into the analysis. There are many
      different variables that could be analyzed that might yeild different results, but we can say with a high
      degree of confidence that for the data set that we had available, the demographic variables and the severity of case
      variables were not highly correlated."
    )

    HTML( "<span style='font-size:200%'>", title, "</span>","<br>" ,
          "<span style='font-size:100%'>", exp, "</span>","<br>", "</span>","<br>", exp2)

  })

} # End server

#Run the app ----

shinyApp(ui=ui, server=server)
