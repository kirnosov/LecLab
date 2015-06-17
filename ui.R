library(shiny)
library(ggplot2)  
shinyUI(fluidPage(
        titlePanel("Lab and Exams Grades Analysis"),
        sidebarLayout(
                sidebarPanel(
                        radioButtons('ds', 'Data source',
                                     c("Sample Input"='S',
                                       "Upload ZIP file"='ZIP'),"S"),
                        conditionalPanel("input.ds == 'ZIP'",
                        fileInput('file1', 'Choose ZIP File')),
                        actionButton("SubmitButton", "Submit"),
                        uiOutput("ui_cb"),
                        checkboxInput('bar', 'All/None'),
                        hr(),
                        sliderInput("size", 
                                    label = "Font size, %",
                                    min = 0, max = 100, value = 50),
                        sliderInput("angle", 
                                    label = "Angle, degrees",
                                    min = -90, max = 90, value = 45),
                        sliderInput("range", 
                                    label = "Max / Min, sigmas",
                                    min = 0, max = 5, value = 2) 
                ),
                
                mainPanel(
                        h6("The following sections have significant deviations:"),
                        textOutput("text1"),
                        tabsetPanel(
                                tabPanel("Plots",
                        fluidRow(
                        column(4,uiOutput("ui_si_1"),plotOutput("sectionPlot1")),
                        column(4,uiOutput("ui_si_2"),plotOutput("sectionPlot2")),
                        column(4,uiOutput("ui_si_3"),plotOutput("sectionPlot3"))),
                        hr(),
                        radioButtons('format', 'Download Report', c('PDF', 'HTML', 'Word'),
                                     inline = TRUE),
                        downloadButton('downloadReport')
                        ),
                        tabPanel("Table",
                        dataTableOutput("view")
                        )
                )
        )
)))