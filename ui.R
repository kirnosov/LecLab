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
                        tabsetPanel(
                                tabPanel("App",
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
        ),
        tabPanel("Instructions",
                 h3("Purpose"),
                 h6("This app is created for education managers who have to
                    make grading decisions for big courses, splitted into
                    several components (e.g., lecture and lab). The  
                    evaluation of the student's performance is based on 
                    objective (electronic quizzes,exams) and subjective 
                    (human-graded project reports) criteria."),
                 h6("Quite often such courses are taught by several
                    professors and multiple teaching assistants. Some of
                    them are 'easy' graders and some grade 'hard'. In order
                    to decide whether low grades are the result of a low
                    level student permormance or too high expectations of
                    a grader, it is important to compare grades obtained in
                    objective and subjective activities."),
                 h6("Since normally there is a single grader per section,
                    it is possible to average the students performance 
                    over the lab unit."),
                 
                 h3("How to Use"),
                 h6("Select the input data set. You can just use 'Sample Input'
                    or you can upload a .zip file. This .zip file should contain
                    two folders: 'lab' and 'lecture'. Both folders should carry
                    .csv files obtained from the course website (e.g., D2L)
                    with one column having a header containing 'section' word.
                    Click 'Submit'."),
                 h6("After the data is successfully processed, you will see
                    the list of features available to display. Check all the 
                    boxes you are interested in. 'All/None' box will 
                    select/deselect all."),
                 h6("In 'Plots' tab you can compare three sections' results
                    side-by-side. You need to select sections from the 
                    dropdown menu."),
                 h6("Sliders can be used to adjust plots appearance.
                    Font size and Angle change the geometry of the labels,
                    Max/Min changes the the axis maximum and minimum in
                    units of standard deviation from the mean."),
                 h6("'Table' tab displays the numerical values of 
                    every section features' deviation from average.
                    Same table can be downloaded from the 'Plots' tab
                    as pdf, html, or Word document.")
        )
)))))