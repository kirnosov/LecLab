source("helpers.R")
library(shiny)
library(reshape2)
library(knitr)


shinyServer(function(input, output,session) {

        observeEvent(input$SubmitButton, {        
        if(input$ds == 'S'){
                LabData <- Read_all_files("Sample/lab")
                LectureData <- Read_all_files("Sample/lecture")
        }
        
        if(input$ds == 'ZIP'){
                unzip(input$file1$datapath,exdir = "ZIP")
                LabData <- Read_all_files("ZIP/lab")
                LectureData <- Read_all_files("ZIP/lecture")
                }
                
        tidy_lab_mean <- Tidy_Data(LabData,mean)
        tidy_lec_mean <- Tidy_Data(LectureData,mean)
        total_mean <- merge(tidy_lab_mean,tidy_lec_mean,by="SectionID",all=TRUE)
        names(total_mean) <- strtrim(names(total_mean), 25)
        total_mean <- total_mean[complete.cases(total_mean),]
        
        melted <- melt(total_mean,id="SectionID")
        sigma <- sqrt(var(melted$value,na.rm = TRUE))
        
        
        output$ui_cb <- renderUI({
                checkboxGroupInput('col_choices', 'Select features to display:',
                                   choices=names(total_mean)[-1])
        })
        
        
        observe({
                updateCheckboxGroupInput(
                        session, 'col_choices', choices = names(total_mean)[-1],
                        selected = if (input$bar) names(total_mean)[-1]
                )
        })
        
        
        output$text1 <- renderText({
         t <- paste(
                sort(as.character(unique(
                        melted$SectionID[sapply(melted$value,
                                                function(x) abs(x) > 2*sigma)]))),
                ", ")
         t
        })
        
        output$view <- renderDataTable({
                library(ggplot2)
                total_mean[,c("SectionID",input$col_choices), drop = FALSE]
        })
        
        output$ui_si_1 <- renderUI({
                selectInput("section1", "Select sections:", 
                            choices= c("None",sort(as.character
                                                   (unique(total_mean$SectionID)))))
        })
        output$sectionPlot1 <- renderPlot({
                validate(
                        need(input$col_choices != "", "Please select features")
                )
                validate(
                        need(input$section1 != "None", "Please select section")
                )
                Make_BarPlot(melted,input$section1,input$col_choices,
                             size=input$size/100,input$angle,input$range)
        })
        
        output$ui_si_2 <- renderUI({
                selectInput("section2", "", 
                            choices= c("None",sort(as.character
                                                   (unique(total_mean$SectionID)))))
        })
        output$sectionPlot2 <- renderPlot({
                validate(
                        need(input$col_choices != "", "Please select features")
                )
                validate(
                        need(input$section2 != "None", "Please select section")
                )
                Make_BarPlot(melted,input$section2,input$col_choices,
                             size=input$size/100,input$angle,input$range)
        })
        
        output$ui_si_3 <- renderUI({
                selectInput("section3", "", 
                            choices= c("None",sort(as.character
                                                   (unique(total_mean$SectionID)))))
        })
        output$sectionPlot3 <- renderPlot({
                validate(
                        need(input$col_choices != "", "Please select features")
                )
                validate(
                        need(input$section3 != "None", "Please select section")
                )
                Make_BarPlot(melted,input$section3,input$col_choices,
                             size=input$size/100,input$angle,input$range)
                
        })
        
        })
        
        output$downloadReport <- downloadHandler(
                filename = function() {
                        paste('my-report', sep = '.', switch(
                                input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
                        ))
                },
                
                content = function(file) {
                        src <- normalizePath('report.Rmd')
                        
                        # temporarily switch to the temp dir, in case you do not have write
                        # permission to the current working directory
                        owd <- setwd(tempdir())
                        on.exit(setwd(owd))
                        file.copy(src, 'report.Rmd')
                        
                        library(rmarkdown)
                        out <- render('report.Rmd', switch(
                                input$format,
                                PDF = pdf_document(), HTML = html_document(), Word = word_document()
                        ))
                        file.rename(out, file)
                }
        )
})