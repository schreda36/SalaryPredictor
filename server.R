library(shiny)
library(ggplot2)
library(scales)
library(caret)
library(rpart)

predSal <<- 0 #predicted salary

sal <- read.csv("salary_samples.csv")
sal.unique.title <- sort(unique(trimws(sal$Job.Title)))

shinyServer(  
      function(input, output, session) {
            
            dataset <- reactive({
                  sal <- read.csv("salary_samples.csv")
                  sal
            })
            
            output$salary <- renderText({input$salary})
            updateSelectizeInput(session, 'title', choices = as.character(sal.unique.title), server = TRUE)
            
            predSal <- reactive({
                        salTitlePerf3 <- dataset()[grep(input$title, sal$Job.Title), ]
                        
                        modFit <- train(Salary.Rate ~ Overall.Performance.Rating + Job.Title,method="rpart",data=salTitlePerf3)  
                        inRatingTitle <- data.frame(Job.Title=input$title, Overall.Performance.Rating=as.numeric(input$performance))
                        predict(modFit,newdata=inRatingTitle)
            })
            
            #Salary Histogram
            output$salaryHist <- renderText({
                  percentile <- ecdf(dataset()$Salary.Rate)
                  if (input$goButton > 0)
                        paste("Based on the title and rating you entered, your predicted salary is ", as.integer(predSal()), "(usd$) which would place you at the ", percentile(predSal())*100, " percentile of all employees in our data.")
            })
            output$newHist <- renderPlot({
                  if (input$goButton > 0) {
                        hist(dataset()$Salary.Rate, xlab='Annual Salary (USD$)', col='lightblue',main='Histogram')
                        lines(c(predSal(), predSal()), c(0, 200),col="red",lwd=5)
                  }
            }, height=300)


             #Salary vs Performance
             output$perfSalText <- renderText({
                   salPerf <- subset(dataset(), Overall.Performance.Rating==input$performance, select=Salary.Rate)
                   percentile <- ecdf(salPerf$Salary.Rate)
            
                   if (input$goButton > 0)
                         paste("A salary of ", as.integer(predSal()), "(usd$) places you at the ", percentile(as.integer(predSal()))*100, " percentile of employees with a performance rating of ", input$performance, " regardless of title.")
             })
             output$perfSalplot <- renderPlot({
                   if (input$goButton > 0) {
                         inPerf <- input$performance
                         inSal <- as.integer(predSal())
            
                         inDF <- data.frame(inPerf,inSal)
            
                         p <- ggplot(dataset(), aes(dataset()$Overall.Performance.Rating, dataset()$Salary.Rate))
                         p <- p + geom_point()
                         p <- p + geom_point(data = inDF, aes(x=as.integer(input$performance),y=as.double(inSal)), color='red', size=4)
                         p <- p + labs(x="Performance Rating",y="Annual Salary (USD$)") + scale_y_continuous(labels=scales::dollar)
                         p
                   }
             }, height=300)
            
             #Salary vs Title, factored by Performance
             output$perfTitleText <- renderText({
                   if (input$goButton > 0) {
                         salTitlePerf2 <- dataset()[grep(input$title, sal$Job.Title), ]
                         percentile <- ecdf(salTitlePerf2$Salary.Rate)
                   
                         paste("A salary of ", as.integer(predSal()), "(usd$) places you at the ", percentile(as.integer(predSal()))*100, " percentile of employees with a title similar to ", input$title, " regardless of performance rating. Here's a look at where this salary would fit in with data from similar titles:")
                   }
             })
             output$perfTitleplot <- renderPlot({
                   if (input$goButton > 0) {
                         inPerf <- input$performance
                         inSal <- predSal()
                         inTitle <- input$title
                         inDF <- data.frame(inPerf,inSal,inTitle)
                         
                         salTitlePerf2 <- dataset()[grep(input$title, sal$Job.Title), ]
                         
                         p <- ggplot(salTitlePerf2, aes(salTitlePerf2$Job.Title, salTitlePerf2$Salary.Rate, color=salTitlePerf2$Overall.Performance.Rating)) 
                         p <- p + geom_point() 
                         p <- p + geom_point(data = inDF, aes(x=input$title,y=as.double(predSal())), color='red', size=4)
                         p <- p + labs(x="Job Title",y="Annual Salary (USD$)") + scale_y_continuous(labels=scales::dollar)
                         p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
                         p <- p + guides(color=guide_legend(title="Perf Rating"))
                         p
                   }
             }, height=300)
             
      }
)