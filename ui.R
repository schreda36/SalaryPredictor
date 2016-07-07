shinyUI(pageWithSidebar(
      headerPanel("Predict Your Salary"),
      sidebarPanel(
            p('Enter the following:'),
            #numericInput(inputId="salary", value=0, step=1000, label = "Salary"),
            #sliderInput("salary", "Salary (usd)", 30000, 300000, value = c(100000), format="###,###"),
            selectInput("performance", "Performance Rating:", c("5 - Excellent" = "5","4 - Very Good" = "4","3 - Good" = "3","2 - Below Avg" ="2","1 - Poor"="1")),
            selectizeInput("title", "Job Title", choices = NULL),
            actionButton("goButton", "Go!")
      ),
      mainPanel(
            #p('Output title'),
            #textOutput('title'),
            textOutput('predSal'),
            textOutput('output$salary'),
            textOutput('salaryHist'),
            plotOutput('newHist'), 
            textOutput('perfSalText'),
            plotOutput('perfSalplot'),
            textOutput('perfTitleText'),
            plotOutput('perfTitleplot')
      )
))

