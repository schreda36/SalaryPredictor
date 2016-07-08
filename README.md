# SalaryPredictor

## Purpose
SalaryPredictor is a typical Shiny app consisting of server.R and ui.R files. It's purpose is aimed at predicting the salary for a given Job Title and Performance Rating provided by the user. The predicted salary is then displayed in several plots and a percentile is calculated to show where that predicted salary falls among: all employees in our dataset; employees with similar performance ratings; and employees with similar job titles.

## Data 
A data file is required as it forms the foundation for the prediction and percentile formulas displayed by the app. The sample data also provides the UI with the list of unique Job Titles which the user may select from. Any csv file in the format below may be substituted for our sample data:

- Job.Title - character(60)
- Overall.Performance.Rating - character(20)
- Salary.Rate - double


## server.R 
Several libraries are required, such as rpart for the decision tree-based prediction. The scales library is also used for modifying the axis in a couple plots to display in usd$ format.

## ui.R
The UI file uses standard selectInput, textOutput and plotOutput elements. One slightly unique feature is that a selectizeInput element is used for Job Title in order to retrieve the unique set of Job Titles that the user may select from the dataset. The selectizeInput is updated by the server.R file, see updateSelectizeInput() on line 21.
