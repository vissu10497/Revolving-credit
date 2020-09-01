
library(ggplot2)
library(ggthemes)
library(party)
library(caret)
library(plotly)
library(curl)
library(RCurl)
library(e1071)
library(rpart)
library(ranger)
library(lazyeval)
library(readr)
library(plyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(funModeling)
library(scales)
library(tidyverse)
library(corrplot)
library(GGally)
library(caret)
library(rpart)
library(randomForest)
library(pROC)
library(gbm)
library(choroplethr)
library(choroplethrMaps)
library(microbenchmark)
library(doParallel)
library(e1071)
library(ggthemes)
library(RColorBrewer)
library(funModeling)
library(dplyr)
library(DataExplorer)
library(tidyverse)
library(Hmisc)
library(ggplot2)
library(mvinfluence)
library(car)
library(rgl)
library(xgboost)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(party)
library(caret)
library(plotly)
library(curl)
library(RCurl)
library(e1071)
library(rpart)
library(ranger)
library(glmnet)
library(mlbench)
#library(psych)
library(dplyr)
library(lattice)
library(xgboost)
credit_data <- read.csv("C:/Users/srivi/OneDrive/Desktop/Excelr/Project/data.csv")

median_impute_model = preProcess(credit_data[names(credit_data)],method="medianImpute")
credit_data = predict(median_impute_model,credit_data)
sort(sapply(credit_data, function(x) sum(length(which(is.na(x)))))/nrow(credit_data),decreasing = TRUE)

attach(credit_data)


ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Dashboard"
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Input", tabName = "Input", icon = icon("dashboard")),
                        menuItem("Summary", tabName = "Summary", icon = icon("th")),
                        menuItem("Results", tabName = "Results", icon = icon("th"))
                      )
                    ),
                    dashboardBody( 
                      # Boxes need to be put in a row (or column)
                      tabItems(
                        # First tab content
                        
                        tabItem(tabName = "Input",
                                h2("Input Details"),
                                fluidRow(
                                  box(title = "Model Parameters",
                                      width = 3, background = "purple",
                                      radioButtons(inputId = "model",label=h5(" Revolving Balance Prediction Model"),
                                                   choices = c("Linear"),selected = "Linear"),
                                      numericInput("cv", label = h5("Cross Validations"), value = 3),
                                      numericInput("tl", label = h5("tuneLength"), value = 3)
                                  ),
                                  box(title = "Enter Loan Amount",
                                      height = 130,width = 3, background = "lime",
                                      numericInput("loan_amnt", label = h6(""), value = 14350)
                                  ),
                                  box(title = "Select Intrest Rate Category",
                                      height = 130,width = 3, background = "olive",
                                      numericInput("Rate_of_intrst",label=h6(""),
                                                   value = 8)
                                  ),
                                  box(title = "Select  terms in years",
                                      height = 130,width = 3, background = "blue",
                                      selectInput(inputId = "terms",label=h6(""),
                                                  choices = c("0-3","4-5"),selected = "0-3")
                                  ),
                                  box(title = "Enter Annual Income",
                                      height = 130,width = 3, background = "blue",
                                      numericInput("annual_inc", label = h6(""), value = 105000)
                                  ),
                                  box(title = " Enter total credits",
                                      height = 130,width = 3 , background = "lime",
                                      numericInput("total_credits", label = h6(""), value = 3)),
                                  
                                  box(title = "total current balance",
                                      height = 130,width = 3, background = "olive",
                                      numericInput("tot_curr_bal", label = h6(""), value = 40000)),
                                  
                                  box(title = "Submission",
                                      height = 90,width = 3, background = "red",
                                      actionButton("submit","Submit")
                                  )
                                )
                        ),
                        
                        tabItem(tabName = "Model",
                                h2("Model"),
                                fluidRow(
                                  box(title = "Bar Plots",  width = 9,  background = "red",
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      plotOutput("view4")
                                  )
                                )
                        ),
                        
                        # Second tab content
                        tabItem(tabName = "Summary",
                                h2("Summary"),
                                fluidRow(
                                  box(title = "Dataset Summary", background = "purple",
                                      width = 10,
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      verbatimTextOutput("summary1")
                                  ),
                                  box(title = "Model Summary", background = "purple",width = 7, 
                                      status = "primary",solidHeader = TRUE, collapsible = TRUE,
                                      verbatimTextOutput("summary")
                                  )
                                )
                        ),
                        tabItem(tabName = "Results",
                                h2("Prediction"),
                                fluidRow(
                                  box(title = "Entered Observation", background = "red",width = 20,
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      tableOutput("table")
                                  ),
                                  box(title = "Revolving Balance Prediction", background = "red", 
                                      status = "primary",  solidHeader = TRUE, collapsible = TRUE,
                                      verbatimTextOutput("predt")
                                  )
                                )
                        )
                      )
                    )
)


server <- function(input, output) {
  datainput = reactive({
    
    credit_data <- read.csv("C:/Users/srivi/OneDrive/Desktop/Excelr/Project/data.csv")
    
    median_impute_model = preProcess(credit_data[names(credit_data)],method="medianImpute")
    credit_data = predict(median_impute_model,credit_data)
    sort(sapply(credit_data, function(x) sum(length(which(is.na(x)))))/nrow(credit_data),decreasing = TRUE)
    
    
    credit_data$emp_cat <- rep(NA, length(credit_data$terms))
    credit_data$emp_cat[which(credit_data$terms == "36 months")] <- "0-3"
    credit_data$emp_cat[which(credit_data$terms == "60 months")] <- "4-5"
    credit_data$emp_cat[which(is.na(credit_data$terms))] <- "Missing"
    credit_data$emp_cat <- as.factor(credit_data$terms)
    credit_data$terms=NULL
    credit_data$ir_cat <- rep(NA, length(credit_data$Rate_of_intrst))
    credit_data$ir_cat[which(credit_data$Rate_of_intrst <= 8)] <- "0-8"
    credit_data$ir_cat[which(credit_data$Rate_of_intrst > 8 & credit_data$Rate_of_intrst <= 11)] <- "8-11"
    credit_data$ir_cat[which(credit_data$Rate_of_intrst > 11 & credit_data$Rate_of_intrst <= 13.5)] <- "11-13.5"
    credit_data$ir_cat[which(credit_data$Rate_of_intrst > 13.5)] <- "13.5+"
    credit_data$ir_cat[which(is.na(credit_data$Rate_of_intrst))] <- "Missing"
    credit_data$ir_cat <- as.factor(credit_data$ir_cat) 
    credit_data$Rate_of_intrst=NULL
    
    credit_data$total.revol_bal = as.factor(credit_data$total.revol_balance)
    
  })
  
  test = reactive({
    if (input$submit > 0) {
      df <- data.frame(loan_amnt = (input$loan_amnt),Rate_of_intrst = (input$Rate_of_intrst),terms = (input$terms),
                       annual_inc = (input$annual_inc),total_credits = (input$total_credits), tot_curr_bal = (input$tot_curr_bal))
      
      
      return(list(df=df))
    }
  })
  
  
  fn2 = reactive({
    credit_data <- read.csv("C:/Users/srivi/OneDrive/Desktop/Excelr/Project/data.csv")
    new_data <- credit_data %>% select(loan_amnt,Rate_of_intrst,annual_inc,total.revol_bal,terms,total_credits,tot_curr_bal)
    new_data <- new_data %>% mutate(Rate_of_intrst = as.numeric(sub("%","", Rate_of_intrst)),
                                    total_credits = as.numeric(total_credits),
                                    total.revol_bal = as.numeric(total.revol_bal),
                                    loan_amnt = as.numeric(loan_amnt),tot_curr_bal = as.numeric(tot_curr_bal))
    
    # imputing na values
    
    median_impute_model = preProcess(new_data[names(new_data)],method="medianImpute")
    newdata = predict(median_impute_model,new_data)
    sort(sapply(newdata, function(x) sum(length(which(is.na(x)))))/nrow(newdata),decreasing = TRUE)
    
    #newdata <- as.matrix(newdata)
    
    # Data Partition
    set.seed(222)
    ind <- sample(2, nrow(newdata), replace = T, prob = c(0.7, 0.3))
    loan_train <- newdata[ind==1,]
    loan_test <- newdata[ind==2,]
    
    
    
    # Custom Control Parameters
    custom <- trainControl(method = "cv",
                           number = 5,
                           repeats = 3,
                           verboseIter = T)
    
    
    set.seed(1234)
    model <- train(total.revol_bal~.,
                   loan_train,
                   #method='glmnet',
                   method='gblinear',
                   nrounds=80,
                   max_depth=6,
                   eval_metric= 'rmse',
                   eta=0.1,
                   early_stopping_rounds = 30
                   #tuneGrid= expand.grid(alpha=1, lambda= seq(0.001,1, length=5)),
                   #trControl=custom
                   )
    
    # Plot Results
    plot(model)
    plot(model$finalModel, xvar = 'lambda', label=T)
    
    model
    
    
  })
  
  new_theme = theme(panel.background = element_blank(),
                    axis.line.x   = element_line(color='black'),
                    axis.line.y   = element_line(color='black'),
                    axis.ticks    = element_line(color='black'),
                    axis.title.x  = element_text(family="Times",face = "bold", size = 12),
                    axis.title.y  = element_text(family="Times",face = "bold", size = 12),
                    axis.text     = element_text(family="Trebuchet MS",face = "italic", size = 10),
                    legend.title  = element_text(family="Times",face = "bold", size = 8),
                    legend.text   = element_text(family="Trebuchet MS",face = "italic", size = 8))
  
  
  output$view4 = renderPlot({
    if (input$submit > 0) {
      model = fn2()
      plot(model)
    }
    else{
      NULL
    }
  })
  
  
  output$summary1 = renderPrint({
    if (input$submit > 0) {
      summary(credit_data)
    }
    else{
      NULL
    }
  })
  
  output$table = renderTable({
    if (input$submit > 0) {
      test()$df
    }
    else{
      NULL
    }
  })
  
  output$predt <- renderPrint({
    if (input$submit > 0) {
      test = test()$df
      #credit_data = datainput()
      credit_data_1 = credit_data[ ,c(2,3,5,11,23,36)]
     # A <- as.numeric(credit_data_1$terms)
      ##AA <- for (i in A) { ifelse(A<2,"0-3","4-5")      }
      
      ww =(rbind(credit_data_1,test))
      test = ww[29092, ]
      model = fn2()
      pred = predict(model,newdata = test, class='response')
      
      if(pred>0) {
        print(paste('Revolving Balance', pred))
      }
      else{
        print("null")
      }
    }
    else{
      NULL
    }
  })
  
  output$summary <- renderPrint({
    if (input$submit > 0) {
      (fn2())
    }
    
    else{
      NULL
    }
    
  })
}


shinyApp(ui, server)
