#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(Amelia)
library(ROCR)
library(VIM)
library(mice)
library(ggplot2)
library(dplyr)
library(caret)
library(lubridate)
library(DT)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(readr)

messageData <- read_csv("Message_Data.csv")

#  In this stage we are going to be looking at the test and train data that 
#  are utilized in the Kaggle Dataset competition
train_titanic <- read_csv("train.csv")
test_titanic  <- read_csv("test.csv")

train_join    <- train_titanic %>% 
                    select(-Survived)

combined_titanic <- bind_rows(train_join, test_titanic)

#  Data preparation that will be utilized for modeling the logistic regression
filtered_table <- train_titanic %>% 
                    select(-PassengerId, -Name, -Ticket, -Cabin)

train_output   <- filtered_table


train_output$Age[is.na(train_output$Age)] <- 
                                mean(train_output$Age, na.rm = T)

train_output <-  train_output[!(is.na(train_output$Embarked)),]

set.seed(215)
train_out    <-  floor(0.75*nrow(train_output))
train_ind    <-  sample(seq_len(nrow(train_output)), size = train_out)

training     <-  train_output[train_ind,]
testing      <-  train_output[-train_ind,]

train_model <- glm(Survived ~., family = binomial(link = 'logit'),
                   data = training)

test_filter    <- testing %>% select(-Survived)

prediction     <- predict(train_model, testing, type = "response")
fitted_results <- ifelse(prediction > 0.5, 1, 0)

misClasificError <- mean(fitted_results != testing$Survived)

print(paste('Accuracy', 1-misClasificError))
testing$Survived <- as.factor(testing$Survived)
fitted_results   <- as.factor(fitted_results)

confusionMatrix(fitted_results, testing$Survived)

auc <- predict(train_model, testing, type = "response")

pr  <- prediction(auc, testing$Survived)
prf <- performance(pr, measure = "tpr", x.measure = 'fpr')

#  Model 2: Now that we have our test and train set created, lets 
#           try a second model to see what performance looks like

train_dt    <- rpart(Survived ~. , data = training, method = "class")
dt_prediction <- predict(train_dt, testing, type = "class")

confusionMatrix(dt_prediction, testing$Survived)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Titanic Data Products", 
                  titleWidth = 450,
                  
        dropdownMenuOutput("messageMenu")),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Titanic_Statistics", tabName = "titanic", icon = icon("ship")),
      menuItem("Training_Dataset", tabName = "training", icon = icon("heartbeat")),
      menuItem("Logistic_Regression", tabName = "model1", icon = icon("area-chart")),
      menuItem("Decision_Trees", tabName = "model2", icon = icon("briefcase")),
      menuItem("Summary", tabName = "results", icon = icon("mortar-board"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 450)),
                
                box(plotOutput("age_plot", height = 450)),
                
                box(
                  title = "Fare_Paid",
                  sliderInput("slider", "Number of observations:", 1, nrow(train_titanic), 100)
                ),
                
                box(
                  title = "Age_Distribution",
                  sliderInput("age_slider","Age_Bins", 1, nrow(train_titanic), 50)
                ),
              
                h4("For this project we are going to look at the Titanic 
                   dataset to see the comparsion of two predictive models to see
                   if we can determine the survival rates.",
                   "Survival rates are going to be treated as a:",
                   tags$ul(
                     tags$li("Binary Response"),
                     tags$li("Survived = 1"),
                     tags$li("Died = 0")),
                   
                   tags$div(class = "header", checked = NA,
                      tags$p("Check out the data here:"),
                      tags$a(href = "https://www.kaggle.com/c/titanic",
                             "Titanic Kaggle Dataset")
                   ))
              )
      ),
      # Second tab content
      
      tabItem(tabName = "titanic",
              fluidPage(
                box(
                  title = "Titanic survival rates", 
                  status = "primary", solidHeader = TRUE, 
                  collapsible = TRUE,
                  plotOutput("plot2", height = 450)),
                box(
                  title = "Proportional Statistics on training dataset",
                  status = "warning", solidHeader = TRUE,
                  collapsible = TRUE,
                  tableOutput("table1")
                )
              ),
              box(
                title = "Summary Details",
                status = "warning", solidHeader = TRUE,
                collapsible = TRUE,
                h4("In this box we are just summarizing the details for this tab.  
                   We have a plot to look at survival rates by gender and class, 
                   as well as a proportional table to show the similar relationship")),
              
              fluidPage(
                tags$div(img(src = "titanic_image.jpg", height = 450))
              )),
      
      tabItem(tabName = "training",
              fluidPage(
                titlePanel("Training_Data"),
                
                fluidRow(
                  column(4, 
                         selectInput("pclass",
                                     "Passenger_Class:",
                                     c("All",
                                       unique(as.character(train_titanic$Pclass))))),
                  column(4,
                         selectInput("gender",
                                     "Passenger_Gender:",
                                     c("All",
                                       unique(as.character(train_titanic$Sex))))),
                  column(4,
                         selectInput("embarked",
                                     "Departure_Point:",
                                     c("All",
                                       unique(as.character(train_titanic$Embarked)))))
                ),
                
                fluidRow(
                  DT::dataTableOutput("table")
                )
              ),
              h4("In this Data Table Summary, select a choice for all three of the drill downs.  
                 Once the initial selection is made, filters can be selected for",
                 tags$ul(
                   tags$li("Passenger_Class"),
                   tags$li("Passenger_Gender"),
                   tags$li("Passenger_Departure_Point")
                 ))),
      
      tabItem(tabName = "model1",
              h4("We are going to start with a logistic regression to see if we can see 
                  what our survival rate will look like.  First we are going to start with 
                  observing missing values utilizing the VIM package"),
              box(title = "Details regarding displaying missing values",
                  status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("aggr_plot", height = 450)),
              box(title = "Additional missing details",
                  status = "warning", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("matrix_plot",height = 450 )),
              box(title = "Filtered_Data",
                  status = "warning", solidHeader = TRUE,
                  collapsible = TRUE,
                  tableOutput("Filtered_Table")),
              box(title = "ROC_CURVE",
                  status = "warning", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("ROC_CURVE"))),
      # Third tabs content
      tabItem(tabName = "model2",
              h4("Lets start with looking at the distribution of the results utilzing
                  rpart and rattle to look at our decision trees and nodes look like"),
              box(title = "Decision Tree Visualization",
                  status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("rpart_plot", height = 435)),
              mainPanel(width = 6,
                verbatimTextOutput("confusionmatrix")
              )),
      
      #Fourth tabs result
      tabItem(tabName = "results",
              h4("Comparison and selection of the logistic regression and decision trees;
                 in addition to discussion on future enhancements"),
              box(title = "Logistic Regression",
                  status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  h5("As we can see from above the Logistic Regression had a correct prediction rate:"),
                  tags$ul(
                    tags$li(paste(round(100*(1-misClasificError),2),"%",sep="")),
                    tags$li("With an approximately 79% correct classification rate vs"),
                    tags$li("A 21% incorrectly classified rate"))
                  
                  ),
              box(title = "Decision Trees",
                  status = "warning", solidHeader = TRUE,
                  collapsible = TRUE,
                  h5("The decision tree model accurately predicted the survival rate by:"),
                  tags$ul(
                    tags$li("82.51% correctly classified in the decision tree approach"),
                    tags$li("This was an improvement over the 79% correctly predicted utilizing
                            the logistic regression"),
                    tags$li("In the next box we will discuss other candidate models")
                  )),
              box(title = "Other_Candidate_Models",
                  status = "warning", solidHeader = TRUE,
                  collapsible = TRUE,
                  h5("There are several other candidate models that were considered but
                     were not implemented inside the dashboard:"),
                  tags$ul(
                    tags$li("Random Forest"),
                    tags$li("XGB"),
                    tags$li("Support Vector Machines"),
                    tags$li("Naive Bayes")
                  ),
                  h5("These are models that I'd like to build out for future iterations of the 
                     dashboard to allow for greater depth as far as the predictive power")),
              box(title = "Additional_Items",
                  status = "warning", solidHeader = TRUE,
                  collapsible = TRUE,
                  h5("Additional enhancements that I'd like to make in addition to additional models are:"),
                  tags$ol(
                    tags$li("Additional Reactivity to allow the users to drill into data"),
                    tags$li("Selections for different ensembling to allow choices as how to create models"),
                    tags$li("Tasks and notifications: I put in a fun example for sample messaging but would
                            like to expand the options in the panel"),
                    tags$li("Options to choose different datasources, to allow the user to choose different
                            datasets and explore techniques aside from the Titatnic dataset"),
                    tags$li("Also, had the rattle package installed but that caused a Shiny 
                            deployment error since RGTK isn't supported by Shiny Server"),
                    tags$li("Because of that, the decision tree isn't as well branched out
                            as we would see in a typical rattle visualization")
                  ))
    
    )
  )
    )
  )



server <- function(input, output) {
  set.seed(122)
  histdata <- train_titanic$Fare
  
  age_hist <- train_titanic$Age
  
  output$messageMenu <- renderMenu({
    msgs <- apply(messageData, 1, function(row){
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    
    dropdownMenu(type = "messages", .list = msgs)
  })
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data, col = "green", main = "Passenger Fare", breaks = nrow(train_titanic)/10)
  })
  
  output$age_plot <- renderPlot({
    data <- age_hist[seq_len(input$age_slider)]
    hist(data, col = "blue", main = "Age_Distribution", breaks = nrow(train_titanic)/50)
  })
  
  output$aggr_plot <- renderPlot({
    data <- aggr(train_titanic, prop = FALSE, numbers = TRUE)
    data
  })
  
  output$matrix_plot <- renderPlot({
    matrix_data <- matrixplot(train_titanic, interactive = F)
    matrix_data
  })
  
  output$Filtered_Table <- renderTable({
    filtered_table <- train_titanic %>% 
                        select(-Cabin, -Name ,-PassengerId, -Ticket)
    head(filtered_table)
    
    
  })
  
  output$ROC_CURVE <- renderPlot({
    plot(prf)
    abline(a= 0, b = 1)
    
  })
  output$rpart_plot <- renderPlot({
    plot(train_dt)
  })
  # Data table output server side.  Here we look to run a filter utilizing the datatable
  # package in order to filter our data
  output$table <- DT::renderDataTable({
    data <- train_titanic
    if(input$pclass != "All"){
      data <- data[data$Pclass == input$pclass,]
    }
    if(input$gender != "All"){
      data <- data[data$Sex == input$gender,]
    }
    if(input$embarked != "All"){
      data <- data[data$Embarked == input$embarked,]
    }
  })
  
  output$plot2 <- renderPlot({
    passengers <- ggplot(train_titanic, aes(Pclass)) + 
              geom_bar(aes(fill = Sex), position = position_stack(reverse = TRUE)) + 
              coord_flip()+
              theme(legend.position = "top" ) +
              theme_minimal() +
              ggtitle("Titanic passengers by class and title")
    passengers
  })
  
  output$table1 <- renderTable({
    table <- prop.table(table(train_titanic$Sex, train_titanic$Survived))
    table
  })
  
  output$confusionmatrix <- renderPrint({
    confusionMatrix(dt_prediction, testing$Survived)
    
  })
}

  

shinyApp(ui, server)