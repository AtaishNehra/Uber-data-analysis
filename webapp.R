library(shiny)
library(shinydashboard)        
# library(nortest)
# library(mvnormtest)
# library(MASS)
library(shinyLP)
library(class)
library(gmodels)
library(caret)
library(rattle)
# library(ranger)
# library(klaR)
library(kernlab)
#library(micad)
library(e1071)
# library(NeuralNetTools)
# library(neuralnet)
# library(nnet)
# library(mclust)


ui <- fluidPage(
  navbarPage(title = "Cabs Data Analytics",
             tabPanel("Home",
                      jumbotron("Welcome", paste("An application made for Uber Data Analytics to ensure your every ride is the best ride.") )
                      
             ),
             tabPanel("Data Sets", 
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file1", "Choose CSV File", accept=c('text/csv', 'text/comma-separated-values', 'text/plain', '.csv')),
                          radioButtons("indata", "Choice:", choices = c("Full", "Columns")),
                          selectInput("cols", "Choose the variable", choices = "", selected = " ", multiple = TRUE), 
                          downloadButton('downloaddatset', "Download"),
                          hr(),
                          radioButtons("trans1", "Transformation:", choices = c("Not-Required", "log", "inverselog", "exponential")),
                          hr()
                          
                        ), 
                        
                        mainPanel(tableOutput("tab1"))
                      )
                      
             ),
             navbarMenu("Statistical Analysis",
                        tabPanel("Summary Statistics",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cols1", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     radioButtons("ssoption", "Select Option", choices = c("Summary", "Length", "Dim", "Type of", "Class"))
                                     
                                   ), 
                                   mainPanel(
                                     fluidRow(
                                       h3("Summary Statistics"),
                                       div(
                                         verbatimTextOutput("summar")
                                       )
                                     )
                                   )
                                 )
                        ), 
                        
                        tabPanel("Plots",
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons("plotoption", "Choose the Option:", choices = c("Histogram", "BarPlot", "Scatter", "Pie" )),
                                     selectInput("cols6", "Choose Varibale 1:", choices = "", selected = " ", multiple = TRUE),
                                     textInput("xaxisname", "Write X Axis Name"),
                                     textInput("yaxisname", "Write Y Axis Name"),
                                     textInput("title", "Write Title For the Graph")
                                   ), 
                                   mainPanel(
                                     h3("Plots"),
                                     fluidRow(
                                       plotOutput("plot")
                                     )
                                   )
                                 )
                                 
                        ),
                        
                        
                        tabPanel("Correlation", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cols9", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     selectInput("cols10", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     radioButtons("cormethod", "Select Method:", choices = c("Covariance", "KarlPearson", "Spearman")),
                                     hr()
                                   ), 
                                   mainPanel(
                                     h3("Covariance & Correlation"),
                                     verbatimTextOutput("cor_t")
                                   )
                                   
                                 )
                                 
                        )
                        
                        
                        
             ),
             navbarMenu("Supervised Learning",
                        
                        tabPanel("Logistic Reg.",
                                 
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("logrvar", "Select Variable", choices = "", selected = ""),
                                     textInput("logrprop", "Select Proportion", value = 0.8, placeholder = "Percentage of rows"),
                                     # textInput("logryname", "Class Variable", value = "num", placeholder = "Class Variable"),
                                     radioButtons("logroption", "Select Method", choices = c("Show Prop.", "Fit", "Coef.","Accuracy")),
                                     hr()
                                   ),
                                   mainPanel(
                                     div(verbatimTextOutput("logroutput"))
                                   )
                                 )
                        ),
                        

                        tabPanel("SVM",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("svmvar", "Select Variable", choices = "", selected = ""),
                                     textInput("svmprop", "Select Proportion", value = 0.8, placeholder = "Percentage of rows"),
                                     #textInput("svmyname", "Class Variable", value = "num", placeholder = "Class Variable"),
                                     radioButtons("svmoption", "Select Method", choices = c("Show Prop.", "Fit", "Predicted","Accuracy","Custom Data")),
                                     hr(),
                                     fileInput("svmtest", "Custom Test Set", accept=c('text/csv', 'text/comma-separated-values', 'text/plain', '.csv')),
                                     radioButtons("indata2", "Choice:", choices = c("View","Clear","Make Predictions"))
                                     
                                     
                                   ),
                                   mainPanel(verbatimTextOutput("svmoutput"),tableOutput("tab2"))
                                 )
                        )
                        
             )
)
)

server <- function(input, output, session) {
  
  # for DATASET TAB
  
  data_input <- reactive({
    infile <- input$file1
    req(infile)
    data.frame(read.csv(infile$datapath)) 
  })
  
  data_input2 <- reactive({
    infile2 <- input$svmtest
    req(infile2)
    data.frame(read.csv(infile2$datapath)) 
  })
  
  observeEvent(input$file1,{
    updateSelectInput(session, inputId = "cols", choices = names(data_input()))
  }
  )
  
  logdata <- reactive({
    df <- data_input()
    ld <- log(df[, input$cols])
    return(ld)
  })
  
  invlogdata <- reactive({
    df <- data_input()
    ild <- 1/log(df[, input$cols])
    return(ild)
  })
  
  expdata <- reactive({
    df <- data_input()
    expd <- log(df[input$cols])
    return(expd)
  })
  
  
  output$tab1 <- renderTable(
    {
      df <- data_input()
      
      if (input$indata == "Full"){
        print(df)
      } else if(input$trans1 == "Not-Required"){
        data <- df[, input$cols]
        print(data)
      } else if(input$trans1 == "log"){
        logdata()
        
      } else if(input$trans1 == "inverselog"){
        invlogdata()
      } else if(input$trans1 == "exponential"){
        expdata()}
      
    }
  )
  
  
  output$downloaddatset <- downloadHandler(
    
    filename <- function(){
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    
    content <- function(file){
      df <- data_input()
      if(input$trans1 == "log"){
        write.csv(logdata(), file, row.names = TRUE)
      } else if(input$trans1 == "inverselog"){
        write.csv(invlogdata(), file, row.names = TRUE)
      } else if(input$trans1 == "exponential"){
        write.csv(expdata(), file, row.names = TRUE)
      } else if(input$trans1 == "lognormal"){
        write.csv(logno(), file, row.names = TRUE)
      } else if(input$trans1 == "standardize"){
        write.csv(standout(), file, row.names = TRUE)
      }
      
    }
    
  )
  
  
  # summary statistics
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols1", choices = names(data_input()))
  }
  )
  
  summ <- reactive({
    var1 <- data_input()[,input$cols1]
    
    if (input$ssoption == "Summary"){
      su <- summary(var1)
      return(su)
    } else if (input$ssoption == "Length"){
      return(length(var1))
    } else if(input$ssoption == "Dim"){
      return(dim(var1))
    } else if (input$ssoption == "Type of"){
      return(typeof(var1))
    } else if(input$ssoption == "Class"){
      return(class(var1))
    }
  })
  
  output$summar <- renderPrint({
    
    if (input$ssoption == "Summary"){
      summ()
    } else if (input$ssoption == "Length"){
      summ()
    } else if(input$ssoption == "Dim"){
      summ()
    } else if (input$ssoption == "Type of"){
      summ()
    } else if(input$ssoption == "Class"){
      summ()
    }
  })
  
  # Plots 
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols6", choices = names(data_input()))
  }
  )
  
  output$plot <- renderPlot({
    df <- data_input()
    if(input$plotoption == "Histogram"){
      hist(df[, input$cols6], freq = FALSE, xlab = input$xaxisname, ylab = input$yaxisname, main = input$title); lines(density(df[, input$cols6]), col = "red", lwd = 1.5)
    } else if(input$plotoption == "BarPlot"){
      barplot(df[, input$cols6], xlab = input$xaxisname, ylab = input$yaxisname, main = input$title)
    } else if(input$plotoption == "Scatter"){
      scatter.smooth(df[, input$cols6], xlab = input$xaxisname, ylab = input$yaxisname, main = input$title)
    } else {
      pie(table(df[, input$cols6]))
    }
  })
  
  # correlation & regression 
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols9", choices = names(data_input()))
    updateSelectInput(session, inputId = "cols10", choices = names(data_input()))
  }
  )
  
  cortest <- reactive({
    var1 <- data_input()[,input$cols9]
    var2 <- data_input()[,input$cols10]
    
    if (input$cormethod == "Covariance"){
      return(cov(var1, var2))
    } else if (input$cormethod == "KarlPearson"){
      return(cor.test(var1, var2, method = "pearson"))
    } else if(input$cormethod == "Spearman"){
      return(cor.test(var1, var2, method="spearman"))
    }
  }
  )
  
  output$cor_t <- renderPrint({
    
    cortest()
  })
  
  # Logistic Regression
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "logrvar", choices = names(data_input()))
  }
  )
  
  logrout <- reactive({
    
    df <- data_input()
    
    var <- input$logrvar
    
    df[,var] <- as.factor(df[,var]) 
    
    Train <- createDataPartition(df[, var], p=as.numeric(input$logrprop), list=FALSE)
    training <- df[Train, ]
    testing <- df[-Train, ]
    
    trainprop <- nrow(training)/(nrow(testing)+nrow(training))
    
    # var1 <- input$logryname
    
    mod_fit <- train(as.formula(paste(var, "~", ".")),  data=training, method="glm", family="binomial")
    
    expout <- exp(coef(mod_fit$finalModel))
    
    if (input$logroption == "Show Prop."){
      return(trainprop)
    } else if (input$logroption == "Fit"){
      return(mod_fit)
    }
    
    if (input$logroption == "Coef."){
      return(data.frame(expout))
    }
    
    predout <- predict(mod_fit, newdata=testing)
    # predoutproba <- predict(mod_fit, newdata=testing, type="prob")
    accuracy <- table(predout, testing[, input$logrvar])
    out <- sum(diag(accuracy))/sum(accuracy)
    confmat <- confusionMatrix(predout, testing[, input$logrvar])

    
    
    if (input$logroption == "Accuracy"){
      return(confmat)
    }
    return(var1)
  })
  
  
  output$logroutput <- renderPrint({
    
    if(input$logroption == "Coef."){
      logrout()
    } else if (input$logroption == "Show Prop."){
      logrout()
    } else if (input$logroption == "Fit"){
      logrout()
    } else if (input$logroption == "Accuracy"){
      logrout()
    }
    
  })
  
  
  
  
  # SVM
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "svmvar", choices = names(data_input()))
  }
  )
  
  
  svmout <- reactive({
    
    df <- data_input()
    cust = FALSE
    
    if(input$svmoption == "Custom Data"){
      df2 <- data_input2() 
      cust = TRUE
    }
    
    
    intrain <- createDataPartition(y = df[, input$svmvar], p= as.numeric(input$svmprop), list = FALSE)
    training <- df[intrain,]
    testing <- df[-intrain,]
    
    if (input$svmoption == "Show Prop."){
      return(list(dim(training), dim(testing)))
    }
    
    training[,input$svmvar] = factor(training[,input$svmvar])
    
    var <- input$svmvar
    
    trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
    
    svm_Linear <- train(as.formula(paste(var, "~", ".")), data = training, method = "svmLinear",
                        trControl=trctrl,
                        preProcess = c("center", "scale"),
                        tuneLength = 10)
    
    if (input$svmoption == "Fit"){
      return(svm_Linear)
    }
    
    test_pred <- predict(svm_Linear, newdata = testing)
    if(cust == TRUE){
      cust_pred <- predict(svm_Linear, newdata = df2[, !(names(df2) %in% var)])
    }
    
    
    if (input$svmoption == "Predicted"){
      return(data.frame(test_pred))
    }
    
    confmat <- confusionMatrix(as.factor(test_pred), as.factor(testing[, var]) )
    
    if (input$svmoption == "Accuracy"){
      return(confmat)
    }
    
    if (input$indata2 == "Make Predictions"){
      return(data.frame(cust_pred))
    }
    

    
  })
  
  output$svmoutput <- renderPrint({
    
    if(input$svmoption == "Coef."){
      svmout()
    } else if (input$svmoption == "Show Prop."){
      svmout()
    } else if (input$svmoption == "Fit"){
      svmout()
    } else if (input$svmoption == "Accuracy"){
      svmout()
    } else if (input$svmoption == "Predicted"){
      svmout()
    } else if(input$indata2 == "Make Predictions"){
      print("Prediction on custom test set.")
      svmout()
    } else if(input$svmoption == "Custom Data"){
      svmout()
    }
    
    
  })
  
  output$tab2 <- renderTable({
    
    df2 <- data_input2()
    
    if(input$indata2 == "View"){
      print(df2)
    } else if(input$indata2 == "Clear"){
      print("Clear")
    }
  })


  


}


shinyApp(ui, server)

