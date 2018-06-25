#Designing Web Interface using Shiny package for an interactive Gradient Boosted Model Output"
#LAAVANYA GANESH, UIC , UIN:654324917, Masters in Business Analytics

#Workspace setting
getwd()
setwd("/Users/laavanyaganesh/Desktop/Web/PredictiveAnalytics/")
getwd()

#installing necessary packages
library("shiny")
library("gbm")
library("ggplot2")

#Designing User Interface
ui <- pageWithSidebar(
  titlePanel('Select Parameters For Gradient Boosted Model', align="center"),
  sidebarPanel(
    sliderInput(inputId = "NumberOfTrees", label = "Number of decision trees", min = 1, max = 500, value = 10),
    selectInput(inputId = "TrainingSampleSize", label = "Sub-sample train data size for each tree", choices = list(0.5,0.6,0.7,0.8,0.9,"1.0" = 1.0)),
    sliderInput(inputId = "Depth", label = "Depth to which each tree should be grown", min = 1, max = 50, value = 1),
    selectInput(inputId = "Shrinkage", label = "Shrinkage parameter", choices = list(1,0.8,0.5,0.3,0.1,0.08,0.05,0.01,0.005,0.001))
  ),
  mainPanel(
    plotOutput(outputId = "PredictionPlot")
  )
)



server <- function(input, output){
  
  output$PredictionPlot <- renderPlot({

# Creating the data
    set.seed(12345)
    x = runif(100, min = 0, max = 10)
    x = sort(x, decreasing = F)
    df = data.frame(x = x,y = sin(x))
# Fitting the model
    fit <- gbm(y~x, data=df, distribution="gaussian", n.trees = input$NumberOfTrees, shrinkage = as.numeric(input$Shrinkage), interaction.depth = input$Depth, bag.fraction = as.numeric(input$TrainingSampleSize))
    
# Make predictions on the train data itself
    predictions <- predict(fit, df, n.trees = input$NumberOfTrees)
    df$pred = predictions
    
# Plotting Actual vs Predicted
    d <- ggplot(df, aes(x)) + 
      geom_line(aes(y = y, colour = "Actual"), size=1) +  
      geom_line(aes(y = pred, colour = "Predicted"), size=1) +  
      xlab("Input Variable (x)") + ylab("Output Variable (y)") +  
      theme(
        axis.title.x = element_text(color="dark green", size=18, face="bold"),
        axis.title.y = element_text(color="dark green", size=18, face="bold"),
        axis.text.x= element_text(size=18),
        axis.text.y= element_text(size=18),
        legend.text = element_text(size = 16),
        legend.position = "right",
        legend.title = element_blank()
      )
    d + scale_color_manual(values=c("blue", "red"))
    
  }, height = 600, width = 900)
  
}

shinyApp(ui = ui, server = server)