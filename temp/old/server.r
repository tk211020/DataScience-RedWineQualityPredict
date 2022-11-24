library(shiny)
library(caret)


wine_df1 <- read.csv("winequality-red.csv", header = T)
x = wine_df1[, 1:11] 
y = as.factor(wine_df1$quality)
cart_p <- read.csv("CART_performance.csv", header = T)
randF_p <- read.csv("randomForest_performance.csv", header = T)
xgb_p <- read.csv("xgboost_performance.csv", header = T)
shinyServer(function(input, output, session) {
  
  
  output$box<- renderPlot({
    if(input$feature=="all")
      bp <- featurePlot(x,y, plot = "box", labels = "box plot",
                        scales = list(x = list(relation="free"), y = list(relation="free")), 
                        adjust = 1.5, pch = ".", 
                        layout = c(4, 3), auto.key = list(columns = 3))
    else  
      bp <- featurePlot(x[,input$feature], y, plot = "box",labels = list(x="box plot",y=NA) )
  print(bp)
    })
  output$density<- renderPlot({
    if(input$feature=="all")
      dp <- featurePlot(x,y, plot = "density", labels = "density plot",
                        scales = list(x = list(relation="free"), y = list(relation="free")), 
                        adjust = 1.5, pch = ".", 
                        layout = c(4, 3), auto.key = list(columns = 3))
    else 
      dp <- featurePlot(x[,input$feature],y, plot = "density",labels = "density plot")
    print(dp)
  })
  
  output$cart_ptable <- renderTable({cart_p})
  output$randF_ptable <- renderTable({randF_p})
  output$xgb_ptable <- renderTable({xgb_p})
})