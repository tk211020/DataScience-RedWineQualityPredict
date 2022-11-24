
library(caret)
#library(ggplot2)
#library(ellipse)


wine_df1 <- read.csv("winequality-red.csv", header = T)
x = wine_df1[, 1:11] 
y = as.factor(wine_df1$quality)
choices=colnames(x)
choices=c("all",choices)
box <- featurePlot(x,y, plot = "box", 
            scales = list(x = list(relation="free"), y = list(relation="free")), 
            adjust = 1.5, pch = ".", 
            layout = c(4, 3), auto.key = list(columns = 3))

print(box)
bp <- featurePlot(x, y, plot = "box",labels = "box plot",scales = list(x = list(relation="free"), y = list(relation="free")), adjust = 1.5,layout = c(4, 3))
print(bp)
box1 <- featurePlot(x[,1],y,plot="box",labels="box plot")
print(box1)
den <- featurePlot(x, y, plot = "density", 
                  scales = list(x = list(relation="free"), y = list(relation="free")), 
                  adjust = 1.5, pch = ".", 
                  layout = c(4, 3), auto.key = list(columns = 3))

nullmodel <- read.csv("nullModel.rds_performance_.csv", header = T)
nullmodel
