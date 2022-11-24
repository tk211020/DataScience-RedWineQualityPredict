library(ggbiplot)

wine_df1 <- read.csv("winequality-red.csv", header = T)
x = wine_df1[, 1:11]

shinyUI(pageWithSidebar(
  div(),
  sidebarPanel(
    selectInput("feature","Feature:",
                choices=c("all",colnames(x)),
                selected='val'),
  ),
  mainPanel(
    plotOutput("box"),plotOutput("density"),
    tableOutput("cart_ptable"),tableOutput("randF_ptable"),tableOutput("xgb_ptable")
  )
)
)