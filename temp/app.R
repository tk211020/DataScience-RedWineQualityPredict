#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(DT)
library(data.table)
library(caret)
library(corrplot)
ggbiplot <- function(pcobj, choices = 1:2, scale = 1, pc.biplot = TRUE, 
                     obs.scale = 1 - scale, var.scale = scale, 
                     groups = NULL, ellipse = FALSE, ellipse.prob = 0.68, 
                     labels = NULL, labels.size = 3, alpha = 1, 
                     var.axes = TRUE, 
                     circle = FALSE, circle.prob = 0.69, 
                     varname.size = 3, varname.adjust = 1.5, 
                     varname.abbrev = FALSE, ...)
{
    library(ggplot2)
    library(plyr)
    library(scales)
    library(grid)
    
    stopifnot(length(choices) == 2)
    
    # Recover the SVD
    if(inherits(pcobj, 'prcomp')){
        nobs.factor <- sqrt(nrow(pcobj$x) - 1)
        d <- pcobj$sdev
        u <- sweep(pcobj$x, 2, 1 / (d * nobs.factor), FUN = '*')
        v <- pcobj$rotation
    } else if(inherits(pcobj, 'princomp')) {
        nobs.factor <- sqrt(pcobj$n.obs)
        d <- pcobj$sdev
        u <- sweep(pcobj$scores, 2, 1 / (d * nobs.factor), FUN = '*')
        v <- pcobj$loadings
    } else if(inherits(pcobj, 'PCA')) {
        nobs.factor <- sqrt(nrow(pcobj$call$X))
        d <- unlist(sqrt(pcobj$eig)[1])
        u <- sweep(pcobj$ind$coord, 2, 1 / (d * nobs.factor), FUN = '*')
        v <- sweep(pcobj$var$coord,2,sqrt(pcobj$eig[1:ncol(pcobj$var$coord),1]),FUN="/")
    } else if(inherits(pcobj, "lda")) {
        nobs.factor <- sqrt(pcobj$N)
        d <- pcobj$svd
        u <- predict(pcobj)$x/nobs.factor
        v <- pcobj$scaling
        d.total <- sum(d^2)
    } else {
        stop('Expected a object of class prcomp, princomp, PCA, or lda')
    }
    
    # Scores
    choices <- pmin(choices, ncol(u))
    df.u <- as.data.frame(sweep(u[,choices], 2, d[choices]^obs.scale, FUN='*'))
    
    # Directions
    v <- sweep(v, 2, d^var.scale, FUN='*')
    df.v <- as.data.frame(v[, choices])
    
    names(df.u) <- c('xvar', 'yvar')
    names(df.v) <- names(df.u)
    
    if(pc.biplot) {
        df.u <- df.u * nobs.factor
    }
    
    # Scale the radius of the correlation circle so that it corresponds to 
    # a data ellipse for the standardized PC scores
    r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
    
    # Scale directions
    v.scale <- rowSums(v^2)
    df.v <- r * df.v / sqrt(max(v.scale))
    
    # Change the labels for the axes
    if(obs.scale == 0) {
        u.axis.labs <- paste('standardized PC', choices, sep='')
    } else {
        u.axis.labs <- paste('PC', choices, sep='')
    }
    
    # Append the proportion of explained variance to the axis labels
    u.axis.labs <- paste(u.axis.labs, 
                         sprintf('(%0.1f%% explained var.)', 
                                 100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
    
    # Score Labels
    if(!is.null(labels)) {
        df.u$labels <- labels
    }
    
    # Grouping variable
    if(!is.null(groups)) {
        df.u$groups <- groups
    }
    
    # Variable Names
    if(varname.abbrev) {
        df.v$varname <- abbreviate(rownames(v))
    } else {
        df.v$varname <- rownames(v)
    }
    
    # Variables for text label placement
    df.v$angle <- with(df.v, (180/pi) * atan(yvar / xvar))
    df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar)) / 2)
    
    # Base plot
    g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
        xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal()
    
    if(var.axes) {
        # Draw circle
        if(circle) 
        {
            theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
            circle <- data.frame(xvar = r * cos(theta), yvar = r * sin(theta))
            g <- g + geom_path(data = circle, color = muted('white'), 
                               size = 1/2, alpha = 1/3)
        }
        
        # Draw directions
        g <- g +
            geom_segment(data = df.v,
                         aes(x = 0, y = 0, xend = xvar, yend = yvar),
                         arrow = arrow(length = unit(1/2, 'picas')), 
                         color = muted('red'))
    }
    
    # Draw either labels or points
    if(!is.null(df.u$labels)) {
        if(!is.null(df.u$groups)) {
            g <- g + geom_text(aes(label = labels, color = groups), 
                               size = labels.size)
        } else {
            g <- g + geom_text(aes(label = labels), size = labels.size)      
        }
    } else {
        if(!is.null(df.u$groups)) {
            g <- g + geom_point(aes(color = groups), alpha = alpha)
        } else {
            g <- g + geom_point(alpha = alpha)      
        }
    }
    
    # Overlay a concentration ellipse if there are groups
    if(!is.null(df.u$groups) && ellipse) {
        theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
        circle <- cbind(cos(theta), sin(theta))
        
        ell <- ddply(df.u, 'groups', function(x) {
            if(nrow(x) <= 2) {
                return(NULL)
            }
            sigma <- var(cbind(x$xvar, x$yvar))
            mu <- c(mean(x$xvar), mean(x$yvar))
            ed <- sqrt(qchisq(ellipse.prob, df = 2))
            data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = '+'), 
                       groups = x$groups[1])
        })
        names(ell)[1:2] <- c('xvar', 'yvar')
        g <- g + geom_path(data = ell, aes(color = groups, group = groups))
    }
    
    # Label the variable axes
    if(var.axes) {
        g <- g + 
            geom_text(data = df.v, 
                      aes(label = varname, x = xvar, y = yvar, 
                          angle = angle, hjust = hjust), 
                      color = 'darkred', size = varname.size)
    }
    # Change the name of the legend for groups
    # if(!is.null(groups)) {
    #   g <- g + scale_color_brewer(name = deparse(substitute(groups)), 
    #                               palette = 'Dark2')
    # }
    
    # TODO: Add a second set of axes
    
    return(g)
}


PATH="../input/"
wine_df <- read.csv("winequality-red.csv")
library(GGally)
wine_df1 <- wine_df
wine_df1$quality <- as.factor(wine_df1$quality)

x = wine_df1[, 1:11]
y = as.factor(wine_df1$quality)
cart_p <- read.csv("CART_performance.csv", header = T)
randF_p <- read.csv("randomForest_performance.csv", header = T)
xgb_p <- read.csv("xgboost_performance.csv", header = T)

cart_prec <- read.csv("CART_performance_recall.csv", header = T)
randF_prec <- read.csv("randomForest_performance_recall.csv", header = T)
xgb_prec <- read.csv("xgboost_performance_recall.csv", header = T)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Red Wine Quality Prediction"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h4("Data")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            DT::dataTableOutput("data"),
        )
    ),
    sidebarLayout(
        sidebarPanel(
            h4("Features box and density plots"),
        selectInput("feature","Feature:",
                    choices=c("all",colnames(x)),
                    selected='val'),
        ),
        mainPanel(
            plotOutput("box"),plotOutput("density")
        )
    ),
    sidebarLayout(
        sidebarPanel(
            h4("Correalation of the features"),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("pearsonPlot")
        )
    ),
    sidebarLayout(
        sidebarPanel(
            h4("PCA"),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("pcaWeightPlot"),
            plotOutput("pcaPlot")
        )
    ),
    sidebarLayout(
        sidebarPanel(
            h4("PCA Info"),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            DT::dataTableOutput("pcaTable")
        )
    ),
    sidebarLayout(
        sidebarPanel(
            h4("Performance"),
        ),
        mainPanel(
           tabsetPanel(
                tabPanel("CART",splitLayout(cellWidths =c("50%","50%"),
                                            tableOutput("cart_ptable"),
                                            tableOutput("cart_prectable"))
                ),
                tabPanel("Random Forest",splitLayout(cellWidths =c("50%","50%"),
                                                     tableOutput("randF_ptable"),
                                                     tableOutput("randF_prectable"))
                ),
                tabPanel("XGBOOST",splitLayout(cellWidths =c("50%","50%"),
                                               tableOutput("xgb_ptable"),
                                               tableOutput("xgb_prectable"))
                )
          )
        )
    ),
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$data <- DT::renderDataTable({
        as.data.table(wine_df1)
    })
    
    output$pearsonPlot <- renderPlot({
        nc=ncol(wine_df1)
        df <- wine_df1[,1:11]
        df$quality <- as.integer(wine_df1[,12])
        correlations <- cor(df,method="pearson")
        corrplot(correlations, number.cex = .9, method = "square", 
                 hclust.method = "ward", order = "FPC",
                 type = "full", tl.cex=0.8,tl.col = "black")
    })
    
    output$pcaWeightPlot <- renderPlot({
        q.pca <- prcomp(wine_df[1:11], center=TRUE, scale.=TRUE)
        plot(q.pca, type="l", main='')
        grid(nx = 10, ny = 14)
        title(main = "Principal components weights", sub = NULL, xlab = "Components")
        box()
    })
    
    output$pcaPlot <- renderPlot({
        q.pca <- prcomp(wine_df[1:11], center=TRUE, scale.=TRUE)
        q.diag = wine_df1[,12]
        ggbiplot(q.pca, choices=1:2, obs.scale = 1, var.scale = 1, groups = q.diag, 
                 ellipse = TRUE, circle = TRUE, varname.size = 4, ellipse.prob = 0.68, circle.prob = 0.69) +
            scale_color_discrete(name = 'Quality (from 3 to 8)') + theme_bw() + 
            labs(title = "Principal Component Analysis", 
                 subtitle = "1. Data distribution in the plan of PC1 and PC2\n2. Directions of components in the same plane") +
            theme(legend.direction = 'horizontal', legend.position = 'bottom')
    })
    
    output$pcaTable <- DT::renderDataTable({
        q.pca <- prcomp(wine_df[1:11], center=TRUE, scale.=TRUE)
        dt_pca <- as.data.table(data.frame(summary(q.pca)$importance))
        cbind(Info = c("Standard deviation","Proportion of Variance","Cumulative Proportion"), dt_pca)
    })
    
    output$box<- renderPlot({
        if(input$feature=="all")
            bp <- featurePlot(x,y, plot = "box", labels = c("Qualities","Values"),
                              scales = list(x = list(relation="free"), y = list(relation="free")), 
                              adjust = 1.5, pch = ".", 
                              layout = c(4, 3), auto.key = list(columns = 3))
        else  
            bp <- featurePlot(x[,input$feature], y, plot = "box",labels = c("Qualities","Values"),
                              scales = list(x = list(relation="free"), y = list(relation="free")),
                              )
        print(bp)
    })
    output$density<- renderPlot({
        if(input$feature=="all")
            dp <- featurePlot(x,y, plot = "density", labels =c("Values", "Density"),
                              scales = list(x = list(relation="free"), y = list(relation="free")), 
                              adjust = 1.5, pch = ".", 
                              layout = c(4, 3), auto.key = list(columns = 3))
        else 
            dp <- featurePlot(x[,input$feature],y, plot = "density",labels =c("Values", "Density"),
                              scales = list(x = list(relation="free"), y = list(relation="free")),
                              )
        print(dp)
    })
    output$cart_ptable <- renderTable({cart_p},caption="Accuracy",
                                      caption.placement = getOption("xtable.caption.placement", "top"),
                                      caption.width = getOption("xtable.caption.width", NULL))
    output$randF_ptable <- renderTable({randF_p},caption="Accuracy",
                                       caption.placement = getOption("xtable.caption.placement", "top"),
                                       caption.width = getOption("xtable.caption.width", NULL))
    output$xgb_ptable <- renderTable({xgb_p},caption="Accuracy",
                                     caption.placement = getOption("xtable.caption.placement", "top"),
                                     caption.width = getOption("xtable.caption.width", NULL))
    output$cart_prectable <- renderTable({cart_prec},caption="Recall",
                                         caption.placement = getOption("xtable.caption.placement", "top"),
                                         caption.width = getOption("xtable.caption.width", NULL))
    output$randF_prectable <- renderTable({randF_prec},caption="Recall",
                                          caption.placement = getOption("xtable.caption.placement", "top"),
                                          caption.width = getOption("xtable.caption.width", NULL))
    output$xgb_prectable <- renderTable({xgb_prec},caption="Recall",
                                        caption.placement = getOption("xtable.caption.placement", "top"),
                                        caption.width = getOption("xtable.caption.width", NULL))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
