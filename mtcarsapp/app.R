library(shiny)
library(ggplot2)
data("mtcars")

# Define UI fluidPage arrangement by adding columns
ui <- fluidPage(
    fluidRow(
        column(12,
               h3("Graphs and Linear Regression Summary"),
               h4("Dataset: mtcars"),
               h5("About the App:This app plot the graph for X and Y variables of your choice from the selection
                menu of variables. Once you selected the XY vars you can opt for the Fcator variable to color and size the 
                points.This app also give you the liberty to choose the graph type from the checkbox menu. You can also check 
                the boxes to show the title and X-Y labels.The great advantage of this app is, besides giving you the plot it also 
                provide you the linear regression summary for the fit along with residual vs. fitted values graph."),
               # fluidRow inside fluidRow
               fluidRow(
                   column(3,
                   h4("1.Please select the variables"),
                          selectInput("X", "X Variable", choices = names(mtcars),selected = "hp"),
                          selectInput("Y", "Y Variable", choices = names(mtcars)),
                          selectInput("color", "color Factor", choices = c("cyl","vs","am","gear","carb")),
                   h4("2.Select the graph type"),
                          selectInput("geom", "geom", c("point", "smooth",  "fit","jitter","violin","box","density")),
                   h4("3.Check to show the labels"),
                          checkboxInput("show_xlab","show X Axis Label",value=FALSE),
                          checkboxInput("show_ylab","show Y Axis Label",value=FALSE),
                   h4("4.Check to show the title"),
                          checkboxInput("Title","Show Title")
                   ),
                   column(9,
                          plotOutput("plot")
                   )
               )
        ),
        fluidRow(
            column(12,
                   column(7,
                          verbatimTextOutput("Fit_Summary")
                   ),
                   column(5,
                          plotOutput("plotFit")
                   )
            )
        )
    )
)


# Define server logic required to show output
server <- function(input, output,session) {
    # generating options for plot_geom according to choice
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(position="identity"),
               smooth = geom_smooth(se = TRUE,alpha=0.1),
               fit= geom_smooth(method = lm,alpha=0.1),
               jitter = geom_jitter(),
               violin= geom_violin(position="identity"),
               box= geom_boxplot(position="identity"),
               density=geom_density2d()
               
               
               
        )
    })
    # create ggplot according to var choices and factor
    output$plot <- renderPlot({
        x=mtcars[[input$X]]
        y=mtcars[[input$Y]]
        factor= as.factor(mtcars[[input$color]])
        Fit<-lm(y ~ x + factor)
        xlab<-ifelse(input$show_xlab,input$X,"")
        ylab<-ifelse(input$show_ylab,input$Y,"")
        title<-ifelse(input$Title, paste(input$Y,"vs", input$X,"with",input$color,"as factor"),"")
        ggplot(mtcars, aes(x, y,fill=factor,color=factor,size=y,shape=factor)) + xlab(xlab) + ylab(ylab)+
            theme(plot.title = element_text(hjust = 0.5))+
            ggtitle(title)+
            plot_geom() 
    })
    # generating Fit summary
    output$Fit_Summary <- renderPrint ({
        x=mtcars[[input$X]]
        y=mtcars[[input$Y]]
        a=as.factor(mtcars[[input$color]])
        Fit<-lm(y ~ x + a)
        summary(Fit)
    })
    # create fitplot residual vs fitted.values according to var choices and factor
    output$plotFit<-renderPlot({
        x=mtcars[[input$X]]
        y=mtcars[[input$Y]]
        a=as.factor(mtcars[[input$color]])
        Fit<-lm(y ~ x + a)
        plot(Fit$residuals~Fit$fitted.values)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
