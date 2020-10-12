#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Fantasy Premier League Player Data"),

    # Sidebar with cost and position inputs 
    sidebarLayout(
        sidebarPanel(
            sliderInput("max.cost",
                        "Max Cost",
                        min = 4.5,
                        max = 12.5,
                        value = 12.5,
                        step = 0.1),
            
            checkboxGroupInput("elements", "positions",
                               c(1,2,3,4), selected = c(1,2,3,4)),
            selectInput("max.selected",
                        "Max Selected By",
                        choices = c(100,50,25,10,5,2.5,1))
            ),
        
        # Show a plot of possible players
        mainPanel(
           plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    info <- as.data.table(read.csv("player_info.csv"))
    
    info <- info[, points.per.cost := as.numeric(total_points)/as.numeric(now_cost)]
    
    best.benchmark <- round(as.numeric(max(info[, points.per.cost])),1)
    
    output$plot <- renderPlot({
        ggplot(info[minutes>90 & as.numeric(now_cost)<((input$max.cost*10)+0.1) & element_type %in% input$elements & as.numeric(selected_by_percent) < (as.numeric(input$max.selected)+0.1)], aes(x=as.numeric(now_cost), y=as.numeric(total_points))) +
                             geom_point(aes(colour=factor(element_type))) +
                             geom_abline(intercept=0, slope=c(best.benchmark, best.benchmark/1.5, best.benchmark/2), alpha=0.5) +
                            theme_minimal() +
                            geom_text(aes(label=ifelse(rank(-as.numeric(total_points))<10,as.character(web_name),"")), vjust=-1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
