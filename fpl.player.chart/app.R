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
library(ggrepel)
library(DT)

info <- as.data.table(read.csv("player_info.csv"))

# Reset names of positions to those from FPL
info$element_type <- factor(info$element_type)
levels(info$element_type) <- c("GKP", "DEF", "MID", "FWD")


# Putting costs in line with FPL values and adding a points per cost variable
info$now_cost <- info$now_cost/10

info <- info[, points.per.cost := as.numeric(round(total_points/as.numeric(now_cost),1))]

info <- info[order(-total_points)]


#These three variables help fit the chart automatically

best.benchmark <- round(as.numeric(max(info[, points.per.cost])),1)

top.score <- max(as.numeric(info$total_points))

minute.threshold <- max(as.numeric(info$minutes))/3


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Fantasy Premier League Player Data"),

    # Sidebar with cost, max selected and position inputs 
    sidebarLayout(
        sidebarPanel(
            sliderInput("max.cost",
                        "Max Cost",
                        min = 5,
                        max = 12.5,
                        value = 12.5,
                        step = 0.1,
                        pre = "£",
                        post = "m",
                        ticks = FALSE),
          
              
            sliderInput("max.selected",
                        "Maximum Selected By",
                        min = 5,
                        max = 100,
                        value = 100,
                        step = 5,
                        post = "%",
                        ticks = FALSE),
            
            checkboxGroupInput("elements", "Positions",
                               levels(info$element_type), selected = levels(info$element_type)),
    
            ),
        
        # Show a plot of possible players
        mainPanel(
           plotOutput("plot"),
           
           dataTableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Build chart.
    
    output$plot <- renderPlot({
        ggplot(info[minutes>90 & as.numeric(now_cost)<(input$max.cost+0.1) &
                                     element_type %in% input$elements &
                                    as.numeric(selected_by_percent) < (as.numeric(input$max.selected)+0.1) &
                                    total_points>0], aes(x=as.numeric(now_cost), 
                                    y=as.numeric(total_points))) +
                            geom_point(aes(colour=element_type)) +
                            geom_abline(intercept=0, slope=c(best.benchmark, best.benchmark/1.5, best.benchmark/2), alpha=0.5) +
                            theme_bw() +
                            geom_label_repel(aes(label=ifelse(rank(-as.numeric(total_points))<5,as.character(web_name),"")), vjust=-1) +
                            scale_x_continuous(limits = c(4, input$max.cost+0.1), expand = c(0,0)) +
                            scale_y_continuous(limits = c(0, top.score + 10), expand = c(0,0)) +
                            scale_color_brewer(palette = "Set1") +
                            annotate("text", x=4.2, y=top.score+5, label="Lines match values with equal points per cost", hjust = "left", color='grey30') +
                            labs(x="Price", y="Total Points", color="Position")
    })
    
    # Build table with eligible players
    
    output$table <- renderDataTable({
        datatable(info[minutes>90 &
                           as.numeric(now_cost)<(input$max.cost+0.1) &
                           element_type %in% input$elements &
                           as.numeric(selected_by_percent) < (as.numeric(input$max.selected)+0.1) &
                           total_points>0,
                       
                       list(web_name, now_cost, total_points, points.per.cost)],
                  
                  colnames = c('Player', 'Cost', 'Total Points', 'Points per Cost')
                      )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
