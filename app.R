
library(shiny)
library(dplyr)
library(ggplot2)
library(reshape)

anz<- read.csv("https://sms.wgtn.ac.nz/foswiki/pub/Courses/DATA301_2022T2/Assignments/ANZ_Premiership_2017_2022.csv")
anz_teams<- as.factor(anz$Team)


anz_stack_melt <- melt( anz,
                        measure.vars = c("W", "L"),
                        variable_name = "Value")
years<- range(anz_stack_melt$Year)
ui <- fluidPage(
  
  titlePanel("ANZ Premiership Netball"),
  
  sidebarLayout(
    
    sidebarPanel(
    selectInput("Teams","Teams",
                   choices = unique(anz$Team), 
                   selected = unique(anz$Team)[1]),
  
    sliderInput( "Year",
                 label = "Select Year Range", 
                 min = years[1], 
                 max = years[2], 
                 value = years,
                 step = 1
    )),
  mainPanel(
    #highchartOutput("plot"),
    plotOutput("stack"),
    plotOutput("line"),
    plotOutput("bubble")
  )
  )
)

# Define server logic
server <- function(input, output) {
 
  reactive_anz<-reactive({
    anz<-anz_stack_melt %>% filter(Year >= input$Year[1])  %>% filter(Year <= input$Year[2]) %>% filter(Team == input$Teams)
    anz
    })
  
# Plotting stacked boxplots for win/loss 
  
  output$stack <- renderPlot({
    ggplot(data=reactive_anz(),
           aes(x = Team ,y = Year,fill=Value)) +
              geom_bar(stat = "identity") +
              scale_fill_brewer()+facet_grid(~Year) +
             labs(title = "AnZ teams Win-Loss Stackplot",
              x = "Teams",
              y = "Games")
  })
  
  output$line <- renderPlot({
    ggplot(data=reactive_anz(),
           aes(x = Year, y = Pts,col=Team)) +
           geom_line(size =2,
           stat = "identity")+scale_fill_brewer()+
           labs(tite = "Team points",
           x = "Year",
           y = "Points")
  })
  
  output$bubble <- renderPlot({
    ggplot(data = reactive_anz(),
           aes(x = GF,
               y = GA,
               colour = Team)) +
      geom_point() +
      labs(tite = "Teams points",
           x = "GF",
           y = "GA")
  })
 
}

# Create Shiny app
shinyApp(ui = ui, server = server)