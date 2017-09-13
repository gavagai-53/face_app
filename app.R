library(shiny)
source("face_functions.R")
#load('pls_res_individual.Rdat')
load('pls_res_mean.Rdat')
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Emotions"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "happy",
                  label = "happy rating",
                  min = 0,
                  max = 100,
                  value = 0),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "sad",
                  label = "sad rating",
                  min = 0,
                  max = 100,
                  value = 0),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "surprised",
                  label = "surprised rating",
                  min = 0,
                  max = 100,
                  value = 0),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "disgusted",
                  label = "disgusted rating",
                  min = 0,
                  max = 100,
                  value = 0),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "angry",
                  label = "angry rating",
                  min = 0,
                  max = 100,
                  value = 0),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "fearful",
                  label = "fearful rating",
                  min = 0,
                  max = 100,
                  value = 0),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "interested",
                  label = "interested rating",
                  min = 0,
                  max = 100,
                  value = 0),
      sliderInput(inputId = "time",
                  label = "time",
                  min = 1,
                  max = 100,
                  value = 50)
      
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: face plot
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    
    new_vec=c(input$happy,input$sad, input$surprised,input$disgusted , input$angry,input$fearful,input$interested)
    plot_middle_prediction(new_vec,pls_res,input$time)
    # 
    # x    <- faithful$waiting
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    # hist(x, breaks = bins, col = "#75AADB", border = "white",
    #      xlab = "Waiting time to next eruption (in mins)",
    #      main = "Histogram of waiting times")
    
  },height = 800, width = 600 )
  
}
shinyApp(ui = ui, server = server)
