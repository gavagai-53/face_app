library(shiny)
source("face_functions.R")
#load('pls_res_individual.Rdat')
#load('pls_res_mean_sex.Rdat')

load('pls_res_indi_sex_age.Rdat')

connections <- read.csv('adj_face_points.csv', header = FALSE)

# define time range globally 
time_range <- c(1,100)
time_start_val <- 0

# Define UI for app
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
      sliderInput(inputId = "age",
                  label = "age",
                  min = 16,
                  max = 60,
                  value = 25),

      sliderInput(inputId = "time",
                  label = "time",
                  min = time_range[1],
                  max = time_range[2],
                  value = time_start_val),
      
      radioButtons("sex", "sex",
                   c("male" = 0,
                     "female" = 1)),
      
      actionButton("play", "Play!")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: face plot
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  state <- reactiveValues()
  state$running <- FALSE
  
  observeEvent(input$play, {
    state$running <- TRUE
  })
  
  output$distPlot <- renderPlot({
    
    if ((input$time < time_range[2]) && state$running){
      # raise time state
      updateSliderInput(
        session,
        inputId = "time",
        label = "time",
        value = {
          # fix this to change moivng "speed" 
          input$time + 5
        }
      )
      
      # this causes the plot to re-render every 10 ms
      invalidateLater(10)
    } else if (state$running){
      state$running <- FALSE
      updateSliderInput(
        session,
        inputId = "time",
        label = "time",
        value = 0
      )
    }
    
    
    new_vec = c(
      input$happy,
      input$sad,
      input$surprised,
      input$disgusted,
      input$angry,
      input$fearful,
      input$interested,
      as.integer(input$sex),
      input$age
    )
    plot_middle_prediction(
      new_vec[1:(dim(pls_res$scores)[2])],
      pls_res,
      input$time
    )


  }, height=800, width=600
  )

  
  
  
}
shinyApp(ui = ui, server = server)
