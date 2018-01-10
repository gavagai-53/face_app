library(shiny)
source("face_functions.R")

connections <- read.csv('adj_face_points.csv', header = FALSE)

# define time range globally 
time_range <- c(1,100)
time_start_val <- 50

# Define UI for app
ui <- fluidPage(
  
  # App title ----
  titlePanel("Emotions"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
       
      sliderInput(inputId = "happy",
                  label = "happy rating",
                  min = -300,
                  max = 300,
                  value = 0),
       
      sliderInput(inputId = "sad",
                  label = "sad rating",
                  min = -300,
                  max = 300,
                  value = 0),
       
      sliderInput(inputId = "surprised",
                  label = "surprised rating",
                  min = -300,
                  max = 300,
                  value = 0),
       
      sliderInput(inputId = "disgusted",
                  label = "disgusted rating",
                  min = -300,
                  max = 300,
                  value = 0),
       
      sliderInput(inputId = "angry",
                  label = "angry rating",
                  min = -300,
                  max = 300,
                  value = 0),
       
      sliderInput(inputId = "fearful",
                  label = "fearful rating",
                  min = -300,
                  max = 300,
                  value = 0),
       
      sliderInput(inputId = "interested",
                  label = "interested rating",
                  min = -300,
                  max = 300,
                  value = 0),


      sliderInput(inputId = "time",
                  label = "time",
                  min = time_range[1],
                  max = time_range[2],
                  value = time_start_val),
      

      
      actionButton("play", "Play!"),
      actionButton("loadmodel1","Model1"),
      actionButton("loadmodel2","Model2")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      fluidRow(textOutput(outputId="labels"),plotOutput(outputId = "distPlot")
      )
  

    )
  )
)

# Define server logic required to draw the plots
server <- function(input, output, session) {

  #load two default models here
  #model1
  object_name=load("models/pls_res_NTASDvids.Rdat")
  pls_res1=get(object_name)
  rm(object_name)
  #model2
  object_name=load("models/pls_res_asd.Rdat")
  pls_res2=get(object_name)
  rm(object_name)

  
  state <- reactiveValues()
  state$running <- FALSE
  
  observeEvent(input$play, {
    state$running <- TRUE
  })
  
  observeEvent(input$loadmodel1, {
    chosenfile=file.choose()
    object_name=load(chosenfile)
    pls_res1<<-get(object_name)
    rm(object_name)
  })
  
  observeEvent(input$loadmodel2, {
    chosenfile=file.choose()
    object_name=load(chosenfile)
    pls_res2<<-get(object_name)
    rm(pls_res)
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
      input$interested
    )

    plot_same_window(new_vec,pls_res1,pls_res2,input$time)
    output$labels=renderText(paste(find_labels(new_vec,70)," ",collapse=''))
    
  }, height=1000, width=1000
  )

  
  
  
}
shinyApp(ui = ui, server = server)
