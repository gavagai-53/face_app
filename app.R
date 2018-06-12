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
      
      fileInput(
        "modelfile", "Upload model",
        accept = c(".Rdata", ".rda", ".Rdat")
      ),
      tags$div(
        id="divActiveModels", checked=NA, 
        checkboxGroupInput(
          "activeModels", "Select models for display",
          choices = c("default"),
          selected = c("default")
        )
      ),
      actionButton("play", "Play!"),
      
      # Reset button in UI 
      actionButton("reset", "Reset")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      fluidRow(
        #textOutput(outputId="labels"),
        plotOutput(
          outputId = "distPlot"
        )
      )
  

    )
  )
)

# Define server logic required to draw the plots
server <- function(input, output, session) {
  
  # set maximum upload size to 10MB
  options(shiny.maxRequestSize=10*1024^2) 
  
  # init reactive context
  context <- reactiveValues()
  context$running <- FALSE

  models <- reactiveValues()
  models$default <- get(load("models/pls_res_NTASDvids.Rdat"))
  
  observeEvent(input$play, {
    context$running <- TRUE
  })
  
  # create non-reactive buffer
  active <- c()
  
  observeEvent(input$activeModels, {
    active <<- names(models)[names(models) %in% input$activeModels]
  })
  
  observeEvent(input$modelfile, {
    # event gets triggered when files are uploaded
    # load model
    name <- input$modelfile$name
    
    models[[name]] <<- get(
      load(input$modelfile$datapath)
    )
    
    removeUI("#activeModels")
    
    # and add model to checkbox
    insertUI(
      "#divActiveModels",
      where = "afterEnd",
      ui = checkboxGroupInput(
        "activeModels", "Select models for display",
        choices = names(models),
        selected = c(active, name)
      )
    )
  })
  
  output$distPlot <- renderPlot({
    if ((input$time < time_range[2]) && context$running){
      # raise time state
      updateSliderInput(
        session,
        inputId = "time",
        label = "time",
        value = {
          # fix this to change moving "speed" 
          input$time + 5
        }
      )
      
      # this causes the plot to re-render every 10 ms
      invalidateLater(10)
    } else if (context$running){
      context$running <- FALSE
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
    
    active <- input$activeModels
    if (!is.null(active)){
      plot_same_window(new_vec, models, active, input$time)
    }
    
    #output$labels = renderText(paste(find_labels(new_vec,70)," ",collapse=''))
    
  }, height=1000, width=1000
  )
  
  # Reset button listener to reset all values to 0.
  observeEvent(input$reset, {
    updateSliderInput(session, "happy", value = 0)
    updateSliderInput(session, "sad", value = 0)
    updateSliderInput(session, "surprised", value = 0)
    updateSliderInput(session, "disgusted", value = 0)
    updateSliderInput(session, "angry", value = 0)
    updateSliderInput(session, "fearful", value = 0)
    updateSliderInput(session, "interested", value = 0)
  })
}
shinyApp(ui = ui, server = server)
