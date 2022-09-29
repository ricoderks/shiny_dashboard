library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyjqui)
library(plotly)

ui <- dashboardPage(
  #### header ####
  dashboardHeader(title = "SODA - dashboard"),
  
  #### sidebar menu ####
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Dummy", 
               tabName = "Dummy", 
               icon = icon("file-excel")),
      menuItem(text = "Visualisation",
               tabName = "visualisation",
               icon = icon("bar-chart-o"))
    )
  ),
  
  #### body ####
  dashboardBody(
    # determine the size of the browser window
    tags$head(tags$script('
            var dimension = [0, 0];
            $(document).on("shiny:connected", function(e) {
              dimension[0] = window.innerWidth;
              dimension[1] = window.innerHeight;
              Shiny.onInputChange("dimension", dimension);
            });
            $(window).resize(function(e) {
              dimension[0] = window.innerWidth;
              dimension[1] = window.innerHeight;
              Shiny.onInputChange("dimension", dimension);
            });
            ')),
    
    tabItems(
      tabItem(
        tabName = "Dummy",
        fluidRow(
          p("Load files here"),
          verbatimTextOutput("dimension_display")
        )
      ),
      
      tabItem(
        tabName = "visualisation",
        fluidRow(
          column(
            width = 4,
            checkboxGroupButtons(inputId = "showPlots",
                                 label = "Show plots:",
                                 choices = c("Histogram" = "histogram",
                                             "Histogram with controls" = "histogramCntrl",
                                             "Plotly" = "plotly"),
                                 selected = c("histogram", "histogramCntrl", "plotly"))
          ),
          column(
            width = 4,
            radioGroupButtons(inputId = "moveZoomPlots",
                              label = "Interaction:",
                              choices = c("Zoom plotly" = "zoom",
                                          "Move plots" = "move"))
          ),
        ), # end fluidrow 1
        uiOutput(outputId = "uiPlots")
      ) # end shinyjqServer
    ) # end tabitems
  )
)

#### server function ####
server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)
  plotly_data <- data.frame(x = 1:100,
                            y = rnorm(100))
  
  boxDimension <- reactiveValues(width = NULL,
                                 height = NULL)
  
  output$dimension_display <- renderText({
    paste("width:", input$dimension[1], "| height:", input$dimension[2], "\n",
          boxDimension$width, boxDimension$height)
  })
  
  # set the dimensions of the boxes
  observe({
    boxWidth <- switch(
      as.character(length(input$showPlots)),
      "1" = 12,
      "2" = 6,
      "3" = 6
    )
    
    boxHeight <- switch(
      # newHeight = windowHeight - header - buttons - margin
      as.character(length(input$showPlots)),
      "1" = paste0(input$dimension[2] - 50 - 125 - 50, "px"),
      "2" = paste0(input$dimension[2] - 50 - 125 - 50, "px"),
      "3" = paste0((input$dimension[2] - 50 - 225 - 50) / 2, "px")
    )
    
    boxDimension$width <- boxWidth
    boxDimension$height <- boxHeight
  })
  
  # calculate the width
  boxWidth <- reactive({
    req(input$showPlots)
    # use observe otherwise I can not see that input$showPlot is empty.
    boxWidth <- switch(
      as.character(length(input$showPlots)),
      "1" = 12,
      "2" = 6,
      "3" = 6
    )
    
    return(boxWidth)
  })
  
  #### shinyjquiServer ####
  
  ###### plots" for shinyjquiServer ######
  # histogram
  output$histPlot <- renderPlot({
    hist(histdata)
  })
  
  # histogram with controls
  output$histCntrlPlot <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  # plotly plot
  output$plotlyPlot <- renderPlotly({
    # needed to make the plotly graph resize with the size of the box
    req(input$showPlots)
    
    plotly_data |>
      plot_ly(x = ~x,
              y = ~y) |>
      add_markers() |> 
      config(displayModeBar = input$moveZoomPlots == "zoom") |> 
      layout(xaxis = list(fixedrange = input$moveZoomPlots == "move"), 
             yaxis = list(fixedrange = input$moveZoomPlots == "move"))
  })
  
  # render all plots
  output$uiPlots <- renderUI({
    req(boxDimension)
    
    tagList(
      fluidRow(
        # show plots
        div(id = "divPlots",
            if("histogram" %in% input$showPlots) {
              box(
                id = "histogramBox",
                title = "Histogram",
                width = boxDimension$width,
                solidHeader = TRUE,
                collapsible = TRUE,
                status = "primary",
                plotOutput(outputId = "histPlot",
                           height = boxDimension$height)
              )
            } else {
              NULL
            }, 
            if("histogramCntrl" %in% input$showPlots) {
              box(
                id = "histogramCntrlBox",
                title = "Histogram with controls",
                width = boxDimension$width,
                solidHeader = TRUE,
                status = "primary",
                collapsible = TRUE,
                plotOutput(outputId = "histCntrlPlot",
                           height = boxDimension$height),
                sidebar = boxSidebar(
                  id = "hist_sidebar",
                  width = 40,
                  sliderInput(inputId = "slider",
                              label = "Number of observations:",
                              min = 1,
                              max = 100,
                              value = 50)
                ) # end boxSidebar
              )
            } else {
              NULL
            },
            if("plotly" %in% input$showPlots) {
              box(
                id = "plotlyBox",
                title = "Plotly plot",
                width = boxDimension$width,
                solidHeader = TRUE,
                status = "primary",
                collapsible = TRUE,
                plotlyOutput(outputId = "plotlyPlot",
                             height = boxDimension$height)
              )
            } else {
              NULL
            }
        )
        
      ) # end fluidRow 2
    ) # end tagList
  }) # end renderUI
  
  observeEvent(input$moveZoomPlots, {
    
    if (input$moveZoomPlots == "zoom") {
      jqui_sortable(ui = "#divPlots",
                    operation = "disable")
    } else {
      jqui_sortable(ui = "#divPlots",
                    operation = "enable")
    }
  })
  
} # end server


#### run everything ####
shinyApp(ui = ui, 
         server = server,
         options = list("launch.browser" = TRUE))