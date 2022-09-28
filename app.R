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
      menuItem(text = "shinyjqui Server side",
               tabName = "shinyjquiServer",
               icon = icon("bar-chart-o"))
    )
  ),
  
  #### body ####
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Dummy",
        fluidRow(
          p("Load files here")
        )
      ),
      
      tabItem(
        tabName = "shinyjquiServer",
        fluidRow(
          column(
            width = 4,
            checkboxGroupButtons(inputId = "showPlots4",
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
  
  #### shinyjquiServer ####
  
  ###### plots for shinyjquiServer ######
  # histogram
  output$histPlot4 <- renderPlot({
    hist(histdata)
  })
  
  # histogram with controls
  output$histCntrlPlot4 <- renderPlot({
    data <- histdata[seq_len(input$slider4)]
    hist(data)
  })
  
  # plotly plot
  output$plotlyPlot4 <- renderPlotly({
    # needed to make the plotly graph resize with the size of the box
    req(input$showPlots4)
    
    plotly_data |>
      plot_ly(x = ~x,
              y = ~y) |>
      add_markers() |> 
      config(displayModeBar = input$moveZoomPlots == "zoom") |> 
      layout(xaxis = list(fixedrange = input$moveZoomPlots == "move"), 
             yaxis = list(fixedrange = input$moveZoomPlots == "move"))
  })
  
  # calculate the width
  boxWidth <- reactive({
    req(input$showPlots4)
    # use observe otherwise I can not see that input$showPlot4 is empty.
    boxWidth <- switch(
      as.character(length(input$showPlots4)),
      "1" = 12,
      "2" = 6,
      "3" = 6
    )
    
    return(boxWidth)
  })
  
  # render all plots
  output$uiPlots <- renderUI({
    req(boxWidth)
    
    tagList(
      fluidRow(
        # show plots
        div(id = "divPlots4",
            if("histogram" %in% input$showPlots4) {
              box(
                id = "histogramBox4",
                title = "Histogram",
                width = boxWidth(),
                solidHeader = TRUE,
                collapsible = TRUE,
                status = "primary",
                plotOutput(outputId = "histPlot4")
              )
            } else {
              NULL
            }, 
            if("histogramCntrl" %in% input$showPlots4) {
              box(
                id = "histogramCntrlBox4",
                title = "Histogram with controls",
                width = boxWidth(),
                solidHeader = TRUE,
                status = "primary",
                collapsible = TRUE,
                plotOutput(outputId = "histCntrlPlot4"),
                sidebar = boxSidebar(
                  id = "hist_sidebar4",
                  width = 40,
                  sliderInput(inputId = "slider4",
                              label = "Number of observations:",
                              min = 1,
                              max = 100,
                              value = 50)
                ) # end boxSidebar
              )
            } else {
              NULL
            },
            if("plotly" %in% input$showPlots4) {
              box(
                id = "plotlyBox4",
                title = "Plotly plot",
                width = boxWidth(),
                solidHeader = TRUE,
                status = "primary",
                collapsible = TRUE,
                plotlyOutput(outputId = "plotlyPlot4")
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
      jqui_sortable(ui = "#divPlots4",
                    operation = "disable")
    } else {
      jqui_sortable(ui = "#divPlots4",
                    operation = "enable")
    }
  })
  
} # end server


#### run everything ####
shinyApp(ui = ui, 
         server = server,
         options = list("launch.browser" = TRUE))