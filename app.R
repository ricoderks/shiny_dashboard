library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyjqui)
library(shinyjs)
library(plotly)

ui <- dashboardPage(
  #### header ####
  dashboardHeader(title = "SODA - dashboard"),
  
  #### sidebar menu ####
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Not movable", 
               tabName = "notMovable", 
               icon = icon("bar-chart-o")),
      menuItem(text = "shinyjqui sortable", 
               tabName = "shinyjquiSort", 
               icon = icon("bar-chart-o")),
      menuItem(text = "shinyjqui movable", 
               tabName = "shinyjquiMovable", 
               icon = icon("bar-chart-o")),
      menuItem(text = "shinyjqui Server side",
               tabName = "shinyjquiServer",
               icon = icon("bar-chart-o"))
    ) # end sideBarMenu
  ),
  
  #### body ####
  dashboardBody(
    # activate shinyjs
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "notMovable",
        fluidRow(
          checkboxGroupButtons(inputId = "showPlots1",
                               label = "Show plots:",
                               choices = c("Histogram" = "histogram",
                                           "Histogram with controls" = "histogramCntrl",
                                           "Plotly" = "plotly"),
                               selected = c("histogram", "histogramCntrl", "plotly")),
          p("Some times empty space!")
        ), # end fluidrow 1
        fluidRow(
          # show plots
          box(
            id = "histogramBox1",
            title = "Histogram",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            plotOutput(outputId = "histPlot1")
          ),
          box(
            id = "histogramCntrlBox1",
            title = "Histogram with controls",
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            plotOutput(outputId = "histCntrlPlot1"),
            sidebar = boxSidebar(
              id = "hist_sidebar1",
              width = 40,
              sliderInput(inputId = "slider1",
                          label = "Number of observations:",
                          min = 1,
                          max = 100,
                          value = 50)
            ) # end boxSidebar
          ),
          box(
            id = "plotlyBox1",
            title = "Plotly plot",
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            plotlyOutput(outputId = "plotlyPlot1")
          )
        ) # end fluidrow 2
      ), # end notMovable
      
      tabItem(
        tabName = "shinyjquiSort",
        fluidRow(
          checkboxGroupButtons(inputId = "showPlots2",
                               label = "Show plots:",
                               choices = c("Histogram" = "histogram",
                                           "Histogram with controls" = "histogramCntrl",
                                           "Plotly" = "plotly"),
                               selected = c("histogram", "histogramCntrl", "plotly")),
          p("Some times empty space. Can not zoom the plotly plot without moving the plot.")
        ), # end fluidRow 1
        fluidRow(
          jqui_sortable(
            div(
              id = "movePlots",
              box(
                id = "histogramBox2",
                title = "Histogram",
                solidHeader = TRUE,
                collapsible = TRUE,
                status = "primary",
                plotOutput(outputId = "histPlot2")
              ),
              box(
                id = "histogramCntrlBox2",
                title = "Histogram with controls",
                solidHeader = TRUE,
                status = "primary",
                collapsible = TRUE,
                plotOutput(outputId = "histCntrlPlot2"),
                sidebar = boxSidebar(
                  id = "hist_sidebar2",
                  width = 40,
                  sliderInput(inputId = "slider2",
                              label = "Number of observations:",
                              min = 1,
                              max = 100,
                              value = 50)
                ) # end boxSidebar
              ),
              box(
                id = "plotlyBox2",
                title = "Plotly plot",
                solidHeader = TRUE,
                status = "primary",
                collapsible = TRUE,
                plotlyOutput(outputId = "plotlyPlot2")
              )
            ) # end div
          ) # end jqui_sortable
        ) # end fluidRow 2
      ), # end tab shinyjquiSort
      
      tabItem(
        tabName = "shinyjquiMovable",
        fluidRow(
          checkboxGroupButtons(inputId = "showPlots3",
                               label = "Show plots:",
                               choices = c("Histogram" = "histogram",
                                           "Histogram with controls" = "histogramCntrl",
                                           "Plotly" = "plotly"),
                               selected = c("histogram", "histogramCntrl", "plotly")),
          p("Same problem as sortable. Some times empty space. Can not zoom the plotly plot without moving the plot.")
        ), # end fluidRow 1
        fluidRow(
          jqui_draggable(
            div(
              id = "movePlots",
              box(
                id = "histogramBox3",
                title = "Histogram",
                solidHeader = TRUE,
                collapsible = TRUE,
                status = "primary",
                plotOutput(outputId = "histPlot3")
              ),
              box(
                id = "histogramCntrlBox3",
                title = "Histogram with controls",
                solidHeader = TRUE,
                status = "primary",
                collapsible = TRUE,
                plotOutput(outputId = "histCntrlPlot3"),
                sidebar = boxSidebar(
                  id = "hist_sidebar3",
                  width = 40,
                  sliderInput(inputId = "slider3",
                              label = "Number of observations:",
                              min = 1,
                              max = 100,
                              value = 50)
                ) # end boxSidebar
              ),
              box(
                id = "plotlyBox3",
                title = "Plotly plot",
                solidHeader = TRUE,
                status = "primary",
                collapsible = TRUE,
                plotlyOutput(outputId = "plotlyPlot3")
              )
            ) # end div
          ) # end jqui_draggable
        ) # end fluidRow 2 
      ), # end tab shinyjquiMovable
      
      tabItem(
        tabName = "shinyjquiServer",
        fluidRow(
          checkboxGroupButtons(inputId = "showPlots4",
                               label = "Show plots:",
                               choices = c("Histogram" = "histogram",
                                           "Histogram with controls" = "histogramCntrl",
                                           "Plotly" = "plotly"),
                               selected = c("histogram", "histogramCntrl", "plotly")),
          p("Can not change order of the plots.")
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
  
  # toggle the plots
  observe({
    ## static
    shinyjs::toggle(id = "histogramBox1",
                    condition = "histogram" %in% input$showPlots1)
    shinyjs::toggle(id = "histogramCntrlBox1",
                    condition = "histogramCntrl" %in% input$showPlots1)
    shinyjs::toggle(id = "plotlyBox1",
                    condition = "plotly" %in% input$showPlots1)
    
    ## shinyjquiSortable
    shinyjs::toggle(id = "histogramBox2",
                    condition = "histogram" %in% input$showPlots2)
    shinyjs::toggle(id = "histogramCntrlBox2",
                    condition = "histogramCntrl" %in% input$showPlots2)
    shinyjs::toggle(id = "plotlyBox2",
                    condition = "plotly" %in% input$showPlots2)
    
    ## shinyjquiMovable
    shinyjs::toggle(id = "histogramBox3",
                    condition = "histogram" %in% input$showPlots3)
    shinyjs::toggle(id = "histogramCntrlBox3",
                    condition = "histogramCntrl" %in% input$showPlots3)
    shinyjs::toggle(id = "plotlyBox3",
                    condition = "plotly" %in% input$showPlots3)
  })
  
  
  #### Static ####
  
  ###### plots for Static ######
  # histogram
  output$histPlot1 <- renderPlot({
    hist(histdata)
  })
  
  # histogram with controls
  output$histCntrlPlot1 <- renderPlot({
    data <- histdata[seq_len(input$slider1)]
    hist(data)
  })
  
  # plotly plot
  output$plotlyPlot1 <- renderPlotly({
    plotly_data |>
      plot_ly(x = ~x,
              y = ~y) |>
      add_markers()
    
  })
  
  #### shinyjquiSortable ####
  
  ###### plots for shinyjquiSortable ######
  # histogram
  output$histPlot2 <- renderPlot({
    hist(histdata)
  })
  
  # histogram with controls
  output$histCntrlPlot2 <- renderPlot({
    data <- histdata[seq_len(input$slider2)]
    hist(data)
  })
  
  # plotly plot
  output$plotlyPlot2 <- renderPlotly({
    plotly_data |>
      plot_ly(x = ~x,
              y = ~y) |>
      add_markers()
    
  })
  
  #### shinyjquiMovable ####
  
  ###### plots for shinyjquiMovable ######
  # histogram
  output$histPlot3 <- renderPlot({
    hist(histdata)
  })
  
  # histogram with controls
  output$histCntrlPlot3 <- renderPlot({
    data <- histdata[seq_len(input$slider3)]
    hist(data)
  })
  
  # plotly plot
  output$plotlyPlot3 <- renderPlotly({
    plotly_data |>
      plot_ly(x = ~x,
              y = ~y) |>
      add_markers()
    
  })
  
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
    req(input$showPlots4)
    
    plotly_data |>
      plot_ly(x = ~x,
              y = ~y) |>
      add_markers()  
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
      ) # end fluidRow 2
    )
  })
} # end server


#### run everything ####
shinyApp(ui = ui, 
         server = server,
         options = list("launch.browser" = TRUE))