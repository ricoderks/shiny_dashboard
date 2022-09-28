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
    tabItems(
      tabItem(
        tabName = "notMovable",
        fluidRow(
          actionButton(inputId = "restoreHistogram1",
                       label = "Histogram"),
          actionButton(inputId = "restoreHistogramControls1",
                       label = "Histogram with controls"),
          actionButton(inputId = "restorePlotly1",
                       label = "Plotly"),
          p("Some times empty space!")
        ), # end fluidrow 1
        fluidRow(
          # show plots
          box(
            id = "histogramBox1",
            title = "Histogram",
            solidHeader = TRUE,
            collapsible = TRUE,
            closable = TRUE,
            status = "primary",
            plotOutput(outputId = "histPlot1")
          ),
          box(
            id = "histogramCntrlBox1",
            title = "Histogram with controls",
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            closable = TRUE,
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
            closable = TRUE,
            status = "primary",
            collapsible = TRUE,
            plotlyOutput(outputId = "plotlyPlot1")
          )
        ) # end fluidrow 2
      ), # end notMovable
      
      tabItem(
        tabName = "shinyjquiSort",
        fluidRow(
          actionButton(inputId = "restoreHistogram2",
                       label = "Histogram"),
          actionButton(inputId = "restoreHistogramControls2",
                       label = "Histogram with controls"),
          actionButton(inputId = "restorePlotly2",
                       label = "Plotly"),
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
                closable = TRUE,
                status = "primary",
                plotOutput(outputId = "histPlot2")
              ),
              box(
                id = "histogramCntrlBox2",
                title = "Histogram with controls",
                solidHeader = TRUE,
                status = "primary",
                collapsible = TRUE,
                closable = TRUE,
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
                closable = TRUE,
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
          actionButton(inputId = "restoreHistogram3",
                       label = "Histogram"),
          actionButton(inputId = "restoreHistogramControls3",
                       label = "Histogram with controls"),
          actionButton(inputId = "restorePlotly3",
                       label = "Plotly"),
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
                closable = TRUE,
                status = "primary",
                plotOutput(outputId = "histPlot3")
              ),
              box(
                id = "histogramCntrlBox3",
                title = "Histogram with controls",
                solidHeader = TRUE,
                status = "primary",
                collapsible = TRUE,
                closable = TRUE,
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
                closable = TRUE,
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
          p("Server side moving.")
        )
      )
    ) # end tabitems
  )
)

server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)
  plotly_data <- data.frame(x = 1:100,
                            y = rnorm(100))
  
  #### buttons for notMovable ####
  observeEvent(input$restoreHistogram1, {
    updateBox(id = "histogramBox1",
              action = "restore")
  })
  
  observeEvent(input$restoreHistogramControls1, {
    updateBox(id = "histogramCntrlBox1",
              action = "restore")
  })
  
  observeEvent(input$restorePlotly1, {
    updateBox(id = "plotlyBox1",
              action = "restore")
  })
  
  #### plots for notMovable ####
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
  
  #### buttons for shinyjquiSortable ####
  observeEvent(input$restoreHistogram2, {
    updateBox(id = "histogramBox2",
              action = "restore")
  })
  
  observeEvent(input$restoreHistogramControls2, {
    updateBox(id = "histogramCntrlBox2",
              action = "restore")
  })
  
  observeEvent(input$restorePlotly2, {
    updateBox(id = "plotlyBox2",
              action = "restore")
  })
  
  #### plots for shinyjquiSortable ####
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
  
  #### buttons for shinyjquiMovable ####
  observeEvent(input$restoreHistogram3, {
    updateBox(id = "histogramBox3",
              action = "restore")
  })
  
  observeEvent(input$restoreHistogramControls3, {
    updateBox(id = "histogramCntrlBox3",
              action = "restore")
  })
  
  observeEvent(input$restorePlotly3, {
    updateBox(id = "plotlyBox3",
              action = "restore")
  })
  
  #### plots for shinyjquiMovable ####
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
}


# run everything
shinyApp(ui = ui, 
         server = server,
         options = list("launch.browser" = TRUE))