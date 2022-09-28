library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
library(shinyjqui)

ui <- dashboardPage(
  dashboardHeader(title = "SODA - dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Files", 
               tabName = "Files", 
               icon = icon("file-excel")),
      menuItem(text = "Lipidomics", 
               tabName = "Lipidomics", 
               icon = icon("bar-chart-o")),
      menuItem(text = "Proteomics", 
               tabName = "Proteomics", 
               icon = icon("bar-chart-o"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    
    tabItems(
      tabItem(
        tabName = "Files",
        h2("Files"),
        p("Load files here")
      ), # end files
      
      tabItem(
        tabName = "Lipidomics",
        fluidRow(
          # buttons to show/hide boxes
          checkboxGroupButtons(inputId = "showPlots",
                               label = "",
                               choices = c("Histogram" = "histogram",
                                           "Histogram with controls" = "histogram2"),
                               selected = "histogram",
                               status = "info",
                               justified = TRUE,
                               width = "60%")
        ),
        fluidRow(
          jqui_sortable(
            div(
              id = "movePlots",
              box(
                id = "histogram",
                title = "Histogram",
                solidHeader = TRUE,
                status = "primary",
                plotOutput(outputId = "plot1")
              ),
              box(
                id = "histogram2",
                title = "Histogram with controls",
                solidHeader = TRUE,
                status = "primary",
                collapsible = TRUE,
                plotOutput(outputId = "plot2"),
                sidebar = boxSidebar(
                  id = "hist_sidebar",
                  width = 40,
                  sliderInput(inputId = "slider",
                              label = "Number of observations:",
                              min = 1,
                              max = 100,
                              value = 50)
                )
              )
            )
          ) # end jqui_sortable
        )
      ), # end lipidomics
      
      tabItem(
        tabName = "Proteomics",
        fluidRow(
          box(
            title = "Scatter plot",
            solidHeader = TRUE,
            plotOutput(outputId = "plot3", 
                       height = 250)
          ),
        )
      ) # end proteomics
      
    ) # end tabitems
  )
)

server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)
  
  # toggle hide/show some boxes
  # this can also be done with actionbuttons and shinydashboardplus
  observe({
    shinyjs::toggle(
      id = "histogram",
      condition = "histogram" %in% input$showPlots
    )
    
    shinyjs::toggle(
      id = "histogram2",
      condition = "histogram2" %in% input$showPlots
    )
  })
  
  # histogram
  output$plot1 <- renderPlot({
    hist(histdata)
  })
  
  output$plot2 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  # scatterplot
  output$plot3 <- renderPlot({
    plot(x = 1:length(histdata), 
         y = histdata)
  })
}


# run everything
shinyApp(ui = ui, 
         server = server,
         options = list("launch.browser" = TRUE))