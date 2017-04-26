library(shiny)
#
shinyUI(fluidPage(
  titlePanel("New Product Introdction Demo"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("outlay", 
                  label = "Initial Outlay (million)",
                  min = 0.1, max = 10, value = 0.5),
      sliderInput("hurdleRate", 
                  label = "Hurdle Rate",
                  min = 0.01, max = 1, value = 0.1),
      sliderInput("salesRange", 
                  label = "Range of sales",
                  min = 100, max = 50000, value = c(500, 24000)),
      sliderInput("salesMode", 
                  label = "Mode of sales",
                  min = 1000, max = 40000, value = 6000),
      sliderInput("demDeclMean", 
                  label = "Demand declining factor - Mean",
                  min = 0.0, max = 1.0, value = 0.9),
      sliderInput("demDeclSd", 
                  label = "Demand declining factor - SD",
                  min = 0.0, max = 1.0, value = 0.1),
      sliderInput("fixedCostRange", 
                  label = "Fixed Costs - Min and Max",
                  min = 30000, max = 50000, value = c(36000, 44000)),
      sliderInput("varCostMean", 
                  label = "Variable Costs - Mean",
                  min = 10, max = 50, value = 20),
      sliderInput("varCostSd", 
                  label = "Variable Costs - SD",
                  min = 1, max = 10, value = 3)
      ),
    mainPanel(
      plotOutput("npdPlot"),
      tableOutput("npdTable")
    )
  )
))