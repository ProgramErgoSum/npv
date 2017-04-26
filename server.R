library(shiny)
source("npd-c.R")
# Define server logic
getConstants <- function(input) {
  C <- data.frame(
    3,
    100,
    0.0,
    0.05,
    input$outlay,
    input$hurdleRate,
    input$salesRange[1],
    input$salesRange[2],
    input$salesMode,
    input$demDeclMean,
    input$demDeclSd,
    input$varCostMean,
    input$varCostSd,
    input$fixedCostRange[1],
    input$fixedCostRange[2]
  )
  names(C) <-
    c(
      "DIGITS",
      "PRICE",
      "NPV_BREAK_EVEN_VALUE",
      "NPV_BREAK_EVEN_WORST_ODDS",
      "OUTLAY",
      "HURDLE_RATE",
      "SALES_TRIANG_MIN",
      "SALES_TRIANG_MAX",
      "SALES_TRIANG_MODE",
      "DEM_DECL_FACTOR_MEAN",
      "DEM_DECL_FACTOR_SD",
      "VAR_COST_RATE_MEAN",
      "VAR_COST_RATE_SD",
      "FIX_COST_RATE_MIN",
      "FIX_COST_RATE_MAX"
    )
  
  return(C)
}
#
npvOddsPlot <- function(input) {
  C <- getConstants(input)
  threshold <- -1
  g <- npvPlot(C,threshold)
  return (g)
}
#
npvOddsTable <- function(input) {
  C <- getConstants(input)
  df <- npvTable(C)
  return(df)
}
#
shinyServer(function(input, output) {
  output$npdPlot <- renderPlot(npvOddsPlot(input))
  output$npdTable <- renderTable(npvOddsTable(input))
})