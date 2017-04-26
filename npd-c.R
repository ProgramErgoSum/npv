library(FinCal)
library(dplyr)
library(triangle)
library(ggplot2)
#
SEED <- 2017
ITERATIONS <- 5000
MILLION <- 1000000
#
salesIter <- function(C, threshold = -1) {
  y1 <-
    rtriangle(ITERATIONS,
              C$SALES_TRIANG_MIN,
              C$SALES_TRIANG_MAX,
              C$SALES_TRIANG_MODE)
  demDecl <-
    rnorm(
      ITERATIONS,
      mean = C$DEM_DECL_FACTOR_MEAN,
      sd = C$DEM_DECL_FACTOR_SD
    )
  
  yr1Demand <- round(y1, C$DIGITS)
  y2 <- round(yr1Demand * demDecl, C$DIGITS)
  yr2Demand <- ifelse(y2 <= threshold, 0.0, y2)
  yr3Demand <- round(yr2Demand * demDecl, C$DIGITS)
  yr4Demand <- round(yr3Demand * demDecl, C$DIGITS)
  
  sales <-
    data.frame(yr1Demand,
               yr2Demand,
               yr3Demand,
               yr4Demand)
  names(sales) <- c("Y1Sales", "Y2Sales", "Y3Sales", "Y4Sales")
  
  return (sales)
}
#
revenueIter <- function(C, sales) {
  revenue <-
    data.frame(
      round(sales$Y1Sales * C$PRICE, C$DIGITS),
      round(sales$Y2Sales * C$PRICE, C$DIGITS),
      round(sales$Y3Sales * C$PRICE, C$DIGITS),
      round(sales$Y4Sales * C$PRICE, C$DIGITS)
    )
  names(revenue) <-
    c("Y1Revenue", "Y2Revenue", "Y3Revenue", "Y4Revenue")
  
  return (revenue)
}
#
varCostIter <- function(C, sales) {
  varCostRate <-
    rnorm(ITERATIONS,
          mean = C$VAR_COST_RATE_MEAN,
          sd = C$VAR_COST_RATE_SD)
  varCost <-
    data.frame(
      round(sales$Y1Sales * varCostRate, C$DIGITS),
      round(sales$Y2Sales * varCostRate, C$DIGITS),
      round(sales$Y3Sales * varCostRate, C$DIGITS),
      round(sales$Y4Sales * varCostRate, C$DIGITS)
    )
  names(varCost) <-
    c("Y1VarCost", "Y2VarCost", "Y3VarCost", "Y4VarCost")
  
  return (varCost)
}
#
fixedCostIter <- function(C) {
  fixedCost <-
    runif(ITERATIONS,
          min = C$FIX_COST_RATE_MIN,
          max = C$FIX_COST_RATE_MAX)
  
  return (fixedCost)
}
#
cashFlow <- function(C, revenue, varCost, fixedCost) {
  cashFlow <-
    data.frame(
      revenue$Y1Revenue - varCost$Y1VarCost - fixedCost,
      revenue$Y2Revenue - varCost$Y2VarCost - fixedCost,
      revenue$Y3Revenue - varCost$Y3VarCost - fixedCost,
      revenue$Y4Revenue - varCost$Y4VarCost - fixedCost
    )
  names(cashFlow) <- c("Y1", "Y2", "Y3", "Y4")
  
  return (cashFlow)
}
#
cashFlowNpv <- function(C, cashFlow) {
  n <- NULL
  outlay <- -1 * C$OUTLAY * MILLION
  j <- ITERATIONS
  for (i in c(1:j)) {
    j <- cashFlow[i:i, ]
    yearlyCashFlow <- c(outlay, j$Y1, j$Y2, j$Y3, j$Y4)
    n <-
      append(n, round(npv(
        r = C$HURDLE_RATE, cf = yearlyCashFlow
      ), C$DIGITS))
  }
  cashFlowNpv <- mutate(cashFlow, npvY0 = n)
  
  return (cashFlowNpv)
}
#
salesCashFlowNpv <- function(C, sales, cfNpv) {
  salesCashFlowNpv <- data.frame(
    Y1Sales = sales$Y1Sales,
    Y1CashFlow = cfNpv$Y1,
    Y2Sales = sales$Y2Sales,
    Y2CashFlow = cfNpv$Y2,
    Y3Sales = sales$Y3Sales,
    Y3CashFlow = cfNpv$Y3,
    Y4Sales = sales$Y4Sales,
    Y4CashFlow = cfNpv$Y4,
    NPV = cfNpv$npvY0
  )
  
  return(salesCashFlowNpv)
}
#
npvCdf <- function(n) {
  N <- sort(n)
  P <- ecdf(N)
  return(P)
}
#
getBreakEven <- function(C, n) {
  x <- filter(n, NPV >= 0)
  breakEvenNpv = min(x$NPV)
  breakEvenSales = round(select(filter(x, round(NPV, C$DIGITS) == breakEvenNpv),
                                Y1Sales)$Y1Sales, 0)
  
  return(breakEvenSales)
}
#
npdDistribution <- function(C, m) {
  N <- m$NPV / MILLION
  P <- npvCdf(N)
  #
  # NPV distribution curve
  n <- sort(N)
  p <- P(n) * 100
  df <- data.frame(npv = n, odds = p)
  
  return(df)
}
#
pointsOnPlot <- function(C, m) {
  # Points of interest
  s <- getBreakEven(C, m)
  o <- C$NPV_BREAK_EVEN_WORST_ODDS
  w <- o * 100
  n <- m$NPV / MILLION
  n <- sort(n)
  q <- round((quantile(n, o)), C$DIGITS)
  e <- C$NPV_BREAK_EVEN_VALUE
  b <- npvCdf(m$NPV / MILLION)(0) * 100
  
  p <- data.frame(s, o, w, q, e, b)
  names(p) <- c("s", "o", "w", "q", "e", "b")
  
  return(p)
}
#
makePlot <- function(d, p) {
  # Labels
  npvOdds <- paste("Odds of break-even : ", p$b, "%")
  salesThresh <- paste("Sales threshold : ", p$s)
  worstCase <-
    paste("Worst case (@ 5% odds) : ", p$q, "million")
  #
  # Make plot
  #
  g <- ggplot(d, aes(x = d$npv, y = d$odds)) +
    geom_line(colour = "blue") +
    labs(title = "Net Present Value (NPV) and Odds") +
    labs(x = "NPV (million)") +
    labs(y = "Percent (%)") +
    geom_vline(xintercept = p$e,
               colour = "red",
               linetype = "longdash") +
    geom_hline(yintercept = p$b,
               colour = "green",
               linetype = "longdash") +
    geom_vline(xintercept = p$q,
               colour = "green",
               linetype = "dotdash") +
    geom_hline(yintercept = p$w,
               colour = "red",
               linetype = "dotdash")
  return (g)
}
#
buildOutputTable <- function(C, p) {
  outputTable <-
    data.frame(C$OUTLAY, C$HURDLE_RATE * 100, p$b, p$s, p$q)
  names(outputTable) <-
    c("Outlay (mn)",
      "Hurdle Rate (%)",
      "BE Odds (%)",
      "BE Sales",
      "WC Loss (mn)")
  
  return(outputTable)
}
#
npvCalc <- function(C, threshold = -1) {
  s <- salesIter(C, threshold)
  r <- revenueIter(C, s)
  v <- varCostIter(C, s)
  f <- fixedCostIter(C)
  c <- cashFlow(C, r, v, f)
  n <- cashFlowNpv(C, c)
  t <- salesCashFlowNpv(C, s, n)
  
  return (t)
}
#
npvPlot <- function(C, threshold = -1) {
  set.seed(C$SEED)
  n <- npvCalc(C, threshold)
  d <- npdDistribution(C, n)
  p <- pointsOnPlot(C, n)
  g <- makePlot(d, p)
  
  return(g)
}
#
npvTable <- function(C, threshold=-1) {
  set.seed(C$SEED)
  n <- npvCalc(C, threshold)
  d <- npdDistribution(C, n)
  p <- pointsOnPlot(C, n)
  t <- buildOutputTable(C, p)
  
  return(t)
}