source("R/r-stock-lib-script.R")
source("R/load-stock-index.R")
#stock_name = "2337.TW"
stock_name = "6147.TWO"
diff_year = 0
diff_month = diff_year*12 + 8
ed_date = Sys.time()
st_date <- format((ed_date - as.difftime(diff_month*30, unit = "days")),"%Y-%m-%d")
stock_hist = na.omit(getSymbols(stock_name, auto.assign = FALSE, from = st_date))
pos <- bband_strat(stock_name ,stock_hist)
ret <- OpCl(stock_hist)*pos
eq <- exp(cumsum(na.omit(ret)))

#plot(eq)

### print chart
#chartSeries(stock_hist)
#addBBands(n = 22, sd = 1.5, maType = "SMA", draw = 'bands', on = 1)
