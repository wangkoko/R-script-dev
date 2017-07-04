
library("quantmod")
library("RCurl")
library("XML")
library(stringr)
library("readr")

# return hold position with array of TRUE FALSE corresponding to date
bband_strat <- function(stock_name, hist, n = 22, sd = 1.5) {
  hlc <- hist[, -c(1,5,6)]
  bb_data <- na.omit(BBands(na.omit(hlc), maType = 'SMA', n, sd))
  # print(bb_data)
  # version 4
  # hold stock if previous day close price > bband up
  # @324 discuss depends on 323 we buy at 324 then at 324 it fall through up,
  # then we sell it on 325 OPEN
  buy <- na.omit(Lag(Cl(hist))>Lag(bb_data$"up"))
  #print(buy) # debug for holding point
  pos <- ifelse(buy, 1, 0)
}
TW_Reduce <-c(27, 575, 594, 629, 708)
get_stock_list <- function(cata = 'TW') {
  index_src <- "StockList"
  index_src <- paste(index_src, sep="-", cata)
  index_src <- paste(index_src, sep = "", ".csv")
  src_pth <- paste("info/", sep = "",index_src)
  idx <- read_csv(src_pth)[,c(1, 2)]
  colnames(idx) = c("No", "Name")
  idx$No <- str_extract(idx$No, "[0-9]+")
  idx#[-TW_Reduce, ]
}




bband_for_single_stock <- function(stock_entry, cata = 'TW', month = 8, years = 0, n = 22, sd = 1.5) {
  print(stock_entry)
  stock_id = stock_entry
  stock_id <- paste(stock_id, sep = ".", cata)
  diff_year = 0
  diff_month = diff_year*12 + 8
  ed_date = Sys.time()
  # get stock history
  st_date <- format((ed_date - as.difftime(diff_month*30, unit = "days")),"%Y-%m-%d")
  stock_hist = getSymbols(stock_id, auto.assign = FALSE, from = st_date)
  # print(stock_hist)
  stock_hist = na.omit(stock_hist)
  if (nrow(stock_hist)<n)
    return(c(stock_id, NA, NA))
  # print(stock_hist)
  #print(st_date)
  #print(ed_date)
  # calculate the performance of bbnad sd = 1.5
  chartSeries(stock_hist)
  hlc <- stock_hist[, -c(1,5,6)]
  # print(hlc)
  bb_data <- na.omit(BBands(na.omit(hlc), maType = 'SMA', n, sd))
  # print(bb_data)
  buy <- na.omit(Lag(Cl(stock_hist))>Lag(bb_data$"up"))
  pos <- ifelse(buy, 1, 0)
  # print(pos)
  ret <- OpCl(stock_hist)*pos
  # print(ret)
  eq <- exp(cumsum(na.omit(ret)))
  return(stock_hist)
}
bband_analysis <- function(stock_entry, cata = 'TW', month = 8, years = 0, n = 22, sd = 1.5) {
  print(stock_entry)
  stock_id = stock_entry
  stock_id <- paste(stock_id, sep = ".", cata)
  diff_year = 0
  diff_month = diff_year*12 + 8
  ed_date = Sys.time()
  # get stock history
  st_date <- format((ed_date - as.difftime(diff_month*30, unit = "days")),"%Y-%m-%d")
  stock_hist = na.omit(getSymbols(stock_id, auto.assign = FALSE, from = st_date))
  if (nrow(stock_hist)<n)
    return(c(stock_id, NA, NA))
  # print(stock_hist)
  #print(st_date)
  #print(ed_date)
  # calculate the performance of bbnad sd = 1.5
  # pos <- bband_strat(stock_id, stock_hist)
  hlc <- stock_hist[, -c(1,5,6)]
  # print(hlc)
  bb_data <- na.omit(BBands(na.omit(hlc), maType = 'SMA', n, sd))
  # print(bb_data)
  if (is.null(colnames(bb_data)))
    return(c(stock_id, NA, NA))
  buy <- na.omit(Lag(Cl(stock_hist))>Lag(bb_data$"up"))
  pos <- ifelse(buy, 1, 0)
  ret <- OpCl(stock_hist)*pos
  eq <- exp(cumsum(na.omit(ret)))
  last_eq <- as.numeric(tail(eq, n=1))
  # print(last_eq)

  # calculate the bband last day (up-dn)/close
  last_bb <- tail(bb_data, n=1)
  last_hist <- tail(stock_hist, n=1)
  bb_range <- last_bb$'up' - last_bb$'dn'
  bumpy_range <- as.numeric(bb_range/Cl(stock_hist))
  # print(bumpy_range)
  result <- c(stock_id, bumpy_range, last_eq-1)
  # print(result)
  return(result)
}



