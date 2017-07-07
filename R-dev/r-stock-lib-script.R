library("quantmod")
library("RCurl")
library("XML")
library(stringr)
source("R/methods.R")

get_stock_list <- function(cata = 'TW') {
  index_src <- "StockList"
  index_src <- paste(index_src, sep="-", cata)
  index_src <- paste(index_src, sep = "", ".csv")
  src_pth <- paste("info-dev/", sep = "",index_src)
  idx <- read.csv(src_pth)[,c(1, 2)]
  colnames(idx) = c("No", "Name")
  idx$No <- str_extract(idx$No, "[0-9]+")
  idx
}

get_stock_hist <- function(stock_id, month = 8, years = 0) {
  diff_year = years
  diff_month = diff_year*12 + 8
  ed_date = Sys.time()
  # get stock history
  st_date <- format((ed_date - as.difftime(diff_month*30, unit = "days")),"%Y-%m-%d")
  # print(st_date)
  # print(ed_date)
  # stock_hist = suppressWarnings(getSymbols(stock_id, auto.assign = FALSE, from = st_date))

  stock_hist = tryCatch({
    hist <- suppressWarnings(getSymbols(stock_id, auto.assign = FALSE, from = st_date))
    return(na.omit(hist))
  } , error = function(err) {
    print(paste("MY_ERROR:  ",err))
    return(NA)
  } , final = {
  })
  # return(na.omit(stock_hist))
  return(stock_hist)
}

GET_VOLUME_POS_EXT <-function(sdata)
{
  if (20 > NROW(sdata))
    return(NA)

  vol5_pos <- runMean(sdata[,5], n = 5)
  vol10_pos <- runMean(sdata[,5], n = 10)
  vol20_pos <- runMean(sdata[,5], n = 20)

  abs_vol_pos <- ifelse(sdata[,5] > vol10_pos, 1, 0)
  vol5_pos <- vol5_pos > vol20_pos

  return(abs_vol_pos & vol5_pos)
}

GET_MA_POS_EXT <- function(sdata, tgt = 60, ref = 120)
{
  if (ref > NROW(sdata))
    return(NA)

  tgt_ma <- runMean(sdata[,4], n = tgt)
  ref_ma <- runMean(sdata[,4], n = ref)
  ma_pos <- tgt_ma > ref_ma
  return(ma_pos)
}

# return hold position with array of TRUE FALSE corresponding to date
pick_strategy <- function(stock_name, cata = 'TW'
                           , hist = NULL, n = 22, sd = 1.5
                           , month = 0, years = 1, draw = FALSE, pick = FALSE) {
  if (is.null(hist)) {
    if (!(cata == ''))
      stock_name <- paste(stock_name, sep = ".", cata)
    hist <- get_stock_hist(stock_name, month, years)
    print(stock_name)
    if (is.null(colnames(hist))||is.na(hist)) {
      print("WARN: cannot download stock")
      return(NA)
    }
    # print(hist)
  }

  hlc <- hist[, -c(1,5,6)]
  # print(hlc)
  if (nrow(hist)<n) {
    print("> ERROR:: not enough data to calculate SMA")
    return(NA)
  }

  bb_data <- na.omit(BBands(na.omit(hlc), maType = 'SMA', n, sd))

  if (is.null(colnames(bb_data)))
    return(NA)

  # print(bb_data)
  #1. b-band
  bb_hold <- na.omit(Cl(hist)>bb_data$"up")
  #2. ma x > ma y
  ma_hold <- GET_MA_POS_EXT(hist, tgt = 20, ref = 60)
  #3. volume - 5 ave. > 20 ave. buy & keep, otherwise, sell
  vol_hold <- GET_VOLUME_POS_EXT(hist)

  if (is.null(colnames(bb_hold))||is.null(ma_hold)||is.null(vol_hold))
    return(NA)

  hold <- bb_hold & ma_hold & vol_hold
  # print(hold) # debug for holding point
  pos_hold <- ifelse(hold, 1, 0)

  if(isTRUE(pick)) {
    # calculate the bband last day (up-dn)/mavg
    last_bb <- tail(bb_data, n=1)
    range <- as.numeric((last_bb$'up' - last_bb$'dn')/last_bb$'mavg')
    # holding position
    last_2pos <- as.numeric(tail(hold, n=2))
    # volumn of last day for the stock
    vol <- as.numeric(tail(Vo(hist), n=1))/1000
    # calculate the profit
    buy <- Lag(pos_hold)
    buy <- ifelse(buy, 1, 0)
    # dbg usage
    if (0) {
      # print(buy)
      print("nrow(buy)")
      print(nrow(buy))
      print("nrow(hist)")
      print(nrow(hist))
    }
    prof <- OpOp(hist)*buy
    # print(ret)
    eq <- exp(cumsum(na.omit(prof)))
    prof <- tail(eq, n=1) - 1
    # summarize
    res <- data.frame(matrix(c(range, last_2pos[1], last_2pos[2], vol, prof),nrow=1,ncol=5))
    colnames(res) <- c("var","T-1", "T", "Vo", "Prof")
    rownames(res) <- stock_name
    # eq3[with(eq3, order(var)), ] # order with column var
    return(res)
  } else if (isTRUE(draw)) {
    show_month = 4
    show_ed_date = Sys.time()
    # get stock history
    show_st_date <- format((show_ed_date - as.difftime(show_month*30, unit = "days")),"%Y-%m")
    show_ed_date <- format(show_ed_date, "%Y-%m")
    show_period <- paste(show_st_date,sep = "::", show_ed_date)
    chartSeries(hist[show_period], name = stock_name, up.col = 'red', dn.col = 'green')
    plot(addBBands(n = 22, sd = 1.5, maType = "SMA", draw = 'bands', on = 1), legend = NULL)
    if (10 < nrow(hist)) {
      ma_10<-runMean(hist[,4],n=10)
      plot(addTA(ma_10[show_period], on=1, col="blue", legend = NA))
    }
    if (60 < nrow(hist)) {
      ma_60<-runMean(hist[,4],n=60)
      plot(addTA(ma_60[show_period],on=1,col="yellow", legend = NULL))
    }
    if (120 < nrow(hist)) {
      ma_120<-runMean(hist[,4],n=120)
      plot(addTA(ma_120[show_period],on=1,col=8, legend = NULL))
    }
    twii_hist <- get_stock_hist('^TWII', month, years)
    plot(addTA(Cl(twii_hist[show_period]), on=NA, col="red", legend = NA))
    hold_price <- max(Cl(hist))*Lag(pos_hold)
    # print(hold_price)
    plot(addTA(hold_price[show_period],on=1,col="grey"))
  }
  return(pos_hold)
}

performance_analysis <- function(stock_entry, cata = 'TW', month = 8, years = 0, draw = FALSE) {
  stock_id = stock_entry
  stock_id <- paste(stock_id, sep = ".", cata)
  print(stock_id)

  stock_hist <- get_stock_hist(stock_id, month, years)
  # print(stock_hist)

  # calculate the performance by bbnad sd = 1.5
  hold <- pick_strategy(stock_id, hist = stock_hist, draw = draw)

  if (is.na(hold) || nrow(hold)<1) {
    print("> ERROR:: hold is null return fail")
    return(c(stock_id, NA))
  }
  # print("hold")
  # print(hold)

  buy <- Lag(hold)
  # print("buy")
  # print(buy)

  # if (is.null(colnames(hold)))
    # return(c(stock_id, NA))

  pos <- ifelse(buy, 1, 0)
  # dbg usage
  if (0) {
    # print(pos)
    print("nrow(pos)")
    print(nrow(pos))
    print("nrow(stock_hist)")
    print(nrow(stock_hist))
  }
  ret <- OpOp(stock_hist)*pos
  # print(ret)

  eq <- exp(cumsum(na.omit(ret)))
  # print(eq)
  return(c(stock_id, tail(eq, n=1)-1))
}



