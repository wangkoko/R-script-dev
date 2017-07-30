library("quantmod")
library("RCurl")
library("XML")
library(stringr)
source("R/methods.R")
require(lubridate)

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

get_stock_hist <- function(stock_id, month = 8, years = 0, ed = Sys.time()) {
  diff_year = years
  diff_month = diff_year*12 + month
  ed_date = as.Date(ed)
  # get stock history
  st_date <- format((ed_date - as.difftime(diff_month*30, unit = "days")),"%Y-%m-%d")
  day(ed_date) <- day(ed_date) + 1
  ed_date <- format(ed_date, "%Y-%m-%d")
  # print(paste(st_date, sep = " to ", ed_date))
  # print(c(stock_id, st_date, ed_date))
  # stock_hist = suppressWarnings(getSymbols(stock_id, auto.assign = FALSE, from = st_date))

  stock_hist = tryCatch({
    hist <- suppressWarnings(getSymbols(stock_id, auto.assign = FALSE, from = st_date, to = ed_date))
    # hist <- getSymbols(stock_id, auto.assign = FALSE, from = st_date, to = ed_date)
    # print(hist)
    return(na.omit(hist))
  } , error = function(err) {
    print(paste("MY_ERROR:  ",err))
    return(NA)
  } , final = {
  })
  # return(na.omit(stock_hist))
  return(stock_hist)
}

# return test period for quantmod
get_test_period <- function(month=4, years=0, from = Sys.time())
{
  test_month = years*12 + month
  test_ed_date = as.Date(from)
  # get stock history
  test_st_date <- format((test_ed_date - as.difftime(test_month*30, unit = "days")),"%Y-%m")
  test_ed_date <- format(test_ed_date, "%Y-%m")
  test_period <- paste(test_st_date,sep = "::", test_ed_date)
  return(test_period)
}
# show the pos in your strategy
pos_plot <- function(stock_name, cata = 'TW', month = 4, years = 0, from = Sys.time(), n = 22, sd = 1.5, FUNC = pick_strategy, ...)
{
  if (cata == '')
    stock_idx <- stock_name
  else
    stock_idx <- paste(stock_name, sep = ".", cata)
  hist <- get_stock_hist(stock_idx, month+4, years)
  show_period <- get_test_period(month = month, years = years, from = from)
  # print(show_period)
  # print(hist[show_period])
  # print(hist)
  pos_hold <- FUNC(stock_name, hist = hist, month = month, years = years, from = from, n = n, sd = sd, ...)
  # print(pos_hold)

  chartSeries(hist[show_period], name = stock_idx, up.col = 'red', dn.col = 'green')
  plot(addBBands(n = n, sd = sd, maType = "SMA", draw = 'bands', on = 1), legend = NULL)
  if (5 < nrow(hist)) {
    ma_10<-runMean(Cl(hist),n=5)
    plot(addTA(ma_10[show_period], on=1, col= 7, legend = NA))
  }

  if (10 < nrow(hist)) {
    ma_10<-runMean(Cl(hist),n=10)
    plot(addTA(ma_10[show_period], on=1, col= 6, legend = NA))
  }

  if (20 < nrow(hist)) {
    ma_20<-runMean(Cl(hist),n=20)
    plot(addTA(ma_20[show_period], on=1, col= 2, legend = NA))
  }

  if (60 < nrow(hist)) {
    ma_60<-runMean(Cl(hist),n=60)
    plot(addTA(ma_60[show_period],on=1,col= 3, legend = NULL))
  }

  if (120 < nrow(hist)) {
    ma_120<-runMean(Cl(hist),n=120)
    plot(addTA(ma_120[show_period],on=1,col=4, legend = NULL))
  }
  hold_price <- max(Cl(hist[show_period]))*Lag(pos_hold)
  # print(hold_price)
  plot(addTA(hold_price[show_period],on=1,col=5))
  invisible(readline(prompt="Press [enter] to continue"))
}
# buy add sell
pick_strategy5 <- function(stock_name, cata = 'TW'
                           , hist = NULL, n = 22, sd = 1.5
                           , month = 0, years = 1, from = Sys.time(), pick = FALSE, prof_verify=FALSE) {
  if (is.null(hist)) {
    if (!(cata == ''))
      stock_name <- paste(stock_name, sep = ".", cata)
    hist <- get_stock_hist(stock_name, month+6, years, ed = from)
    # print(stock_name)
    if (is.null(colnames(hist))||is.na(hist)) {
      print("WARN: cannot download stock")
      return(NA)
    }
    # print(hist)
  }

  test_period <- get_test_period(month = month, years = years, from = from)
  # print(test_period)
  hlc <- cbind(Hi(hist), Lo(hist), Cl(hist))

  # need to use 120 ma in rising
  if (nrow((hist)) < 120) {
    print(paste("> ERROR:: not enough data to calculate SMA", sep = ' ', nrow(hist)))
    return(NA)
  }
  # 0. prepare data for later calculation
  bb_data <- BBands(hlc, maType = 'SMA', n = n, sd = sd)
  ma5 <- runMean(Cl(hist), n = 5)
  ma10 <- runMean(Cl(hist), n = 10)
  ma20 <- runMean(Cl(hist), n = 20)
  ma60 <- runMean(Cl(hist), n = 60)
  ma120 <- runMean(Cl(hist), n = 120)

  # criteria 1. get desired bband range
  bb_var <- (bb_data$'up' - bb_data$'dn')/bb_data$'mavg'
  bb_var_limit <- ifelse(bb_var <= 0.15, 1, 0)
  bb_var_limit <- na.omit(bb_var_limit)
  # print(bb_var_limit)

  # criteria 2. get desired sma tangling within bbands
  tangle5 <- (ma5<bb_data$'up') & (ma5>bb_data$'dn')
  tangle10 <- (ma10<bb_data$'up') & (ma10>bb_data$'dn')
  tangle20 <- (ma20<bb_data$'up') & (ma20>bb_data$'dn')
  tangle60 <- (ma60<bb_data$'up') & (ma60>bb_data$'dn')
  tangle_close <- (Cl(hist)<bb_data$'up') & (Cl(hist)>bb_data$'mavg')
  ma_limit <- tangle_close[test_period] & tangle5[test_period] & tangle10[test_period] & tangle20[test_period]# & tangle60[test_period]
  ma_limit <- na.omit(ma_limit)

  # criteria 3. get rising trend
  rising <- na.omit(ma60 > ma120)

  # 4. get valid period by the longest range 120 ma
  valid_from <- head(index(rising),n=1)
  valid_to <- tail(index(rising),n=1)
  test_period <- paste(valid_from, sep = '::', valid_to)
  print(paste(stock_name, sep = ': ', test_period))

  # 5. check buy_range
  buy_range <- bb_var_limit[test_period] & ma_limit[test_period] & rising[test_period]
  # debug for each criteria
  # buy_range <- bb_var_limit[test_period]
  # buy_range <- ma_limit[test_period]
  # buy_range <- rising[test_period]
  # print(buy_range) # debug for holding point
  # 5-1. check add_range
  # print(tail(Cl(hist), n=10))
  # print(tail(bb_data$"up", n=10))
  add_range <- na.omit(Cl(hist)>bb_data$"up")
  # 6. check hold range
  hold_range <- Cl(hist) > bb_data$"dn"
  hold_range <- hold_range[test_period]
  # print(hold_range)

  # 7. check whole period if buy_range happened then hold_range
  total_range <- buy_range
  bought <- FALSE
  if (isTRUE(prof_verify)) {
    for (idx in index(hold_range)) {
      idx <- format(as.Date(idx), "%Y-%m-%d")
      if (total_range[idx] == TRUE)
        bought <- TRUE
      if (isTRUE(bought)) {
        total_range[idx] <- buy_range[idx] | hold_range[idx] | add_range[idx]
        if (total_range[idx] <= 0)
          bought <- FALSE
      }
    }
  }
  pos_hold <- ifelse(total_range, 1, 0)
  # print(pos_hold)
  if(isTRUE(pick)) {
    # holding position
    last_2pos <- as.numeric(tail(pos_hold, n=2))
    last_2add <- as.numeric(tail(add_range, n=2))
    last_buy <- as.numeric(tail(buy_range, n=1))
    # print(tail(add_range,n=10))
    # volumn of last day for the stock
    vol <- (as.numeric(tail(Vo(hist), n=1))/1000)*tail(Cl(hist), n=1)
    # calculate the profit
    # print(tail(pos_hold, n=10))
    # 8. to align the correct gain/loss when we can actually made decision to buy/sell
    buy <- na.omit(Lag(Lag(pos_hold)))
    # print(tail(buy, n=10))
    buy <- ifelse(buy, 1, 0)
    op_hist <- na.omit(Op(hist))
    colnames(op_hist) <- "Open"
    # print(tail(op_hist,n=10))
    prof_persent <- OpOp(op_hist)[test_period]
    # prof_persent <- na.omit(Lag(prof_persent))[test_period]
    # print(tail(prof_persent,n=10))
    prof <- prof_persent*buy
    eq <- exp(cumsum(na.omit(prof)))
    prof <- tail(eq, n=1) - 1
    # summarize
    range <- as.numeric(tail(bb_var, n=1))
    valid_period <- difftime(valid_to, valid_from, units = 'days')
    hold_days <- sum(pos_hold)
    res <- data.frame(matrix(c(range, last_2pos[2], vol, prof, last_buy, !isTRUE(last_2add[1])&isTRUE(last_2add[2]), hold_days, valid_period), nrow=1, ncol=8))
    colnames(res) <- c("var", "Hold", "Vo", "Prof", "Buy", "Add", "Hold_days", "Period")
    res$'valid_from' <- as.Date(valid_from)
    res$'valid_to' <- as.Date(valid_to)
    rownames(res) <- stock_name
    # eq3[with(eq3, order(var)), ] # order with column var
    return(res)
  }
  return(pos_hold)
}

# with the bband and vol and sma the define when to enter
pick_strategy4 <- function(stock_name, cata = 'TW'
                           , hist = NULL, n = 22, sd = 1.5
                           , month = 0, years = 1, from = Sys.time(), pick = FALSE) {
  if (is.null(hist)) {
    if (!(cata == ''))
      stock_name <- paste(stock_name, sep = ".", cata)
    hist <- get_stock_hist(stock_name, month+6, years, ed = from)
    # print(stock_name)
    if (is.null(colnames(hist))||is.na(hist)) {
      print("WARN: cannot download stock")
      return(NA)
    }
    # print(hist)
  }

  test_period <- get_test_period(month = month, years = years, from = from)
  # print(test_period)
  hlc <- cbind(Hi(hist), Lo(hist), Cl(hist))

  # need to use 60 ma
  if (nrow((hist)) < 120) {
    print(paste("> ERROR:: not enough data to calculate SMA", sep = '', nrow(hist)))
    return(NA)
  }
  # print(tail(hist, n=10))
  #1. b-band
  bb_data <- BBands(hlc, maType = 'SMA', n = n, sd = sd)

  # calculate the bband last day (up-dn)/mavg
  # print(tail(bb_data, n=20))
  bb_var <- (bb_data$'up' - bb_data$'dn')/bb_data$'mavg'
  # print(tail(bb_var, n=20))
  bb_var_limit <- ifelse(bb_var <= 0.10, 1, 0)
  # print(bb_var_limit)

  #2. calulate sma tangling within bbands
  ma5 <- runMean(Cl(hist), n = 5)
  ma10 <- runMean(Cl(hist), n = 10)
  ma20 <- runMean(Cl(hist), n = 20)
  ma60 <- runMean(Cl(hist), n = 60)
  ma120 <- runMean(Cl(hist), n = 120)
  # print(head(hist, n=5))
  # print("...")
  # print(tail(hist, n=5))
  bull <- na.omit(ma60 > ma120)
  # print(head(bull,n=10))
  test_period <- paste(head(index(bull),n=1), sep = '::', tail(index(bull),n=1))
  print(paste(stock_name, sep = ': ', test_period))
  tangle5 <- (ma5<bb_data$'up') & (ma5>bb_data$'dn')
  tangle10 <- (ma10<bb_data$'up') & (ma10>bb_data$'dn')
  tangle20 <- (ma20<bb_data$'up') & (ma20>bb_data$'dn')
  tangle60 <- (ma60<bb_data$'up') & (ma60>bb_data$'dn')
  close <- (Cl(hist)<bb_data$'up') & (Cl(hist)>bb_data$'dn')
  ma_hold <- bull[test_period] & close[test_period] & tangle5[test_period] & tangle10[test_period] & tangle20[test_period] & tangle60[test_period]
  # print(ma_hold)
  ma_hold <- na.omit(ma_hold)
  # check hold
  hold <- bb_var_limit[test_period] & ma_hold[test_period]
  # hold <- bb_var_limit[test_period]
  # hold <- ma_hold[test_period]
  # print(hold) # debug for holding point
  pos_hold <- ifelse(hold, 1, 0)
  # print(tail(pos_hold,n=20))
  if(isTRUE(pick)) {
    # holding position
    last_2pos <- as.numeric(tail(pos_hold, n=2))
    # volumn of last day for the stock
    vol <- (as.numeric(tail(Vo(hist), n=1))/1000)*tail(Cl(hist), n=1)
    prof <- 0
    # summarize
    range <- as.numeric(tail(bb_var, n=1))
    last_ma <- tail(ma_hold, n=1)
    res <- data.frame(matrix(c(range, last_2pos[1], last_2pos[2], vol, prof, last_ma), nrow=1, ncol=6))
    colnames(res) <- c("var", "T-1", "T", "Vo", "Prof", "Last_ma")
    rownames(res) <- stock_name
    # eq3[with(eq3, order(var)), ] # order with column var
    return(res)
  }
  return(pos_hold)
}

# with the bband and vol and sma the define when to enter
pick_strategy3 <- function(stock_name, cata = 'TW'
                           , hist = NULL, n = 22, sd = 1.5
                           , month = 0, years = 1, from = Sys.time(), pick = FALSE) {
  if (is.null(hist)) {
    if (!(cata == ''))
      stock_name <- paste(stock_name, sep = ".", cata)
    hist <- get_stock_hist(stock_name, month+4, years, ed = from)
    print(stock_name)
    if (is.null(colnames(hist))||is.na(hist)) {
      print("WARN: cannot download stock")
      return(NA)
    }
    # print(hist)
  }

  test_period <- get_test_period(month = month, years = years, from = from)
  hlc <- cbind(Hi(hist), Lo(hist), Cl(hist))

  # need to use 60 ma
  if (nrow(hist)< 60) {
    print("> ERROR:: not enough data to calculate SMA")
    return(NA)
  }

  #1. b-band
  bb_data <- na.omit(BBands(hlc, maType = 'SMA', n=n, sd = sd))
  bb_hold <- na.omit(Cl(hist)>bb_data$"up")

  # calculate the bband last day (up-dn)/mavg
  # print(bb_data)
  bb_var <- (bb_data$'up' - bb_data$'dn')/bb_data$'mavg'
  # print(bb_var)
  bb_var_limit <- ifelse(bb_var <= 0.16, 1, 0)

  # print(bb_var_limit)
  #2. calulate sma tangling within bbands
  ma20 <- runMean(Cl(hist), n = 22)
  ma60 <- runMean(Cl(hist), n = 60)

  tangle20 <- (ma20<bb_data$'up') & (ma20>bb_data$'dn')
  tangle60 <- (ma60<bb_data$'up') & (ma60>bb_data$'dn')
  close <- (Cl(hist)<bb_data$'up') & (Cl(hist)>bb_data$'mavg')
  bull <- ma20 > ma60
  ma_hold <- bull[test_period] & close[test_period] & tangle60[test_period]
  #3. volumn increase suddenly for several days
  vol_pos <- Vo(hist[test_period]) > 500000 # > 500 * 1000
  vol_hold <- vol_pos[test_period]
  # print(Vo(hist))
  # print(vol_hold)
  if (is.null(vol_hold))
    return(NA)

  # check hold
  # hold <- bb_hold|(bb_var_limit[test_period] & ma_hold[test_period] & vol_hold[test_period])
  hold <- bb_var_limit[test_period] & ma_hold[test_period] & vol_hold[test_period]
  # hold <- bb_var_limit[test_period]
  # hold <- ma_hold[test_period] & vol_hold[test_period]
  # hold <- vol_hold[test_period]
  # print(hold) # debug for holding point
  pos_hold <- ifelse(hold, 1, 0)

  if(isTRUE(pick)) {
    # holding position
    last_2pos <- as.numeric(tail(pos_hold, n=2))
    # volumn of last day for the stock
    vol <- as.numeric(tail(Vo(hist), n=1))/1000
    prof <- 0
    # summarize
    range <- as.numeric(tail(bb_var, n=1))
    last_ma <- tail(ma_hold, n=1)
    last_vo <- tail(vol_hold, n=1)
    res <- data.frame(matrix(c(range, last_2pos[1], last_2pos[2], vol, prof, last_ma, last_vo), nrow=1, ncol=7))
    colnames(res) <- c("var", "T-1", "T", "Vo", "Prof", "Last_ma", "Last_vo")
    rownames(res) <- stock_name
    # eq3[with(eq3, order(var)), ] # order with column var
    return(res)
  }
  return(pos_hold)
}

# 5, 10, 20, 60日均線糾結, cl 在bband mavg上
pick_strategy2 <- function(stock_name, cata = 'TW'
                          , hist = NULL, n = 22, sd = 1.5
                          , month = 0, years = 1, from = Sys.time(), pick = FALSE) {
  if (is.null(hist)) {
    if (!(cata == ''))
      stock_name <- paste(stock_name, sep = ".", cata)
    hist <- get_stock_hist(stock_name, month+4, years, ed = from)
    print(stock_name)
    if (is.null(colnames(hist))||is.na(hist)) {
      print("WARN: cannot download stock")
      return(NA)
    }
    # print(hist)
  }

  test_period <- get_test_period(month = month, years = years, from = from)
  hlc <- cbind(Hi(hist), Lo(hist), Cl(hist))

  # need to use 60 ma
  if (nrow(hist)< 60) {
    print("> ERROR:: not enough data to calculate SMA")
    return(NA)
  }

  #1. b-band
  bb_data <- na.omit(BBands(na.omit(hlc), maType = 'SMA', n, sd))
  bb_hold <- na.omit(Cl(hist)>bb_data$"up")

  # calculate the bband last day (up-dn)/mavg
  bb_var <- (bb_data$'up' - bb_data$'dn')/bb_data$'mavg'
  bb_var_limit <- ifelse(bb_var <= 0.12, 1, 0)

  #2. calulate sma tangling within bbands
  ma5 <- runMean(Cl(hist), n = 5)
  ma10 <- runMean(Cl(hist), n = 10)
  ma20 <- runMean(Cl(hist), n = 20)
  ma60 <- runMean(Cl(hist), n = 60)

  tangle5 <- (ma5<bb_data$'up') & (ma5>bb_data$'dn')
  tangle10 <- (ma10<bb_data$'up') & (ma10>bb_data$'dn')
  tangle20 <- (ma20<bb_data$'up') & (ma20>bb_data$'dn')
  tangle60 <- (ma60<bb_data$'up') & (ma60>bb_data$'dn')
  close <- (Cl(hist)<bb_data$'up') & (Cl(hist)>bb_data$'dn')
  ma_hold <- tangle10[test_period] & tangle20[test_period] & tangle5[test_period] & close[test_period] & tangle60[test_period]
  #3. volumn increase suddenly for several days
  # vol_ma5 <- runMean(Vo(hist), n = 5)
  vol_ma_ref <- runMean(Vo(hist), n = 10)
  # vol_pos <- Vo(hist) > (vol_ma_ref)
  vol_pos <- Vo(hist[test_period]) > 500000 # > 500 * 1000
  vol_ma_pos <- Vo(hist[test_period]) > vol_ma_ref[test_period]
  vol_hold <- vol_pos[test_period] & vol_ma_pos[test_period]
  # print(Vo(hist))
  # print(vol_hold)
  if (is.null(vol_hold))
    return(NA)

  # check hold
  # hold <- bb_hold|(bb_var_limit[test_period] & ma_hold[test_period] & vol_hold[test_period])
  hold <- bb_var_limit[test_period] & ma_hold[test_period]
  # print(hold) # debug for holding point
  pos_hold <- ifelse(hold, 1, 0)

  if(isTRUE(pick)) {
    # holding position
    last_2pos <- as.numeric(tail(pos_hold, n=2))
    # volumn of last day for the stock
    vol <- as.numeric(tail(Vo(hist), n=1))/1000
    # calculate the profit
    buy <- Lag(pos_hold)
    buy <- ifelse(buy, 1, 0)
    prof <- OpOp(hist)*buy
    # print(ret)
    eq <- exp(cumsum(na.omit(prof)))
    prof <- tail(eq, n=1) - 1
    # summarize
    range <- as.numeric(tail(bb_var, n=1))
    last_ma <- tail(ma_hold, n=1)
    last_vo <- tail(vol_hold, n=1)
    res <- data.frame(matrix(c(range, last_2pos[1], last_2pos[2], vol, prof, last_ma, last_vo), nrow=1, ncol=7))
    colnames(res) <- c("var", "T-1", "T", "Vo", "Prof", "Last_ma", "Last_vo")
    rownames(res) <- stock_name
    # eq3[with(eq3, order(var)), ] # order with column var
    return(res)
  }
  return(pos_hold)
}
# 突破bb上軌，ma20>ma60
pick_strategy <- function(stock_name, cata = 'TW'
                           , hist = NULL, n = 22, sd = 1.5
                           , month = 0, years = 1, from = Sys.time(), pick = FALSE) {
  if (is.null(hist)) {
    if (!(cata == ''))
      stock_name <- paste(stock_name, sep = ".", cata)
    hist <- get_stock_hist(stock_name, month+4, years, ed = from)
    # print(stock_name)
    if (is.null(colnames(hist))||is.na(hist)) {
      print("WARN: cannot download stock")
      return(NA)
    }
    # print(hist)
  }
  test_period <- get_test_period(month = month, years = years, from = from)
  hlc <- cbind(Hi(hist), Lo(hist), Cl(hist))
  # print(hlc)
  # for 60 ma
  if (nrow(hist)<60) {
    print("> ERROR:: not enough data to calculate SMA")
    return(NA)
  }
  bb_data <- na.omit(BBands(na.omit(hlc), maType = 'SMA', n, sd))
  bb_hold <- na.omit(Cl(hist)>bb_data$"up")

  #2. ma x > ma y
  ma20 <- runMean(Cl(hist), n = 20)
  ma60 <- runMean(Cl(hist), n = 60)
  ma_hold <- na.omit(ma20 > ma60)

  test_period <- paste(head(index(ma_hold),n=1), sep = '::', tail(index(ma_hold),n=1))
  print(paste(stock_name, sep = ': ', test_period))
  #3. volume - 5 ave. > 20 ave. buy & keep, otherwise, sell
  # vol_hold <- GET_VOLUME_POS_EXT(hist)

  # check hold
  # hold <- bb_hold & ma_hold & vol_hold
  hold <- bb_hold[test_period] & ma_hold[test_period]
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
  ret <- ClCl(stock_hist)*pos
  # print(ret)

  eq <- exp(cumsum(na.omit(ret)))
  # print(eq)
  return(c(stock_id, tail(eq, n=1)-1))
}



