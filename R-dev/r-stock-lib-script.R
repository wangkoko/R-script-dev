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
  valid_from <- head(index(pos_hold),n=1)
  valid_to <- tail(index(pos_hold),n=1)
  show_period <- paste(valid_from, sep = '::', valid_to)
  # print(paste(stock_name, sep = ': ', show_period))

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
  # hold_price <- (max(Cl(hist[show_period]))*Lag(pos_hold))
  hold_price <- ifelse(Lag(pos_hold)>1, max(Cl(hist[show_period]))*Lag(pos_hold)/2, min(Cl(hist[show_period]))*Lag(pos_hold))
  # print(hold_price)
  plot(addTA(hold_price[show_period],on=1,col=5))
  invisible(readline(prompt="Press [enter] to continue"))
}
# buy add sell
pick_strategy <- function(stock_name, cata = 'TW'
                           , hist = NULL, n = 22, sd = 1.5
                           , month = 0, years = 1, from = Sys.time(), pick = FALSE, prof_verify=FALSE) {
  BB_VAR_RANGE = 0.15
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
  #### 0. prepare data for later calculation
  bb_data <- BBands(hlc, maType = 'SMA', n = n, sd = sd)
  ma5 <- runMean(Cl(hist), n = 5)
  ma10 <- runMean(Cl(hist), n = 10)
  ma20 <- runMean(Cl(hist), n = 20)
  ma60 <- runMean(Cl(hist), n = 60)
  ma120 <- runMean(Cl(hist), n = 120)

  #### criteria 1. get desired bband range
  bb_var <- (bb_data$'up' - bb_data$'dn')/bb_data$'mavg'
  bb_inband <- (bb_data$'pctB' < 1) & (bb_data$'pctB' > 0)
  bb_var_limit <- ifelse(bb_var <= BB_VAR_RANGE, 1, 0)
  bb_var_limit <- bb_inband & bb_var_limit
  bb_var_limit <- na.omit(bb_var_limit)
  # print(bb_var_limit)
  #### criteria test 20170802 vol 5 > 22
  vo_5 <- runMean(Vo(hist), n = 5)
  vo_22 <- runMean(Vo(hist), n = 22)
  vo_limit <- vo_5 > vo_22
  #### criteria 3. get rising trend
  rising <- (ma60 > ma120)
  rising <- na.omit(rising)

  ##### 4. get valid period by the longest range 120 ma
  valid_from <- head(index(rising),n=1)
  valid_to <- tail(index(rising),n=1)
  test_period <- paste(valid_from, sep = '::', valid_to)
  print(paste(stock_name, sep = ': ', test_period))

  ##### 5-0. check buy_range
  buy_range <- bb_var_limit[test_period] & rising[test_period]
  ##### 5-1. check add_range
  add_range <- na.omit(Cl(hist)>bb_data$"up")[test_period]
  ##### 5-2. check hold range
  hold_range <- Cl(hist) > bb_data$"mavg"
  hold_range <- hold_range[test_period]

  # print(hold_range)
  #### 7. check whole period if buy_range happened then hold_range
  if (isTRUE(prof_verify)) {
    total_range <- ifelse(buy_range, 1, 0)
    bought <- 0
    break_through <- 0
    for (idx in index(hold_range)) {
      idx <- format(as.Date(idx), "%Y-%m-%d")
      if (buy_range[idx] == TRUE) {
        bought <- TRUE
        # print(idx)
      }
      if (bought == TRUE) {
        keep_critera <- buy_range[idx] | hold_range[idx] | add_range[idx]
        if (keep_critera >= 1) {
          total_range[idx] <- 1
          if (add_range[idx] == TRUE) {
            break_through <- TRUE
          }
          if (break_through == TRUE) {
            # print(idx)
            total_range[idx] <- 2
          }
        } else
          total_range[idx] <- 0
        if (keep_critera == 0) {
          bought <- FALSE
          break_through <- FALSE
        }
      }
    }
  } else {
    total_range <- buy_range | add_range
    total_range <- ifelse(total_range, 1, 0)
  }

  pos_hold <- total_range
  # print(pos_hold)
  if(isTRUE(pick)) {
    #### summary preparation
    last_pos <- as.numeric(tail(pos_hold, n=1))
    last_2add <- as.numeric(tail(add_range, n=2))
    last_buy <- as.numeric(tail(buy_range, n=1))
    range <- as.numeric(tail(bb_var, n=1))
    last_bb <- tail(bb_data, n = 1)
    buy_cumulated <- get_test_period(month = 2, from = tail(index(rising),n=1))
    vol <- (as.numeric(tail(Vo(hist), n=1))/1000)
    #### calculate the profit
    #### 8. to align the correct gain/loss when we can actually made decision to buy/sell
    buy <- na.omit(Lag(Lag(pos_hold)))
    op_hist <- na.omit(Op(hist))
    colnames(op_hist) <- "Open"
    prof_persent <- OpOp(op_hist)[test_period]
    prof <- prof_persent*buy
    eq <- exp(cumsum(na.omit(prof)))
    prof <- tail(eq, n=1) - 1
    # summarize
    buy_keep <- sum(buy_range[buy_cumulated])
    valid_period <- difftime(valid_to, valid_from, units = 'days')
    hold_days <- sum(pos_hold)
    res <- data.frame(matrix(c(range, last_pos, vol, last_bb$'pctB', buy_keep, last_buy, (last_2add[1] == FALSE) & (last_2add[2] == TRUE), prof, hold_days, valid_period), nrow=1, ncol=10))
    colnames(res) <- c("var", "Hold", "Vo", "BB_rank", "Buy_keep", "Buy", "Add", "Prof", "Hold_days", "Period")
    res$'valid_from' <- as.Date(valid_from)
    res$'valid_to' <- as.Date(valid_to)
    rownames(res) <- stock_name
    return(res)
  }
  return(pos_hold)
}
