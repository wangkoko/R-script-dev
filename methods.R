
library(tidyverse)

GET_LIST <- function(path = 'info/sTW_list.csv')
{
  idx <- read_csv(path)[,c(2, 3)]
  colnames(idx) = c("No", "NAME")
  idx$No <- str_extract(idx$No, "[0-9]+")
  return(idx)
}

#Strategy
ANALYSIS_STRATEGY <- function(cata ='TW', id = 2337, region = 180)
{

  #define
  bypass <- c(5258, 4171)
  cNull <- c(id, NA, NA, NA)

  #ignore id
  for(i in 1:length(bypass))
  {
    if(id == bypass[i])  return(cNull)
  }

  TEST_REGION <- GET_TEST_REGION(days = region)

  try.xts(
    sdata <- GET_DATA(id, cata, year = 1, month = 0),
    error =  sdata <- NULL
  )
  if(is.null(sdata) || length(sdata[,4]) < 60*1.5){
    return(cNull)
  }


  #1. ma x > ma y
  try.xts(
    ma_pos <- na.omit(GET_MA_POS(sdata, tgt = 20, ref = 60)[TEST_REGION]),
    error =  ma_pos <- NULL
  )
  if(is.null(ma_pos) || length(ma_pos) < 1){
    return(cNull)
  }

  #3. b-band
  bb_pos <- na.omit(GET_BBand_POS(sdata)[TEST_REGION])

  #4. volume - 5 ave. > 20 ave. buy & keep, otherwise, sell
  vol_pos <- (GET_VOLUME_POS(sdata)[TEST_REGION])

  #Hold
  hold_pos <- ifelse(ma_pos & bb_pos & vol_pos, 1, 0)
  hold_time <- GET_HOLD_TIME(hold_pos)

  #profit
  profit <- ROC(Cl(sdata)) * hold_pos
  eq <- exp(cumsum(na.omit(profit)))
  last_eq <- as.numeric(tail(eq, n=1))

  if(hold_time == 0) return (cNull)
  else
  {
    plot(eq)
    print(c(id, round((last_eq - 1) * 100, 2), round((last_eq - 1)/hold_time * 100, 2), hold_time))
    return(c(id, round((last_eq - 1) * 100, 2), round((last_eq - 1)/hold_time * 100, 2), hold_time))
  }
}

GET_VOLUME_POS <-function(sdata)
{
  vol5_pos <- runMean(sdata[,5], n = 5)
  vol10_pos <- runMean(sdata[,5], n = 10)
  vol20_pos <- runMean(sdata[,5], n = 20)
  #return(ma_vt_pos > ma_vr_pos)

  abs_vol_pos <- ifelse(sdata[,5] > vol10_pos, 1, 0)
  vol5_pos <- vol5_pos > vol20_pos

  return(abs_vol_pos & vol5_pos)
}

GET_TEST_REGION <- function(days = 12 * 30)
{
  ed_date <- Sys.Date()
  st_date <- format((ed_date - as.difftime(days, unit = "days")),"%Y-%m-%d")
  return(paste(st_date, sep="/", ed_date))
}

GET_DATA <-function(id, cata = 'TW', year = 10, month = 0)
{
  target <- paste(id, sep = ".", cata)
  days <- (year * 12 + month) * 30
  ed_date <- Sys.time()
  st_date <- format((ed_date - as.difftime(days, unit = "days")),"%Y-%m-%d")

  try.xts(
    data <- getSymbols(target, auto.assign = FALSE, from = st_date),
    error = data <- NULL
  )
  if(is.null(data)){
    print("E1-1: ")
    return(NULL)
  }
  return(na.omit(data))
}


GET_BBand_POS <- function(sdata, n = 22, sd = 1.5, Mode = 0)
{
  hlc <- sdata[, -c(1,5,6)]
  bb_data <- na.omit(BBands(na.omit(hlc), maType = 'SMA', n, sd))

  if(Mode == 1){
    buy <- na.omit(Cl(sdata)>bb_data$"up")
  }
  else{
    buy <- na.omit(Lag(Cl(sdata))>Lag(bb_data$"up"))
  }

  if(Mode == 1){
    keep <- na.omit(Cl(sdata)>bb_data$"mavg")
  }
   else
   {
     keep <- na.omit(Lag(Cl(sdata))>Lag(bb_data$"mavg"))
   }

  bb_pos <- GET_BBand_HOLD_POS(buy, keep)
  return(bb_pos)
}

GET_BBand_HOLD_POS <- function(buy, keep)
{
  hold = 0;
  for(i in 1:length(buy))
  {
    if(!hold)
      buy[i] = ifelse(buy[i], 1, 0)
    else
      buy[i] = ifelse(keep[i], 1, 0)

    hold = buy[i]
  }
  return(buy)
}


GET_MA_POS <- function(sdata, tgt = 20, ref = 60)
{
  #ma_5 <- runMean(sdata[,4], n = 5)
  #ma_10 <- runMean(sdata[,4], n = 10)
 # ma_20 <- runMean(sdata[,4], n = 20)
  #ma_60 <- runMean(sdata[,4], n = 60)


 # ma_5_10_pos <- ifelse(abs(ma_5 - ma_10)/ma_5 < 0.05, 1, 0)
 # ma_5_20_pos <- ifelse(abs(ma_20 - ma_5)/ma_5 < 0.05, 1, 0)
 # ma_5_60_pos <- ifelse(abs(ma_5 - ma_60)/ma_5 < 0.05, 1, 0)

  #ma_pos <- ifelse((ma_5_10_pos & ma_5_20_pos & ma_5_60_pos), 1, 0)

  tgt_ma <- runMean(sdata[,4], n = tgt)
  ref_ma <- runMean(sdata[,4], n = ref)
  ma_pos <- tgt_ma > ref_ma
  up_ma <- runMean(sdata[,4], n = 60)
  return(ma_pos & up_ma)
}

GET_MA_Vol_POS <- function(sdata, tgt = 5)
{
  tgt_ma <- runMean(sdata[,5], tgt)
  ma_pos <- lag(sdata[,5], k = tgt) > tgt_ma
  return(ma_pos)
}

GET_HOLD_TIME <- function(hold_pos)
{
  return(sum(hold_pos))
}
