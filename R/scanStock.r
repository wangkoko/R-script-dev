
library(tidyverse)

Found_Buy_ID <- function(cata ='TW', id = 2337, region = 180)
{
  #define
  bypass <- c(5258, 4171)
  cNull <- c(NA)

  #ignore id
  for(i in 1:length(bypass))
  {
    if(id == bypass[i])  return(cNull)
  }

  TEST_REGION <- GET_TEST_REGION(region)

  try.xts(
    sdata <- GET_DATA(id, cata, year = 1, month = 0),
    error =  sdata <- NULL
  )
  if(is.null(sdata) || length(sdata[,4]) < 60*1.5){
    return(cNull)
  }

  #1. ma x > ma y, close > 60ma
  try.xts(
    ma_pos <- na.omit(GET_MA_POS(sdata, tgt = 20, ref = 60)[TEST_REGION]),
    error =  ma_pos <- NULL
  )
  if(is.null(ma_pos) || length(ma_pos) < 1){
    return(cNull)
  }

  #3. b-band
  bb_pos <- na.omit(GET_BBand_POS(sdata, Mode = 1)[TEST_REGION])

  #4. volume - 5 ave. > 20 ave. buy & keep, otherwise, sell
  vol_pos <- (GET_VOLUME_POS(sdata)[TEST_REGION])

  #Hold
  hold_pos <- ifelse(ma_pos & bb_pos & vol_pos, 1, 0)

  t1_pos <- as.numeric(tail(hold_pos, n = 1))
  t2_pos <- ifelse(as.numeric(sum(tail(hold_pos, n = 5))) == 1, 1, 0)

  if(t1_pos && t2_pos) {
    print(id)
    return(id)
  }
  return(cNull)
}
