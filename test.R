source("R/r-stock-lib-script.R")
source("R/methods.R")

#define
res_tit <- c("no", "profit(%)", "ave(%)", "hold")
region <- 3 * 30

#TW Stock List
tw_list <- GET_LIST();

result <- do.call(rbind, lapply(cata = 'TW', tw_list$No, region = region, FUN=ANALYSIS_STRATEGY))
colnames(result) <- res_tit
result <- na.omit(result)
profit <- sum(as.numeric(result[,'profit(%)']))
order_pro <- result[order(as.numeric(result[,'profit(%)']), decreasing = TRUE),]
write.csv(order_pro, file = "order_profit(5y).csv")

#### Get OTC
two_list <- GET_LIST(path = "info/sTWO_list.csv");

result <- do.call(rbind, lapply(cata = 'TWO', two_list$No, region = region, FUN=ANALYSIS_STRATEGY))
colnames(result) <- res_tit
result <- na.omit(result)
profit <- sum(as.numeric(result[,'profit(%)']))
order_pro <- result[order(as.numeric(result[,'profit(%)']), decreasing = TRUE),]
write.csv(order_pro, file = "order_profit_OTC(5y).csv")


source("R/methods.R")
source("R/scanStock.R")
region <- 3 * 30
tw_list <- GET_LIST();
two_list <- GET_LIST(path = "info/sTWO_list.csv");
result <- do.call(rbind, lapply(cata = 'TW', tw_list$No, region, FUN=Found_Buy_ID))
result <- na.omit(result)
colnames(result) <- c("no")
#write.csv(result, file = "Buy_ID.csv")
result <- do.call(rbind, lapply(cata = 'TWO', two_list$No, region, FUN=Found_Buy_ID))
result <- na.omit(result)
colnames(result) <- c("no")
#write.csv(result, file = "Buy_ID.csv")
