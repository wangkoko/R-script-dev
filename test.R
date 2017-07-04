source("R/r-stock-lib-script.R")
source("R/methods.R")

#define
region <- 12*30

#TW Stock List
tw_list <- GET_LIST();
#write.csv(tw_list,  file = "info/sTW_list.csv", fileEncoding="UTF-8")

result <- do.call(rbind, lapply(cata = 'TW', tw_list$No, region, FUN=ANALYSIS_STRATEGY))
colnames(result) <- c("no", "tProfit", "ave(%)")
result <- na.omit(result)
profit <- sum(as.numeric(result[,'tProfit']))
order_pro <- result[order(as.numeric(result[,'tProfit']), decreasing = TRUE),]
write.csv(order_pro, file = "order_profit.csv")

#### Get OTC
two_list <- GET_LIST(path = "info/sTWO_list.csv");
result <- do.call(rbind, lapply(cata = 'TWO', two_list$No, region, FUN=ANALYSIS_STRATEGY))
colnames(result) <- c("no", "tProfit", "ave(%)")
result <- na.omit(result)
profit <- sum(as.numeric(result[,'tProfit']))
order_pro <- result[order(as.numeric(result[,'tProfit']), decreasing = TRUE),]
write.csv(order_pro, file = "order_profit_OTC.csv")
