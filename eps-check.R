library("RCurl")
library("XML")
library(stringr)
require(lubridate)

# 至goodino 下載
CONTINUE_YEARS <- 4
# 1) 歷年 eps, 留下 股號|名稱|近x年eps, x>CONTINUE_YEARS
eps_src <- "stock-eps-year"
eps_src <- paste(eps_src, sep = "", ".csv")
eps_src_pth <- paste("info-dev/", sep = "",eps_src)
# 2) 近12季 eps, 留下 股號|名稱|收盤價|近4季eps (必須皆有值)
eps_sessional <- "stock-eps-sessional"
eps_sessional <- paste(eps_sessional, sep = "", ".csv")
eps_sessional_pth <- paste("info-dev/", sep = "",eps_sessional)
# 3) 近12季 每股自由現金流量，留下 股號|名稱稱|近4季 現金流量: 自由現金流量 = 營業現金流量+投資現金流量
cash_sessional <- "stock-free-cash-sessional"
cash_sessional <- paste(cash_sessional, sep = "", ".csv")
cash_sessional_pth <- paste("info-dev/", sep = "",cash_sessional)
# 4) 近12季 每股淨現金流量，留下 股號|名稱稱|近4季 現金流量: 淨現金流量 = 營業現金流量+投資現金流量+理財現金流量
net_cash_sessional <- "stock-net-cash-sessional"
net_cash_sessional <- paste(net_cash_sessional, sep = "", ".csv")
net_cash_sessional_pth <- paste("info-dev/", sep = "",net_cash_sessional)
# 5) 近12季 每股營業現金流量，留下 股號|名稱稱|近4季 營業現金流量:真正收進來的現金 扣掉 真正花出去的現金
earn_cash_sessional <- "stock-earn-cash-sessional"
earn_cash_sessional <- paste(earn_cash_sessional, sep = "", ".csv")
earn_cash_sessional_pth <- paste("info-dev/", sep = "",earn_cash_sessional)

growth_stocks <- NA
stocks_sessional_eps <- NA
growthing_stocks <- NA
cash_check <- NA
## of STOCKS PASS 1st CRITERIA(EPS > 0)
if (1) {
  # init
  stock_eps <- read.csv(eps_src_pth)
  col_names <- colnames(stock_eps)
  len_names <- length(col_names)
  stock_eps[,col_names[1]] <- str_extract(stock_eps[,col_names[1]], "[0-9]+")
  colnames(stock_eps) <- c(c('No','Name'),paste('Y',sep='-',c(1:(len_names-2))))
  col_names <- colnames(stock_eps)
  len_names <- length(col_names)
  # get stocks that pass eps criteria
  stock_list <- stock_eps
  pass_stock_idx <- 1
  for (idx in 1:c(nrow(stock_list))) {
    pass <- TRUE
    stock_info <- stock_list[idx,]
    eps_check <- stock_info[(len_names - CONTINUE_YEARS +1):len_names]
    eps_check[,is.na(eps_check)] <- 0
    for (i in 1:(CONTINUE_YEARS-1)) {
      if (eps_check[i] > eps_check[i+1] || eps_check[i] <= 0)
        pass <- FALSE
    }
    if (pass) {
      if (!is.data.frame(growth_stocks))
        growth_stocks <- stock_info
      else
        growth_stocks <- rbind(growth_stocks, stock_info)
    }
    pass_stock_idx <- pass_stock_idx + 1
  }
  stock_needed_info = c(c(1:2),c((len_names-CONTINUE_YEARS+1):len_names))
  growth_stocks <- growth_stocks[,stock_needed_info]
  colnames(growth_stocks) <- c(c('No','Name'),paste("Y", sep="-",c(1:CONTINUE_YEARS)))
  row.names(growth_stocks) <- c(1:nrow(growth_stocks))
}
#print(growth_stocks)
## of STOCKS PASS 2nd CRITERIA(add last 4 session EPS, check increase and P/E ratio)
if (1) {
  stock_eps_sessional <- read.csv(eps_sessional_pth)
  col_names <- colnames(stock_eps_sessional)
  len_names <- length(col_names)
  stock_eps_sessional[,col_names[1]] <- str_extract(stock_eps_sessional[,col_names[1]], "[0-9]+")
  colnames(stock_eps_sessional) <- c(c('No','Name','Price'),c(1:(len_names-3)))
  # main for 3rd criteria
  stock_list <- stock_eps_sessional
  pass_stock_idx <- 1
  for (idx in 1:c(nrow(stock_list))) {
    pass <- TRUE
    stock_info <- stock_list[idx,]
    #print(paste(stock_info[1],sep=':',growth_stocks[pass_stock_idx,1]))
    if (stock_info$No == growth_stocks[pass_stock_idx,1]) {
      # 計算最後4季eps加總，模擬一整年份
      eps_last_4 <- stock_info[(len_names-3):len_names]
      eps_last_4[,is.na(eps_last_4)] <- 0
      eps_last_4 <- sum(eps_last_4)
      # 與前次4年eps皆為成長之最後一年比較，如無繼續成長，不列入考慮
      eps_compare_to <- growth_stocks[pass_stock_idx, length(colnames(growth_stocks))]
      if (0) {
        print(paste(stock_info[1],sep=':',eps_last_4))
        print(paste("compare_to",sep=':',eps_compare_to))
      }
      if (eps_last_4 < eps_compare_to)
        pass <- FALSE
      if (pass) {
        tmp_eps <- growth_stocks[pass_stock_idx,]
        tmp_sessional <- stock_info[(len_names-3):len_names]
        colnames(tmp_sessional) <- paste('Q\'',sep="-",c(1:4))
        tmp_sessional$sessional_sum <- eps_last_4
        tmp_sessional$price <- stock_info$Price
        tmp_sessional$PE <- stock_info$Price/eps_last_4
        tmp_sessional$growth_rate <- (eps_last_4/eps_compare_to)-1
        tmp_sessional$PE_growth <- tmp_sessional$PE/(tmp_sessional$growth_rate*100)
        tmp_sessional$earn_cash_flow <- NA
        tmp_sessional$free_cash_flow <- NA
        tmp_sessional$net_cash_flow <- NA
        tmp_eps <- cbind(tmp_eps, tmp_sessional)
        if (!is.data.frame(growthing_stocks))
          growthing_stocks <- tmp_eps
        else
          growthing_stocks <- rbind(growthing_stocks, tmp_eps)
      }
      pass_stock_idx <- pass_stock_idx + 1
      if (pass_stock_idx > NROW(growth_stocks))
        break
    }
  }
  row.names(growthing_stocks) <- c(1:nrow(growthing_stocks))
  #print(growthing_stocks)
}
## 3) update free cash flow last 4 session's
if (1) {
  stock_case_sessional <- read.csv(cash_sessional_pth)
  col_names <- colnames(stock_case_sessional)
  len_names <- length(col_names)
  stock_case_sessional[,col_names[1]] <- str_extract(stock_case_sessional[,col_names[1]], "[0-9]+")
  colnames(stock_case_sessional) <- c(c('No','Name'),paste('cash',sep='-', c(1:(len_names-2))))
  # main for 3rd criteria
  stock_list <- stock_case_sessional
  pass_stock_idx <- 1
  for (idx in 1:c(nrow(stock_list))) {
    stock_info <- stock_list[idx,]
    if (stock_info$No == growthing_stocks[pass_stock_idx,1]) {
      # 計算最後4季CASH加總，模擬一整年份
      cash_last_4 <- stock_info[(len_names-3):len_names]
      cash_last_4[,is.na(cash_last_4)] <- 0
      cash_last_4 <- sum(cash_last_4)
      growthing_stocks[pass_stock_idx,]$free_cash_flow = cash_last_4
      pass_stock_idx <- pass_stock_idx + 1
      if (pass_stock_idx > NROW(growthing_stocks))
        break
    }
  }
  #print(growthing_stocks)
}
## 4) update net cash flow last 4 session's
if (1) {
  stock_net_cash_sessional <- read.csv(net_cash_sessional_pth)
  col_names <- colnames(stock_net_cash_sessional)
  len_names <- length(col_names)
  stock_net_cash_sessional[,col_names[1]] <- str_extract(stock_net_cash_sessional[,col_names[1]], "[0-9]+")
  colnames(stock_net_cash_sessional) <- c(c('No','Name'),paste('cash',sep='-', c(1:(len_names-2))))
  # main for 3rd criteria
  stock_list <- stock_net_cash_sessional
  pass_stock_idx <- 1
  for (idx in 1:c(nrow(stock_list))) {
    stock_info <- stock_list[idx,]
    if (stock_info$No == growthing_stocks[pass_stock_idx,1]) {
      # 計算最後4季CASH加總，模擬一整年份
      cash_last_4 <- stock_info[(len_names-3):len_names]
      cash_last_4[,is.na(cash_last_4)] <- 0
      cash_last_4 <- sum(cash_last_4)
      growthing_stocks[pass_stock_idx,]$net_cash_flow = cash_last_4
      pass_stock_idx <- pass_stock_idx + 1
      if (pass_stock_idx > NROW(growthing_stocks))
        break
    }
  }
  #print(growthing_stocks)
}

## 5) update 每股營業現金流 last 4 session's
if (1) {
  stock_earn_cash_sessional <- read.csv(earn_cash_sessional_pth)
  col_names <- colnames(stock_earn_cash_sessional)
  len_names <- length(col_names)
  stock_earn_cash_sessional[,col_names[1]] <- str_extract(stock_earn_cash_sessional[,col_names[1]], "[0-9]+")
  colnames(stock_earn_cash_sessional) <- c(c('No','Name'),paste('cash',sep='-', c(1:(len_names-2))))
  # main for 3rd criteria
  stock_list <- stock_earn_cash_sessional
  pass_stock_idx <- 1
  for (idx in 1:c(nrow(stock_list))) {
    stock_info <- stock_list[idx,]
    if (stock_info$No == growthing_stocks[pass_stock_idx,1]) {
      # 計算最後4季CASH加總，模擬一整年份
      cash_last_4 <- stock_info[(len_names-3):len_names]
      cash_last_4[,is.na(cash_last_4)] <- 0
      cash_last_4 <- sum(cash_last_4)
      growthing_stocks[pass_stock_idx,]$earn_cash_flow = cash_last_4
      pass_stock_idx <- pass_stock_idx + 1
      if (pass_stock_idx > NROW(growthing_stocks))
        break
    }
  }
  #print(growthing_stocks)
}
# order with 本益成長比
potential_target <- growthing_stocks
# 後處理獲得想要條件 post processing criteria
#potential_target <- potential_target[potential_target$growth_rate<0.3, ]
#potential_target <- potential_target[potential_target$sessional_sum < potential_target$earn_cash_flow, ]
#potential_target <- potential_target[potential_target$net_cash_flow>0, ]
#potential_target <- potential_target[potential_target$PE_growth < 1, ]
potential_target <- potential_target[with(potential_target, order(PE_growth)), ]
row.names(potential_target) <- c(1:nrow(potential_target))
# print result
print(paste("# of STOCKS PASS 1st CRITERIA(EPS > 0): ",nrow(growth_stocks)))
#print(growth_stocks)
print(paste("# of STOCKS PASS 2nd CRITERIA(last four session eps sum > last year): ",nrow(growthing_stocks)))
#print(growthing_stocks)
print(potential_target)

today <- format(Sys.time(), "%Y-%m-%d")
f_name <- paste("growth-stock", sep = "-", today)
f_name <- paste(f_name, sep = "", ".csv")
write.csv(potential_target, file = f_name)
