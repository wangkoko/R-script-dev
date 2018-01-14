library(stringr)
require(lubridate)

# ranking, num, name, price, eps-avg4, Y-3, Y-2, Y-1
eps_src <- "StockList-eps-2018"
eps_src <- paste(eps_src, sep = "", ".csv")
eps_src_pth <- paste("info-dev/magic-formula/", sep = "",eps_src)

# ranking, num, name, price, roa-avg12, Y, Y+1, Y+2, Y+3, Y+4
roa_src <- "StockList-roa-2018"
roa_src <- paste(roa_src, sep = "", ".csv")
roa_src_pth <- paste("info-dev/magic-formula/", sep = "",roa_src)

# eps last 4 session , num, name, price, s+1, s+2, s+3, s+4
session_src <- "StockList-session-2017-q3"
session_src <- paste(session_src, sep = "", ".csv")
session_src_pth <- paste("info-dev/magic-formula/", sep = "",session_src)

# roa last 4 session , num, name, price, s+1, s+2, s+3, s+4
session_roa_src <- "StockList-session-roa-2017-q3"
session_roa_src <- paste(session_roa_src, sep = "", ".csv")
session_roa_src_pth <- paste("info-dev/magic-formula/", sep = "",session_roa_src)

# pre-process eps list
stock_eps <- read.csv(eps_src_pth)
eps_col_names <- colnames(stock_eps)
eps_len_names <- length(eps_col_names)
stock_eps[,eps_col_names[2]] <- str_extract(stock_eps[,eps_col_names[2]], "[0-9]+")
colnames(stock_eps) <- c(c('ranking','No','Name','price','avg-eps'),paste('Y',sep='-',c((eps_len_names-5):1)))
stock_eps$'Y-0' <- 0
# pre-process last 4 session eps for estimate
session_eps <- read.csv(session_src_pth)
session_col_names <- colnames(session_eps)
session_len_names <- length(session_col_names)
session_eps[,session_col_names[2]] <- str_extract(session_eps[,session_col_names[2]], "[0-9]+")
colnames(session_eps) <- c(c('ranking','No','Name','price'),paste('S',sep='-',c((session_len_names-5):0)))
for (idx in 1:c(nrow(stock_eps))) {
#  if (idx < 4)
#    print(session_eps[idx,])
  stock_eps[idx,]$'Y-0' <- round(sum(session_eps[idx,]$'S-0',session_eps[idx,]$'S-1',session_eps[idx,]$'S-2',session_eps[idx,]$'S-3'),3)
}
stock_eps$`avg-eps` <- round((stock_eps$`Y-3`+stock_eps$`Y-2`+stock_eps$`Y-1`+stock_eps$`Y-0`)/4,3)
stock_eps$PE <- round(stock_eps$pric/stock_eps$`Y-0`,2)
stock_eps <- stock_eps[with(stock_eps, order(PE, decreasing = FALSE)), ]
# ranking
stock_eps <- stock_eps[stock_eps$PE > 0, ]
for (idx in 1:c(nrow(stock_eps)))
  stock_eps[idx,]$ranking <- idx
stock_eps <- stock_eps[with(stock_eps, order(No, decreasing = FALSE)), ]
stock_eps$combined_rank <- nrow(stock_eps)
stock_eps_check <- stock_eps[with(stock_eps, order(ranking, decreasing = FALSE)), ]

# pre-process roa list
stock_roa <- read.csv(roa_src_pth)
roa_col_names <- colnames(stock_roa)
roa_len_names <- length(roa_col_names)
stock_roa[,roa_col_names[2]] <- str_extract(stock_roa[,roa_col_names[2]], "[0-9]+")
colnames(stock_roa) <- c(c('ranking','No','Name','price','avg_roa'),paste('Y',sep='-',c((eps_len_names-5):1)))
stock_roa$'Y-0' <- 0
# pre-process last 4 session roa for estimate
session_roa <- read.csv(session_roa_src_pth)
session_roa_col_names <- colnames(session_roa)
session_roa_len_names <- length(session_roa_col_names)
session_roa[,session_roa_col_names[2]] <- str_extract(session_roa[,session_roa_col_names[2]], "[0-9]+")
colnames(session_roa) <- c(c('ranking','No','Name','price'),paste('S',sep='-',c((session_roa_len_names-5):0)))
for (idx in 1:c(nrow(session_roa))) {
  #if (idx < 4)
  #   print(session_roa[idx,])
  stock_roa[idx,]$'Y-0' <- round(sum(session_roa[idx,]$'S-0',session_roa[idx,]$'S-1',session_roa[idx,]$'S-2',session_roa[idx,]$'S-3'),3)
}

for (idx in 1:c(nrow(stock_roa))) {
  #print(stock_roa[idx,]$No)
  stock_roa[idx,]$`avg_roa` <- round(sum(stock_roa[idx,]$`Y-4`,stock_roa[idx,]$`Y-3`, stock_roa[idx,]$`Y-2`,stock_roa[idx,]$`Y-1`)/4,3)
}
stock_roa <- stock_roa[with(stock_roa, order(avg_roa, decreasing = TRUE)), ]
#stock_roa <- stock_roa[with(stock_roa, order(`Y-0`, decreasing = TRUE)), ]
for (idx in 1:c(nrow(stock_roa)))
  stock_roa[idx,]$ranking <- idx

stock_roa <- stock_roa[with(stock_roa, order(No, decreasing = FALSE)), ]
stock_roa_check <- stock_roa[with(stock_roa, order(ranking, decreasing = FALSE)), ]

stock_roa$eps_Y_1 <- nrow(stock_roa)
stock_roa$eps_last <- nrow(stock_roa)
stock_roa$PE <- nrow(stock_roa)
stock_roa$eps_ranking <- nrow(stock_roa)
stock_roa$growth_rate <- nrow(stock_roa)
stock_roa$combined_rank <- nrow(stock_roa)

# calculate
eps_stock_idx <- 1
for (idx in 1:c(nrow(stock_roa))) {
  stock_info <- stock_eps[eps_stock_idx,]
  if (stock_eps[eps_stock_idx,]$No == stock_roa[idx,]$No) {
    stock_roa[idx,]$combined_rank <- stock_eps[eps_stock_idx,]$ranking + stock_roa[idx,]$ranking
    stock_roa[idx,]$eps_ranking <- stock_eps[eps_stock_idx,]$ranking
    stock_roa[idx,]$eps_Y_1 <- stock_eps[eps_stock_idx,]$'Y-1'
    stock_roa[idx,]$eps_last <- stock_eps[eps_stock_idx,]$'Y-0'
    stock_roa[idx,]$PE <- stock_eps[eps_stock_idx,]$'PE'
    stock_roa[idx,]$growth_rate <- round(stock_eps[eps_stock_idx,]$'PE'/((stock_eps[eps_stock_idx,]$'Y-0'/stock_eps[eps_stock_idx,]$'Y-1'-1)*100),3)
    stock_eps[eps_stock_idx,]$combined_rank <- stock_eps[eps_stock_idx,]$ranking + stock_roa[idx,]$ranking
    eps_stock_idx <- eps_stock_idx + 1
  }
}

stock_eps <- stock_eps[with(stock_eps, order(combined_rank, decreasing = FALSE)), ]
stock_roa <- stock_roa[with(stock_roa, order(combined_rank, decreasing = FALSE)), ]

top_60 <- head(stock_roa,60)

today <- format(Sys.time(), "%Y-%m-%d")
f_name <- paste("magic-formular-", sep = "", today)
f_name <- paste(f_name, sep = "", ".csv")
write.csv(top_60, file = f_name)
