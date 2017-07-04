get_stock_info <- function(cata = 'TW') {
  index_src <- "StockList"
  index_src <- paste(index_src, sep="-", cata)
  index_src <- paste(index_src, sep = "", ".csv")
  src_pth <- paste("info/", sep = "",index_src)
  idx <- read.csv(src_pth)[,c(1, 2)]
  colnames(idx) = c("No", "Name")
  idx$No <- str_extract(idx$No, "[0-9]+")
  idx
}

idx_tw <- get_stock_info()
idx_two <- get_stock_info(cata = 'TWO')