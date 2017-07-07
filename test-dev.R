source("R-dev/r-stock-lib-script.R")
#
do_pick = TRUE
do_perf_check = FALSE
# Get stock exchange market index
tw_list <- get_stock_list();
two_list <- get_stock_list(cata = 'TWO')
#  pick candidate
if (isTRUE(do_pick)) {
  today <- format(Sys.time(), "%Y-%m-%d")
  print(c("> start pick", today))
  # pick <- do.call(rbind, lapply(head(tw_list$No, n=28), FUN=pick_strategy, cata = 'TW', pick = TRUE))
  # pick_otc <- do.call(rbind, lapply(head(two_list$No, n=10), FUN=pick_strategy, cata = 'TWO', pick = TRUE))
  pick <- do.call(rbind, lapply(tw_list$No, FUN=pick_strategy, cata = 'TW', pick = TRUE))
  pick_otc <- do.call(rbind, lapply(two_list$No, FUN=pick_strategy, cata = 'TWO', pick = TRUE))

  # fix NA value
  pick <- na.omit(pick)
  pick_otc <- na.omit(pick_otc)
  pick_all <- rbind(pick, pick_otc)

  # Get those stock is NOT held yesterday but DO held today
  pick_today <- pick_all[pick_all$'T-1'==0 & pick_all$'T'==1,]
  pick_today_2 <- pick_all[pick_all$'T'==1,]
  # Get those stock is held MORE than TWO days
  pick_than_2days  <- pick_all[pick_all$'T-1'==1 & pick_all$'T'==1,]
  # order with column var
  pick_all <- pick_all[with(pick_all, order(var)), ]
  pick_today <- pick_today[with(pick_today, order(var)), ]
  pick_today_vol <- pick_today[with(pick_today, order(Vo, decreasing = TRUE)), ]
  pick_than_2days <- pick_than_2days[with(pick_than_2days, order(var)), ]
  pick_today_2 <- pick_today_2[with(pick_today_2, order(var)), ]
  # write to file
  f_name <- paste("pick-", sep = "", today)
  f_name <- paste(f_name, sep = "", ".csv")
  write.csv(pick_all, file = f_name)

  f_name <- paste("pick-today", sep = "", today)
  f_name <- paste(f_name, sep = "", ".csv")
  write.csv(pick_today_2, file = f_name)
  print("> Pick end!")
}

# Test performance
if (isTRUE(do_perf_check)) {
  # perf <- do.call(rbind, lapply(head(tw_list$No, n=28), FUN=performance_analysis))
  # perf_otc <- do.call(rbind, lapply(head(two_list$No, n=1), FUN=performance_analysis, cata = 'TWO'))
  perf <- do.call(rbind, lapply(tw_list$No, FUN=performance_analysis))
  perf_otc <- do.call(rbind, lapply(two_list$No, FUN=performance_analysis, cata = 'TWO'))

  colnames(perf) <- c("no","per")
  colnames(perf_otc) <- c("no","per")
  perf <- na.omit(perf)
  perf_otc <- na.omit(perf_otc)
  perf_all <- rbind(perf, perf_otc)
  perf_all <- perf_all[order(as.numeric(perf_all[,'per']), decreasing = TRUE),]
  perf_mean <- mean(as.numeric(perf_all[,'per']))
  write.csv(perf_all, file = "perf_all.csv")
  # pick_all[pick_all$Prof < 0,]
  win_rate <- sum(ifelse(pick_all$'Prof'>=0, 1, 0))/nrow(pick_all)
}

# see single stock analysis
source("R-dev/r-stock-lib-script.R")
pick_strategy('1216', cata = 'TW', draw = TRUE)
