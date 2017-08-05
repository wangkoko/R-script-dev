# bug: 9927 tw
source("R-dev/r-stock-lib-script.R")
# strategy for break through shortern, strategy4 for longtern
do_pick = TRUE
PROF_VERIFY = FALSE
file_name1 = 'pick-today1-strategy5'
file_name2 = 'pick-today2-strategy5'
file_name3 = 'pick-today3-strategy5'
# Get stock exchange market index
tw_list <- get_stock_list();
two_list <- get_stock_list(cata = 'TWO')
# for new stratgy
#  pick candidate new strategy, only for strategy5
if (isTRUE(do_pick)) {
  today <- format(Sys.time(), "%Y-%m-%d")
  print(c("> start pick", today))
  # pick <- do.call(rbind, lapply(head(tw_list$No, n=28), FUN=pick_strategy, cata = 'TW', pick = TRUE))
  # pick_otc <- do.call(rbind, lapply(head(two_list$No, n=10), FUN=pick_strategy, cata = 'TWO', pick = TRUE))
  pick <- do.call(rbind, lapply(tw_list$No, FUN=pick_strategy5, cata = 'TW', pick = TRUE, prof_verify = PROF_VERIFY))
  pick_otc <- do.call(rbind, lapply(two_list$No, FUN=pick_strategy5, cata = 'TWO', pick = TRUE, prof_verify = PROF_VERIFY))

  # fix NA value
  pick <- na.omit(pick)
  pick_otc <- na.omit(pick_otc)
  pick_all <- rbind(pick, pick_otc)

  # Get those stock is NOT held yesterday but DO held today
  pick_all <- pick_all[with(pick_all, order(Prof, decreasing = TRUE)), ]
  pick_today_buy <- pick_all[pick_all$'Buy'==1,]
  pick_today_add <- pick_all[pick_all$'Add'==1,]
  pick_today_buy <- pick_today_buy[pick_today_buy$'var'<0.15,]
  pick_today_buy <- pick_today_buy[pick_today_buy$'Vo'>200,]
  pick_today_add <- pick_today_add[pick_today_add$'Vo'>200,]
  # order with column var
  # pick_all <- pick_all[with(pick_all, order(var)), ]
  pick_today_buy <- pick_today_buy[with(pick_today_buy, order(var)), ]
  pick_today_buy <- pick_today_buy[with(pick_today_buy, order(Buy_keep, decreasing = TRUE)), ]
  pick_today_add <- pick_today_add[with(pick_today_add, order(var)), ]
  # pick_all_prof <- pick_all[with(pick_all, order(Prof, decreasing = TRUE)), ]
  # write to file
  if (PROF_VERIFY == TRUE) {
    f_name <- paste("pick-prof-", sep = "", today)
    f_name <- paste(f_name, sep = "", ".csv")
    write.csv(pick_all, file = f_name)
  }
  f_name <- paste("pick-buy-", sep = "", today)
  f_name <- paste(f_name, sep = "", ".csv")
  write.csv(pick_today_buy, file = f_name)

  f_name <- paste("pick-add-", sep = "-", today)
  f_name <- paste(f_name, sep = "", ".csv")
  write.csv(pick_today_add, file = f_name)

  prof_mean <- mean(as.numeric(pick_all$'Prof'))
  win_rate <- sum(ifelse(pick_all$'Prof'>=0, 1, 0))/nrow(pick_all)
  hold_mean <- mean(as.numeric(pick_all$'Hold_days'))
  print(paste("prof_mean", sep = ":", prof_mean))
  print(paste("win_rate", sep = ":", win_rate))
  print(paste("hold_mean", sep = ":", hold_mean))
  print("> Pick end!")
}
## latest version end


#  pick candidate
if (isTRUE(do_pick)) {
  today <- format(Sys.time(), "%Y-%m-%d")
  print(c("> start pick", today))
  # pick <- do.call(rbind, lapply(head(tw_list$No, n=28), FUN=pick_strategy, cata = 'TW', pick = TRUE))
  # pick_otc <- do.call(rbind, lapply(head(two_list$No, n=10), FUN=pick_strategy, cata = 'TWO', pick = TRUE))
  pick <- do.call(rbind, lapply(tw_list$No, FUN=pick_strategy5, cata = 'TW', pick = TRUE))
  pick_otc <- do.call(rbind, lapply(two_list$No, FUN=pick_strategy5, cata = 'TWO', pick = TRUE))

  # fix NA value
  pick <- na.omit(pick)
  pick_otc <- na.omit(pick_otc)
  pick_all <- rbind(pick, pick_otc)

  # Get those stock is NOT held yesterday but DO held today
  pick_today <- pick_all[pick_all$'T'==1,]
  pick_today_1 <- pick_all[pick_all$'T'==1&pick_all$'T-1'==0,]
  pick_today_2 <- pick_all[pick_all$'T'==1&pick_all$'T-1'==1,]
  pick_today_vol <- pick_today_2[pick_today_2$'Vo'>500,]
  # order with column var
  pick_all <- pick_all[with(pick_all, order(var)), ]
  pick_today <- pick_today[with(pick_today, order(var)), ]
  pick_today_1 <- pick_today_1[with(pick_today_1, order(var)), ]
  pick_today_2 <- pick_today_2[with(pick_today_2, order(var)), ]
  pick_all_prof <- pick_all[with(pick_all, order(Prof, decreasing = TRUE)), ]
  # write to file
  f_name <- paste("pick-", sep = "", today)
  f_name <- paste(f_name, sep = "", ".csv")
  write.csv(pick_today, file = f_name)

  f_name <- paste(file_name1, sep = "-", today)
  f_name <- paste(f_name, sep = "", ".csv")
  write.csv(pick_today_1, file = f_name)

  f_name <- paste(file_name2, sep = "-", today)
  f_name <- paste(f_name, sep = "", ".csv")
  write.csv(pick_today_2, file = f_name)

  f_name <- paste(file_name_vol, sep = "", today)
  f_name <- paste(f_name, sep = "", ".csv")
  write.csv(pick_today_vol, file = f_name)
  print("> Pick end!")
  # pick_all[pick_all$Prof < 0,]
  prof_mean <- mean(as.numeric(pick_all$'Prof'))
  win_rate <- sum(ifelse(pick_all$'Prof'>=0, 1, 0))/nrow(pick_all)
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
}

# see single stock analysis
# source("R-dev/r-stock-lib-script.R")
# pick_strategy5('4402', cata = 'TWO', pick=TRUE, prof_verify=TRUE)
# pos_plot('2337', cata = 'TW', month = 0, years = 1, from = Sys.time(), FUNC = pick_strategy5)
# pick_strategy5('2337', cata = 'TW', prof_verify = TRUE, pick = TRUE)
if (isTRUE(do_pick)) {
  plot_show <- lapply(rownames(pick_today), FUN=pos_plot, cata = '', FUNC = pick_strategy2)
}
