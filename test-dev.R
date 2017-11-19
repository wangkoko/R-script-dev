# bug  10: 6150.TWO, 3625.TWO, 3441.TWO, 4976.TW
# bug -10: 3609.TWO
# add ma limit
source("R-dev/r-stock-lib-script.R")
do_pick = TRUE
PROF_VERIFY = TRUE
tw_list <- get_stock_list();
two_list <- get_stock_list(cata = 'TWO')
# for new stratgy
#  pick candidate new strategy, only for strategy5
if (isTRUE(do_pick)) {
  today <- format(Sys.time(), "%Y-%m-%d")
  print(c("> start pick", today))
  # pick <- do.call(rbind, lapply(head(tw_list$No, n=28), FUN=pick_strategy, cata = 'TW', pick = TRUE))
  # pick_otc <- do.call(rbind, lapply(head(two_list$No, n=10), FUN=pick_strategy, cata = 'TWO', pick = TRUE))
  pick <- do.call(rbind, lapply(tw_list$No, FUN=pick_strategy, cata = 'TW', pick = TRUE, prof_verify = PROF_VERIFY))
  pick_otc <- do.call(rbind, lapply(two_list$No, FUN=pick_strategy, cata = 'TWO', pick = TRUE, prof_verify = PROF_VERIFY))

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
  pick_today_buy <- pick_today_buy[with(pick_today_buy, order(BB_rank)), ]
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
# source("R-dev/r-stock-lib-script.R")
# pick_strategy('4402', cata = 'TWO', pick=TRUE, prof_verify=TRUE)
# pos_plot('2337', cata = 'TW', month = 0, years = 1, from = Sys.time(), FUNC = pick_strategy)
if (isTRUE(do_pick)) {
  plot_show <- lapply(rownames(pick_today_buy), FUN=pos_plot, cata = '', month = 0, years = 1, from = Sys.time(), FUNC = pick_strategy, prof_verify=TRUE)
}
