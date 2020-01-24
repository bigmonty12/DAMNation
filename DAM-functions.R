
# Convert raw data to binary sleep data
find.sleep <- function(df) {
  new <- rep(NA, length(df))
  for (i in seq(from = 3, to = length(df) - 2)) {
    if (sum(df[i:(i+2)]) == 3) {
      new[c(i:(i+2))] = 1
    } else if (sum(df[i:(i-2)]) == 3) {
      new[c(i:(i-2))] = 1
    } else{
      new[[i]] = 0
    }
  }
  new[is.na(new)] <- 0
  return(new)
}

# Find dead flies (no activity for last 2 hours and no activity after experiment ends)
find.death.before <- function(df) {
  return(sum(df %in% 60, na.rm = T) == 2)
}

find.death.after <- function(df) {
  return(sum(df) == 0)
}

# Find latency (how long it takes to fall asleep) from sleep data
get.latency = function(df, dates, days) {
  nights <- df[hour(df[[1]]) >= 12,]
  results <- data.frame(matrix(nrow = days, ncol = length(df) - 1))
  results[1] <- dates
  for (j in seq_len(days)) {
    night <- nights[date(nights[[1]]) == dates[j], ]
    loc = 2
    for (col in night[3:length(night)]) {
      count = 0
      for (i in seq_along(col)) {
        if (col[i] == 0) {
          count <- count + 1
        } else {
          results[j, loc] <- count * 2
          loc <- loc + 1
          break
        }
      }
      if (count == 360) {
        results[j, loc] <- count * 2
        loc <- loc + 1
      }
    }
  }
  return(results)
}

# Find the time of each shake and the previous measuring period
# x is number of days in experiment
find.each_shake <- function(x) {
  nums <- c()
  for (i in seq_len(x) - 1) {
    a = 1:2 + i * 30
    nums <- c(nums, a)
  }
  return(nums)
}

# Find if flies woke up when nutated
find.arousal <- function(df, deadf, dates, days, num_shakes) {
  results <- data.frame(matrix(nrow = days * num_shakes, ncol = length(df) - 1))
  results[1] <- rep(dates, num_shakes)
  iloc = 1
  for (col in df[3:length(df)]) {
    iloc <- iloc + 1
    loc <- 0
    for (i in seq(1, length(col), by = 2)) {
      loc <- loc + 1
      day = col[i:(i+1)]
      if (day[1] == 0) {
        results[loc, iloc] <- 0
      } else {
        if (day[2] == 1) {
          results[loc, iloc] <- 1
        } else {
          results[loc, iloc] <- 2
        }
      }
    }
  }
  iloc <- iloc + 1
  if (deadf > 0) {
  dead_results <- results[,-c(deadf)]
  } else {
    dead_results <- results 
  }
  for (i in seq_len(days * num_shakes)) {
    r <- length(which(dead_results[i,] == 2))
    w <- length(which(dead_results[i,] == 1))
    t <- r + w
    results[i, iloc] <- paste0(r, sep='/', t, ' (woke up/were asleep)')
  }
  return(results[order(results[1]),])
}

# Find bouts: when and how long
find.bouts <- function(df) {
  bouts_list <- list()
  index = 1
  for (i in seq(3, length(df))) {
    runs <- rle(df[[i]])
    myruns <- which(runs$values == 1)
    runs.lengths.cumsum = cumsum(runs$lengths)
    ends = runs.lengths.cumsum[myruns]
    newindex = ifelse(myruns > 1, myruns - 1, 0)
    starts = runs.lengths.cumsum[newindex] + 1
    if (0 %in% newindex) starts = c(1,starts)
    width <- (ends - starts + 1) * 2
    start_time <- df[[1]][starts]
    #print(hour(start_time))
    end_time <- df[[1]][ends + 1]
    period <- ifelse(hour(start_time) < 12, "Light", "Dark") 
    bouts <- data.frame(idx = seq_along(start_time), start = start_time, end = end_time, length = width, period = period)
    bouts_list[[index]] <- bouts
    colnames(bouts_list[[index]]) <- c("idx", paste0("start_", index), paste0("end_", index), paste0("length_", index),
                                       paste0("period_", index))
    index <- index + 1
  }
  return(bouts_list)
}

find.boutAverages <- function(bouts, deadf) {
  bouts <- bouts[c(F,F,T,T)]
  head(bouts)
  
  splitdf <- function(df, n) {
    indx <- matrix(seq_len(ncol(df)), ncol = n)
    lapply(seq_len(n), function(x) df[, indx[, x]])
  }
  
  dfs <- splitdf(bouts, 32)
  colnames <- c("Length", "Period")
  dfs <- lapply(dfs, setNames, colnames)
  
  for (i in seq(from = 1, to = length(deadf), by = 1)) {
    fly <- deadf[i]
    idx <- min(which(is.na(dfs[[fly]]))) - 1
    dfs[[fly]][[1]][idx] <- NA
    dfs[[fly]][[2]][idx] <- NA
  }
  
  df <- rbindlist(dfs) 
  df <- na.omit(df)
  
  light <- df %>% filter(Period == "Light")
  dark <- df %>% filter(Period == "Dark")
  
  meanNumBoutsLight <- length(light[[1]]) / 32
  meanNumBoutsDark <- length(dark[[1]]) / 32
  
  meanBoutLengthLight <- mean(light[[1]])
  sdBoutLengthLight <- sd(light[[1]])
  meanBoutLengthDark <- mean(dark[[1]])
  sdBoutLengthDark <- sd(dark[[1]])
  
  boutAverages <- data.frame("Mean Bout Number (Light)" = meanNumBoutsLight,
                             "Mean Bout Length (Light)" = meanBoutLengthLight,
                             "SD Bout Length (Light)" = sdBoutLengthLight,
                             "Mean Bout Number (Dark)" = meanNumBoutsDark,
                             "Mean Bout Length (Dark)" = meanBoutLengthDark,
                             "SD Bout Length (Dark)" = sdBoutLengthDark)
  return(boutAverages)
  
}