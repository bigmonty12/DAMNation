# settings_server.R
# Home page of Shiny app

source("DAM-functions.R")

# Generate list of input files
inFile <- reactive(input$file1)

# Reads date range from input
#date_range <- reactive(as.Date(input$dateStart))

# A list of all inputs (used to access specific variables)
allInputs <- reactive(reactiveValuesToList(input))


# Number of readings per 24 hours
data_freq <- reactive(1440 / as.numeric(input$data_recording_frequency))

# Press go to start analysis
go_on_files <- eventReactive(input$go, {
  input$file1
})

# Get dates of experiment
date_range <- reactive({
  dates <- as.character(as.Date(
    seq(from = input$dateStart, to = (input$dateStart + input$experimentLength), by = 1)))
  dates
})

#====Functions====
plot_sleep_gram <- function(df){
  ggplot(df, aes(x=Date, y=Mean, colour=Period)) +
    geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se), width=0.1) +
    geom_point(aes(group=1)) + 
    geom_line(aes(group=1)) +
    theme_bw() +
    scale_color_manual(values = c("#CC9933", "#000066")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Hour (ZT)", y = "Mean Sleep (minutes) [w/SE]")
}

plot_act_gram <- function(df){
  ggplot(df, aes(x=Date, y=Mean, colour=Period)) +
    geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se), width=0.1) +
    geom_point(aes(group=1)) + 
    geom_line(aes(group=1)) +
    theme_bw() +
    scale_color_manual(values = c("#CC9933", "#000066")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    labs(x = "Hour (ZT)", y = "Mean Beam Breaks [w/SE]")
}

#====Raw Data====

# Format raw data
# Do not filter by light onset in this function. Need all data to call dead flies
format_raw <- reactive({
  rawFile <- input$file1
  
  if (is.null(rawFile))
    return(NULL)
  
  isolate({
    dates <- date_range()

    raw <- read.table(rawFile$datapath, sep = "\t") %>% filter(V1 > 0) %>%
      dplyr::select(-c(1, 4:10))
    
    
    raw$V2 <- as.character(as.POSIXct(strptime(raw$V2, format = "%d %b %y")))
    
    # Filter by selected dates
    raw <- raw %>% filter(V2 %in% dates)
    
    # Combine date with time
    raw$V2 <- with(raw, paste0(V2, " ", V3))
    raw$V2 <- strftime(raw$V2, "%Y-%m-%d %H:%M:%S")
    
    # Convert ZT time to an integer
    zt <- as.numeric(gsub(":00", "", input$light_onset_time))
    
    # Create night and day category, convert to ZT time
    time <- (hour(raw$V2) + minute(raw$V2)/60)
    
    raw <- raw %>%
      mutate(V3 = ifelse(time > zt & time <= (zt + 12),
                         "Day",
                         "Night")) %>%
      mutate(V2 = as.character(as.POSIXct(strptime(V2, "%Y-%m-%d %H:%M:%S")) - dhours(zt) - dminutes(2)))
    
    # Add column names to raw data
    raw_names <- c("Date", "Period", as.character(seq(1, length(raw) - 2)))
    colnames(raw) <- raw_names
    raw
  })
})

# Separate function to filter by light onset
format_raw_filter <- reactive({
  raw <- format_raw()
  
  # Filter by time light turns on
  begin <- which(minute(raw$Date) == 0 & hour(raw$Date) == 0) 
  start <- as.numeric(begin[1]) 
  end <- as.numeric(begin[length(begin)]) - 1
  raw <- raw[start:end,]
  raw
})

# Function used to separate the combined date & time column into two separate columns
datetime_separate <- function(df){
  df <- tidyr::separate(df, Date, c("Date", "Time"), sep = " ")
}


#====Sleep Data====

format_sleep <- reactive({
  raw <- format_raw_filter()
  
  sleep <- raw %>%
    mutate_at(vars(3:34), list(~ ifelse( . == 0, 1, 0)))
  
  sleep[3:34] <- apply(sleep[3:34], 2, find.sleep)
  sleep
})

#====Convert sleep data into sleep in min/hour for each hour for each fly====

format_agg_sleep <- reactive({
  sleep <- format_sleep()
  
  # Change date column to string with only date and hour
  agg <- sleep %>%
    mutate(Date = paste0(date(sleep$Date), " ", hour(sleep$Date)))
  
  # Merge sleep by each hour of each day
  agg = merge(
    unique(agg[1:2]), 
    aggregate(agg[3:34], 
              by = agg[1], 
              FUN = function(x) sum(x) * 2),  
    by = "Date")
  
  # Sort date and hour in ascending order
  agg = agg[mixedorder(gsub('-', '', agg$Date)), ]

  agg
  
})

format_agg_ave_sleep <- reactive({
  agg <- format_agg_sleep()
  
  dead_flies <- dead_flies()
  
  if (dead_flies > 0) {
    agg[c(dead_flies) + 2] <- NA
  }
  
  columns <- c(as.character(seq(1, 32)))
  agg <- agg %>% 
    mutate(Mean = rowMeans(.[columns], na.rm = TRUE),
           std = rowSds(as.matrix(.[columns]), na.rm = TRUE),
           se = std / sqrt(32 - length(dead_flies)))
  
  agg$Date <- factor(agg$Date, levels = agg$Date, ordered = TRUE)
  
  agg
})
#====Find Dead Flies====

# Get sleep totals for last 2 hours of experiment and after end of experiment
dead_flies <- reactive({
  
  dates <- date_range()
  raw <- format_raw()
  
  last_day <- format_agg_sleep()
  last_day <- datetime_separate(last_day)
  last_day <- last_day %>%
    filter(Date == dates[length(dates) - 1] & Period == 'Night') %>%
    dplyr::slice(11:12)
  
  
  extra_day <- dates[length(dates)]
  raw <- raw %>% filter(date(Date) == extra_day)
  
  # Return which flies are dead (if any)
  dead_before <- apply(last_day[4:35], 2, find.death.before)
  dead_after <- apply(raw[3:34], 2, find.death.after)
  
  dead_idx <- dead_before & dead_after 
  
  if (any(dead_idx)) {
    dead_flies <- which(dead_idx)
  } else {
    dead_flies <- 0
  }
  print(dead_flies)
  dead_flies
})

#====Aggregated sleep data with dead flies====

# Total aggregated sleep with dead flies
total_agg_sleep <- reactive({
  agg <- format_agg_sleep()
  dead_flies <- dead_flies()
  
  if (dead_flies > 0) {
    agg[c(dead_flies) + 2] <- NA
  }
  
  # Find average of flies for each hour of each day
  agg["Averages"] <- rowMeans(agg[3:length(agg)], na.rm = TRUE)
  
  # Find total sleep during day, night, and total
  total_agg <- aggregate(agg[3:35], 
                               by = agg[2], 
                               FUN = function(x) sum(x)) %>% 
    adorn_totals("row", na.rm = FALSE)
  
  total_agg
})

# Convert sleep data into aggregated sleep by date
format_agg_date_sleep <- reactive({
  sleep <- format_sleep()
  dead_flies <- dead_flies()
  
  agg_date = aggregate(sleep[3:34],
                       by = list(date(sleep$Date)),
                       FUN = function(x) sum(x) * 2)
  
  names(agg_date)[1] <- "Period"
  
  agg_date["Averages"] <- rowMeans(agg_date[3:length(agg_date)], na.rm = TRUE)
  
  if (dead_flies > 0) {
    agg_date[c(dead_flies) + 1] <- NA
  }
  
  agg_date
  
})

#====Convert sleep data into average sleep in min/hour for each hour of ZT time====

ave_sleep <- reactive({
  sleep <- format_sleep()
  dead_flies <- dead_flies() 
  num_days <- length(date_range()) - 1
  
  # Change Date column to hour string
  ave_sleep <- sleep %>%
    mutate(Date = paste0(hour(sleep$Date)))
  
  # Merge sleep by each hour of the day and divide by num_days to find average
  average_hour <- function(x) sum(x) * 2 / num_days
  
  ave_sleep = merge(
    unique(ave_sleep[1:2]), 
    aggregate(ave_sleep[3:34], by = ave_sleep[1], FUN = average_hour),  
    by = "Date")
  
  # Sort hour in ascending order
  ave_sleep = ave_sleep[order(as.integer(ave_sleep$Date)), ]
  
  # Convert dead fly values to NA
  if (dead_flies > 0) {
    ave_sleep[c(dead_flies) + 2] <- NA
  }
  
  # Find average time slept for each hour of the day
  ave_sleep["Averages"] = rowMeans(ave_sleep[3:length(ave_sleep)], na.rm = TRUE)
  ave_sleep
})

total_ave_sleep <- reactive({
  ave_sleep <- ave_sleep()
  
  # Find average time slept during day, night, and total
  total_ave_sleep = aggregate(ave_sleep[3:35], 
                              by = ave_sleep[2], 
                              FUN = function(x) sum(x)) %>% 
    adorn_totals("row", na.rm = FALSE)
  
  total_ave_sleep
})

#====Get latency for each night for each fly====
latency <- reactive({
  sleep <- format_sleep()
  dead_flies <- dead_flies()
  dates <- date_range()
  dates <- dates[-c(length(dates))]
  num_days <- length(dates) 

  nightly_latency <- get.latency(sleep, dates, num_days)
  
  # Convert dead fly values to NA
  if (dead_flies > 0) {
    nightly_latency[c(dead_flies) + 1] <- NA
  }
  
  # Find average latency for each night
  nightly_latency["Averages"] = rowMeans(nightly_latency[2:length(nightly_latency)],
                                         na.rm = TRUE)
  
  nightly_latency
})

#====Get specific activity for each fly====
format_activity <- reactive({
  raw <- format_raw_filter()
  hours <- input$experimentLength * 24 - 1
  
  # Change date column to string with only date and hour
  agg <- raw %>%
    mutate(Date = paste0(date(raw$Date), " ", hour(raw$Date)))
  
  # Merge sleep by each hour of each day
  agg = merge(
    unique(agg[1:2]), 
    aggregate(agg[3:34], 
              by = agg[1], 
              FUN = function(x) sum(x)),  
    by = "Date")
  
  # Sort date and hour in ascending order
  agg = agg[mixedorder(gsub('-', '', agg$Date)), ]
  
  dead_flies <- dead_flies()
  
  if (dead_flies > 0) {
    agg[c(dead_flies) + 2] <- NA
  }
  
  columns <- c(as.character(seq(1, 32)))
  agg <- agg %>% 
    mutate(Mean = rowMeans(.[columns], na.rm = TRUE),
           std = rowSds(as.matrix(.[columns]), na.rm = TRUE),
           se = std / sqrt(32 - length(dead_flies)))
  
  agg$Date <- factor(agg$Date, levels = agg$Date, ordered = TRUE)
  agg$hours <- seq(from = 0, to = hours, by = 1)
  
  agg
  
})
waking_time <- reactive({
  total_agg_sleep <- total_agg_sleep()
  raw <- format_raw_filter()
  
  # Find total time of experiment (in minutes)
  total_time <- as.numeric(difftime(raw$Date[length(raw$Date)], 
                                    raw$Date[1], 
                                    units = "mins"))
  
  # Convert total sleep to total awake for day, night, and total
  waking_time = total_agg_sleep
  waking_time[1:2, 2:33] = -1 * (total_agg_sleep[1:2, 2:33] - total_time / 2)
  waking_time[3, 2:33] = -1 * (total_agg_sleep[3, 2:33] - total_time)
  waking_time
})

total_activity <- reactive({
  # Create raw data df but with dead flies
  raw_na <- format_raw_filter()
  dead_flies <- dead_flies()
  
  if (dead_flies > 0) {
    raw_na[c(dead_flies) + 2] <- NA
  }
  
  # Convert raw data to total beam breaks for day, night, and total
  total_activity <- aggregate(raw_na[3:34],
                              by = raw_na[2],
                              FUN = function(x) sum(x)) %>%
    adorn_totals("row", na.rm = FALSE)
  
  # Find average beam breaks for day, night, and total
  total_activity["Averages"] = rowMeans(total_activity[2:length(total_activity)],
                                        na.rm = TRUE)
  total_activity
})

# Find activity rate by dividing total beam breaks by minutes awake 
activity_rate <- reactive({
  total_activity <- total_activity()
  waking_time <- waking_time()
  activity_rate <- cbind(total_activity[1], 
                         round(total_activity[-1] / waking_time[-1],1))
  activity_rate
})

#====Find Bouts====
get_bouts <- reactive({
  sleep <- format_sleep()
  
  # Find bout length and starting time for each fly
  bouts <- find.bouts(sleep)
  
  # Merge bouts for each fly
  all_bouts <- bouts %>% reduce(full_join) %>% dplyr::select(-c("idx"))
})

#====Find arousal threshold====

find_arousal <- reactive({
  shake_time <- NULL
  sleep <- format_sleep()
  raw <- format_raw_filter()
  dead_flies <- dead_flies()
  dates <- unique(date(raw$Date))
  num_days = length(dates)
  
  # Find arousal threshold if parameter given
  if (!is.null(shake_time)) {
    
    # Find index of each shake
    shake_idx <- which(hour(sleep$Date) %in% shake_time)
    shakes <- find.each_shake(num_days * length(shake_time))
    
    # Create dataframe from indices of shakes
    shake_results <- sleep[shake_idx[shakes],]
    
    # Find number of flies that woke up given that they were asleep at shake
    arousal_results <- find.arousal(shake_results, 
                                    dead_flies, 
                                    dates, 
                                    num_days, 
                                    length(shake_time))
    
    # Convert dead flies to NA
    if (dead_flies > 0) {
      arousal_results[c(dead_flies) + 1] <- NA
    }
    arousal_results
  }
})


findBoutAverages <- reactive({
  bouts <- get_bouts()
  dead_flies <- dead_flies()
  
  boutAverages <- find.boutAverages(bouts, dead_flies)
})

#====Outputs====

summarizer <- reactive({
  num_days <- length(date_range()) - 1
  shake_time <- NULL
  # Load required dataframes
  activity_rate <- activity_rate()
  total_agg_sleep <- total_agg_sleep()
  total_ave_sleep <- total_ave_sleep()
  agg_date_sleep <- format_agg_date_sleep()
  nightly_latency <- latency()
  total_activity <- total_activity()
  
  cnames <- c("Period", as.character(seq(1, length(activity_rate) - 2)), "Averages")
  total_agg_sleep$Type <- "Total sleep (min)"
  total_agg_sleep <- total_agg_sleep[c("Type", setdiff(names(total_agg_sleep), "Type"))]
  
  total_ave_sleep$Type <- "Mean sleep (min/period)"
  total_ave_sleep <- total_ave_sleep[c("Type", setdiff(names(total_ave_sleep), "Type"))]
  
  agg_date_sleep$Type <- "Sleep per date (min)"
  agg_date_sleep$Period <- as.character(agg_date_sleep$Period)
  agg_date_sleep <- agg_date_sleep[c("Type", setdiff(names(agg_date_sleep), "Type"))]
  
  colnames(nightly_latency) <- cnames
  nightly_latency$Period <- as.character(nightly_latency$Period)
  nightly_latency$Type <- "Sleep latency (min)"
  nightly_latency <- nightly_latency[c("Type", setdiff(names(nightly_latency), "Type"))]
  
  total_activity$Type <- "Activity (average beam breaks/period)"
  total_activity <- total_activity[c("Type", setdiff(names(total_activity), "Type"))]
  total_activity[3:length(total_activity)] <- total_activity[3:length(total_activity)] / num_days
  
  colnames(activity_rate) <- cnames
  activity_rate$Type <- "Activity rate (beams breaks/min awake)"
  activity_rate <- activity_rate[c("Type", setdiff(names(activity_rate), "Type"))]
  
  if (!is.null(shake_time)) {
    arousal_results <- find_arousal()
    colnames(arousal_results) <- cnames
    arousal_results$Period <- as.character(arousal_results$Period)
    arousal_results$Type <- "Arousal results"
    arousal_results <- arousal_results[c("Type", setdiff(names(arousal_results), "Type"))]
    
    summary_stats <- rbind(
      total_agg_sleep,
      agg_date_sleep,
      total_ave_sleep,
      total_activity,
      activity_rate,
      nightly_latency,
      arousal_results)
  } else {
    summary_stats <- rbind(
      total_agg_sleep,
      agg_date_sleep,
      total_ave_sleep,
      total_activity,
      activity_rate,
      nightly_latency)
  }
})

output$summary <- renderTable({
  go_on_files()
  summarizer()
  }
)

output$downloadSummary <- downloadHandler(
  filename = function() {
    paste(input$dateStart, "_DAM-summary_", input$experimentLength, "days.", input$monitorNumber, ".csv", sep = "")
  },
  content = function(file) {
    summary <- summarizer()
    write.csv(summary, file)
  }
)

# output$bouts <- renderTable({
#   go_on_files()
#   bouts <- get_bouts()
#   head(bouts)
# })

output$bouts <- renderTable({
  go_on_files()
  boutAverages <- findBoutAverages()
})

output$downloadBouts <- downloadHandler(
  filename = function() {
    paste(input$dateStart, "_DAM-bouts_", input$experimentLength, "days.", input$monitorNumber, ".csv", sep = "")
  },
  content = function(file) {
    bouts <- get_bouts()
    write.csv(bouts, file)
  }
)

output$downloadBoutAverages <- downloadHandler(
  filename = function() {
    paste(input$dateStart, "_DAM-bout-averages_", input$experimentLength, "days.", input$monitorNumber,
          ".csv", sep="")
  },
  content = function(file) {
    bouts <- get_bouts()
    dead_flies <- dead_flies()
    boutAverages <- find.boutAverages(bouts, dead_flies)
    write.csv(boutAverages, file)
  }
)

output$ave_agg_sleep <- renderPlot({
  go_on_files()
  ave_agg_sleep <- format_agg_ave_sleep()
  
  plot_sleep_gram(ave_agg_sleep)
})

output$downloadSleepGram <- downloadHandler(
  filename = function() {
    paste(input$dateStart, "_sleep-o-gram_", input$experimentLength, "days.", input$monitorNumber, ".png", sep = "")
  },
  content = function(file){
    ave_agg_sleep <- format_agg_ave_sleep()
    ggsave(file, plot=plot_sleep_gram(ave_agg_sleep), units = "in", height = 10, width = 20)
  }
)

output$downloadSleepGramData <- downloadHandler(
  filename = function() {
    paste(input$dateStart, "_sleep-o-gram_", input$experimentLength, "days.", input$monitorNumber, ".csv", sep = "")
  },
  content = function(file){
    ave_agg_sleep <- format_agg_ave_sleep()
    write.csv(ave_agg_sleep, file)
  }
)

output$activity_graph <- renderPlot({
  go_on_files()
  activity <- format_activity()
  
  plot_act_gram(activity)
})

output$downloadActGram <- downloadHandler(
  filename = function() {
    paste(input$dateStart, "_act-o-gram_", input$experimentLength, "days.", input$monitorNumber, ".png", sep = "")
  },
  content = function(file){
    activity <- format_activity()
    ggsave(file, plot=plot_sleep_gram(activity), units = "in", height = 10, width = 20)
  }
)

output$downloadActGramData <- downloadHandler(
  filename = function() {
    paste(input$dateStart, "_act-o-gram_", input$experimentLength, "days.", input$monitorNumber, ".csv", sep = "")
  },
  content = function(file){
    activity <- format_activity()
    write.csv(activity, file)
  }
)

output$ave_sleep <- renderTable({
  go_on_files()
  ave_sleep <- ave_sleep()
  names(ave_sleep)[1] <- "Hour"
  head(ave_sleep)
})

output$downloadAveSleep <- downloadHandler(
  filename = function() {
    paste(input$dateStart, "_DAM-ave-sleep_", input$experimentLength, "days.", input$monitorNumber, ".csv", sep = "")
  },
  content = function(file) {
    ave_sleep <- ave_sleep()
    names(ave_sleep)[1] <- "Hour"
    write.csv(ave_sleep, file)
  }
)