# analysis_server.R
# Second tab of Shiny app

source("DAM-functions.R")
#====Sleep Data====

analysisLoadingScreen <- tagList(
  h3("Analyzing Data...", style = "color:gray;"),
  img(src="logo.png", height = "300")
)

observeEvent(input$go, {
  waiter::waiter_show(html = analysisLoadingScreen)
  updateTabItems(session, "tabs", "analysis")
  averagesSummarizer()
  dailySummarizer()
  aveSleepDT()
  formatActivity()
  waiter::waiter_hide()
})

formatSleep <- reactive({
  rawConditions <- filterLightOnset()
  sleep <- list()
  sleep <- lapply(1:findNumConditions(), function(i){
    numCols <- ncol(rawConditions[[i]]) 
    sleep[[i]] <- rawConditions[[i]] %>%
      mutate_at(vars(3:all_of(numCols)), list(~ ifelse( . == 0, 1, 0)))
    sleep[[i]][3:numCols] <- apply(sleep[[i]][3:numCols], 2, find.sleep, freq = as.numeric(input$data_recording_frequency))
    sleep[[i]]
  })
  sleep
})
#====Convert sleep data into sleep in min/hour for each hour for each fly====

formatAggSleep <- reactive({
  sleep <- formatSleep()
  
  agg <- list()
  agg <- lapply(1:findNumConditions(), function(i){
    numCols <- ncol(sleep[[i]]) 
    
    # Change date column to string with only date and hour
    agg[[i]] <- sleep[[i]] %>%
      mutate(DateTime = paste0(date(sleep[[i]]$DateTime), " ", hour(sleep[[i]]$DateTime)))
    
    # Merge sleep by each hour of each day
    agg[[i]] = merge(
      unique(agg[[i]][1:2]),
      aggregate(agg[[i]][3:numCols], by = agg[[i]][1], FUN = function(x) sum(x) * as.numeric(input$data_recording_frequency)),
      by = "DateTime")
    
    # Sort date and hour in ascending order
    agg[[i]] = agg[[i]][mixedorder(gsub('-', '', agg[[i]]$Date)), ]
    agg[[i]]
  })
  agg
  
})

formatAggAveSleep <- reactive({
  aggList <- formatAggSleep()
  deadFlies <- deadFlies()
  aggAve <- list()
  aggAve <- lapply(1:findNumConditions(), function(i){
    agg <- aggList[[i]]
    numCols <- ncol(agg) 
    dead <- deadFlies[[i]]
    if (dead > 0){
      agg[c(dead) + 2] <- NA
    }
    names <- colnames(agg)
    cols <- seq(3, numCols)
    agg[, names[cols]] <- sapply(agg[, names[cols]], as.numeric)
    agg <- agg %>%
      mutate(Mean = rowMeans(.[cols], na.rm = TRUE),
             std = rowSds(as.matrix(.[cols]), na.rm = TRUE),
             se = std / sqrt((numCols-2) - length(dead)))
    agg$DateTime <- factor(agg$DateTime, levels = agg$DateTime, ordered = TRUE)
    aggAve[[i]] <- agg
    aggAve[[i]]
  })
  aggAve
})
#====Find Dead Flies====

# Get sleep totals for last 2 hours of experiment and after end of experiment
deadFlies <- reactive({
  dates <- dateRange()
  extraDay <- dates[length(dates)]
  raw <- subsetCondition()
  aggSleep <- formatAggSleep()
  
  deadFlies <- list()
  deadFlies <- lapply(1:findNumConditions(), function(i){
    lastDay <- datetime_separate(aggSleep[[i]])
    lastDay <- lastDay %>% filter(Date == dates[length(dates) - 1])  %>%
    dplyr::slice(23:24)
    numCol <- ncol(lastDay)
    rawExtra <- datetime_separate(raw[[i]])
    rawExtra <- rawExtra %>% filter(Date == extraDay)
     
    # # Return which flies are dead (if any)
    deadBefore <- apply(lastDay[4:numCol], 2, find.death.before)
    deadAfter <- apply(rawExtra[4:numCol], 2, find.death.after)
    deadIdx <- deadBefore & deadAfter

    if (any(deadIdx)) {
      deadFlies[[i]] <- which(deadIdx)
    } else {
      deadFlies[[i]] <- 0
    }
    deadFlies[[i]]
  })
  deadFlies
})

#====Aggregated sleep data with dead flies====

# Total aggregated sleep with dead flies
totalAggSleep <- reactive({
  aggList <- formatAggSleep()
  deadFlies <- deadFlies()
  
  totalAgg <- list()
  totalAgg <- lapply(1:findNumConditions(), function(i){
    agg <- aggList[[i]]
    numCols <- ncol(agg) 
    dead <- deadFlies[[i]]
    
    if (dead > 0){
      agg[c(dead) + 2] <- NA
    }
    
    # Find average of flies for each hour of each day
    agg["Averages"] <- rowMeans(agg[3:numCols], na.rm = TRUE)
    
    # Find total sleep during day, night, and total
    agg <- aggregate(agg[3:(numCols+1)],
                           by = agg[2],
                            FUN = function(x) sum(x))  %>%
       adorn_totals("row", na.rm = FALSE)
    
    totalAgg[[i]] <- agg
  })
  totalAgg
})

# Convert sleep data into aggregated sleep by date
formatAggDateSleep <- reactive({
  sleepList <- formatSleep()
  deadFlies <- deadFlies()
  
  aggDateSleep <- list()
  aggDateSleep <- lapply(1:findNumConditions(), function(i){
    sleep <- sleepList[[i]]
    numCol <- ncol(sleep)
    dead <- deadFlies[[i]]
    
    aggDate <-aggregate(sleep[3:numCol],
                        by = list(date(sleep$DateTime)),
                        FUN = function(x) sum(x) * 2)
    names(aggDate)[1] <- "Period"
    aggDate["Averages"] <- rowMeans(aggDate[3:length(aggDate)], na.rm = TRUE)
    if (dead > 0){
      aggDate[c(dead) + 1] <- NA
    }
    aggDateSleep[[i]] <- aggDate
    aggDateSleep[[i]]
  })
  aggDateSleep
  
})

#====Convert sleep data into average sleep in min/hour for each hour of ZT time====

aveSleep <- reactive({
  sleepList <- formatSleep()
  deadFlies <- deadFlies() 
  numDays <- length(dateRange()) - 1
  
  # Merge sleep by each hour of the day and divide by numDays to find average
  averageHour <- function(x) sum(x) * as.numeric(input$data_recording_frequency) / numDays
  
  aveSleepList <- list()
  aveSleepList <- lapply(1:findNumConditions(), function(i){
    sleep <- sleepList[[i]]
    numCol <- ncol(sleep)
    dead <- deadFlies[[i]]
    
    # Change Date column to hour string
    aveSleep <- sleep %>%
      mutate(DateTime = paste0(hour(sleep$DateTime)))
    
    aveSleep <- merge(
      unique(aveSleep[1:2]), 
      aggregate(aveSleep[3:numCol], by = aveSleep[1], FUN = averageHour),  
      by = "DateTime")
    
    # Sort hour in ascending order
    aveSleep <- aveSleep[order(as.integer(aveSleep$DateTime)), ]
    
    # Convert dead fly values to NA
    if (dead > 0) {
      aveSleep[c(dead) + 2] <- NA
    }
    
    # Find average time slept for each hour of the day
    aveSleep["Averages"] <- rowMeans(aveSleep[3:numCol], na.rm = TRUE)
    aveSleepList[[i]] <- aveSleep
    aveSleepList[[i]]
  })
  aveSleepList
})

aveSleepDT <- reactive({
  sleepList <- aveSleep()
  conditionNames <- getConditionNames()
  
  DTList <- list()
  DTList <- lapply(1:findNumConditions(), function(i){
    df <- sleepList[[i]]
    colnames(df)[colnames(df) == 'DateTime'] <- 'Hour'
    df$Condition.Genotype <- conditionNames[[i]]
    df$Period <- NULL
    df$Averages <- NULL
    DTList[[i]] <- df %>% select(Hour, Condition.Genotype, everything())
  })
  DTList
})

aveSleepTransposed <- reactive({
  sleepList <- aveSleep()
  conditionNames <- getConditionNames()

  transposedList <- list()
  transposedList <- lapply(1:findNumConditions(), function(i){
    aveSleep <- sleepList[[i]]
    aveSleep[[2]] <- "Hour"
    aveSleep <- aveSleep %>% tidyr::unite(Hour, Period, DateTime, remove = TRUE, sep=" ")
    rownames(aveSleep) <- aveSleep$Hour
    aveSleep$Hour <- NULL
    aveSleepT <- as.data.frame(t(as.matrix(aveSleep)))
    aveSleepT$Condition.Genotype <- conditionNames[[i]]
    transposedList[[i]] <- aveSleepT %>% select(Condition.Genotype, everything())
  })
  transposedList
})

totalAveSleep <- reactive({
  aveSleepList <- aveSleep()
  
  totalAveSleepList <- list()
  totalAveSleepList <- lapply(1:findNumConditions(), function(i){
    aveSleep <- aveSleepList[[i]]
    
    # Find average time slept during day, night, and total
    totalAveSleepList[[i]] = aggregate(aveSleep[3:ncol(aveSleep)], 
                                by = aveSleep[2], 
                                FUN = function(x) sum(x)) %>% 
      adorn_totals("row", na.rm = FALSE)
    totalAveSleepList[[i]]
  })
  
  totalAveSleepList
})

#====Get latency for each night for each fly====
latency <- reactive({
  sleepList <- formatSleep()
  deadFlies <- deadFlies()
  dates <- dateRange()
  dates <- dates[-c(length(dates))]
  numDays <- length(dates)
  
  latencyList <- list()
  latencyList <- lapply(1:findNumConditions(), function(i){
    sleep <- sleepList[[i]]
    dead <- deadFlies[[i]]
    nightlyLatency <- get.latency(sleep, dates, numDays)
    
    # Convert dead fly values to NA
    if (dead > 0) {
      nightlyLatency[c(dead) + 1] <- NA
    }
    
    # Find average latency for each night
    nightlyLatency["Averages"] = rowMeans(nightlyLatency[2:length(nightlyLatency)],
                                           na.rm = TRUE)
    latencyList[[i]] <- nightlyLatency
    latencyList[[i]]
    
  })
  latencyList
})

#====Get specific activity for each fly====
formatActivity <- reactive({
  rawList <- filterLightOnset()
  deadList <- deadFlies()
  numDays <- length(dateRange()) - 1
  conditionNames <- getConditionNames()
  
  # Merge sleep by each hour of the day and divide by numDays to find average
  averageHour <- function(x) sum(x) / numDays

  aveActivityList <- list()
  aveActivityList <- lapply(1:findNumConditions(), function(i){
    raw <- rawList[[i]]
    numCol <- ncol(raw)
    dead <- deadList[[i]]

    # Change Date column to hour string
    aveActivity <- raw %>%
      mutate(DateTime = paste0(hour(raw$DateTime)))
    
    aveActivity <- merge(
      unique(aveActivity[1:2]), 
      aggregate(aveActivity[3:numCol], by = aveActivity[1], FUN = averageHour),  
      by = "DateTime")
    
    # Sort hour in ascending order
    aveActivity <- aveActivity[order(as.integer(aveActivity$DateTime)), ]
    
    # Convert dead fly values to NA
    if (dead > 0) {
      aveActivity[c(dead) + 2] <- NA
    }
    
    colnames(aveActivity)[colnames(aveActivity) == 'DateTime'] <- 'Hour'
    aveActivity$Condition.Genotype <- conditionNames[[i]]
    aveActivity$Period <- NULL
    
    # Find average time slept for each hour of the day
    aveActivityList[[i]] <- aveActivity %>% select(Hour, Condition.Genotype, everything())
    aveActivityList[[i]]
  })
  aveActivityList
})

wakingTime <- reactive({
  totalAggSleep <- totalAggSleep()
  
  # Find total time of experiment (in minutes)
  totalTime <- as.numeric(1440 * (length(dateRange())-1))
  
  wakingTimeList <- list()
  wakingTimeList <- lapply(1:findNumConditions(), function(i){
    wakingTime <- totalAggSleep[[i]]
    numCol <- ncol(wakingTime)
    
    # Convert total sleep to total awake for day, night, and total
    wakingTime[1:2, 2:numCol] = -1 * (totalAggSleep[[i]][1:2, 2:numCol] - totalTime/2)
    wakingTime[3, 2:numCol] = -1 * (totalAggSleep[[i]][3, 2:numCol] - totalTime)
    wakingTimeList[[i]] <- wakingTime
    wakingTimeList[[i]]
  })
  wakingTimeList
})

totalActivity <- reactive({
  # Create raw data df but with dead flies
  rawList <- filterLightOnset()
  deadFlies <- deadFlies()
  
  activityList <- list()
  activityList <- lapply(1:findNumConditions(), function(i){
    raw <- rawList[[i]]
    numCol <- ncol(raw)
    dead <- deadFlies[[i]]
    
    if (dead > 0) {
      raw[c(dead) + 2] <- NA
    }
    
    # Convert raw data to total beam breaks for day, night, and total
    raw <- aggregate(raw[3:numCol],
                                by = raw[2],
                                FUN = function(x) sum(x)) %>%
      adorn_totals("row", na.rm = FALSE)
    
    # Find average beam breaks for day, night, and total
    raw["Averages"] = rowMeans(raw[2:(numCol-1)], na.rm = TRUE)
    raw[2:numCol] <- raw[2:numCol] / (length(dateRange()) - 1)
    
    activityList[[i]] <- raw
    activityList[[i]]
  })
  activityList
})

# Find activity rate by dividing total beam breaks by minutes awake 
activityRate <- reactive({
  totalActivity <- totalActivity()
  wakingTime <- wakingTime()
  numDays <- length(dateRange()) - 1
  
  activityRateList <- list()
  activityRateList <- lapply(1:findNumConditions(), function(i){
    raw <- totalActivity[[i]]
    numCol <- ncol(raw)
    raw[2:numCol] <- raw[2:numCol] * numDays
    activityRateList[[i]] <- cbind(raw[1], round(raw[-1] / wakingTime[[i]][-1], 1))
    activityRateList[[i]]
  })
  activityRateList
})

#====Find Bouts====
getBouts <- reactive({
  sleepList <- formatSleep()
  
  boutsList <- list()
  boutsList <- lapply(1:findNumConditions(), function(i){
    # Find bout length and starting time for each fly
    bouts <- find.bouts(sleepList[[i]])
    
    # Merge bouts for each fly
    boutsList[[i]] <- bouts %>% reduce(full_join) %>% dplyr::select(-c("idx"))
    boutsList[[i]]
  })
  boutsList
})

findBoutAverages <- reactive({
  boutsList <- getBouts()
  deadFlies <- deadFlies()
  aveSumsList <- averagesSummarizer()

  boutAveragesList <- list()
  boutAveragesList <- lapply(1:findNumConditions(), function(i){
    aveSums <- aveSumsList[[i]]
    deathTimes <- aveSums[1:nrow(aveSums), 1]
    bouts <- boutsList[[i]]
    dead <- deadFlies[[i]]
    foundBoutsAves <- find.boutAverages(bouts, dead)
    foundBoutsAves$DeathTime <- deathTimes
    boutAveragesList[[i]] <- foundBoutsAves %>% select(Fly, DeathTime, everything())
    boutAveragesList[[i]]
  })
  boutAveragesList
})

#====Find Death Times====
getDeathTimes <- reactive({
  boutsList <- getBouts()
  deadFlies <- deadFlies()
  
  deathTimesList <- list()
  deathTimesList <- lapply(1:findNumConditions(), function(i){
    dead <- deadFlies[[i]]
    bouts <- boutsList[[i]]
    starts <- bouts[,seq(1, ncol(bouts), 4)]
    deathTimes <- as.character(sapply(starts, function(x) x[max(which(!is.na(x)))]))
    deathTimes <- as.data.frame(t(deathTimes))
    
    if(dead > 0){
      deathTimes[-c(dead)] <- NA
    }else{
      deathTimes[c(seq(1,length(deathTimes)))] <- NA
    }
    deathTimesList[[i]] <- deathTimes
    deathTimesList[[i]]
  })
  deathTimesList
})
#====Find arousal threshold====

findArousal <- reactive({
  shakeTime <- NULL
  sleepList <- formatSleep()
  rawList <- filterLightOnset()
  deadFlies <- deadFlies()
  dates <- dateRange()
  dates <- dates[-c(length(dates))]
  numDays <- length(dates)

  arousalList <- list()
  arousalList <- lapply(1:findNumConditions(), function(i){
    # Find arousal threshold if parameter given
    if (!is.null(shakeTime)){
      sleep <- sleepList[[i]]
      dead <- deadFlies[[i]]
      
      # Find index of each shake
      shakeIdx <- which(hour(sleep$DateTime) %in% shakeTime)
      shakes <- find.each_shake(numDays * length(shakeTime))
      
      # Create dataframe from indices of shakes
      shakeResults <- sleep[shakeIdx[shakes],]
      
      # Find number of flies that woke up given that they were asleep at shake
      arousalResults <- find.arousal(shakeResults, dead, dates, numDays, length(shakeTime))
      
      # Convert dead flies to NA
      if (dead > 0) {
        arousalResults[c(dead) + 1] <- NA
      }
      arousalList[[i]] <- arousalResults
      arousalList[[i]]
    }
  })
  arousalList
})

#====Outputs====
averagesSummarizer <- reactive({
  # Load required dataframes
  deathTimes <- getDeathTimes()
  activityRate <- activityRate()
  totalAggSleep <- totalAggSleep()
  totalAveSleep <- totalAveSleep()
  totalActivity <- totalActivity()
  
  conditionNames <- getConditionNames()

  averageSumList <- list()
  averageSumList <- lapply(1:findNumConditions(), function(i){
    colNames <- colnames(totalAggSleep[[i]])
    colnames(deathTimes[[i]]) <- colNames[2:(length(colNames)-1)]
    deathTimes[[i]]$Period <- "Time of"
    deathTimes[[i]]$Type <- "Death"
    deathTimes[[i]]$Averages <- NA
    totalAggSleep[[i]]$Type <- "Total sleep (min)"
    totalAveSleep[[i]]$Type <- "Mean sleep (min/period)"
    totalActivity[[i]]$Type <- "Activity average (average beam breaks/period)"
    activityRate[[i]]$Type <- "Activity rate (beams breaks/min awake)"
    averageSummarized <- rbind(deathTimes[[i]], totalAggSleep[[i]], totalAveSleep[[i]], totalActivity[[i]], activityRate[[i]])
    averageSummarized <- averageSummarized %>% tidyr::unite(Value, Period, Type, remove = TRUE, sep=" ")
    rownames(averageSummarized) <- averageSummarized$Value
    averageSummarized$Value <- NULL
    averageSummarizedT <- as.data.frame(t(as.matrix(averageSummarized)))
    averageSummarizedT$Condition.Genotype <- conditionNames[[i]]
    averageSumList[[i]] <- averageSummarizedT %>% select(Condition.Genotype, everything()) %>% slice(1:(n()-1))
  })
  averageSumList
})

dailySummarizer <- reactive({
  shakeTime <- NULL
  # Load required dataframes
  aggDateSleep <- formatAggDateSleep()
  nightlyLatency <- latency()
  if (!is.null(shakeTime)) {
    arousalResults <- findArousal()
  }
  
  conditionNames <- getConditionNames()
  
  dailySumList <- list()
  dailySumList <- lapply(1:findNumConditions(), function(i){
    colNames <- colnames(aggDateSleep[[i]])
    colnames(nightlyLatency[[i]]) <- colNames
    aggDateSleep[[i]]$Type <- "Total Sleep (min)"
    nightlyLatency[[i]]$Type <- "Sleep latency (min)"
    
    if (!is.null(shakeTime)){
      colnames(arousalResults[[i]]) <- colNames
      arousalResults[[i]]$Type <- "Arousal Results"
      dailySummarized <- rbind(aggDateSleep[[i]], nightlyLatency[[i]], arousalResults[[i]])
    } else {
      dailySummarized <- rbind(aggDateSleep[[i]], nightlyLatency[[i]])
    }
    
    dailySummarized <- dailySummarized %>% tidyr::unite(Value, Period, Type, remove = TRUE, sep=" ")
    rownames(dailySummarized) <- dailySummarized$Value
    dailySummarized$Value <- NULL
    dailySummarizedT <- as.data.frame(t(as.matrix(dailySummarized)))
    dailySummarizedT$Condition.Genotype <- conditionNames[[i]]
    dailySumList[[i]] <- dailySummarizedT %>% select(Condition.Genotype, everything()) %>% slice(1:(n()-1))
  })
  dailySumList
})
#====Shiny Outputs====

output$previews <- renderUI({
  numConditions <- findNumConditions()
  names <- getConditionNames()
  vals <- 1:5
  tabBoxList <- list()
  tabBoxList <- lapply(1:numConditions, function(i) {

    output[[paste0('averageSummarizer', i)]] <- DT::renderDataTable({
      goButton()
      isolate(DT::datatable(data = averagesSummarizer()[[i]], options = list(scrollX=TRUE, pageLength = 5)))
    })
    output[[paste0('dailySummarizer', i)]] <- DT::renderDataTable({
      goButton()
      isolate(DT::datatable(data = dailySummarizer()[[i]], options = list(scrollX=TRUE, pageLength = 5)))
    })
    output[[paste0('averageSleep', i)]] <- DT::renderDataTable({
      goButton()
      isolate(DT::datatable(data = aveSleepDT()[[i]], options = list(scrollX=TRUE, pageLength = 5)))
    })
    output[[paste0('averageActivity', i)]] <- DT::renderDataTable({
      goButton()
      isolate(DT::datatable(data = formatActivity()[[i]], options = list(scrollX=TRUE, pageLength = 5)))
    })
    
    tabBoxList[[i]] <- fluidRow(
      tabBox(
        id = paste0("tabset", i),
        title = names[[i]],
        tabPanel(title = paste(names[[i]], "Activity and Sleep Average Values"), DT::dataTableOutput(paste0("averageSummarizer",i))),
        tabPanel(title = paste(names[[i]], "Activity and Sleep Daily Values"), DT::dataTableOutput(paste0("dailySummarizer",i))),
        tabPanel(title = paste(names[[i]], "Sleep Average (min) by Hour of Day"), DT::dataTableOutput(paste0("averageSleep",i))),
        tabPanel(title = paste(names[[i]], "Average Beam Breaks by Hour of Day"), DT::dataTableOutput(paste0("averageActivity",i))),
        width = 12
      )
    )
    tabBoxList[[i]]
  })
  tabBoxList
})