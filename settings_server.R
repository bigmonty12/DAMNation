# settings_server.R
# Home page of Shiny app

source("DAM-functions.R")

#====Input Preprocessing====
# Generate list of input files
inFile <- reactive(input$fin)

# Find number of file inputs (needed to know how many total wells uploaded)
findFlyNumber <- reactive({
  monitors <- inFile()
  numFiles <- length(monitors$name)
  totalFlies <- 32 * numFiles
})

# Number of readings per 24 hours
dataFreq <- reactive(1440 / as.numeric(input$data_recording_frequency))

# Press status check button to check for DAM errors
shinyjs::hide("go")
statusButton <- eventReactive(input$status, {
  inFile()
  shinyjs::show("go")
})

# Press go to start analysis after data check
goButton <- eventReactive(input$go, {
  inFile()
})

observeEvent(input$go, {
  updateTabItems(session, "tabs", "analysis")
})

# Get dates of experiment
dateRange <- reactive({
  dates <- as.character(as.Date(
    seq(from = input$range[[1]], to = input$range[[2]] + 1, by = 1)))
  dates
})
#====Conditions====
# How many different conditions
findNumConditions <- reactive({
  numConditions <- as.integer(input$numConditions)
})

findNumRanges <- reactive({
  numRanges <- as.integer(input$numberRangesCondition)
})

output$conditions <- renderUI({
  numConditions <- findNumConditions()
  numRanges <- findNumRanges()
  numFlies <- findFlyNumber()
  lapply(1:numConditions, function(i) {
    dropdown(
      inputId = paste0("dropdownCondition", i),
      label = paste0("Condition/Genotype #", i),
      icon = icon("sliders"),
      status = "primary",
      circle = FALSE,
      textInput(
        inputId = paste0("nameCondition", i),
        label = "Condition/Genotype Name"
      ),
      lapply(1:numRanges, function(j){
        numericRangeInput(
          inputId = paste0("wellsCondition", i, "range", j),
          label = paste0("Channels with Condition/Genotype #", i),
          value = c(1, numFlies)
        )
      })
    )
  })
})

# Get wells associated with each conditions
getConditionNames <- reactive({
  numConditions <- findNumConditions()
  names <- list()
  names <- lapply(1:numConditions, function(i) {
    names[[ i ]] <- input[[paste0('nameCondition', i)]]
  })
  names
})

wellConditions <- reactive({
  numConditions <- findNumConditions()
  numRanges <- findNumRanges()
  conditions <- list()
  names <- list()
  conditions <- lapply(1:numConditions, function(i) {
    ranges <- list()
    ranges <- lapply(1:numRanges, function(j){
      range <- input[[ paste0('wellsCondition', i, 'range', j)]]
      ranges[[ j ]] <- range[[1]]:range[[2]]
    })
    conditions[[ i ]] <- unique(unlist(ranges))
  })
  names(conditions) <- getConditionNames()
  conditions
})

#====Set Monitor Order====
# Interactive way to create UI for selection of monitor order
output$monitorOrder <- renderUI({
  monitorFiles <- inFile()
  lapply(setdiff(1:length(monitorFiles$name), 0), function(i){
    selectInput(paste0('Monitor_order', i), paste('Monitor file order:', i), monitorFiles$name, selected = monitorFiles$name[i])
  })
})


# A vector of desired monitor order
desiredMonitorOrder <- reactive({
  monitors <- inFile()
  desiredMonitorOrder <- unlist(lapply(1:length(monitors$name), function(i){
    input[[paste0("Monitor_order", i)]]
  }))
  desiredMonitorOrder
})

# List of monitor files in user specified order
monitor.list <- reactive({
  fin <- input$fin
  finOrdered <- fin[match(desiredMonitorOrder(), fin$name),]
  monitor.list <- lapply(finOrdered$datapath, read.table, fill=TRUE)
  monitor.list
})

# Get short monitor file names
monitorFilenameShort <- reactive({
  sub(".txt", "", desiredMonitorOrder())
})

# Remove unnecessary columns from monitor objects in list
monitor.list.edited <- reactive({
  lapply(monitor.list(), function(x) x[!(names(x) %in% c("V7", "V8","V9", "V10","V11", "V12"))])
  })

# Merge data frames of monitor files into one data frame
mergeMonitors <- reactive({
  withProgress(
    message = 'Reading data', {
      incProgress(1, detail = paste("In progress"))
      
      Reduce(function(x, y) merge(x, y, all=TRUE, by=c("V1", "V2", "V3", "V4", "V5"), sort = FALSE),
             monitor.list.edited(), accumulate=FALSE)
    }
  )
})

# Get error code columns
monitorStatus <- reactive({
  monitors <- mergeMonitors()
  status <- monitors[,c(2, 3, 4, 5, grep(glob2rx("V6*"), colnames(mergeMonitors())))]
  status <- status %>% tidyr::unite("Date", 1:3, sep = " ", remove = TRUE) %>% select(Date, everything())
  status$Date <- as.character(as.POSIXct(strptime(status$Date, format = "%d %b %y")))
  colnames(status) <- c("Date", "Time", paste(monitorFilenameShort(), "Status", sep = "_"))
  status
})

# Remove error code columns and first column
monitorsNoStatus <- reactive({
  m <- mergeMonitors()[,-grep(glob2rx("V6*"), colnames(mergeMonitors()))] %>%
    dplyr::select(-c(1))
  channels <- m[5:ncol(m)]
  fileNameShort <- monitorFilenameShort()
  colnames(channels) <- paste(rep(fileNameShort, each=32), colnames(channels), sep="_")
  colnames(channels) <- gsub("\\V.{1,}", "ch", colnames(channels))
  colnames(channels) <- paste(grep(glob2rx("*_"), "", colnames(channels)), colnames(channels), c(1:32), sep="")
  colnames(m) <- c("Day", "Month", "Year", "Time", colnames(channels))
  m
})

formatDate <- reactive({
  raw <- monitorsNoStatus()
  status <- monitorStatus()
  raw <- raw %>% tidyr::unite("Date", Day:Year, sep = " ", remove = TRUE)
  raw$Date <- as.character(as.POSIXct(strptime(raw$Date, format = "%d %b %y")))
  df <- merge(raw, status, all = TRUE, sort = FALSE, by = c("Date", "Time")) %>%
    select(Date, Time, colnames(status), everything())
  df
})

#==== Format Raw Data by User Inputs====
# Format date and time
filterDates <- reactive({
  raw <- formatDate()
  dates <- dateRange()
  raw <- raw %>% filter(Date %in% dates)
  raw
})

checkStatusTable <- reactive({
  status <- filterDates()
  status <- data.table(status[,c(1, 2, grep(glob2rx("*Status*"), colnames(status)))])
  meltedStatus <- as.data.frame(melt.data.table(status, id.vars = c(1, 2)))
  meltedStatus
})

checkStatus <- reactive({
  meltedStatus <- checkStatusTable()
  bad <- filter(meltedStatus, value != 1)
  numBad <- nrow(bad)
  daysBad <- unique(bad$Date)
  monBad <- unique(bad$variable)
  monBad <- gsub("*_Status", "", monBad)
  isComplete <- as.numeric(dataFreq()) * length(dateRange()) == nrow(filterDates())
  
  if (!isComplete){
    print("WARNING! The full selected dates are not available in the data.")
    print("Check that there is data for the entirety of each selected date.")
  } else {
    if (all(meltedStatus$value == 1)){
      print("Good to go! Looks like the DAM system recorded the whole time for each monitor")
    } else {
      print("WARNING! DAM system may have stopped working during experiment.")
      print("If you want to continue to analysis, the rows with errors will be deleted. This could negatively impact results.")
      print("Errors were found in the following monitors: ")
      print(paste(monBad, collapse = ", "))
      print("Dates when errors occurred:")
      print(paste(daysBad, collapse = ", "))
      print("The first and last five occurences of bad data: ")
      if (numBad < 11){
        print(bad[, 1:2])
      } else {
        print(bad[c(1:5, (numBad-5):numBad), 1:2])
      }
    }
  }
})

output$statusText <- renderPrint({
  statusButton()
  if (is.null(inFile()))
    return(NULL)
  isolate(checkStatus())
})

formatTimes <- reactive({
  raw <- filterDates()
  # Combine date with time
  raw$DateTime <- with(raw, paste0(Date, " ", Time))
  raw$DateTime <- strftime(raw$DateTime, "%Y-%m-%d %H:%M:%S")

  # Convert ZT time to an integer
  zt <- as.numeric(gsub(":00", "", input$light_onset_time))

  raw <- raw %>%
    mutate(DateTime = as.character(as.POSIXct(strptime(DateTime, "%Y-%m-%d %H:%M:%S")) - dhours(zt)))
  
  # Create night and day category, convert to ZT time
  time <- (hour(raw$DateTime) + minute(raw$DateTime)/60)
  
  raw <- raw %>%
    mutate(Period = ifelse(time > 0 & time <= 12,
                           "Day",
                           "Night")) %>%
    mutate(DateTime = as.character(as.POSIXct(strptime(DateTime, "%Y-%m-%d %H:%M:%S")) - dminutes(as.numeric(input$data_recording_frequency))))
  raw <- raw[-c(1,2)] %>% select(DateTime, Period, everything())
  raw
})

# Subset by condition
subsetCondition <- reactive({
  raw <- formatTimes()
  numConditions <- findNumConditions()
  conditions <- wellConditions()
  monitors <- inFile()
  numFiles <- length(monitors$name)
  
  df.list <- list()
  df.list <- lapply(1:numConditions, function(i){
    df.list[[i]] <- raw[,c(1,2, conditions[[i]] + 2 + numFiles)]
  })
  df.list
})

# Separate function to filter by light onset
filterLightOnset <- reactive({
  rawList <- subsetCondition()
  
  filter.list <- list()
  filter.list <- lapply(1:findNumConditions(), function(i){
    raw <- rawList[[i]]
    begin <- which(minute(raw$DateTime) == 0 & hour(raw$DateTime) == 0) 
    start <- as.numeric(begin[1]) 
    end <- as.numeric(begin[length(begin)]) - 1
    raw <- raw[start:end,]
    filter.list[[i]] <- raw
    filter.list[[i]]
  })
  filter.list
})