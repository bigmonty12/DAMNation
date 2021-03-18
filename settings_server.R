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
})

# Press go to start analysis after data check
goButton <- eventReactive(input$go, {
  inFile()
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

output$conditions <- renderUI({
  numConditions <- findNumConditions()
  monitors <- monitorFilenameShort()
  lapply(1:numConditions, function(i) {
    dropMenu(
      placement = "right",
      actionBttn(
        inputId = paste0("dropdownCondition", i),
        label = paste0("Condition/Genotype #", i),
        icon = icon("sliders"),
        color = "primary",
        style = "material-flat",
        size = "sm"
      ),
      circle = FALSE,
      div(style='max-height: 80vh; overflow-y: auto;',
          
          textInput(
            inputId = paste0("nameCondition", i),
            label = "Condition/Genotype Name"
          ),
          lapply(1:length(monitors), function(j) {
            list(
              h4(monitors[j]),
              selectInput(
                inputId = paste0(monitors[j], i),
                label = "Select Grouping",
                choices = c("1-32", "1-16", "17-32", "Manually Select")
              ), 
              pickerInput(
                inputId = paste0(monitors[j], "choices", i),
                label = "Channels",
                choices = c(Choose = '', 1:32),
                multiple = TRUE,
                selected = 1:32,
                options = list(
                  `selected-text-format` = "count > 3",
                  `actions-box` = TRUE)
              )
            )
          })
      )
    )
  })
})

observe({
  lapply(1:findNumConditions(), function(i) {
    lapply(1:length(monitorFilenameShort()), function(j) {
      x <- input[[paste0(monitorFilenameShort()[j], i)]]
      y <- NULL
      
      if (is.null(x)){
        x <- '1-32'
      } else if (x == '1-16'){
        y <- c(1:16)
      } else if (x == '17-32'){
        y <- c(17:32)
      } else if (x == '1-32'){
        y <- c(1:32)
      } else {
        y <- character(0)
      }
      
      if (!is.null(y)) {
        updateSelectInput(session, paste0(monitorFilenameShort()[j], "choices", i),
                          label = NULL,
                          selected = y)
      }
    })
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
  monitors <- monitorFilenameShort()
  conditions <- list()
  names <- list()
  conditions <- lapply(1:numConditions, function(i) {
    ranges <- list()
    ranges <- lapply(1:length(monitors), function(j){
      range <- as.integer(input[[ paste0(monitors[j], "choices", i) ]])
      range <- ((j - 1) * 32) + range
      ranges[[ j ]] <- range
    })
    conditions[[ i ]] <- unique(unlist(ranges))
  })
  names(conditions) <- getConditionNames()
  conditions
})

#====Set Monitor Order====

# A vector of desired monitor order
desiredMonitorOrder <- reactive({
  monitors <- inFile()
  monitors$name
})

# List of monitor files in user specified order
monitor.list <- reactive({
  monitors <- inFile()
  monitor.list <- lapply(monitors$datapath, read.table, fill=TRUE)
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
    Reduce(function(x, y) merge(x, y, all=TRUE, by=c("V1", "V2", "V3", "V4", "V5"), sort = FALSE),
             monitor.list.edited(), accumulate=FALSE)
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
  isComplete <- as.numeric(dataFreq()) * (length(dateRange())-1) <= nrow(filterDates())
  
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

dataLoadingScreen <- tagList(
  h3("Checking Data Status...", style = "color:gray;"),
  img(src="logo.png", height = "300")
)

observeEvent(input$status, {
  waiter::waiter_show(html = dataLoadingScreen)
  meltedStatus <- checkStatusTable()
  bad <- filter(meltedStatus, value != 1)
  numBad <- nrow(bad)
  daysBad <- unique(bad$Date)
  monBad <- unique(bad$variable)
  monBad <- gsub("*_Status", "", monBad)
  isComplete <- as.numeric(dataFreq()) * (length(dateRange())-1) <= nrow(filterDates())
  
  if (!isComplete){
    show_alert(
      title = "WARNING! The full selected dates are not available in the data.",
      text = tags$div(
        print("Check that there is data for the entirety of each selected date.")
      ),
      type = "error",
      html = TRUE,
      width = "80%"
    )
  } else {
    shinyjs::show("go")
    if (all(meltedStatus$value == 1)){
      show_alert(
        title = "Success!",
        text = tags$div(
          print("Good to go! Looks like the DAM system recorded the whole time for each monitor")
        ),
        type = "success",
        html = TRUE,
        width = "80%"
      )
    } else {
      show_alert(
        title = "WARNING! DAM system may have stopped working during experiment.",
        text = tags$div(
          print("DAM system may have stopped working during experiment."),
        ),
        type = "warning",
        html = TRUE,
        width = "80%"
      )
    }
  }
  waiter::waiter_hide()
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
    mutate(DateTime = as.character(as.POSIXct(strptime(DateTime, "%Y-%m-%d %H:%M:%S")) - dhours(zt)- dminutes(as.numeric(input$data_recording_frequency))))
  
  # Create night and day category, convert to ZT time
  time <- (hour(raw$DateTime) + minute(raw$DateTime)/60)
  
  raw <- raw %>%
    mutate(Period = ifelse(time >= 0 & time < 12,
                           "Day",
                           "Night")) %>%
    mutate(DateTime = as.character(as.POSIXct(strptime(DateTime, "%Y-%m-%d %H:%M:%S"))))# - dminutes(as.numeric(input$data_recording_frequency))))
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