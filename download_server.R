# download_server.R
# Download capabilities of DAMnation

getOutputFiles <- reactive({
  conditionNames <- getConditionNames()
  outputTypes <- c("DAM Average Values", "DAM Daily Values", "DAM Average Hourly Sleep", "DAM Average Hourly Activity", "DAM Bouts", "DAM Bout Averages")
  outputNames <- paste(rep(conditionNames, each = length(outputTypes)), outputTypes, sep = " ")
  outputNames
})

output$selectFiles <- renderUI({
  pickerInput(
    inputId = "downloadChoices",
    label = "Files to Download:",
    choices = getOutputFiles(),
    selected = getOutputFiles(),
    options = list(`actions-box` = TRUE, `selected-text-format` = "count > 3"),
    multiple = TRUE
  )
})

createOutputFiles <- reactive({
  dfList <- list(averagesSummarizer(), dailySummarizer(), aveSleepDT(), formatActivity(), getBouts(), findBoutAverages())
  possibleFiles <- getOutputFiles()
  selectedFiles <- input$downloadChoices
  selectedIdx <- match(selectedFiles, possibleFiles)
  fileList <- list()
  fileList <- lapply(1:length(selectedIdx), function(i){
    name <- ((selectedIdx[i] - 1) %/% 6) + 1
    df <- selectedIdx[i] %% 6
    if(df == 0){df <- 6}
    fileList[[i]] <- dfList[[ df ]][[ name ]]
    fileList[[i]]
  })
  names(fileList) <- selectedFiles
  fileList
})

output$downloadFiles <- downloadHandler(
  filename = function(){
    dates <- dateRange()
    paste0(dates[1], ".", dates[(length(dates)-1)], ".DAM.zip")
  },
  content = function(file){
    dfList <- createOutputFiles()
    dir <- tempdir()
    files <- c()
    files <- lapply(1:length(dfList), function(i){
      name <- names(dfList)[[i]]
      name <- gsub(" ", "_", name)
      df <- dfList[[i]]
      path <- paste0(dir, "/", name, ".csv")
      write.csv(df, path)
      files <- c(files, path)
    })
    zip(file, files, flags = "-r9Xj")
  }
)

output$downloadPlot <- downloadHandler(
  filename = function(){
    dates <- dateRange()
    variable <- input$plotVariable
    variable <- gsub(" ", "", variable)
    
    type <- input$plotType
    if (input$addPoints == "Yes"){
      addPoints <- "dataPoints"
    } else {
      addPoints <- "noDataPoints"
    }
    if (input$stderr == "Yes"){
      stderr <- "SE"
    } else {
      stderr <- "noSE"
    }
    paste(dates[1], dates[(length(dates)-1)], variable, type, addPoints, stderr, "pdf", sep = ".")
    
  },
  content = function(file){
    plot <- createPlot()
    num <- findNumConditions()
    chosenVariable <- input$plotVariable
    numDays <- length(dateRange())-1
    
    bars <- num * numDays
    
    if (chosenVariable %in% averageValues){
      bars <- num
    } else if (chosenVariable %in% dailyValues){
      bars <- num * numDays
    } else {
      bars <- 13
    }

    if(bars > 12){
      w <-13
    } else if (bars %in% c(9, 10, 11, 12)){
      w <- 11
    } else if (bars %in% c(5, 6, 7, 8)){
      w <- 9
    } else if (bars %in% c(3,4)){
      w <- 7
    } else {
      w <- 6
    }
    if (chosenVariable == "Time of Death"){
      ggsave(file, plot=print(plot, newpage=FALSE), scale=0.8, units="in", height=7, width=11)
    } else {
      ggsave(file, plot=plot, scale = 0.8, units = "in", height = 7, width = w)
    }
  }
)