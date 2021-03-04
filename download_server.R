# download_server.R
# Download capabilities of DAMnation

getOutputFiles <- reactive({
  conditionNames <- getConditionNames()
  outputTypes <- c("DAM Average Values", "DAM Daily Values", "DAM Average Hourly Sleep", "DAM Bouts", "DAM Bout Averages")
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
  dfList <- list(averagesSummarizer(), dailySummarizer(), aveSleepTransposed(), getBouts(), findBoutAverages())
  possibleFiles <- getOutputFiles()
  selectedFiles <- input$downloadChoices
  selectedIdx <- match(selectedFiles, possibleFiles)
  fileList <- list()
  fileList <- lapply(1:length(selectedIdx), function(i){
    name <- ((selectedIdx[i] - 1) %/% 5) + 1
    df <- selectedIdx[i] %% 5
    if(df == 0){df <- 5}
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