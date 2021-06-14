# guide_server.R

output$documentation <- renderUI({
  #includeHTML(rmarkdown::render("Guide.Rmd"))
  includeHTML("Guide.html")
})