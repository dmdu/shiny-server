library(shiny)
library(stringr)

power_files=list.files(path="/var/log/power/")
resource_list=list()
for (i in 1:length(power_files) ) {
  f = power_files[i]
  resource_list[[f]] = f
}
default = power_files[1]

shinyUI(pageWithSidebar(

  headerPanel("Analysis of Experiment Power Data"),

  sidebarPanel(
    radioButtons("resource", label = h4("Select the source of power data"),
      choices = resource_list,
      selected = default),
    helpText("Note: all these files are found in /var/log/power"),
    hr(),
    sliderInput("SelectionRange", label = h4("Select boundaries for visualization"), min = 0,
        max = 100, value = c(90, 100)),
    helpText("Note: 100% corresponds to the latest record in the file, 0% - to the oldest")
  ),

  mainPanel(
    verbatimTextOutput("summary"),
    hr(),
    verbatimTextOutput("analysis"),
    plotOutput("Scatter", height=350),
    plotOutput("HistPower", height=150),
    plotOutput("HistGaps", height=150)
  )
))
