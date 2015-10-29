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
        max = 100, value = c(95, 100)),
    helpText("Note: 100% corresponds to the latest record in the file, 0% - to the oldest"),
    hr(),
    sliderInput("LowThresh", label = h4("Low Power Threshold"), min = 0, max = 50, value = 10),
    sliderInput("HighThresh", label = h4("High Power Threshold"), min = 100, max = 500, value = 300),
    hr(),
    sliderInput("AvgWindow", label = h4("Averaging Window"), min = 0, max = 30, value = 3),
    hr(),
    sliderInput("SamplingLimit", label = h4("Sampling Limit"), min = 1, max = 30, value = 10)
  ),


  mainPanel(
    verbatimTextOutput("summary"),
    hr(),
    verbatimTextOutput("analysis"),
    plotOutput("Scatter", height=350),
    plotOutput("HistPower", height=150),
    plotOutput("HistGaps", height=150),
    plotOutput("Sampling", height=250)
  )
))
