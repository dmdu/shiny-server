library(shiny)
library(stringr)

LOGS_DIR="/var/log/power_logs/"
power_files=list.files(path=LOGS_DIR)
resource_list=list()
for (i in 1:length(power_files) ) {
  f = power_files[i]
  resource_list[[f]] = f
}
default = power_files[1]

shinyUI(pageWithSidebar(

  headerPanel("Dashboard for Analysis of Power Data on CloudLab"),

  sidebarPanel(
    radioButtons("resource", label = h4("Select file with power data"),
      choices = resource_list,
      selected = default),
    helpText(paste("Note: listed files are from ", LOGS_DIR)),
    hr(),
    sliderInput("SelectionRange", label = h4("Select boundaries for visualization"), min = 0,
        max = 100, value = c(95, 100)),
    helpText("Note: 100% corresponds to the latest record in the file, 0% - to the oldest"),
    hr(),
    sliderInput("LowThresh", label = h4("Low Power Threshold"), min = 0, max = 50, value = 10),
    sliderInput("HighThresh", label = h4("High Power Threshold"), min = 100, max = 500, value = 300),
    hr(),
    sliderInput("AvgWindow", label = h4("Averaging Window"), min = 1, max = 30, value = 1),
    hr(),
    checkboxInput("DisplayEvents", label = "Display Events", value = FALSE),
    checkboxInput("FocusOnEvents", label = "If Display Events is enabled, override selected boundaries", value = TRUE),
    textInput("EventsFile", label = h4("Path to Event File"), value = "/var/log/power_events.log"),
    helpText("For instructions on creating event files, refer to: http://users.emulab.net/~dmdu/events.txt"),
    hr(),
    checkboxInput("ShowURLs", label = "Show dataset URLs", value = FALSE)
    #
    # Disable adjustment
    # sliderInput("AdjustEvents", label = h4("Adjustment for Events, Seconds"), min = -600, max = 600, value = 0)
    # Disable samping analysis
    #hr(),
    #checkboxInput("SamplingAnalysis", label = "Enable Sampling Analysis", value = FALSE),
    #sliderInput("SamplingLimit", label = h4("Sampling Limit"), min = 1, max = 30, value = 10)
  ),


  mainPanel(
    verbatimTextOutput("summary"),
    hr(),
    verbatimTextOutput("analysis"),
    plotOutput("Scatter", height=550),
    verbatimTextOutput("Energy"),
    plotOutput("HistPower", height=180),
    plotOutput("HistGaps", height=180)
    # Disable samping analysis
    # plotOutput("Sampling", height=250)
  )
))
