library(shiny)
library('ggplot2')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  range_selection <- function(power, start, end) {
    # Convert Time strings to actual Time format 
    power$Time = as.POSIXct(power$TimeRAW)

    t_start = power$Time[1]
    t_end = power$Time[length(power$Time)]
    t_length = difftime(t_end,t_start,units="secs")

    t_sel_start = t_start + (start/100.0)*t_length
    t_sel_end = t_end - ((100-end)/100.0)*t_length
    t_sel_length = difftime(t_sel_end,t_sel_start,units="secs")

    power_sel=power[power$Time>=t_sel_start & power$Time<=t_sel_end,]
    return(power_sel)
  }
  
  time_gaps <- function(times) {
    gaps=NULL
    for (i in 2:length(times)) {
      gaps[i-1] = difftime(times[i],times[i-1],units="secs")
    }
    return(gaps)
  }

  output$summary <- renderPrint({
    file=gsub(" ", "", paste("/var/log/power/",input$resource))
    cat(paste("Stats for ALL power data in: ", file, "\n"))
    power = read.csv(file, col.names=c("Resource","TimeRAW","Power"))
    power$Time = as.POSIXct(power$TimeRAW)

    power_display = data.frame(power$Time, power$Power)
    print(summary(power_display))
  })

  output$analysis <- renderPrint({
    file=gsub(" ", "", paste("/var/log/power/",input$resource))
    cat(paste("Stats for SELECTED power data in: ", file, "\n"))
    power_all = read.csv(file, col.names=c("Resource","TimeRAW","Power"))
    power = range_selection(power_all, start=input$SelectionRange[1], end=input$SelectionRange[2])

    ot=power$Time[1]
    cat(paste("The oldest timestamp: ", ot, "\n"))
    lt=power$Time[length(power$Time)]
    cat(paste("The latest timestamp: ", lt, "\n"))
    N=length(power$Time)
    cat(paste("Number of power measurements: ", N, "\n"))
    cat(paste("Average interval between measurements (sec): ", sprintf("%.2f", difftime(lt,ot,units="secs")/N), "\n"))
  })

  output$Scatter <- renderPlot({
    file=gsub(" ", "", paste("/var/log/power/",input$resource))
    power_all = read.csv(file, col.names=c("Resource","TimeRAW","Power"))
    power = range_selection(power_all, start=input$SelectionRange[1], end=input$SelectionRange[2])
    
    qplot(power$Time, power$Power) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_line() + geom_point()
  })

  output$HistPower <- renderPlot({
    file=gsub(" ", "", paste("/var/log/power/",input$resource))
    power_all = read.csv(file, col.names=c("Resource","TimeRAW","Power"))
    power = range_selection(power_all, start=input$SelectionRange[1], end=input$SelectionRange[2])

    qplot(power$Power,
      geom="histogram",
      main = "Histogram for Power",
      xlab = "Power, Watts",
      fill=I("blue"),
      col=I("black"))
  })

  output$HistGaps <- renderPlot({
    file=gsub(" ", "", paste("/var/log/power/",input$resource))
    power_all = read.csv(file, col.names=c("Resource","TimeRAW","Power"))
    power = range_selection(power_all, start=input$SelectionRange[1], end=input$SelectionRange[2])
    gaps = time_gaps(power$Time)

    qplot(gaps,
      geom="histogram",
      main = "Histogram for Gaps between Measurements",
      xlab = "Seconds",
      fill=I("red"),
      col=I("black"))
  })
})
