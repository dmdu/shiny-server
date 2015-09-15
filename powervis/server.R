library(shiny)
library('ggplot2')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot

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
    power = read.csv(file, col.names=c("Resource","TimeRAW","Power"))
    power$Time = as.POSIXct(power$TimeRAW)

    t_start = power$Time[1]
    t_end = power$Time[length(power$Time)]
    t_length = difftime(t_end,t_start,units="secs")
    #cat(paste("Times: ", t_start, " ", t_end, " ", t_length, "\n"))

    t_sel_start = t_start + (input$SelectionRange[1]/100.0)*t_length
    t_sel_end = t_end - ((100-input$SelectionRange[2])/100.0)*t_length
    t_sel_length = difftime(t_sel_end,t_sel_start,units="secs")
    #cat(paste("Times: ", t_sel_start, " ", t_sel_end, " ", t_sel_length, "\n"))

    power_sel=power[power$Time>=t_sel_start & power$Time<=t_sel_end,]
    power=power_sel

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
    power = read.csv(file, col.names=c("Resource","TimeRAW","Power"))
    power$Time = as.POSIXct(power$TimeRAW)

    t_start = power$Time[1]
    t_end = power$Time[length(power$Time)]
    t_length = difftime(t_end,t_start,units="secs")
    cat(paste("Times: ", t_start, " ", t_end, " ", t_length, "\n"))

    t_sel_start = t_start + (input$SelectionRange[1]/100.0)*t_length
    t_sel_end = t_end - ((100-input$SelectionRange[2])/100.0)*t_length
    t_sel_length = difftime(t_sel_end,t_sel_start,units="secs")

    power_sel=power[power$Time>=t_sel_start & power$Time<=t_sel_end,]
    power=power_sel

    qplot(power$Time, power$Power) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_line() + geom_point()
  })

})
