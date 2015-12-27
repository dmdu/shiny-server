library(shiny)
library('ggplot2')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Moving average function
  ma <- function(z,n=input$AvgWindow){filter(z,rep(1/n,n), sides=1)}

  get_input_filename <- function(selected_resource) {
    source_filename=gsub(" ", "", paste("/var/log/power/",selected_resource))
  }
  
  get_events <- function() {
    if (file.exists(input$EventsFile)) {
      events = read.csv(input$EventsFile, col.names=c("Marker","Output","TimeRAW","TimeEpoch","Label","CMD"), header=FALSE)
      return(data.frame(Time=as.POSIXct(events$TimeRAW), Marker=events$Marker))
    }
    else {
      return(NULL)
    }
  }

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

  power_filter <- function(power, low, high) {
    return (power[power$Power >= low & power$Power <= high,])
  }
  
  time_gaps <- function(times) {
    gaps=NULL
    for (i in 2:length(times)) {
      gaps[i-1] = difftime(times[i],times[i-1],units="secs")
    }
    return(gaps)
  }

  output$summary <- renderPrint({
    source_filename=get_input_filename(input$resource)

    cat(paste("Stats for ALL power data in: ", source_filename, "\n"))
    power = read.csv(source_filename, col.names=c("Resource","TimeRAW","Power"))
    power$Time = as.POSIXct(power$TimeRAW)
    

    power_display = data.frame(power$Time, power$Power)
    print(summary(power_display))
    cat("-----------------------\n")

    susp_lo_num=sum(power$Power<input$LowThresh)
    cat(paste("Number of suspicious \"low\" samples (power<", input$LowThresh, "): ", susp_lo_num, "\n"))
    if (susp_lo_num > 0) {
      cat("First 10 \"low\" samples (most likely, special event markers):\n")
      h=head(power[power$Power<input$LowThresh,], 10) 
      print(h[,c("Resource","TimeRAW","Power")])
    }
    susp_hi_num=sum(power$Power>input$HighThresh)
    cat(paste("Number of suspicious \"high\" samples (power>", input$HighThresh, "): ", susp_hi_num, "\n"))
    if (susp_hi_num > 0) {
      cat("First 10 \"high\" samples:\n")
      h=head(power[power$Power>input$HighThresh,], 10) 
      print(h[,c("Resource","TimeRAW","Power")])
    }
    
  })

  output$analysis <- renderPrint({
    source_filename=get_input_filename(input$resource)
    power_raw = read.csv(source_filename, col.names=c("Resource","TimeRAW","Power"))

    if (nrow(power_raw) > 0) {
      # Select and prune
      power = power_filter(range_selection(power_raw, start=input$SelectionRange[1], end=input$SelectionRange[2]),input$LowThresh,input$HighThresh)

      # Number of samples needs to be at least as high as the size of the window for moving average 
      if (nrow(power) > 0) {
        cat(paste("Stats for SELECTED power data in: ", source_filename, "\n"))
        power_display = data.frame(power$Time, power$Power)
        print(summary(power_display))
        cat(paste("Variance (sigma^2): ", sprintf("%.2f", var(power$Power)), "\n"))

        ot=power$Time[1]
        cat(paste("The oldest timestamp: ", ot, "\n"))
        lt=power$Time[length(power$Time)]
        cat(paste("The latest timestamp: ", lt, "\n"))
        N=length(power$Time)
        cat(paste("Number of power measurements: ", N, "\n"))
        cat(paste("Average interval between measurements (sec): ", sprintf("%.2f", difftime(lt,ot,units="secs")/N), "\n"))

        #cat("-----------------------\n")
        #print(head(power))

      }
      else {
        cat("Selected interval does not have enough samples in the allowed range.\nGraphs and statistics may not be shown.\n")
      }
    }
    else {
      cat("There appears to be no data for the chosen resource.")
    }
  })

  output$Scatter <- renderPlot({
    source_filename=get_input_filename(input$resource)
    power_raw = read.csv(source_filename, col.names=c("Resource","TimeRAW","Power"))
    if (nrow(power_raw) > 0) {

      events_raw = get_events()
      if ((input$DisplayEvents) & (! is.null(events_raw)) & (input$FocusOnEvents)) {
        # In this case, focus on events - select the interval based on the earliest and the latest event
        power_raw$Time = as.POSIXct(power_raw$TimeRAW)
        events=data.frame(Time=events_raw$Time+input$AdjustEvents, Marker=events_raw$Marker)
        lb = min(events$Time)-30
        ub = max(events$Time)+30
        power=power_filter(power_raw[power_raw$Time>=lb & power_raw$Time<=ub,],input$LowThresh,input$HighThresh)
      }
      else {
        # Otherwise, select the interval based on input for the SelectionRange widget
        power = power_filter(range_selection(power_raw, start=input$SelectionRange[1], end=input$SelectionRange[2]),input$LowThresh,input$HighThresh)
      }
      if (nrow(power) > input$AvgWindow) {

        # Adding a column with averaged power
        power$PowerAVG = ma(power$Power) 
        # Replacing initial NULL values with non-averaged values
        power$PowerAVG[1:input$AvgWindow-1]=power$Power[1:input$AvgWindow-1]

        df1=data.frame(Time=power$Time, Power=power$Power)
        df2=data.frame(Time=power$Time, Power=power$PowerAVG)
        p = ggplot(df1,aes(Time,Power))+geom_point(aes(color="Raw Samples"))+
          geom_line(data=df2,aes(color="Moving Average"))+
          labs(color="Series")+
          theme(axis.text.x = element_text(angle = 45, hjust = 1))+
          theme(legend.position="bottom")
	
        if (! input$DisplayEvents) {
          # Display of events is disables via the checkbox
          p
        }
        else { 
          if (is.null(events_raw)) {
            # Specified file doesn't exist, as reported get_events()
            p
          }
          else {
            events=data.frame(Time=events_raw$Time+input$AdjustEvents, Marker=events_raw$Marker)
            if ( (power$Time[1] > events$Time[length(events$Time)]) | (power$Time[length(power$Time)] < events$Time[1]) ) {
              # Events are outside of the selected range
              p
            }
            else {
              # Display events with vertical lines and labels if we get here
              p+
              geom_vline(xintercept=as.numeric(events$Time), color="blue")+
              geom_text(data=events, mapping=aes(x=Time, y=0, label=Marker), size=4, angle=90, vjust=-0.6, hjust=0)
            }
          }
        }
        
      }
    }
  })

  output$HistPower <- renderPlot({
    source_filename=get_input_filename(input$resource)
    power_raw = read.csv(source_filename, col.names=c("Resource","TimeRAW","Power"))
    if (nrow(power_raw) > 0) {
      # Select and prune
      power = power_filter(range_selection(power_raw, start=input$SelectionRange[1], end=input$SelectionRange[2]),input$LowThresh,input$HighThresh)
      if (nrow(power) > 0) {
        qplot(power$Power,
          geom="histogram",
          main = "Histogram for Power (Non-averaged)",
          xlab = "Power, Watts",
          fill=I("blue"),
          col=I("black"))
      }
    }
  })

  output$HistGaps <- renderPlot({
    source_filename=get_input_filename(input$resource)
    power_raw = read.csv(source_filename, col.names=c("Resource","TimeRAW","Power"))
    if (nrow(power_raw) > 0) {
      # Select and prune
      power = power_filter(range_selection(power_raw, start=input$SelectionRange[1], end=input$SelectionRange[2]),input$LowThresh,input$HighThresh)
      if (nrow(power) > 0) {

        gaps = time_gaps(power$Time)

        qplot(gaps,
          geom="histogram",
          main = "Histogram for Gaps between Measurements",
          xlab = "Seconds",
          fill=I("red"),
          col=I("black"))
      }
    }
  })

  output$Sampling <- renderPlot({

    if (input$SamplingAnalysis) {
      source_filename=get_input_filename(input$resource)
      power_raw = read.csv(source_filename, col.names=c("Resource","TimeRAW","Power"))
      if (nrow(power_raw) > 0) {
        # Select and prune
        power = power_filter(range_selection(power_raw, start=input$SelectionRange[1], end=input$SelectionRange[2]),input$LowThresh,input$HighThresh)
        if (nrow(power) > 0) {
          pow_vs_stride= data.frame("Stride"=integer(0), "AvgGap"=double(0), "TotalPower"=double(0), stringsAsFactors=FALSE)
          for(stride in 1:input$SamplingLimit) 
          {
            power_sample = power[seq(1, nrow(power), by=stride),]
            rownames(power_sample)=seq_len(nrow(power_sample))
            # print(power_sample)
            total_pow = 0
            dt_sum = 0
            lim = nrow(power_sample)-1
            for (i in 1:lim)
            {
              h=(power_sample[i,])$Power
              dt=as.numeric((power_sample[i+1,])$Time-(power_sample[i,])$Time, units="secs")
              dt_sum=dt_sum+dt
              total_pow = total_pow + h*dt
              #print(c(h,total_pow))
            }
            dt_avg=round(dt_sum/lim,digits=2)
            # Convert from J to kJ
            total_pow = total_pow/1000
            pow_vs_stride[nrow(pow_vs_stride)+1,]=c(stride,dt_avg,total_pow)
          }
          #print(pow_vs_stride)
          ggplot(pow_vs_stride,aes(Stride,TotalPower))+
          geom_point()+geom_line()+
          geom_text(aes(label=AvgGap), vjust=-2, size=4)+
          xlab("X") + ylab("Total Power, kJ") + 
          ggtitle("Effect of sampling on total power used over the selected interval\nEvery  X'th sample is used in the integral calculation\n(marker labels indicate average intervals between samples in seconds)")
        }
      }
    }
  })
})
