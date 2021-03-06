library(shiny)
library('ggplot2')
library(gridExtra)

LOGS_DIR="/var/log/power_logs/"
PUBLISH_AT="/var/www/"

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Moving average function
  ma <- function(z,n=input$AvgWindow){filter(z,rep(1/n,n), sides=1)}

  get_input_filename <- function(selected_resource) {
    source_filename=gsub(" ", "", paste(LOGS_DIR,selected_resource))
  }
  
  get_events <- function() {
    if (file.exists(input$EventsFile)) {
      events = read.csv(input$EventsFile, col.names=c("ID","Label","START_or_END","TimeRAW"), header=FALSE)
      return(data.frame(Time=as.POSIXct(events$TimeRAW), Label=events$Label, START_or_END=events$START_or_END, ID=events$ID))
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

  event_energy <- function(event_records, power) {
    if (nrow(event_records) != 2) {
      cat("Error: more than two records (START and STOP) for a given event. Records:")
      print(event_records)
    }
    lb=event_records[event_records$START_or_END=="START",]$Time
    ub=event_records[event_records$START_or_END=="END",]$Time
    #cat(paste(event_records[event_records$Type=="START",]$Label,":\n"))

    trace=power[power$Time>=lb & power$Time<ub,]

    first_deltaT=as.numeric(difftime(trace$Time[1],lb,units="secs"))
    last_deltaT=as.numeric(difftime(ub,trace$Time[length(trace$Time)],units="secs"))

    trace$deltaT=c(diff(as.numeric(difftime(trace$Time,trace$Time[1],units="secs"))), last_deltaT)

    return(data.frame(Time=c(lb, trace$Time, ub), Energy=c(0, trace$Power[1]*first_deltaT, cumsum(trace$Power * trace$deltaT) + trace$Power[1]*first_deltaT)))
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

        if (input$ShowURLs) {
          output_file=paste(PUBLISH_AT,input$resource,"-power.csv",sep='')
          # Mode 2 corresponds to write permission
	  if (file.access(dir(PUBLISH_AT),mode=2)) {
            write.csv(power,output_file,row.names=FALSE)
            output_URL=paste("http://", Sys.info()["nodename"], ":8080/", input$resource,"-power.csv",sep='')
            cat(paste("Download the selected power trace at:\n", output_URL,sep=''))
          }
          else {
	    cat(paste("Error: can't open directory for writing:", PUBLISH_AT))
          }
        }
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
        # Disable Adjustment
        #time_range=events_raw$Time+input$AdjustEvents
        time_range=events_raw$Time+0
        lb = min(time_range)-30
        ub = max(time_range)+30
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
            # Disable Adjustment
            # events=data.frame(Time=events_raw$Time+input$AdjustEvents, Label=events_raw$Label, Type=events_raw$Type, ID=events_raw$Output)
            events=data.frame(Time=events_raw$Time+0,Label=events_raw$Label, START_or_END=events_raw$START_or_END, ID=events_raw$ID)
            events$GraphLabel=paste(events$START_or_END,":",events$Label)
            if ( (power$Time[1] > events$Time[length(events$Time)]) | (power$Time[length(power$Time)] < events$Time[1]) ) {
              # Events are outside of the selected range
              p
            }
            else {
              # Calculate energy consumption
              experiments=unique(events$ID)
              energy_trace=NULL
              for (ex in experiments) {
                ex_records=events[events$ID==ex,]
                energy_trace=rbind(energy_trace,event_energy(ex_records, power))
              }

              # Display events with vertical lines and labels if we get here
              p=p+
              geom_vline(xintercept=as.numeric(events$Time), color="blue")+
              geom_text(data=events, mapping=aes(x=Time, y=0, label=GraphLabel), size=4, angle=90, vjust=-0.6, hjust=0)
              
              p2 = ggplot(energy_trace,aes(Time,Energy))+geom_point(aes(color="Raw Samples"))+
                theme(axis.text.x = element_text(angle = 45, hjust = 1))+
                theme(legend.position="none")+
                geom_vline(xintercept=as.numeric(events$Time), color="blue")

              grid.arrange(p,p2, heights = c(2/3, 1/3))  
            }
          }
        }
        
      }
    }
  })

  output$Energy <- renderPrint({
    source_filename=get_input_filename(input$resource)
    power_raw = read.csv(source_filename, col.names=c("Resource","TimeRAW","Power"))
    if (nrow(power_raw) > 0) {

      events_raw = get_events()
      if ((input$DisplayEvents) & (! is.null(events_raw)))  {
        power_raw$Time = as.POSIXct(power_raw$TimeRAW)
        # Disable Adjustment
        #events=data.frame(Time=events_raw$Time+input$AdjustEvents, Label=events_raw$Label, Type=events_raw$Type, ID=events_raw$Output)
        events=data.frame(Time=events_raw$Time+0, Label=events_raw$Label, START_or_END=events_raw$START_or_END, ID=events_raw$ID)
        #print(events)
        lb = min(events$Time)-30
        ub = max(events$Time)+30
        power=power_filter(power_raw[power_raw$Time>=lb & power_raw$Time<=ub,],input$LowThresh,input$HighThresh)

        experiments=unique(events$ID)
        ex_stats=NULL
        cat("Energy analysis of the power events:\n")
        for (ex in experiments) {
          ex_records=events[events$ID==ex,]
          energy=event_energy(ex_records, power)
          #print(energy)

          total_energy=energy$Energy[length(energy$Energy)]
          total_time=as.numeric(difftime(ex_records[ex_records$START_or_END=="END",]$Time, ex_records[ex_records$START_or_END=="START",]$Time,units="secs"))
          label=ex_records[ex_records$START_or_END=="START",]$Label
          ex_stats=rbind(ex_stats, data.frame(Experiment.ID=ex,
                                              Experiment.Label=label, 
		                              Runtime.S=total_time, 
                                              Samples=length(energy$Energy), 
                                              Energy.J=total_energy,
                                              AvgPower.W=total_energy/total_time))
        }
        print(ex_stats)
        # Old way:
        #dump("ex_stats", "/tmp/energy.Rdmpd")
        # New way:
        if (input$ShowURLs) {
          output_file=paste(PUBLISH_AT,input$resource,"-energy.csv",sep='')
          # Mode 2 corresponds to write permission
          if (file.access(dir(PUBLISH_AT),mode=2)) {
            write.csv(ex_stats,output_file,row.names=FALSE)
            output_URL=paste("http://", Sys.info()["nodename"], ":8080/", input$resource,"-energy.csv",sep='')
            cat(paste("Download this energy summary at:\n", output_URL,sep=''))
          }
          else {
            cat(paste("Error: can't open directory for writing:", PUBLISH_AT))
          }
        }
      }
      else {
        cat("Display of events is disabled or bad file is specified")
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

#   output$Sampling <- renderPlot({
# 
#     if (input$SamplingAnalysis) {
#       source_filename=get_input_filename(input$resource)
#       power_raw = read.csv(source_filename, col.names=c("Resource","TimeRAW","Power"))
#       if (nrow(power_raw) > 0) {
#         # Select and prune
#         power = power_filter(range_selection(power_raw, start=input$SelectionRange[1], end=input$SelectionRange[2]),input$LowThresh,input$HighThresh)
#         if (nrow(power) > 0) {
#           pow_vs_stride= data.frame("Stride"=integer(0), "AvgGap"=double(0), "TotalPower"=double(0), stringsAsFactors=FALSE)
#           for(stride in 1:input$SamplingLimit) 
#           {
#             power_sample = power[seq(1, nrow(power), by=stride),]
#             rownames(power_sample)=seq_len(nrow(power_sample))
#             # print(power_sample)
#             total_pow = 0
#             dt_sum = 0
#             lim = nrow(power_sample)-1
#             for (i in 1:lim)
#             {
#               h=(power_sample[i,])$Power
#               dt=as.numeric((power_sample[i+1,])$Time-(power_sample[i,])$Time, units="secs")
#               dt_sum=dt_sum+dt
#               total_pow = total_pow + h*dt
#               #print(c(h,total_pow))
#             }
#             dt_avg=round(dt_sum/lim,digits=2)
#             # Convert from J to kJ
#             total_pow = total_pow/1000
#             pow_vs_stride[nrow(pow_vs_stride)+1,]=c(stride,dt_avg,total_pow)
#           }
#           #print(pow_vs_stride)
#           ggplot(pow_vs_stride,aes(Stride,TotalPower))+
#           geom_point()+geom_line()+
#           geom_text(aes(label=AvgGap), vjust=-2, size=4)+
#           xlab("X") + ylab("Total Power, kJ") + 
#           ggtitle("Effect of sampling on total power used over the selected interval\nEvery  X'th sample is used in the integral calculation\n(marker labels indicate average intervals between samples in seconds)")
#         }
#       }
#     }
#   })
})
