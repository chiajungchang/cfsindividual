require("shiny")
require("ggplot2")
require("plotly")
require("fitbitScraper")

#1. use require instead of library
#2. four functions: name.tabPanel
#                   name.siderbarPanel
#                   name.prepareData
#                   name.preparePlot
#3. profile to pass parameters for specific data
#4. function fileLocate to locate the date in selected folder
#5. In prepareData, use <<- instead of <- or == to bring the reactive variable out
#6. name each variables with prefix to aviod name conflict
#7  global data frame sampleInfo as the sample profiles
#8. touch server.R to enforce the update
#9. use conditionalPanel in sidebarPanel
#10. global data frame all_parameters to get folder specific parameters
#11. reactive data is shareable
#12. To use the module for one data, put "tools	toolname" in main.profile in the folder
#13. To add parameter for a module for a folder, put "key	value" in toolname in the folder

fitbit_activity.tabPanel<-function(profile){
	tabPanel("Fitbit Activity",
		h5("Time Segment"),
		 plotlyOutput("fitbit_activity.plot"),
		h5("Pie chart by Calories"),
		 plotlyOutput("fitbit_activity.calpie"),
		h5("Pie chart by Steps"),
		 plotlyOutput("fitbit_activity.steppie"),
		h5("Pie chart by Time"),
		 plotlyOutput("fitbit_activity.minpie")

		)
}
fitbit_activity.sidebarPanel<-function(profile,input){
	conditionalPanel(condition = "input.tabs1 == 'Fitbit Activity'",
		h3("activities")
        #h4(paste(nrow(profile),"rows")),h5(title),h5(subtitle),
	)
}

#fitbit_activity.prepareData<-function(input){
	fitbit_activity.data <- reactive({ 
		cookie=fitbit_byday.cookie()
		mydata<-get_activity_data(cookie,end_date=as.character(input$fitbit.todate))
		mydata<-mydata[,c("name","formattedDate","startTimeHours","startTimeMinutes","ampm","durationHours","durationMinutes","durationSeconds","calories","manualCalories","steps","hasSpeed","hasLevels","hasDistance")]
		mydata$startTimeHours=as.numeric(mydata$startTimeHours)
        mydata$startTimeMinutes=as.numeric(mydata$startTimeMinutes)
        mydata$durationHours=as.numeric(mydata$durationHours)
        mydata$durationMinutes=as.numeric(mydata$durationMinutes)
        mydata$calories=as.numeric(mydata$calories)
        mydata$manualCalories=as.numeric(mydata$manualCalories)
        mydata$steps=as.numeric(mydata$steps)
		b=mydata[,c("startTimeHours","startTimeMinutes","durationHours","durationMinutes","calories","manualCalories","steps")]
		b[is.na(b)]=0
		mydata[,c("startTimeHours","startTimeMinutes","durationHours","durationMinutes","calories","manualCalories","steps")]=b
		mydata$duration=mydata$durationHours*60+mydata$durationMinutes
		mydata$startTimeHours=mydata$startTimeHours+as.numeric(mydata$ampm=="pm")*12
		mydata$startTime=as.POSIXct(paste(mydata$startTimeHours,mydata$durationMinutes,sep=":"),format="%H:%M")
		mydata$endTime=mydata$startTime+mydata$duration*60
		mydata$formattedDate<-as.Date(mydata$formattedDate)
		mydata
	})
	fitbit_activity.filteredData <- reactive({ 
		mydata=fitbit_activity.data()
		dates=seq(input$fitbit.fromdate,input$fitbit.todate,by="day")
		mydata=mydata[mydata$formattedDate %in% dates,]
		mydata


		})
#}

#fitbit_activity.preparePlot<-function(input,output){

	output$fitbit_activity.plot<- renderPlotly({
		mydata=fitbit_activity.filteredData()
		p=ggplot(mydata,aes(x=startTime,y=formattedDate))+geom_segment(aes(xend=endTime,yend=formattedDate,color=name),size=3)
		p=p+	xlab("Time") +ylab("Date")+
				theme(axis.ticks.x=element_blank(), 
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(), 
                   panel.grid.minor.y = element_blank(), 
                   panel.background=element_blank(), 
                   panel.grid.major.y=element_line(colour="gray", size=.1), 
                   legend.position="bottom")
		p=p+scale_colour_brewer(palette="Set1")
		ggplotly(p)
  })
	output$fitbit_activity.calpie<- renderPlotly({
		mydata=fitbit_activity.filteredData()
		sumdata<-aggregate(calories~ name, data = mydata, sum)
		p <- plot_ly(sumdata, labels = ~name, values = ~calories, type = 'pie',textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste(calories,"Calories"),marker = list( 
                                                              line = list(color = '#FFFFFF', width = 1)),
             
             showlegend = FALSE) %>%
    layout(
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
		p

  })
	output$fitbit_activity.steppie<- renderPlotly({
		mydata=fitbit_activity.filteredData()
		sumdata<-aggregate(steps~ name, data = mydata, sum)
		p <- plot_ly(sumdata, labels = ~name, values = ~steps, type = 'pie',textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste(steps,"Steps"),marker = list(
                                                              line = list(color = '#FFFFFF', width = 1)),
             
             showlegend = FALSE) %>%
    layout(
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
		p

  })
	output$fitbit_activity.minpie<- renderPlotly({
		mydata=fitbit_activity.filteredData()
		sumdata<-aggregate(duration~ name, data = mydata, sum)
		p <- plot_ly(sumdata, labels = ~name, values = ~duration, type = 'pie',textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste(duration,"Minutes"),marker = list(
                                                              line = list(color = '#FFFFFF', width = 1)),
             
             showlegend = FALSE) %>%
    layout(
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
		p

  })
#}
