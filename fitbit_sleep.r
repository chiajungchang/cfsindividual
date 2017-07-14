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

fitbit_sleep.tabPanel<-function(profile){
	tabPanel("Fitbit Sleep",
		h4("Summary"),
		 tableOutput("fitbit_sleep.sum"),
		h4("Sleep plot"),
		div(id = "fitsleep.container"
          , div(class = "plotlybars-wrapper"
            , div( class="plotlybars"
              , div(class="plotlybars-bar b1")
              , div(class="plotlybars-bar b2")
              , div(class="plotlybars-bar b3")
              , div(class="plotlybars-bar b4")
              , div(class="plotlybars-bar b5")
              , div(class="plotlybars-bar b6")
              , div(class="plotlybars-bar b7")
            )
            , div(class="plotlybars-text"
              , p("loading")
            )
          )
          ,
		plotlyOutput("fitbit_sleep.plot")),
		h4("Sleep table"),
		dataTableOutput("fitbit_sleep.sumtable")

		)
#		 plotlyOutput("fitbit_sleep.plot"))
}
fitbit_sleep.sidebarPanel<-function(profile,input){
	conditionalPanel(condition = "input.tabs1 == 'Fitbit Sleep'",
		h3("sleep activity")
        #h4(paste(nrow(profile),"rows")),h5(title),h5(subtitle),
	)
}

#fitbit_sleep.prepareData<-function(input){
	fitbit_sleep.data <- reactive({ 
		cookie=fitbit_byday.cookie()
		mydata<-get_sleep_data(cookie,as.character(input$fitbit.fromdate),as.character(input$fitbit.todate))
		mydata
	})
	fitbit_sleep.sumtabledata <- reactive({ 
		mydata<-fitbit_sleep.data()
		sumtable<-mydata$df
		if(length(sumtable)==0) return(NULL)
		sumtable$startDateTime=as.POSIXct(sumtable$startDateTime)
		sumtable$endDateTime=as.POSIXct(sumtable$endDateTime)
		sumtable$date=as.Date(sumtable$date)
		sumtable[order(sumtable$startDateTime),]
	})
	fitbit_sleep.timedata <- reactive({	
	sumtable<-fitbit_sleep.sumtabledata()
        timetable<-sumtable[,c(1,6,7)]
        timetable$status="SLEEP"
        for(i in 1:nrow(sumtable)){
            tmp<-sumtable[i,20][[1]]
			if(nrow(tmp)==0) next
            tmp$date=sumtable$date[i]
			tt=t(matrix(unlist(tmp$startDateTime),nrow =7))
            tmp$startDateTime=as.POSIXct(paste(tt[,1],tt[,2],tt[,3],tt[,4],tt[,5],sep=":"),format="%Y:%m:%d:%H:%M")
            tmp$endDateTime=tmp$startDateTime+60*as.numeric(tmp$duration)+120
            tmp$status=sumtable[i,19][[1]]$level
            timetable=rbind(timetable,tmp[,c("date","startDateTime","endDateTime","status")])
        }
        timetable$startDate=as.Date(format(timetable$startDateTime,format='%Y-%m-%d'))
        timetable$endDate=as.Date(format(timetable$endDateTime,format='%Y-%m-%d'))
        timetable2=timetable[timetable$startDate!=timetable$endDate,]

        timetable$startDateTime=as.POSIXct(format(timetable$startDateTime,format='%H:%M'),format='%H:%M')
        timetable$endDateTime=as.POSIXct(format(timetable$endDateTime,format='%H:%M'),format='%H:%M')
		timetable$date=timetable$startDate
		if(nrow(timetable2)>0){
        	timetable$endDateTime[timetable$startDate!=timetable$endDate]=as.POSIXct('23:59',format='%H:%M')
        	timetable2$startDateTime=as.POSIXct('00:01',format='%H:%M') 
			timetable2$endDateTime=as.POSIXct(format(timetable2$endDateTime,format='%H:%M'),format='%H:%M')
			timetable2$date=timetable2$endDate
			timetable=rbind(timetable,timetable2)
		}
		timetable$formatTime=format(timetable$startDateTime,"%H:%M")
		timetable
		
	})
#}

#fitbit_sleep.preparePlot<-function(input,output){
	output$fitbit_sleep.sum<- renderTable({
		mydata<-fitbit_sleep.data()
		mysum<-mydata$summary
		t(as.matrix(mysum))
			

	})
	output$fitbit_sleep.sumtable<- renderDataTable({
		sumtable<-fitbit_sleep.sumtabledata()
		sumtable[,c(1,3,4,5,8,9,10,11,12,13,14,15,16)]
			

	})

	output$fitbit_sleep.plot<- renderPlotly({
		timetable=fitbit_sleep.timedata()
		a<-timetable[timetable$status=="SLEEP",]
		b<-timetable[timetable$status!="SLEEP",]
		p=ggplot(a,aes(x=startDateTime,y=date,xend=endDateTime,yend=date,color=status,textx=formatTime))+geom_segment(size=5)+geom_segment(data=b,size=5)+scale_x_datetime("", date_labels = "%H:%M")
		p=p+	xlab("") +ylab("")+
				theme(axis.ticks.x=element_blank(), 
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(), 
                   panel.grid.minor.y = element_blank(), 
                   panel.background=element_blank(), 
                   panel.grid.major.y=element_line(colour="gray", size=.1), 
                   legend.position="bottom")
		ggplotly(p,tooltip = c("y", "textx", "colour"))
  }) 

#}
