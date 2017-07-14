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
#5. In prepareData, use <- instead of <- or == to bring the reactive variable out
#6. name each variables with prefix to aviod name conflict
#7  global data frame sampleInfo as the sample profiles
#8. touch server.R to enforce the update
#9. use conditionalPanel in sidebarPanel
#10. global data frame all_parameters to get folder specific parameters
#11. reactive data is shareable
#12. To use the module for one data, put "tools	toolname" in main.profile in the folder
#13. To add parameter for a module for a folder, put "key	value" in toolname in the folder

fitbit_daily.tabPanel<-function(profile){
	tabPanel("Fitbit Daily",div(id = "fitbit_daily.container"
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
          , plotlyOutput("fitbit_daily.plot")))
}
fitbit_daily.sidebarPanel<-function(profile,input){
	conditionalPanel(condition = "input.tabs1 == 'Fitbit Daily'",
        #h4(paste(nrow(profile),"rows")),h5(title),h5(subtitle),
				selectInput("fitbit_daily.what","What:",c("steps",
          "distance", "floors", "minutesVery", "caloriesBurnedVsIntake",
          "getRestingHeartRateData"))
	)
}

#fitbit_daily.prepareData<-function(input){
	fitbit_daily.data <- reactive({ 
		cookie=fitbit_byday.cookie()
		mydata<-get_daily_data(cookie,input$fitbit_daily.what,as.character(input$fitbit.fromdate),as.character(input$fitbit.todate))
		mydata
	})
#}

#fitbit_daily.preparePlot<-function(input,output){

	output$fitbit_daily.plot<- renderPlotly({
		format_what=sub("-","_",input$fitbit_daily.what)
		if(format_what=="caloriesBurnedVsIntake"){
			format_what="caloriesBurned"
			
		}
		else if(format_what=="getRestingHeartRateData"){
			format_what="restingHeartRate"

		}
	    	p<-ggplot(fitbit_daily.data(),aes_string(x="time",y=format_what))
			p=p+geom_bar(aes_string(fill=format_what),stat="identity")
			p=p+scale_fill_distiller(palette = "YlOrRd",direction=1)
		p=p+	xlab("") +ylab(input$fitbit_daily.what)+
				theme(axis.ticks.x=element_blank(), 
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(), 
                   panel.grid.minor.y = element_blank(), 
                   panel.background=element_blank(), 
                   panel.grid.major.y=element_line(colour="gray", size=.1), 
                   legend.position="bottom")
		ggplotly(p)
  }) 

#}
