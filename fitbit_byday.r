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

fitbit_byday.tabPanel<-function(profile){
	tabPanel("Fitbit by day",div(id = "fitbit_byday.container"
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
          , plotlyOutput("fitbit_byday.plot")))
}
fitbit_byday.sidebarPanel<-function(profile,input){
	conditionalPanel(condition = "input.tabs1.indexOf('Fitbit') >-1",
				h3("fitbit Login:"),
				htmlOutput("fitbit_login"),
				dateInput("fitbit.fromdate", "From Date:(also for Fitbit-by-day)",value=fitbit_byday.userdata()[input$sample,"from_date"]),
				dateInput("fitbit.todate", "To Date:",value=fitbit_byday.userdata()[input$sample,"to_date"]),
				
	conditionalPanel(condition = "input.tabs1 == 'Fitbit by day'",
				selectInput("fitbit_byday.what","What:",c("steps",
          "distance", "floors", "active-minutes", "calories-burned",
          "heart-rate"))
	))
}

#fitbit_byday.prepareData<-function(input){
	fitbit_byday.userdata <- reactive({ 
		mdata<-read.table(fileLocate("account.info"),header=T,sep="\t")
		mdata<-merge(sampleInfo,mdata)
		row.names(mdata)=mdata$SampleID
		mdata
	})
	fitbit_byday.cookie <-reactive({
		userdata=fitbit_byday.userdata()
		username=userdata[input$sample,"username"]
		password=userdata[input$sample,"password"]
		if(is.na(username)) return(NULL)
		cookie<-login(email=as.character(username),password=as.character(password))
		cookie

    })
	fitbit_byday.data <- reactive({ 
		cookie=fitbit_byday.cookie()
		mydata<-get_intraday_data(cookie,input$fitbit_byday.what,date=as.character(input$fitbit.fromdate))
		colnames(mydata)=sub("-","_",colnames(mydata))
		mydata
	})
#}

#fitbit_byday.preparePlot<-function(input,output){
	output$fitbit_login<-renderUI({
		userdata=fitbit_byday.userdata()
		username=as.character(userdata[input$sample,"username"])
		fromdate=paste("suggested from:",as.character(userdata[input$sample,"from_date"]))
		todate=paste("suggested to:",as.character(userdata[input$sample,"to_date"]))
		if(is.na(username)){
			str="No fitbit account"
		}
		else{
			cookie=fitbit_byday.cookie()
			if(length(cookie)==0){
				str=paste("Account",username,"failed to login",sep=" ")
			}
			else{
				str=paste("Account",username,"has loginned ",sep=" ")
			}

		}
		 HTML(paste(str, fromdate,todate, sep = '<br/>'))
		
	})
	output$fitbit_byday.plot<- renderPlotly({
		format_what=sub("-","_",input$fitbit_byday.what)
		if(format_what=="calories_burned"){
	    	p<-ggplot(fitbit_byday.data(),aes_string(x="time",y=format_what))
			p=p+geom_bar(aes(fill=activityLevel),stat="identity")
			
		}
		else if(format_what=="heart_rate"){
			p<-ggplot(fitbit_byday.data(),aes(x=time,y=bpm))
			p=p+geom_line(color="#FF3333")+geom_bar(aes(y=10,fill=confidence),stat="identity")
			

		}
		else{
	    	p<-ggplot(fitbit_byday.data(),aes_string(x="time",y=format_what))
			p=p+geom_bar(aes_string(fill=format_what),stat="identity")
			p=p+scale_fill_distiller(palette = "YlOrRd",direction=1)
		}
		p=p+	xlab("") +ylab(input$fitbit_byday.what)+
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
