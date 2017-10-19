source("global.r")

page="individual"


# Define server logic required to draw a histogram
function(input, output,session) {
	
	source("fitbit_byday.r",  local = TRUE)$value
	source("fitbit_daily.r",  local = TRUE)$value
	source("fitbit_activity.r",  local = TRUE)$value
	source("fitbit_sleep.r",  local = TRUE)$value
	source("renderTable.r",  local = TRUE)$value
	source("indBoxplot.r",  local = TRUE)$value
	source("barPlot.r",  local = TRUE)$value
	source("edfHeader.r",  local = TRUE)$value
	source("edfSignal.r",  local = TRUE)$value
	source("eegHead.r",  local = TRUE)$value

    foldLocate <- reactive({
		file.path(datafolder,input$mainfolder,input$subfolder)
	})
	fileLocate<-function(filename){
		file.path(foldLocate(),filename)
	}

  output$subfolder<-  renderUI({
	 if(userRole()=="subscriber"){
      selectInput("subfolder", "Sub Cateories", unique(all_parameters$subfolder[all_parameters$mainfold==input$mainfolder]))
	  }
		else{
		h5("unauthorized user")

		}
  })
  output$level<-  renderUI({
	 if(input$group_by=="none"){
			h5("All samples")
	  }
	else{
      selectInput("level", "Level", levels(sampleInfo[,input$group_by]))
	}
  })
    userRole <- reactive({input$userRole }) 
  output$sample<-  renderUI({
	 if(userRole()=="subscriber"){
	query <- parseQueryString(session$clientData$url_search)
	 if(input$group_by=="none"){
      	selectInput("sample", "Sample", sampleIDs,selected=sampleIDs[query$sample])
	  }
	else{
      selectInput("sample", "Sample", sampleIDs[sampleInfo[,input$group_by]==input$level])
	}
	}
	else{
		h5("unauthorized user")
	}
  })
  output$dynamicTabPanel=renderUI({
	tools=all_parameters$profile_value[all_parameters$mainfolder==input$mainfolder & all_parameters$subfolder==input$subfolder     & all_parameters$profile==page & all_parameters$profile_key=="tools"]
	myTabs=list()
	i=1
	for(tool in tools){
		profile=all_parameters[all_parameters$mainfolder==input$mainfolder & all_parameters$subfolder==input$subfolder & all_parameters$profile==tool ,]
		myTabs[[i]]=do.call(paste(tool,"tabPanel",sep="."),list(profile=profile))
		i=i+1

	}


	myTabs$id="tabs1"
	do.call(tabsetPanel, myTabs)

  })
  output$toolPanel=renderUI({
	tools=all_parameters$profile_value[all_parameters$mainfolder==input$mainfolder & all_parameters$subfolder==input$subfolder     & all_parameters$profile==page & all_parameters$profile_key=="tools"]
	myTabs=list()
	i=1
	for(tool in tools){
		profile=all_parameters[all_parameters$mainfolder==input$mainfolder & all_parameters$subfolder==input$subfolder & all_parameters$profile==tool ,]
		myTabs[[i]]=do.call(paste(tool,"sidebarPanel",sep="."),list(profile=profile,input=input))
		i=i+1

	}
	do.call(tagList, myTabs)
  })

}
