source("global.r")

# Define UI for application that draws a histogram
fluidPage(
  # Application title
	titlePanel("Visualization of Individual Datasets for the End ME/CFS Project"),
    HTML(' <input type="text" id="userRole" name="userRole" style="display: none;"> '),
#    includeScript("get_user_id.js"),    

  # Sidebar with a slider input for the number of bins
	sidebarLayout(
		sidebarPanel(
			tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "load.css")),
			selectInput("group_by","Group by",c("none",colnames(sampleInfo)[-1])),
			uiOutput("level"),
			uiOutput("sample"),
			selectInput(
      			"mainfolder", "Categories",
				choices = levels(all_parameters$mainfolder)
        	),
		 	uiOutput("subfolder"),
			uiOutput("toolPanel"),
	#		h3(textOutput("ttt")),
			tags$img(src='http://www.openmedicinefoundation.org/wp-content/uploads/2016/05/Logo-transparent-200w.png',height=106,width=150),
			conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                            tags$div("Loading...",id="loadmessage"))

    	),


    # Show a plot of the generated distribution
    	mainPanel(
			uiOutput("dynamicTabPanel")
    	)	
	)
)
