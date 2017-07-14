require("shiny")
require("reshape")
require("ggplot2")
require("plotly")

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

barPlot.tabPanel<-function(profile){
	tabPanel("Bar Plot", div(id = "barplot.container"
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
            ),plotlyOutput("barPlot.plot")),dataTableOutput("barPlot.table"))
#	tabPanel("Box Plot", plotlyOutput("barPlot.plot",width="100%",height ="3000px" ))
}
barPlot.sidebarPanel<-function(profile,input,session){
	files=profile$profile_value[profile$profile_key=="files"]
	
	conditionalPanel(condition = "input.tabs1 == 'Bar Plot'",
        #h4(paste(nrow(profile),"rows")),h5(title),h5(subtitle),
			selectInput(
      			"barfile", "Files",
				choices = files
        	)
	)
}

#barPlot.prepareData<-function(input){
	barPlot.data  <- reactive({
		data=read.table(fileLocate(input$barfile),header=T,sep="\t",check.names = F)
		data<-merge(sampleInfo,data,by="SampleID")
 		melt(data,id=colnames(sampleInfo))

	})
	barPlotInd.data <- reactive({
		indata=barPlot.data()
		indata[indata$SampleID==input$sample,!(colnames(indata) %in%colnames(sampleInfo))]
	})
#}

#barPlot.preparePlot<-function(input,output){
	output$barPlot.plot<- renderPlotly({
		indata=barPlotInd.data()
		p<- ggplot(indata, aes(x="SampleID", y=value, fill=variable))+
    	geom_bar( stat = "identity")
		ggplotly(p,source="barPlot.plot")
  }) 
	output$barPlot.table<-renderDataTable({
		data=barPlotInd.data()
		colnames(data)[colnames(data)=="variable"]="Taxonomy"
		data
	},
    options = list(
  autoWidth = FALSE,
    scrollX=TRUE
  ))

	
#}
