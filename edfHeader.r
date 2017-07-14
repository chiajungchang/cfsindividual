require("shiny")
require("reshape")
require("ggplot2")
require("plotly")
require("edfReader")

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

edfHeader.tabPanel<-function(profile){
	tabPanel("EEG File Info",verbatimTextOutput("edfHeader.print") )
#	tabPanel("Box Plot", plotlyOutput("edfHeader.plot",width="100%",height ="3000px" ))
}
edfHeader.sidebarPanel<-function(profile,input,session){
	conditionalPanel(condition = "input.tabs1.indexOf('EEG') >-1",
        #h4(paste(nrow(profile),"rows")),h5(title),h5(subtitle),
#	conditionalPanel(condition = "input.tabs1 == 'EGG File Info'",
				     selectInput("edf.file","Select a EEG signal file",names(edfHeader.files()))
	)
}

#edfHeader.prepareData<-function(input){
	edfHeader.folders <- reactive({
		folders<-read.table(fileLocate("mapfolder"),sep="\t",header=T)
		rownames(folders)=folders$SampleID
		folders

	})
	edfHeader.files<- reactive({
		folder=edfHeader.folders()[input$sample,"folder"]
		files=dir(fileLocate(folder),"Signals.edf$",full.names=T)
		names(files)=basename(files)
		files

	})
	edfHeader.data <- reactive({
		readEdfHeader(edfHeader.files()[input$edf.file])
	})
	
#}

#edfHeader.preparePlot<-function(input,output){
	output$edfHeader.print<- renderPrint({
		chdr=edfHeader.data()
		chdr$fileName="-"
		summary(chdr)
  }) 

#}
