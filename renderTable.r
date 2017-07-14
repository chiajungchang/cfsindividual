require("shiny")
require("reshape")

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

renderTable.tabPanel<-function(profile){
		tabPanel("Test Results",dataTableOutput("renderTable.table"))
}
renderTable.sidebarPanel<-function(profile,input){
	conditionalPanel(condition = "input.tabs1 == 'Test Results'",
        #h4(paste(nrow(profile),"rows")),h5(title),h5(subtitle),
		h5(profile$profile_value[profile$profile_key=="desc"])
	)
}

#renderTable.prepareData<-function(input){
	share.data <- reactive({ 
		mdata<-read.table(fileLocate("matrix.tsv"),header=T,sep="\t",check.names = F)
		mdata<-merge(sampleInfo,mdata,by="SampleID")      
 		mdata<-melt(mdata,id=colnames(sampleInfo))
		z <- unsplit(lapply(split(mdata$value, mdata$variable), scale), mdata$variable)
		mdata$zscore=z
		mdata
	})
	sample.data <- reactive({
		share.data=share.data()
		share.data[share.data$SampleID==input$sample,!(colnames(share.data) %in%colnames(sampleInfo))]

	})
#}

#renderTable.preparePlot<-function(input,output){

	output$renderTable.table<- renderDataTable({
		data=sample.data()
		colnames(data)[colnames(data)=="variable"]="Test"
		data

	},
    options = list(
  autoWidth = FALSE,
    scrollX=TRUE
  ))
#}
