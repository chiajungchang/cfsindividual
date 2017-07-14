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

indBoxplot.tabPanel<-function(profile){
	tabPanel("Box Plot", plotOutput("indBoxplot.plot",width="100%",height = paste(30+120*length(indBoxSig.data()),"px",sep="") ))
#	tabPanel("Box Plot", plotlyOutput("indBoxplot.plot",width="100%",height ="3000px" ))
}
indBoxplot.sidebarPanel<-function(profile,input,session){
	conditionalPanel(condition = "input.tabs1 == 'Box Plot'",
        #h4(paste(nrow(profile),"rows")),h5(title),h5(subtitle),
		h5(profile$profile_value[profile$profile_key=="desc"])
	)
}

#indBoxplot.prepareData<-function(input){
	indBoxSig.data  <- reactive({
		filename=paste(input$group_by,"sigs.csv",sep="_")
		if(file.exists(fileLocate(filename))){
			sigs<-read.csv(fileLocate(filename))
			sigs<-as.character(sigs$x)
		}
		else{
		share.data=share.data()
		if(length(levels(share.data$variable))>32){
			share.data=share.data[!is.na(share.data$value),]
			ss=split(share.data, share.data$variable)
			f=as.formula(paste("value~",input$group_by))
			uniqp=lapply(ss,function(xx) length(unique(xx[,input$group_by])))
			s2=ss[which(uniqp==2)]
			pp=lapply(s2,function(xx) wilcox.test(f,xx)$p.value)
			sigs=names(which(pp<0.05))
			if(length(sigs)>32){
				sigs=names(which(pp<0.01))

			}
		}
		else{
			sigs=levels(share.data$variable)
		}
			write.csv(sigs,fileLocate(filename),row.names=F)
		}
		sigs

	})
#}

#indBoxplot.preparePlot<-function(input,output){
	output$indBoxplot.plot<- renderPlot({
		data=share.data()
		data=data[data$variable %in% indBoxSig.data(),]
		data$variable=as.factor(as.character(data$variable))
		levels(data$variable)=swr(levels(data$variable))
		data$mark=data$SampleID==input$sample
	  p<-ggplot(data,aes_string(x=input$group_by,y="value"))+
    geom_boxplot(aes_string(fill=input$group_by))+
    geom_jitter(aes(samid=SampleID,shape=mark,size=mark),width=0.1,height=0)+facet_wrap(~variable,scales = "free",ncol=4,strip.position="left")+
		theme(legend.position="none",strip.placement="outside") 
	     # use geom_jitter instead of geom_point so that points with the same value could be separated
	#	ggplotly(p,source="indBoxplot.plot")
		p
  }) 

#}
