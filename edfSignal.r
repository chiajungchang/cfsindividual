require("shiny")
require("reshape")
require("ggplot2")
require("plotly")
require("edfReader")
require("eegkit")

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



edfSignal.tabPanel<-function(profile){
	tabPanel("EEG Signals",plotOutput("edfSignal.plotX",height="100px"),
						   plotOutput("edfSignal.plotY",height="100px"),plotOutput("edfSignal.plotZ",height="100px"),
						div(id = "edfSignal.container"
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
				,			plotlyOutput("edfSignal.plot") )
		)
#	tabPanel("Box Plot", plotlyOutput("edfSignal.plot",width="100%",height ="3000px" ))
}
edfSignal.sidebarPanel<-function(profile,input,session){
	conditionalPanel(condition = "input.tabs1 == 'EEG Signals'",
        #h4(paste(nrow(profile),"rows")),h5(title),h5(subtitle),
#				     selectInput("eggsignal","Select a EGG signal source",edfHeader.data()$sHeader$label[edfHeader.data()$sHeaders$transducerType=="EEG electrode"])
				     selectInput("eggsignal","Select a EEG signal source",c("Fp1" ,"F7" , "F8" ,  "T4" ,  "T6" ,  "T5",   "T3" ,  "Fp2" , "O1" ,  "P3" ,  "Pz" ,  "F3" ,  "Fz" ,  "F4",
								"C4" ,  "P4" ,  "POz" , "C3" ,  "Cz" ,  "O2" ,  "AUX1", "AUX2", "AUX3"))
	)
}

#edfSignal.prepareData<-function(input){
	edfSignal.data <- reactive({
		chdr=edfHeader.data()
		readEdfSignals(chdr,"Ordinary")
	})
	edfSignal.cfdRange <- reactive({
		sigdata=edfSignal.data()
		getConfidRange(sigdata$`Tilt X`$signal) & getConfidRange(sigdata$`Tilt Y`$signal) & getConfidRange(sigdata$`Tilt X`$signal)

	})
	plotSignal<-function(ebdSig){
		smoothSig=eegsmooth(ebdSig$signal,time=(0:(length(ebdSig$signal)-1))/ebdSig$sRate)
		sampletime=(0:(floor(length(ebdSig$signal)/16)-1))/16
		yhat<-predict(smoothSig,newdata=sampletime,se.fit=T)
		sigData=data.frame(time=sampletime,y=yhat$fit,uphat=yhat$fit+yhat$se.fit,bothat=yhat$fit-yhat$se.fit)
		ggplot(sigData,aes(time,y))+geom_line(colour="red")+geom_ribbon(aes(ymin=bothat,ymax=uphat),fill="blue",alpha=0.5)+scale_y_reverse()
	}
	plotSignalWithCfdLine<-function(ebdSig,Cfd){
		mint=(min(which(Cfd))-1)/ebdSig$sRate
		maxt=(max(which(Cfd))-1)/ebdSig$sRate
		sigData=data.frame(time=(0:(length(ebdSig$signal)-1))/ebdSig$sRate,y=ebdSig$signal)
		ggplot(sigData,aes(time,y))+geom_line(colour="red")+geom_vline(xintercept=c(mint,maxt),linetype="dotted")
	
	}
	plotSignalInCfd<<-function(ebdSig,Cfd){
		ebdSig$signal=ebdSig$signal[Cfd]
		plotSignal(ebdSig)
	}
	getConfidRange<-function(s){
		u=300
		l=-300
		co=s>l & s<u
		rl=rle(co)
		i=which(rl$length==max(rl$length[rl$values]) & rl$values)
		if(i>1){
			co[1:sum(rl$lengths[1:(i-1)])]=FALSE
		}
		co[(sum(rl$lengths[1:i])+1):length(co)]=FALSE
		co
	}
	
#}

#edfSignal.preparePlot<-function(input,output){
	output$edfSignal.plotX<-renderPlot({
		plotSignalWithCfdLine(edfSignal.data()$`Tilt X`,edfSignal.cfdRange())+ ggtitle("Tilt X")

	})
	output$edfSignal.plotY<-renderPlot({
		plotSignalWithCfdLine(edfSignal.data()$`Tilt Y`,edfSignal.cfdRange())+ ggtitle("Tilt Y")

	})
	output$edfSignal.plotZ<-renderPlot({
		plotSignalWithCfdLine(edfSignal.data()$`Tilt Z`,edfSignal.cfdRange())+ ggtitle("Tilt Z")

	})
	output$edfSignal.plot<-renderPlotly({
		ggplotly(plotSignalInCfd(edfSignal.data()[[input$eggsignal]],edfSignal.cfdRange())+ ggtitle(input$eggsignal)+ylab("signal"))

	})
	

#}
