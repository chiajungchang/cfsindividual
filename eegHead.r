require("shiny")
require("reshape")
require("ggplot2")
require("plotly")
require("edfReader")
require("eegkit")
require("stringr")


srate=256

eegHead.tabPanel<-function(profile){
	tabPanel("EEG Topoplot",div(id = "eggHead.container"
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
          ,plotlyOutput("eegHead.plot",width="640px",height="640px") 
		)
      )
#	tabPanel("Box Plot", plotlyOutput("eegHead.plot",width="100%",height ="3000px" ))
}
eegHead.sidebarPanel<-function(profile,input,session){
	conditionalPanel(condition = "input.tabs1 == 'EEG Topoplot'",
					h3("View from top")
        #h4(paste(nrow(profile),"rows")),h5(title),h5(subtitle),
#				     selectInput("eggsignal","Select a EGG signal source",edfHeader.data()$sHeader$label[edfHeader.data()$sHeaders$transducerType=="EEG electrode"])
	)
}

#eegHead.prepareData<-function(input){
	eegHead.coor <-reactive({
		read.csv(fileLocate("eegcoord.csv"))
	})
	
	eegHead.data <- reactive({
		datafilename=paste(edfHeader.files()[input$edf.file],".Rdata",sep="")
		if(file.exists(datafilename)){
			load(datafilename)
		}
		else{
			snames=rownames(eegHead.coor())
			withProgress(message = 'Making plot', value = 0, {
			ss=lapply(edfSignal.data()[snames], function(x) x$signal[edfSignal.cfdRange()])
			ss2=lapply(ss,function(x) predict(eegsmooth(x,time=(0:(length(x)-1))/256),newdata=(0:(floor(length(x)/16)-1))/16))
			b<-do.call(cbind.data.frame, ss2)
			b=b[seq(1,nrow(b),64),]
			steps <- list()
			for(i in 1:nrow(b)){
				step <- list(args = list('visible', rep(FALSE, nrow(b))),
             	method = 'restyle')
				step$args[[2]][i] = TRUE
				steps[[i]] = step
			}
			coor=eegHead.coor()
			coor$name=rownames(coor)
			p<-plot_ly()
			for(ff in 1:nrow(b)){
				incProgress(1/nrow(b), detail = paste("Doing part", ff))
				coor$signal=unlist(b[ff,rownames(coor)])
				data.loess <- loess(signal ~ x * y, data = coor,control=loess.control(surface="direct"))
				xgrid <-  seq(-10, 10, .1)
				ygrid <-  seq(-10, 10, .1)
				data.fit <-  expand.grid(x = xgrid, y = ygrid)

				mtrx3d <-  predict(data.loess, newdata = data.fit)

				mtrx.melt <- melt(mtrx3d, id.vars = c('x', 'y'), measure.vars = 'signal')
				mtrx.melt$x <- as.numeric(str_sub(mtrx.melt$x, str_locate(mtrx.melt$x, '=')[1,1] + 1))
				mtrx.melt$y <- as.numeric(str_sub(mtrx.melt$y, str_locate(mtrx.melt$y, '=')[1,1] + 1))
				mtrx.melt$inC=(mtrx.melt$x^2+mtrx.melt$y^2)<=100
				mtrx.melt=mtrx.melt[mtrx.melt$inC,]
				
				vv=F
				if(ff==1){
  					vv=T
				}
#p<-p %>% add_trace(data=mtrx.melt,type = "contour",x=~x,y=~y,z=~value,connectgaps=F,visible=vv,name=paste("v =",ff),showlegend = FALSE,showscale=F,autocontour=F,contours=list(start=bmin,end=bmax))
				p<-p %>% add_trace(data=mtrx.melt,type = "contour",x=~x,y=~y,z=~value,connectgaps=F,visible=vv,name=paste("s =",ff*4-4),showlegend = FALSE,showscale=F)
			}
			})
			p=p %>% add_trace(data=coor,type="scatter",mode="markers+text", x=~x,y=~y,text=~name,textposition = "top right",marker=list(color="white",size=10,line = list(color ="black",width=2)))
			pr=p %>%
				  layout(sliders = list(list(active = 0,
                             steps = steps)))
			save(pr,file=datafilename)
		}
		pr
	})
	
#}

#eegHead.preparePlot<-function(input,output){
	output$eegHead.plot<-renderPlotly({
		eegHead.data()

	})
	

#}
