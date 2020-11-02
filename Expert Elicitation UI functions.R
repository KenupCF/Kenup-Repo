PERTSidebar<-function(suffix="",question_field="",defaultQuestion){
  
  if(!question_field%in%defaultQuestion$question){
    defaultQuestion<-rbind.fill(defaultQuestion,
                                data.frame(question=question_field,
                                           min=0,mode=0,max=0,shape=4,confidence=100,priority=0))
    
    }
  
  
  defaultQuestion<-defaultQuestion%>%filter(question==question_field)

  UI<-sidebarPanel(

        numericInput(inputId = paste("Min",suffix,sep=""),
                     label = "Lowest plausible value",
                     value=defaultQuestion$min,min = 0,max=1),
        
        numericInput(inputId = paste("Mode",suffix,sep=""),
                     label = "Most likely value",
                     value=defaultQuestion$mode,min = 0,max=1),
        
        numericInput(inputId = paste("Max",suffix,sep=""),
                     label = "Highest plausible value",
                     value=defaultQuestion$max,min = 0,max=1),
        
        sliderInput(inputId = paste("Confid",suffix,sep=""),
                    label = "Confidence true value is within range (%)",
                    value=defaultQuestion$confidence,min = 80,max=100,step=1),
        
        sliderInput(inputId = paste("Shape",suffix,sep=""),
                    label=HTML(
            "Shape Parameter <br/>  (greater = more certain)"),
                    min = .5,max=6,value = defaultQuestion$shape,step = .5))

    return(UI)
    }

renderPDFPlot<-function(input,plotMin=NULL,plotMax=NULL,
                         question_field="",
                         fullName="",
                         naturalMin=-Inf,naturalMax=Inf,
                         fullRangePlot=TRUE){
  renderPlot({
      plotAvailable<-all(!sapply(input,is.na))
      plotAvailable<- plotAvailable & 
        na2false(input$Min <= input$Mode) & 
        na2false(input$Mode <= input$Max) &
        na2false(input$Min != input$Max) &
        na2false(input$Min >= naturalMin) &
        na2false(input$Max <= naturalMax) 
        
    
      if(!plotAvailable){
          nullPlot<-TRUE
          DF=data.frame(x=0,y=0)[0,]
          markDF<-data.frame(x=0,y=0)[0,]
          plot.lims<-c(0,0)
        }else{
        
          
          trueMax<-basedf%>%
            dplyr::filter(question==question_field)%>%
            dplyr::pull(trueMax)
          trueMin<-basedf%>%
            dplyr::filter(question==question_field)%>%
            dplyr::pull(trueMin)
        
        lim<-qpert(p = c(0,1),
                   min  = trueMin,
                   max  = trueMax,
                   mode = input$Mode,
                   shape=input$Shape)  
        
        
        values<-seq(from=min(ifelse(!is.null(plotMin),plotMin,lim[1]),trueMin),
                    to=max(ifelse(!is.null(plotMax),plotMax,lim[2]),trueMax),
                              length.out=1e4)
		plot.lims<-lim
		if(fullRangePlot){
  		if(naturalMin > -Inf){plot.lims[1]<-naturalMin}
  		if(naturalMax <  Inf){plot.lims[2]<-naturalMax}
		}
        dens<-dpert(values,min=trueMin,max = trueMax,mode = input$Mode,shape=input$Shape)
        DF<-data.frame(x=values,y=dens)
        markDF<-data.frame(x=c(input$Min,input$Mode,input$Max))
        nullPlot=FALSE
        }
        ggplot(data=DF,aes(x=x,y=y))+
          ylab("Probability Density Function")+xlab(fullName)+
          geom_vline(data=markDF,aes(xintercept=x),lty="dashed")+
          geom_line()+
          scale_y_continuous(expand = c(0, 0))+
          scale_x_continuous(expand = c(0, 0),limits = plot.lims)
      
      
      })
  }

proportionApp<-function(question_field="",suffix="",defaultDF,
                        plotMin=NULL,plotMax=NULL,
                        propVizDivisor=1,invertProp=FALSE,fullName="",
                        naturalMin=0,naturalMax=1,
                        dataType=c("proportion","count")
                        # ,solvePERTprecision=.03,
                        # solvePERTMax=1,solvePERTMin=0
                        ){

  ui<-fluidPage(
      useShinyjs(),
      div(style="display:inline-block",
          actionButton(inputId = "load",label = "Load question")),
      div(style="display:inline-block",uiOutput("fullRangeInput")),
      uiOutput("FullQuestionUI")
     )
  
  options <- list(height = 580)

  server <- function(input, output, session) {
    
    # List of two possible tabs for different data types
    tab2<-list(
      proportion=tabPanel("Proportion Plot",
                radioButtons(inputId = paste("plotVizControl",suffix,sep=""),
                             label = "",
                             choices = 
                               list("Lowest Plausible"="Min",
                                    "Most Likely"="Mode",
                                    "Highest Plausible"="Max"),
                             selected = "Mode",inline = TRUE),
                       plotOutput(paste("plotVizProp",suffix,sep=""))),
      count=tabPanel("Count Plot",
                 plotOutput(paste("plotVizCount",suffix,sep="")))
      )
    
    
    observeEvent({input$load},
     {
       output$fullRangeInput<-renderUI({
          checkboxInput(inputId = "useFullRange",
                          label="Plot distribution over full possible range")
         })
       output$FullQuestionUI<-renderUI({
              sidebarLayout(
                  sidebarPanel =
                    PERTSidebar(question_field=question_field,
                                defaultQuestion=defaultAnswers),
                  mainPanel =  mainPanel(
        tabsetPanel(type = "tabs",
        tabPanel("Probability Distribution Plot",
                 plotOutput(paste("plotPDF",suffix,sep=""))),
        tab2[[dataType]],
        #Uncomment this for phase 2!
        # tabPanel("Answer Summary",
                 # tableOutput(paste("recordtable",suffix,sep=""))),
        checkboxInput(inputId = "checkbox",
            label=HTML("<b>I am happy with these answers</b>"),
            value = defaultAnswers%>%
                    dplyr::filter(question==question_field)%>%
                    dplyr::pull(finishedQuestion)%>%
                    null2false
                  ))))

      })
       
     output$recordtable<-renderTable({
          user_loadedDF<-read.csv("user_loaded.csv")
          currentAliasDF<-read.csv("currentAlias.csv")

          # currentUser<-expert.aliases%>%filter(email%in%email)%>%dplyr::pull(alias)
          temp<-full.responses%>%
            dplyr::filter(question==question_field,
                          !alias%in%c("Average",as.character(currentAliasDF$x[1])))%>%
            dplyr::select(alias,min,mode,max,shape,confidence)
          colnames(temp)<-c("Expert","Smaller plausible value","Most likely value","Greater plausible value","Shape Parameter","Confidence")
          if(user_loadedDF$x & responseSummary){temp}else{
            temp<-data.frame("Not available"="")
            temp
            }
          
      })       
    })
    
      
    #########################################################################################
    ####Code that automatically saves every changes in questionnaire from the user (v1.2)###
    #########################################################################################
    observeEvent({
              input$Min
              input$Mode
              input$Max
              input$Confid
              input$checkbox
              input$useFullRange
              input$Shape},
          {
          

        PERTcondition<-all(!sapply(input,is.na))
        PERTcondition<- PERTcondition & 
          na2false(input$Min <= input$Mode) & 
          na2false(input$Mode <= input$Max) &
          na2false(input$Min != input$Max)  &
          na2false(input$Min >= naturalMin*propVizDivisor) &
          na2false(input$Max <= naturalMax*propVizDivisor)
        
          if(PERTcondition){
          PERTsolved<-solvePERT(Mode = input$Mode,
                                Min = input$Min,
                                Max = input$Max,
                                Shape = input$Shape,
                                Conf = input$Confid/100,
                                naturalMax = naturalMax*propVizDivisor,
                                naturalMin = naturalMin*propVizDivisor)
            
          newdf<-data.frame(name=name,email=email,
                         question=question_field,
                         min=input$Min,
                         mode=input$Mode,
                         max=input$Max,
                         trueMax=PERTsolved$trueMax,
                         trueMin=PERTsolved$trueMin,
                         rmsePERTsolve=PERTsolved$RMSE,
                         naturalMin=naturalMin*propVizDivisor,
                         naturalMax=naturalMax*propVizDivisor,
                         finishedQuestion=input$checkbox,
                         confidence=input$Confid,
                         multiplier=1/propVizDivisor,
                         shape=input$Shape)
          
          temp<-rbind.fill(basedf,newdf)%>%
            filter(!duplicated(question,fromLast = TRUE))
          
          assign(x = "basedf",value = temp , basedfEnv)
          
          }
        
      ###########################################
      ###Plot probabilidy density function######
      ##########################################
        output$plotPDF<-renderPDFPlot(input,
          fullName=fullName,
          question_field=question_field,
          naturalMax = naturalMax*propVizDivisor,
          fullRangePlot = input$useFullRange,
          naturalMin = naturalMin*propVizDivisor)          

          #}
          })
    output$plotVizCount<-renderPlot({
      
          
      # if(any(sapply(input,is.na))){
      #     nullPlot<-TRUE
      #     # propdf=data.frame(x=0,y=0)[0,]
      #     count_df<-data.frame(x=0,y=0,id=0,image=0)[0,]
      # 
      #   }else{
      
      count_df<-data.frame(x=c("Lowest plausible",
                               "Most likely","Highest plausible"),
                           y=c(input$Min,input$Mode,input$Max))
      
      count_df$x<-factor(count_df$x,
                         levels=
                           c("Lowest plausible",
                             "Most likely","Highest plausible"))
  
    # }
      ggplot(data=count_df)+
        geom_bar(aes(y=y,x=x),stat = "identity")+
        ylab(fullName)+xlab("")+
        scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 45,hjust = 1,size=12))

        
        })

      
    output$plotVizProp<-renderPlot({
        
      if(any(sapply(input,is.na))){
          nullPlot<-TRUE
          # propdf=data.frame(x=0,y=0)[0,]
          propdf<-data.frame(x=0,y=0,id=0,image=0)[0,]

        }else{
          
        propdf<-expand.grid(y=1:10,x=1:10)%>%
           arrange(desc(y),x)%>%
           mutate(id=(1:n())/n())
        
        
        # Defining icons to be selected
        
        cutoff<-input[[input$plotVizControl]]/propVizDivisor
        if(invertProp){cutoff<-1-cutoff}
        
        propdf$image<-ifelse(
          propdf$id<=cutoff,
          "Kaka_sil_black.png",
          "Kaka_sil_grey.png")

        }
        ggplot(data=propdf,aes(x=x,y=y))+
          geom_image(aes(image=image),size=.075,show.legend=TRUE)+
          ylab("")+xlab("")+
          theme_bw()+theme(axis.ticks = element_blank(),axis.text=element_blank())
      
      
      })

    
  }
  
  shinyApp(ui = ui,server = server,options=options)
  }
