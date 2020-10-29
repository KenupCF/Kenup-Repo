require(shiny)
require(mc2d)




ui <- fluidPage(
        # Input: Select the random distribution type ----
      radioButtons("dist", "Distribution type:",inline = T,
                   c("Normal" = "norm",
                     "Beta" = "beta",
                     "Gamma" = "gamma",
                     "Binomial" = "binom",
                     "Poisson" = "poisson",
					 "PERT" = "pert",
                     # "Uniform" = "unif",
                     "Logistic" = "logis",
                     "Log-normal" = "lnorm",
                     "Exponential" = "exp")),
       sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # numericInput(inputId = "plotMin",label = "Minimum Plotting Region",value = -5),
      # numericInput(inputId = "plotMax",label = "Maximum Plotting Region",value = +5),
      uiOutput("parInput1"),uiOutput("parInput2"),uiOutput("parInput3"),uiOutput("parInput4"),
      actionButton(inputId = "save_distr",label = "Save Distribution"),
      actionButton(inputId = "flush_distr",label = "Flush Distributions")
      ),mainPanel(plotOutput("densPlot"))))

server <- function(input, output, session) {
  library(shiny)
  library(plyr)
  library(broom)
  library(ggplot2)

  hyperPar<-list(norm=c("mean","sd"),
                 lnorm=c("mean","sd"),
                 unif=c("min","max"),
                 binom=c("size","p"),
                 chisq=c('df','ncp'),
                 logis=c("location","scale"),
				 pert = c("min","mode","max","shape"),
                 gamma=c("shape","rate"),
                 beta=c("alfa","beta"),
                 exp=c("rate"),
                 poisson=c("lambda"))
  
  discrete.dists<-c("poisson","binom")
  
  npars<-reactive({length(hyperPar[[input$dist]])})

  output$parInput1<-renderUI({
   numericInput(inputId = "par1",
            label =  (hyperPar[[input$dist]])[1],
            value = 1)})


   output$parInput2<-renderUI({
        if(npars()>1){
        numericInput(inputId = "par2",
            label =  (hyperPar[[input$dist]])[2],
            value = 1)}else{HTML("")}})

   output$parInput3<-renderUI({
        if(npars()>2){
        numericInput(inputId = "par3",
            label =  (hyperPar[[input$dist]])[3],
            value = 1)}else{HTML("")}})
			
   output$parInput4<-renderUI({
        if(npars()>3){
        numericInput(inputId = "par4",
            label =  (hyperPar[[input$dist]])[4],
            value = 1)}else{HTML("")}})
			
  denFUN<-
    reactive({
    switch(input$dist,
                   norm = dnorm,
                   unif = dunif,
                   binom = dbinom,
                   gamma = dgamma,
                   logis = dlogis,
                   beta= dbeta,
                   poisson = dpois,
				   pert = dpert,
                   lnorm = dlnorm,
                   exp = dexp) 
  })
  
   qFUN<-
    reactive({
    switch(input$dist,
                   norm = qnorm,
                   unif = qunif,
                   binom = qbinom,
                   gamma = qgamma,
                   logis = qlogis,
                   beta= qbeta,
                   poisson = qpois,
				   pert = qpert,
                   lnorm = qlnorm,
                   exp = qexp) 
      })
  
  pdfData<-reactiveValues(
        pdf = list()
      ) 
   
  observeEvent(
        input$save_distr,
        {
          if(npars()==1){
            lim<-qFUN()(c(.001,.999),input$par1)
            x<-seq(from=lim[1],to=lim[2],length.out=5e3)
            if(input$dist%in%discrete.dists){x<-c(floor(x),ceiling(x));x<-x[!duplicated(x)]}
            y<-denFUN()(x=x,input$par1)
            if(input$dist%in%discrete.dists){
			y2<-data.frame(xc=seq(from=min(x),to=max(x),length.out=5e3))
			y2$x<-floor(y2$xc)
			y3<-data.frame(d=y,x=x)
			ym<-merge(y2,y3,by="x")
			x<-ym$xc
			y<-ym$d
			}
			
            z<-data.frame(
               PDF=y,x=x,
               dist=input$dist,
               pars=paste(
                 hyperPar[[input$dist]][1],"=",input$par1))
          }
          if(npars()==2){
            # if(input$dist=='beta'){
              
              # }
            lim<-qFUN()(c(.001,.999),input$par1,input$par2)
            x<-seq(from=lim[1],to=lim[2],length.out=5e3)
            if(input$dist%in%discrete.dists){x<-c(floor(x),ceiling(x));x<-x[!duplicated(x)]}
            y<-denFUN()(x=x,input$par1,input$par2)
                 if(input$dist%in%discrete.dists){
			y2<-data.frame(xc=seq(from=min(x),to=max(x),length.out=5e3))
			y2$x<-floor(y2$xc)
			y3<-data.frame(d=y,x=x)
			ym<-merge(y2,y3,by="x")
			x<-ym$xc
			y<-ym$d
			}
		
		z<-data.frame(
               PDF=y,x=x,
               dist=input$dist,
               pars=paste(
                 hyperPar[[input$dist]][1],"=",input$par1,
                 "/",
                 hyperPar[[input$dist]][2],"=",input$par2))
            }
         
          if(npars()==4){
            # if(input$dist=='beta'){
              
              # }
            lim<-qFUN()(c(.001,.999),input$par1,input$par2,input$par3,input$par4)
            x<-seq(from=lim[1],to=lim[2],length.out=5e3)
            if(input$dist%in%discrete.dists){x<-c(floor(x),ceiling(x));x<-x[!duplicated(x)]}
            y<-denFUN()(x=x,input$par1,input$par2,input$par3,input$par4)
                 if(input$dist%in%discrete.dists){
			y2<-data.frame(xc=seq(from=min(x),to=max(x),length.out=5e3))
			y2$x<-floor(y2$xc)
			y3<-data.frame(d=y,x=x)
			ym<-merge(y2,y3,by="x")
			x<-ym$xc
			y<-ym$d
			}
		
			z<-data.frame(
               PDF=y,x=x,
               dist=input$dist,
               pars=paste(
                 hyperPar[[input$dist]][1],"=",input$par1,            
                 hyperPar[[input$dist]][2],"=",input$par2,
                 hyperPar[[input$dist]][3],"=",input$par3,            
                 hyperPar[[input$dist]][4],"=",input$par4),sep="/")
            }
          
          pdfData$pdf <- c(pdfData$pdf, list(z))
        }
    ) 
  
  observeEvent(
        input$flush_distr,
        {pdfData$pdf<-list()})
    
  output$densPlot<-renderPlot({
    # if(npars()==1){
    #   lim<-qFUN()(c(.01,.99),input$par1)
    #   x<-seq(from=lim[1],to=lim[2],length.out=5e3)
    #   y<-denFUN()(x=x,input$par1)}
    # if(npars()==2){
    #   lim<-qFUN()(c(.01,.99),input$par1,input$par2)
    #   x<-seq(from=lim[1],to=lim[2],length.out=5e3)
    #   y<-denFUN()(x=x,input$par1,input$par2)}
    # plot(x,y)
    if(length(pdfData$pdf)>0){
    z<-rbind.fill(pdfData$pdf)
    
    ggplot(data = z,aes(x=x,y=PDF,color=pars))+
      geom_line()+
      facet_wrap(~dist,scales=c("free"),nrow = length(unique(z$dist)))
    }

    })
  
  
}

shinyApp(ui, server)