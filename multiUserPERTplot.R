plotPERTdist<-function(data,varname="",useAverage=FALSE){
  
  varname<-as.character(varname)
  
  data2<-data%>%filter(question==varname)
  if(!useAverage){data2<-data2%>%filter(name!="Average")}
  
  range<-abs(diff(range(c(data2$trueMin,data2$trueMax))))
  
  jitter<-range/100
  
  
  
  PERTpriors<-lapply(1:nrow(data2),function(i){
  return(data.frame(min=data2$trueMin[i],
             mode=data2$mode[i],
             max=data2$trueMax[i],
             shape=data2$shape[i],
             dist="pert",
             name=data2$alias[i]))
  })
  names(PERTpriors)<-data2$alias

  PERTpriorPDF_All <-priorPDF(PERTpriors)%>%mutate(Expert=par)%>%
    dplyr::group_by(par)%>%
    dplyr::mutate(values=values+runif(n = 1,min=jitter*-1,max=jitter))
  PERTpriorPDF_User<-PERTpriorPDF_All%>%
    filter(par!="Average",!par%in%data2$alias[data2$equal_ABC])
  PERTpriorUniform_User<-PERTpriorPDF_All%>%
    dplyr::filter(par!="Average",par%in%data2$alias[data2$equal_ABC])%>%
    dplyr::group_by(par)%>%
    dplyr::mutate(PDF=seq(from=0,to=max(PERTpriorPDF_All$PDF,na.rm=T),length.out=n()))
                  
  PERTpriorPDF_User<-rbind.fill(PERTpriorPDF_User,PERTpriorUniform_User)
  
  PERTpriorPDFAv<-PERTpriorPDF_All%>%filter(par=="Average")
  

  PERTexamplePlot<-ggplot(PERTpriorPDF_User,aes(y=PDF,x=values,color=Expert))+
  geom_line(lty="solid",size=.5,alpha=.7)+
  ylab("Probability Density")+
    scale_y_continuous(expand=c(0,0))+
  xlab(unique(data2$fullName))
  
  
  if(useAverage){PERTexamplePlot<-PERTexamplePlot+
    geom_line(data=PERTpriorPDFAv,aes(y=PDF,x=values),color="black",lty="dotted")
    }
    
  
  
  return(PERTexamplePlot)
  
  }
