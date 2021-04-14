#####PhD Experimental Functions######

### Formatting expert elicitaiton tables

tablePERT<-function(data,varname=""){
  
  data2<-data%>%
    dplyr::filter(question%in%varname,alias!="Average")
  
  data_formatted<-data2%>%
    dplyr::select(parType,fullName,alias,min,mode,max,shape,confidence)
  
  colnames(data_formatted)<-c("Parameter Type","Question","Expert","Smallest plausible value","Most likely value","Greatest plausible value","Shape Parameter","Confidence")  
  
  return(data_formatted) 
  
}


### Predict values and CIs from JAGS output, using model notation
jags.predict<-function(
  #JAGS object
  jags,
  #Which JAGS parameters describe random variation around the model mean (in SD units!)
  err.term=NULL,
  #Dictionary of variables (dictating which jags variable is which coefficient)
  var.dict,
  # Data to predict using current coefficients
  new.data,
  # Inverse link function to use (must match one used on the JAGS script)
  inv.link=inv.logit){
  
  
  require(plyr)
  require(dplyr)
  require(stringr)
  
  # Named vector of which coefficients belong to which variable
  coef<-setNames(object = var.dict$JAGS_coef,nm = var.dict$Variable)
  
  # Copying data.frame to predict
  new.data2<-new.data
  new.data2$Intercept<-1
  
  # Adding interactions
  interaction.terms<-str_subset(string = var.dict$Variable,"\\:")
  for(int in interaction.terms){
    int.split<-str_split(int,"\\:")[[1]]
    interaction.var<-apply(matrix(sapply(int.split,function(i){
      new.data2[,i]}),nrow = nrow(new.data2)),1,prod)
    new.data2[,int]<-interaction.var
  }
  
  new.data2<-new.data2[,var.dict$Variable]
  
  # Change the new data.frame colnames to the JAGS models coefficients
  colnames(new.data2)<-coef[colnames(new.data2)]
  
  # Convert it to a matrix
  new.data2<-as.matrix(new.data2)
  
  # Get MCMC chains for relevant parameters and bind them to a matrix
  do.call(cbind,jags$sims.list[colnames(new.data2)])-> CoefMatrix
  
  # Get error terms for each MCMC values
  if(!is.null(err.term)){
    err<-sapply(jags$sims.list[[err.term]],function(e){rnorm(mean=0,n=1,sd=e)})  
  }else{err<-0}
  
  # result of the linear model
  resu <- (CoefMatrix %*% t(new.data2)) + err
  
  
  # Applying the inverse link function 
  resu <- inv.link(resu)
  
  resu.summ<-t(sapply(1:ncol(resu),function(x){
    z<-resu[,x]
    zz<-data.frame(mean=mean(z),median=median(z),lcl=quantile(z,.025),ucl=quantile(z,.975))
    return(zz)
  }))%>%
    cbind(new.data)
  
  resu.list<-list(summary=resu.summ,full=resu)
  return(resu.list)
}







###Run stochastic simulations of populations
StochTransMat<-function(N,surv,fec,prop,n.years=10,capping=Inf,verbose=T){
  if(class(N)=="matrix"){N<-as.vector(N)}
  ###N: vector of initial age abundances
  ###surv: vector of age-specific survival
  ###fec: vector of age-specific fecundities
  ###prop: vector of age-specfic proportion of breeding animals
  ###n.years: number of years on which how to simulate a population
  
  results<-list()
  results[[1]]<-N

  for(t in 1:n.years+1){
  
  # system.time({
    M1<-sapply(1:length(results[[t-1]]),function(a){
      rbinom(n = 1,size = results[[t-1]][a],prob=surv[a])})
    # })
  
  M1<-c(0,M1)
  
  M1[length(M1)-1]<-sum(M1[length(M1):(length(M1)-1)])
  
  M1<-M1[1:(length(M1)-1)]
  
  # system.time({
    breeding<-sapply(1:length(M1),function(a){
    rbinom(n=1,size = M1[a],prob = prop[a])
    
    })
    # })
  
  
  # system.time({
    births<-sapply(1:length(breeding),function(a){
    sum(rpois(n=breeding[a],lambda = fec[a]))})
    # })
    
    
    
  M1[1]<-sum(births)         
  
  if(sum(M1)>cap){
    
  pr<-M1/sum(M1)
  dif<-sum(M1)-cap
    
  M1<-M1-round(pr*dif)
  }  
    
  # M1[1]<-cap(M1[1],max=cap.births)
  
  
  
  results[[t]]<-M1  
  if(verbose){print(t)} 
  }
  results<-lapply(1:length(results),function(i){
    data.frame(N=results[[i]],age=1:length(results[[i]]),year=i)
    })
  results<-do.call(rbind,results)
  
  return(results)
  
  
}

StochTransMat2<-function(N,surv,fec,prop,n.years=10,capping=Inf,verbose=T,DemStoch=T){
  if(class(N)=="matrix"){N<-as.vector(N)}
  ###N: vector of initial age abundances
  ###surv: vector of age-specific survival
  ###fec: vector of age-specific fecundities
  ###prop: vector of age-specfic proportion of breeding animals
  ###n.years: number of years on which how to simulate a population
  
  results<-list()
  results[[1]]<-N
  
  if(DemStoch==FALSE){results[[1]]<-cbind(results[[1]])}
  
  if(is.null(dim(surv)) & is.null(dim(fec)) & is.null(dim(prop))){
  surv<-matrix(surv,nrow=n.years,ncol=length(surv),byrow=T)
  fec<-matrix(fec,nrow=n.years,ncol=length(fec),byrow=T)
  prop<-matrix(prop,nrow=n.years,ncol=length(prop),byrow=T)
  
  
  }
  
  TransMatrixList<-list()
  for(t in 1:n.years+1){
  
	L<-matrix(0,ncol=length(N),nrow=length(N))  
    L[1,]<-fec[t-1,]*prop[t-1,]*surv[t-1,]
    diag(L[-1,-ncol(L)])<-surv[t-1,-ncol(surv)]
    L[nrow(L),ncol(L)]<-surv[t-1,ncol(surv)]
	TransMatrixList[[t]]<-L
  
    if(DemStoch==FALSE){
    
    
    
    
    M1<-L%*%results[[t-1]]
    
    M1[1,]<-cap(M1[1,],max = capping)
    
    }else{ 
    
    
    
  # system.time({
    M1<-sapply(1:length(results[[t-1]]),function(a){
      rbinom(n = 1,size = results[[t-1]][a],prob=surv[t-1,a])})
    # })
  
  M1<-c(0,M1)
  
  M1[length(M1)-1]<-sum(M1[length(M1):(length(M1)-1)])
  
  M1<-M1[1:(length(M1)-1)]
  
  # system.time({
    breeding<-sapply(1:length(M1),function(a){
    rbinom(n=1,size = M1[a],prob = prop[t-1,a])
    
    })
    # })
  
  
  # system.time({
    births<-sapply(1:length(breeding),function(a){
    sum(rpois(n=breeding[a],lambda = fec[t-1,a]))})
    # })
    
    
    
  M1[1]<-sum(births)         
  
  if((sum(M1)>capping) & (1>2)){
    
  pr<-M1/sum(M1)
  dif<-sum(M1)-capping
    
  M1<-M1-round(pr*dif)
  }  
    
  M1[1]<-cap(M1[1],max=capping)
  
  }
  
  results[[t]]<-M1
  
  if(verbose){print(t)} 
  }
  results<-lapply(1:length(results),function(i){
    if(DemStoch==F){results[[i]]<-as.vector(results[[i]])}
    data.frame(N=results[[i]],age=1:length(results[[i]]),year=i)
    })
  results<-do.call(rbind,results)
  resultsList<-list(N=results,L=TransMatrixList)
  return(resultsList)
  
  
}

###Get hyper-parameters from MCMC posteriors
estimateBUGShyperPars<-function(input,parDist,start=list(),method=c("BUGSoutput","coda.samples")){
  
  ###Importing Packages
  require(dplyr)
  require(stringr)
  require(abind)
  require(fitdistrplus)
  
  options(stringsAsFactors = FALSE)
    ###Parameters
    #`model` is the fitted JAGS model
    #`parDist` is a named vector of the distributions assigned to each followed parameter
    #`start` is a listing  containg starting values for parameter estimation algorithm 
    ######(optional for most distributions)
    
    if(method=="coda.samples"){
      array<-abind(input,along=3)
      array<-aperm(array,c(1,3,2))
      
      }
    if(method=="BUGSoutput"){
      
      array<-input$BUGSoutput$sims.array
      }
  
  
    #Create a data.frame with   
    parInfo<-data.frame(
      EstPar=dimnames(array)[[3]])%>% #the parameters estimated
      mutate(TruePar=gsub(x=EstPar,"\\[.*$",""))%>%         #the `true parameter (unindexed)
      merge(data.frame(TruePar=names(parDist),Distribution=parDist)) #and the correspoding distribution
    
    #Looping through all parameters which we want to know the underlying hyperparameters
    # hyperPars<-lapply(1:nrow(parInfo),function(p){
    hyperPars<-list()
    for(p in 1:nrow(parInfo)){
      
      distrib<-as.character(parInfo$Distribution[p])
      par<-as.character(parInfo$EstPar[p])
      tpar<-as.character(parInfo$TruePar[p])
      
      
      x=as.numeric(array[,,par])
      #use function  `fitdistr` to estimate hyperparameters from MCMC values
      temp<-fitdistrplus::fitdist(
          data = x,
          method="mme",
          distr=distrib,
          start=start[[tpar]]
          )$estimate    

      temp<-c(temp,Mean=mean(x),median=as.numeric(quantile(x,0.5)),lcl=as.numeric(quantile(x,0.025)),ucl=as.numeric(quantile(x,0.975)),min=min(x),max=max(x))
      #create a data.frame with all hyperparameters of the current parameter
      hyperPars[[p]]<-
        # return(
          data.frame(
          EstPar=par,TruePar=tpar,Distribution=distrib,
          HyperPar=names(temp),Value=temp)
          # )
      # print(p)
      # Sys.sleep(1)
    # })
    }
    
    # p
    #bind and return all data.frames created during loop
    hyperPars<-rbind.fill(hyperPars)
    return(hyperPars)
    
}

extractBugsParsSummary<-function(JAGSinput){
##########WARNING###########
#For now only supports one-dimensioanl indexes
#

require(stringr)
require(MCMCvis)

resu<-as.data.frame(t(apply(MCMCchains(JAGSinput),2,qsummary)))
resu<-resu[,c("2.5%","50%","97.5%","Mean")]
colnames(resu)<-c("lcl","median","ucl","mean")
resu$ParIndex<-rownames(resu)
resu$Par=gsub(x=resu$ParIndex,"\\[.*$","")

Indexes<-str_extract_all(resu$ParIndex, "(?<=\\[).+?(?=\\])")
Indexes[sapply(Indexes,length)==0]<-NA

resu$Index1<-as.numeric(unlist(Indexes))

return(resu)

}

extractBugsPars<-function(JAGSinput){
##########WARNING###########
#For now only supports one-dimensioanl indexes
#

require(stringr)
require(reshape2)
require(MCMCvis)

resu<-reshape2::melt(MCMCchains(JAGSinput),varnames=c("MCMC","ParIndex"))
resu$Par=gsub(x=resu$ParIndex,"\\[.*$","")

Indexes<-str_extract_all(resu$ParIndex, "(?<=\\[).+?(?=\\])")
Indexes[sapply(Indexes,length)==0]<-NA

resu$Index1<-as.numeric(unlist(Indexes))

return(resu)

}



###DECISION MAKING FUNCTIONS




#Function to calculate an objective`s score
ObjectiveScore<-function(x,minimize=FALSE,Max=NULL,Min=NULL){
  if(is.null(Max)){Max=max(x)}
  if(is.null(Min)){Min=min(x)}
  y<- (x - Min) / (Max-Min)
  if(minimize){y<- 1 - y}
  return(y)}


ConsequenceTableAnalysis<-function(data,
		DecisionIndex,GoalIndexes,
		w=rep(1/length(GoalIndexes),length(GoalIndexes)),
		minList=NULL,maxList=NULL,
		MaxMin=NULL){
		
		class(data)<-"data.frame"
		
		decisions<-data[,DecisionIndex]
		
		resu<-data.frame(sapply(1:length(GoalIndexes),function(i){
		
		scored<-ObjectiveScore(
			x=data[,GoalIndexes[i]],minimize=MaxMin[i]=="Min",
			Min=ifelse(is.null(minList),NULL,minList[[GoalIndexes[i]]]),
			Max=ifelse(is.null(maxList),NULL,maxList[[GoalIndexes[i]]]))
			
			
		scoredWeighted<-scored*w[i]
		return(scoredWeighted)
		}))
		colnames(resu)<-GoalIndexes
		
		resu$Score<-apply(resu,1,sum)
		
		resu<-cbind(decisions,resu)
		colnames(resu)[1]<-DecisionIndex
		
		resu.full<-merge(data,resu,by=DecisionIndex)
		
		resu.full<-resu.full[,c(DecisionIndex,sort(colnames(resu.full)[-c(1,ncol(resu.full))]),"Score")]
		colnames(resu.full)<-gsub(x=colnames(resu.full),".x",".absolute")
		colnames(resu.full)<-gsub(x=colnames(resu.full),".y",".scored")
		colnames(resu.full)[ncol(resu.full)]<-"TotalScore"
		return(resu.full)
		
		}

###Sample values from priors

priorSampling<-function(L,size,seed=NULL,method="random"){
  
  require(mc2d)
  
  if(!is.null(seed)){set.seed(seed)}
  
  
  #generate a list of sampling functions for each distribution
  rFUN<-list(beta=rbeta,norm=rnorm,unif=runif,binom=rbinom,
                gamma=rgamma,poisson=rpois,lnorm=rlnorm,exp=rexp,pert=rpert)
				
  dFUN<-list(beta=dbeta,norm=dnorm,unif=dunif,binom=dbinom,
                gamma=dgamma,poisson=dpois,lnorm=dlnorm,exp=dexp,pert=dpert)
  qFUN<-list(beta=qbeta,norm=qnorm,unif=qunif,binom=qbinom,
                gamma=qgamma,poisson=qpois,lnorm=qlnorm,exp=qexp,pert=qpert)
 
  npars<-list(beta=2,norm=2,unif=2,binom=2,gamma=2,poisson=1,lnorm=2,exp=1,pert=4)
 
  if(method%in%c("lhs","oalhs")){
	  require(lhs)}
	  
  if(method==c("lhs"))   {M<-randomLHS(size,length(L))}
  if(method==c("oalhs")) {M<-create_oalhs(size,length(L),TRUE,FALSE)}
  if(method==c("random")){M<-matrix(runif(size*length(L)),nrow=size,ncol=length(L))}
  
  resu<-sapply(1:length(L),function(i){
	  
		  y<-names(L)[i]
		  values<-M[,i]  
		  
		  #get prior data.frame object for each parameter
		  x<-L[[y]]
		  
		  dist<-as.character(x$dist)
		  
		  discrete<-na2false(x$integer)
		  if(length(discrete)==0){discrete<-FALSE}
		  
		  x<-x[,
			   colnames(x)%in%c("min","mode","max","shape","rate","mean","sd","lambda","shape1","shape2")]
		  
		  x$dist<-dist
		
		  #if distribution is not poisson nor exponetial (they need only 1 parameter)
		  if(npars[[x$dist]]==2){
		  z<-qFUN[[x$dist]](p=values,
			x[1,1]-(1*(dist=="unif")*discrete),
			x[1,2]) #generate n values using meta parameters
		  #if distribution IS poisson nor exponetial (they need only 1 parameter)
		  }
		  
		  if(npars[[x$dist]]==1){
		  z<-qFUN[[x$dist]](p=values,x[1,1])        #generate n values using meta parameters
		  }
		  
		  if(npars[[x$dist]]==4){
		  z<-qFUN[[x$dist]](p=values,x[1,1],x[1,2],x[1,3],x[1,4])        #generate n values using meta parameters
		  }
		  
		  if(discrete & dist=="unif"){z<-ceiling(z)}
		  
		  z<-matrix(z,nrow=1) #convert values to a matrix of 1 row
		  
		  rownames(z)<-y #rename the matrix with the parameter name
		  
		  return(z)
		  })
		  
	if(is.null(dim(resu))){resu<-t(resu)}
	resu<-as.data.frame(resu)
	colnames(resu)<-names(L)
	# if(!is.null(seed)){}
	return(resu) 
	
	}
  

 
  
  
  
priorSummary<-function(L,q=c(.025,.5,.975)){
  require(mc2d)
  # require(dplyr)

  resu<-lapply(names(L),function(y){
  
  
  #generate a list of sampling functions for each distribution
  dFUN<-list(beta=dbeta,norm=dnorm,unif=dunif,binom=dbinom,pert=dpert,
                gamma=dgamma,poisson=dpois,lnorm=dlnorm,exp=dexp)
  qFUN<-list(beta=qbeta,norm=qnorm,unif=qunif,binom=qbinom,pert=qpert,
                gamma=qgamma,poisson=qpois,lnorm=qlnorm,exp=qexp)
  meanFUN<-list(beta=function(x,y){x/(x+y)},
				norm=function(x,y){x},
				gamma=function(x,y){x/y},
				unif=function(x,y){(x+y)/2},
				binom=function(x,y){x*y},
				pert=function(a,b,c,d){(a+(d*b)+c)/(d+2)},
				lnorm=function(x,y){exp(x+((y^2)/2))},
				exp=function(x){x^-1},
				poisson=function(x){x})

  npars<-list(beta=2,norm=2,unif=2,binom=2,gamma=2,poisson=1,lnorm=2,exp=1,pert=4)
				

  #get prior data.frame object for each parameter
  x<-L[[y]]

  dist<-as.character(x$dist)
  
  x<-x[,
       colnames(x)%in%c("min","mode","max","shape","rate","mean","sd","lambda","shape1","shape2")]
  
  x$dist<-dist
  
  if(npars[[x$dist]]==2){
  
  quantiles<-qFUN[[x$dist]](q,x[1,1],x[1,2]) #generate n values using meta parameters
  Mean<-meanFUN[[x$dist]](x[1,1],x[1,2])
  }
  #if distribution IS poisson nor exponetial (they need only 1 parameter)
  if(npars[[x$dist]]==1){
  
  quantiles<-qFUN[[x$dist]](q,x[1,1]) #generate n values using meta parameters
  Mean<-meanFUN[[x$dist]](x[1,1])  
  
  }
  if(npars[[x$dist]]==4){
  
  quantiles<-qFUN[[x$dist]](q,x[1,1],x[1,2],x[1,3],x[1,4]) #generate n values using meta parameters
  Mean<-meanFUN[[x$dist]](x[1,1],x[1,2],x[1,3],x[1,4])
  }  
  
  df<-data.frame(par=y,dist=x$dist,lcl=quantiles[1],median=quantiles[2],ucl=quantiles[3],mean=Mean)
  
  return(df)

})

# resu<-do.call(rbind,resu)
names(resu)<-names(L)
return(resu) 
 
} 
  
 
priorPDF<-function(L){
  require(mc2d)
  
  dFUN<-list(beta=dbeta,norm=dnorm,unif=dunif,binom=dbinom,pert=dpert,
             gamma=dgamma,poisson=dpois,lnorm=dlnorm,exp=dexp)
  
  qFUN<-list(beta=qbeta,norm=qnorm,unif=qunif,binom=qbinom,pert=qpert,
             gamma=qgamma,poisson=qpois,lnorm=qlnorm,exp=qexp)
  
  meanFUN<-list(beta=function(x,y){x/(x+y)},
                norm=function(x,y){x},
                gamma=function(x,y){x/y},
                unif=function(x,y){(x+y)/2},
                binom=function(x,y){x*y},
                pert=function(a,b,c,d){(a+(d*b)+c)/(d+2)},
                lnorm=function(x,y){exp(x+((y^2)/2))},
                exp=function(x){x^-1},
                poisson=function(x){x})
  
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
  
  resu<-lapply(names(L),function(n){
    
    x<-L[[n]]
    
    dist<-as.character(x$dist)
    
    x<-x[,
         colnames(x)%in%c(
           "min","mode","max","shape","rate","mean","sd","lambda","shape1","shape2")]
    
    x$dist<-dist
    x$dist<-as.character(x$dist)
    
    npars<-length(hyperPar[[dist]])
    
    if(npars==1){
      lim<-qFUN[[dist]](c(.0001,.9999),x[1,1])
      values<-seq(from=lim[1],to=lim[2],length.out=5e3)
      if(dist%in%discrete.dists){values<-c(floor(values),ceiling(values));values<-values[!duplicated(values)]}
      y<-dFUN[[dist]](values,x[1,1])
      if(dist%in%discrete.dists){
        y2<-data.frame(xc=seq(from=min(values),to=max(values),length.out=5e3))
        y2$values<-floor(y2$xc)
        y3<-data.frame(d=y,values=values)
        ym<-merge(y2,y3,by="values")
        values<-ym$xc
        y<-ym$d
      }
      
      z<-data.frame(
        PDF=y,values=values,
        dist=dist,
        parName=n,
        pars=paste(
          hyperPar[[dist]][1],"=",x[1,1]))
    }
    
    if(npars==2){
      lim<-qFUN[[dist]](c(.0001,.9999),x[1,1],x[1,2])
      values<-seq(from=lim[1],to=lim[2],length.out=5e3)
      if(dist%in%discrete.dists){values<-c(floor(values),ceiling(values));values<-values[!duplicated(values)]}
      y<-dFUN[[dist]](values,x[1,1],x[1,2])
      if(dist%in%discrete.dists){
        y2<-data.frame(xc=seq(from=min(values),to=max(values),length.out=5e3))
        y2$values<-floor(y2$xc)
        y3<-data.frame(d=y,values=values)
        ym<-merge(y2,y3,by="values")
        values<-ym$xc
        y<-ym$d
      }
      
      z<-data.frame(
        PDF=y,values=values,
        dist=dist,
        par=n,
        pars=paste(
          hyperPar[[dist]][1],"=",x[1,1],
          "/",
          hyperPar[[dist]][2],"=",x[1,2]))
    }
    if(npars==4 & dist =="pert"){
      lim<-qFUN[[dist]](c(0,1),x[1,"min"],x[1,"mode"],x[1,"max"],x[1,"shape"])
      values<-seq(from=lim[1],to=lim[2],length.out=5e3)
      if(dist%in%discrete.dists){values<-c(floor(values),ceiling(values));values<-values[!duplicated(values)]}
      y<-dFUN[[dist]](values,x[1,"min"],x[1,"mode"],x[1,"max"],x[1,"shape"])
      if(dist%in%discrete.dists){
        y2<-data.frame(xc=seq(from=min(values),to=max(values),length.out=5e3))
        y2$values<-floor(y2$xc)
        y3<-data.frame(d=y,values=values)
        ym<-merge(y2,y3,by="values")
        values<-ym$xc
        y<-ym$d
      }
      
      z<-data.frame(
        PDF=y,values=values,
        dist=dist,
        par=n,
        pars=paste(
          hyperPar[[dist]][1],"=",x[1,"min"],
          "/",
          hyperPar[[dist]][2],"=",x[1,"mode"],
          "/",
          hyperPar[[dist]][3],"=",x[1,"max"],
          "/",
          hyperPar[[dist]][4],"=",x[1,"shape"]))
    }
    
    return(z)
  })			
  
  resu<-do.call(rbind,resu)
  
  return(resu)
  
} 

combinePriorQuantiling<-function(L){
comb<-expand.grid(lapply(L,function(x){1:nrow(x)}))
colnames(comb)<-names(L)

mat<-matrix(NA,nrow=nrow(comb),ncol=ncol(comb))
prob<-numeric()
for(i in 1:nrow(comb)){

prob[i]<-1
	for(j in 1:ncol(comb)){
	mat[i,j]<-L[[colnames(comb)[j]]][comb[i,j],"value"]
	prob[i]<-prob[i]*L[[colnames(comb)[j]]][comb[i,j],"relProb"]
	
	}

}
colnames(mat)<-colnames(comb)
mat<-as.data.frame(mat)

return(list(values.df=mat,prob=prob))





}  
  

 
expand.grid.mixed<-function(data,old.index,new.index,L){
class(data)<-"data.frame"
df1<-data
index<-data[,old.index]

L2<-L
L2[[length(L2)+1]]<-index
names(L2)[length(L2)]<-old.index

df2<-expand.grid(L2)

df3<-merge(df1,df2,by=old.index)

df3[,new.index]<-1:nrow(df3)

return(df3)

}  




# parallelSwitch<-function(switch=c(FALSE,TRUE),cl=NULL,s){

# if(switch){return(parLapply())}


# }









# for(j in n.surveys:sum(step(years-timeSeriesBinYear))){


# min.year<-max(years[j]-timeSeriesBinYear+1,1)

# surveysIncluded<-sum(step(years-min.year))

# TrendYear[j]<-pow(prod(Lambda[j:max(j-sum(step(years-min.year))+1,1)]),
		# 1/sum(intervals[j:max(j-sum(step(years-min.year))+1,1)]))

# isDeclineTrendYear[j]<-step((1/TrendYear[j])-1) #variable tracking if sample trend value is a decline		 
		


# }



# years<-c(1:5,seq(from=6,to=40,by=3))
# surveys<-1:length(years)
# n.surveys<-length(years)
# timeSeriesBinYear=10

# Lambda<-rnorm(mean=1,sd=.4,n.surveys-1)
# intervals<-rep(1,n.surveys-1)

# TrendYear<-numeric()
# isDeclineTrendYear<-numeric()
# X<-list()
# for(j in
	# 2 
	# sum(step(-(years-timeSeriesBinYear-1)))
	# :n.surveys){


# min.year<-max(years[j]-timeSeriesBinYear+1,1)

# surveysIncluded<-sum(step(min.year-years)):j

# TrendYear[j-1]<-pow(prod(Lambda[surveysIncluded]),
		# 1/sum(intervals[surveysIncluded]))

# isDeclineTrendYear[j]<-step((1/TrendYear[j])-1) #variable tracking if sample trend value is a decline		 
		

# }
  


# Reconstructing list of objects generated through parLapply

reconstruct.parallel<-function(z){

resu<-list()
  
for(i in 1:length(z)){

x<-z[[i]]

valid<-!sapply(x,is.null)

indexes <- 1:sum(valid)  

indexes <- indexes+length(resu)

resu[indexes]<-z[[i]][valid]

}
return(resu)
}


