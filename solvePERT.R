solvePERT<-function(Mode,Min,Max,Shape=4,
					Conf=1,precision=1,
					naturalMax=Inf,naturalMin=-Inf,second.run=T){

#Functin to find the true Min and Max values of a PERT distribution based on a 4-point survey

#Mode: The most likely value, as answered by the expert
#Min: The smallest plausible value, as answered by the expert
#Max: The greatest plausible value, as answered by the expert
#Shape: A shape parameter, defaults to 4
#Conf: The level of confidence the researcher has the true value is between Min and Max
#precision: The precision for generation of candidate true Min and Max values
#naturalMax: A natural upper limit on the possible values (e.g. probabilities cannot be higher than 1)
#naturalMin: A natural lower limit on the possible values (e.g. probabilities cannot be lower than 1)
#second.run: Wether to look for candidate values in two-steps, for greater precision

#Packages used by the function
require(mc2d)
require(dplyr)

#Input error
if(Conf>1 | Conf<0){stop("Confidence level must be between 0-1")}
if(Min> Max | Min> Mode | Max < Mode){stop("Parameters Min, Mode and Max must be in increasing order")}

# If confidence level is smaller than 1, calculate trueMin and trueMax. 
# Otherwise, trueMin and trueMax and provided Min and Max
if(Conf!=1){
#Define range declared of values
range<-Max-Min

#How much uncertainty is to be 'accounted for'
rem.unc<-(1-Conf)

#Create a vector of candidate minimum values
list.min<-c(seq(from=Min-(range*3),to=Min,by=precision),Min,
#Add a `marginal` value, close to the naturalMin
naturalMin+1e-5)

#Create a vector of candidate max values
list.max<-c(Max,seq(to=Max+(range*3),from=Max,by=precision),
#Add a `marginal` value, close to the naturalMax
naturalMax-1e-5)

#All combinations of minimums and maximums
grid<-expand.grid(list.min=list.min,list.max=list.max)%>%
	#remove impossible entries
	filter(list.max>list.min,list.min<=Mode,list.max>=Mode)%>%
	#clip entries with values above the natural limits
	mutate(list.max=sapply(list.max,min,naturalMax-1e-5),
			list.min=sapply(list.min,max,naturalMin+1e-5))%>% 
	filter(!duplicated(data.frame(list.max,list.min)))	

#Reverse engineers to try and find the combinations that give rise to the declared value s
rev.eng.error<-sapply(1:nrow(grid),function(i){ #for each row,
	
	#Probabilities to assess
	p.vec<-c(0+(rem.unc/2),1-(rem.unc/2))
	
	#Vector of comparisons
	comparison<-c(Min,Max)

	#Find the quantiles for the candidate values
	candid<-qpert(min=grid$list.min[i],max=grid$list.max[i],mode=Mode,shape=Shape,p=p.vec)
	
	#Calculate error of candidate values
		x<-comparison-candid
		
	#Calculate RMSE 
		y<-sqrt(sum(x^2,na.rm=T)/sum(!is.na(x)))
		
		return(y)	
	})


winner<-which(rev.eng.error==min(rev.eng.error,na.rm=T)) #which set of values minimizes the error

#get the top 5 more precise pairs of candidate values
top5<-which(rev.eng.error%in%sort(rev.eng.error,decreasing=FALSE)[1:5])

####Run it again, for greater precision
if(second.run){

# Create another set of candidate values, with greater precision
list.min2<-seq(from=min(grid[top5,1])*.5,to=max(grid[top5,1])*1.5,by=precision/5)
list.max2<-seq(from=min(grid[top5,2])*.5,to=max(grid[top5,2])*1.5,by=precision/5)

grid<-expand.grid(list.min=list.min2,list.max=list.max2)%>%
	#remove impossible entries
	filter(list.max>list.min,list.min<=Mode,list.max>=Mode)%>%
	#clip entries with values above the natural limits
	mutate(list.max=sapply(list.max,min,naturalMax-1e-5),
			list.min=sapply(list.min,max,naturalMin+1e-5))%>% 
	filter(!duplicated(data.frame(list.max,list.min)))	

#Reverse engineers to try and find the combinations that give rise to the declared value s
rev.eng.error<-sapply(1:nrow(grid),function(i){ #for each row,
	
	#Probabilities to assess
	p.vec<-c(0+(rem.unc/2),1-(rem.unc/2))
	
	#Vector of comparisons
	comparison<-c(Min,Max)

	#Find the quantiles for the candidate values
	candid<-qpert(min=grid$list.min[i],max=grid$list.max[i],mode=Mode,shape=Shape,p=p.vec)
	
	#Calculate error of candidate values
		x<-comparison-candid
		
	#Calculate RMSE 
		y<-sqrt(sum(x^2,na.rm=T)/sum(!is.na(x)))
		
		return(y)	
	})


winner<-which(rev.eng.error==min(rev.eng.error,na.rm=T)) #which set of values minimizes the error
}

return(list(trueMin=grid[winner,1],trueMax=grid[winner,2],RMSE=min(rev.eng.error,na.rm=T)))
} else{
return(list(trueMin=Min,trueMax=Max,RMSE=0))

}

}
