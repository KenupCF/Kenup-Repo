###Funçõezinhas do Caio###
#===2015-10-22
	##Resumos
		force.numeric<-function(x){as.numeric(as.character(x))}
		ch<-function(x) {as.character(x)} 				
	##Funções de caracteres	
		omit.last<-function(x,y=1){ #omitir o último caracter (ou os últimos, se trocar o y)
			substr(x,1,(nchar(x)-y))}			
		first.upper<-function(x){ #coloca a primeira letra em maiuscula
			paste(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)), sep="")}
		simpleCap <- function(x) {		
		  s <- strsplit(tolower(as.character(x)), " ")
		  return(sapply(s,function(y){paste(toupper(substring(y, 1,1)), substring(y, 2),
			  sep="", collapse=" ")}))
		}
		sentenceCase <- function(x) {		
		  s <- strsplit(tolower(as.character(x)), "\\.\\s")
		  return(sapply(s,function(y){paste(toupper(substring(y, 1,1)), substring(y, 2),
			  sep="", collapse=". ")}))
		}

	capitalize.dict<-function(ch,split,dict.up,dict.cp){
		ch.l<-str_split(ch,fixed(split))
		ch.l<-lapply(ch.l,function(x){y<-x
			y[y%in%c(tolower(dict.up),simpleCap(dict.up))]<-toupper  (y[y%in%c(tolower(dict.up),simpleCap(dict.up))])
			y[y%in%c(tolower(dict.cp),toupper  (dict.cp))]<-simpleCap(y[y%in%c(tolower(dict.cp),toupper  (dict.cp))])
			return(y)})
		ch<-unlist(lapply(ch.l,paste,collapse=fixed(split)))
		return(ch)
	}

	tidy<-function(x){
		x<-trim(x)
		x<-gsub(x=x,"^-\\s","")
		x<-gsub(x=x,"-$","")
		x<-gsub(x=x,"\\n"," ")
		x<-gsub(x=x,"\\t"," ")
		x<-gsub(x=x,"\\s\\s+"," ")
		return(x)
	}
		
		# returns string w/o leading whitespace
			trim.leading <- function (x)  sub("^\\s+", "", x)
		# returns string w/o trailing whitespace
			trim.trailing <- function (x) sub("\\s+$", "", x)
		# returns string w/o leading or trailing whitespace
			trim <- function (x) gsub("^\\s+|\\s+$", "", x)		
			zero_pad<-function(x,w){
				require(stringr)
				y<-str_pad(x, w, side = "left", pad = "0")
				return(y)}	
			insert.line.breaks<-function(string,limit){	
				string<-gsub(paste0('(.{1,',limit,'})(\\s|$)'), '\\1\n', string);string<-gsub("\\n+$","",string)
				return(string)}			
		countCharOcc <- function(char, s) {
			s2 <- gsub(char,"",s)
			return ((nchar(s) - nchar(s2))/nchar(char))}
		pad.line.breaks<-function(str,w,side,pad){
			require(stringr)
			str.resu<-character()
			for(s in 1:length(str)){
				cc<-countCharOcc("\n",str[s])
				if(cc==0){str.resu[s]<-str_pad(str[s],w,side,pad)} else{
					str.div<-apply(str_split_fixed(str[s],'\n',cc+1),2,trim)
					str.div<-sapply(str.div,str_pad,width=w,side=side,pad=pad)
					str.resu[s]<-paste(str.div,collapse="\n")}
			}
			return(str.resu)}
	###Matemática e estatística
		roundUp <- function(x) 10^ceiling(log10(x))
		##inverso
			inv<-function(x){1/x} 
		##complemento	
			compl<-function(x) { 	
					if(x<0 | x>1){stop("x must be between 0 and 1")}else{1-x}} 
		##moda		
			mode.stat<-function(x){
					ux<-unique(x)
					ux[which.max(tabulate(match(x, ux)))]
					}
		##char2num			
			char2num<-function(y,DotAsSep=TRUE,CommaAsDig=TRUE,toNumeric=TRUE){
				require(stringr)
				if(DotAsSep==FALSE & CommaAsDig==FALSE){stop('character convention is already compatible with R')}
				if(DotAsSep==TRUE  & CommaAsDig==FALSE){stop('invalid convention')}
				if(DotAsSep==TRUE){
					y<-gsub(x=y,pattern=".",replacement="",fixed=TRUE)}
				if(CommaAsDig==TRUE){
					y<-gsub(x=y,pattern=",",replacement=".",fixed=TRUE)}
				if(toNumeric==TRUE){y<-as.numeric(y)}
				return(y)}
		##função para achar a zona UTM de acordo com a longitude.
			long2UTM <- function(long) { 
				(floor((long + 180)/6) %% 60) + 1}
		#converte um número inteiro para códigos alfabético (1 = A, 2=B ... 27=AA ... 702 = ZZ)
			number2digit<-function(x){ 
				if(x > 18278 | x < 1){stop("'x' must be between 1 and 18278")}
				if(is.numeric(x)==FALSE){stop("'x' must be numeric")}
				index3 <- ifelse(x>702,floor((x-27)/(26^2)),0)
				index2 <- ifelse(x>26,floor((x-1)/26) - ((26*index3)),0)
				index1 <- x - ( 26 * index2 ) - ((26^2) * index3)
				y<-paste0(LETTERS[index3],LETTERS[index2],LETTERS[index1])
				return(y)}
###Só pra coisas do Mestrado (por enquanto)
	##Operações com tempo
		myear<-function(x,abbr=TRUE){
			if(is.POSIXct(x)==FALSE){stop("'x' must be a POSIXct object")}
			require(lubridate)
			y<-paste(month(x,label=TRUE,abbr=TRUE),year(x),sep=" ")
			y<-as.yearmon(y, format="%b %Y",LANGUAGE=en)
			return(y)}
		hour.dec<-function(x){hour(x)+(minute(x)/60)}	
	##Substituição simples, em grupo		
		kenup.replace<-function(x,pattern,replacement){
			if(length(pattern)!=length(replacement)){stop("pattern and replacement must be of the same length")}
			for (i in 1:length(pattern)){
			x[x==pattern[i]]<-replacement[i]}
			return(x)}
	##Repetição			
		rep2<-function(x,each){
			y<-numeric()
			temp<-numeric()
			i<-1
			while(i <= length(each)){
				temp<-rep(x[i],each=each[i])
				j<-length(y)+1
				y[j:(length(temp)+(j-1))]<-temp
				i<-i+1}
			return(y)}
	##COMBINAÇÕES IMPOSSÍVEIS DE MODELOS
		imposib.mod<-function(x){  #x is a vector of covariate names
			any(c(
				sum(str_detect(x,"mo.rfall"))>=2, 						#modelos que incluem precipitação mensal em mais de um período
				(any(x%in%time.catg.cov) & any(x%in%time.cont.cov)),	#models with a categorical and continuous temporal effect
				(any(x=="rainfall") & any(x=="mo.rfall")),				#modelos que contenham precipitação em intervalos e ao mesmo tempo no mesmo mes
				(any(x=="sample_month") & any(x=="time"))))==FALSE}		#models with an effect of both surveys and sampling occasions
				
duplicated.full<-function(x){return(duplicated(x)|duplicated(x,fromLast=TRUE))}


####EXPERIMENTAIS

percent.convert<-function(y,DotAsSep=TRUE,CommaAsDig=TRUE,srch="all"){
	ry<-rownames(y)
	y<-apply(y,2,as.character)
	rownames(y)<-ry
	if(srch==2){perc.cols<-which(str_detect(colnames(y),"%"));perc.rows<-1:nrow(y)}
	if(srch==1){perc.rows<-which(str_detect(rownames(y),"%"));perc.cols<-1:ncol(y)}
	if(srch=="all"){perc.rows<-1:nrow(y);perc.cols<-1:ncol(y)}
	temp<-y[perc.rows,perc.cols]
		temp<-apply(temp,2,function(x){gsub(x=x,pattern="%",replacement="",fixed=TRUE)})
		temp<-apply(temp,2,char2num,DotAsSep=DotAsSep,CommaAsDig=CommaAsDig)
		temp<-temp/100
	y[perc.rows,]<-temp
	return(y)
	}

totalizer<-function(d.f,factors,numerals,na.rm=TRUE,sumRows=FALSE,sumCols=TRUE){
	if(any(!factors%in%colnames(d.f))){stop('Factors must be column names!')}
	if(any(!numerals%in%colnames(d.f))){stop('Numerals must be column names!')}
	numerals<-as.character(numerals)
	temp_d.f<-d.f
	d.f[,factors]<-apply(d.f[,factors],2,factor)
	temp<-data.frame()
	for(f in 1:length(factors)){
		temp2<-apply(d.f[,numerals],2,function(x){tapply(x,d.f[,factors[f]],sum,na.rm=na.rm)})
		temp2<-cbind(rownames(temp2), temp2)
			colnames(temp2)[1]<-factors[f]
		temp2<-cbind(matrix("Total",nrow=nrow(temp2),ncol=length(factors[-f])),temp2)
			colnames(temp2)[1:length(factors[-f])]<-factors[-f]
		temp2<-temp2[,colnames(d.f)]
		temp<-rbind(temp,temp2)
	}
	temp_d.f<-rbind(temp_d.f,temp)
	if(sumCols==TRUE){
	temp3<-t(data.frame(colSums(d.f[,numerals],na.rm=na.rm)))
	temp_d.f<-rbind(c(rep("Total",length(factors)),temp3),temp_d.f)}
	if(sumRows==TRUE){
	temp_d.f<-cbind(temp_d.f,Total=rowSums(apply(temp_d.f[,numerals],2,force_numeric)))}
	temp_d.f[temp_d.f==0]<-NA
	return(temp_d.f)
	}
