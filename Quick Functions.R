###Caio###
AddToList<-function(x,name,value){
  if(class(x)!="list"){stop("x must be a list.")}
  x[[name]]<-value
  return(x)
}
  


zero_pad<-function(x,w){
require(stringr)
y<-str_pad(x, w, side = "left", pad = "0")
return(y)}	


na2false<-function(x){
  x[is.na(x)]<-FALSE
  return(x)
}


null2false<-function(x){if(length(x)==0){x<-FALSE};return(x)}	
	
	step<-function(x){
		y<-rep(0,length(x))
		y[x>=0]<-1
		return(y)}
		
	
	pow<-function(x,y){
		x^y		
		}
	
    inv.logit<-function (x, min = 0, max = 1){
          p <- exp(x)/(1 + exp(x))
          p <- ifelse(is.na(p) & !is.na(x), 1, p)
          p <- p * (max - min) + min
          return(p)
      }
  	na.rm<-function(x){x<-x[!is.na(x)];return(x)}
  	empty2na<-function(x){if(length(x)==0){x<-NA};return(x)}
  	inf2nan<-function(x){x[x%in%c(Inf,-Inf)]<-NaN;return(x)}
  	nan2zero<-function(x){x[is.nan(x)]<-0;return(x)}
  	na2zero<-function(x){x[is.na(x)]<-0;return(x)}
  	na2false<-function(x){x[is.na(x)]<-FALSE;return(x)}
  	cap<-function(x,max=Inf,min=-Inf){x[x>max]<-max;x[x<min]<-min;return(x)}
  	clip<-function(x,min=-Inf,max=Inf){
  		x[x<min]<-min
  		x[x>max]<-max
  		return(x)}
    randomStrings<-function(n = 5000,no_letters=6,no_numbers=4) {
         a <- do.call(paste0, replicate(no_letters, sample(LETTERS, n, TRUE), FALSE))
          paste0(a, 
                 sprintf(paste("%0",no_numbers,"d",sep=""), 
                         sample(as.numeric(paste0(rep(9,no_numbers),collapse="")), n, TRUE)),
                 sample(LETTERS, n, TRUE))
    }


  	qsummary<-function(x){
  	  y<-quantile(x,c(0,.025,.1,.25,.5,.75,.975,1))
	  y<-c(y,Mean=mean(x,na.rm=T))
	  return(y)
  	  }


read_excel_allsheets <- function(y, tibble = FALSE) {
    # I prefer straight data.frames
    # but if you like tidyverse tibbles (the default with read_excel)
    # then just pass tibble = TRUE
    sheets <- readxl::excel_sheets(y)  
    x <- lapply(sheets, function(X) readxl::read_excel(y, sheet = X))
    if(!tibble) x <- lapply(x, as.data.frame)
    names(x) <- sheets
    x
}

zero_pad<-function(x,w){
require(stringr)
y<-str_pad(x, w, side = "left", pad = "0")
return(y)}	

na2false<-function(x){
  x[is.na(x)]<-FALSE
  return(x)
}

null2false<-function(x){
  if(length(x)==0){x<-FALSE};return(x)}
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
		x<-gsub(x=x,"^[^[:alnum:]]","")
		x<-trim(x)
		x<-gsub(x=x,"^-\\s","")
		x<-gsub(x=x,"-$","")
		x<-gsub(x=x,"\\n"," ")
		x<-gsub(x=x,"\\t"," ")
		x<-gsub(x=x,"\\s\\s+"," ")
		return(as.character(x))
	}
	
is.nan.data.frame <- function(x)
do.call(cbind, lapply(x, is.nan))	
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
		
		floor_digit<-function(x,digits=0){
				y<-x*(10^digits)
				y<-floor(y)
				y<-y/(10^digits)
				return(y)
			}

		ceiling_digit<-function(x,digits=0){
			y<-x*(10^digits)
			y<-ceiling(y)
			y<-y/(10^digits)
			return(y)
		}
		
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
###GERAL###
	duplicated.full<-function(x){return(duplicated(x)|duplicated(x,fromLast=TRUE))}
	pull <- function(x,y) {x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]}
	js.scrape<-function(x){
		writeLines(sprintf("
			var webPage = require('webpage');
			var page = webPage.create();
			var fs = require('fs');
			var path = 'temp.html'

			page.open('%s', function (status) {
			  var content = page.content;
			  fs.write(path,content,'w')
			  phantom.exit();
			});",x),con="scrape.js")
			
		system("phantomjs scrape.js")
		return(htmlParse(".//temp.html"))
	}					
				
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
				any(duplicated(gsub(x=x,'.log',''))),					#modelos com seu valor log e valor sem log
				(any(x%in%time.catg.cov) & any(x%in%time.cont.cov)),	#models with a categorical and continuous temporal effect
				(any(x=="rainfall") & any(x=="mo.rfall")),				#modelos que contenham precipitação em intervalos e ao mesmo tempo no mesmo mes
				(any(x=="sample_month") & any(x=="time"))))==FALSE}		#models with an effect of both surveys and sampling occasions

##Mapas

	kenup.scale<-function (x, y, relwidth = 0.15, metric = TRUE, ratio = TRUE, character=TRUE,
		...) #alteração simples da função maps::map.scale()
	{ 
	   format.pretty <- function(x, digits = 2) {
			x = signif(x, 2)
			prettyNum(formatC(x, format = "fg", digits = digits), 
				big.mark = ".")
		}
		usr <- par("usr")
		if (missing(y)) 
			y <- (9 * usr[3] + usr[4])/10
		if (abs(y) >= 90) 
			warning("location of scale out of this world!")
		if (missing(x)) 
			x <- (9 * usr[1] + usr[2])/10
		cosy <- cos((2 * pi * y)/360)
		perdeg <- (2 * pi * (6356.78 + 21.38 * cosy) * cosy)/360
		scale <- (perdeg * 1e+05)/(2.54 * (par("pin")/diff(par("usr"))[-2])[1])
		if (metric) 
			unit <- "km"
		else {
			perdeg <- perdeg * 0.6213712
			unit <- "mi"
		}
		len <- perdeg * relwidth * (usr[2] - usr[1])
		ats <- pretty(c(0, len), n = 2)
		nats <- length(ats)
		labs <- as.character(ats)
		labs[nats] <- paste(labs[nats], unit)
		linexy <- matrix(NA, ncol = 2, nrow = 3 * nats)
		colnames(linexy) <- c("x", "y")
		cxy <- par("cxy")
		dy <- cxy[2] * par("tcl")
		dx <- ats[nats]/perdeg/(nats - 1)
		linexy[1, ] <- c(x, y)
		linexy[2, ] <- c(x, y + dy)
		for (i in 1:(nats - 1)) {
			linexy[3 * i, ] <- c(x + (i - 1) * dx, y)
			linexy[3 * i + 1, ] <- c(x + i * dx, y)
			linexy[3 * i + 2, ] <- c(x + i * dx, y + dy)
		}
		# lines(linexy)
		# text(x + ats/perdeg, y + dy - 0.5 * cxy[2], labs, adj = c(0.4, 
			# 0.5), ...)
		if(character==FALSE){
		if (ratio) {
			text(x, y + 0.5 * cxy[2], paste("1:", format.pretty(scale), 
				sep = ""), adj = 0, ...)
		invisible(scale)}} else{
		return(paste("1:", format.pretty(scale),sep=""))
		}
	}
