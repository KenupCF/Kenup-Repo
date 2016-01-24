#######################
##Função "ind_record"##
###By: Caio Kenup######
###2014-04-10##########
#######Versão 2.0######
#######################

#######################
####THERE BE BUGS !####
#######################

##Argumentos:
##esp: vetor de espécies dos registros
##ind: vetor de individuos dos registros
##st: vetor de estações de registros
##datatype: "interval", para registros em intervalos contínuos no tempo; "point", para registros pontuais no tempo
##t_s: início do intervalo (para "point", momento do registro).
##t_e: fim do intervalo.
##z: critério de independência temporal (em minutos)
##var: variáveis que acompanham os registros

ind_record<-function(esp, ind, st, t_s, t_e, var=NULL, z, datatype="interval"){  #início da função
	if(datatype=="point"){t_e<-t_s}
	if(is(t_s)[1]=="POSIXct"){
		t_e<-t_e+1 #adiciona 1 segundo para o tempo do fim do intervalo, para a diferença nunca ser 0
		z_s<-z*60
		dados<-data.frame(sp=esp, indiv=ind, station=st, time_s=t_s, time_e=t_e) #junta todas as variáveis em uma dataframe
		if(!is.null(var)){
			dados<-cbind(dados,var)}  															# pool desirable variables together with photo data
		dados<-dados[order(dados$sp, dados$indiv, dados$station, dados$time_s, decreasing=F),]	#e as ordena
		ind_rec<-ceiling(as.numeric(difftime(dados$time_e[1],dados$time_s[1],units="secs")/z_s))
		for (i in 2:nrow(dados)){
			ee<-dados$time_e[i] 				#fim do intervalo 'i'
			ss<-dados$time_s[i] 				#inicio do intervalo 'i'
			li<-max(which(ind_rec!=0))  #ordem do último intervalo considerado independente
			if(dados$sp[i]==dados$sp[li] & dados$station[i]==dados$station[li] & dados$indiv[i]==dados$indiv[li]){
				xx<-difftime(ee,max(ss,dados$time_s[li]+z_s*ind_rec[li]),units="secs")/z_s}
			else {xx<-difftime(dados$time_e[i],dados$time_s[i],units="secs")/z_s}
			xx<-ceiling(as.numeric(xx)) ##arrendondando sempre pra cima, e tornando um número sem unidade
		ind_rec[i]<-xx
		} #fim do for
	dados<-cbind(dados, ind_rec)
	new.dados<-dados[0,]
	b<-rownames(dados[dados$ind_rec>1,]) # vetor com o nome das linhas que possuem + de 1 registro independente
	n<-1	
	r<-1
	x<-1
		while(n <= sum(dados$ind_rec[dados$ind_rec>1])){
			while (r <= dados[b[x],"ind_rec"]){
				new.dados[n,-(4:5)]<-dados[b[x],-(4:5)]
				new.dados[n,"time_s"]<-dados[b[x],"time_s"] + z_s*(r-1)
				new.dados[n,"time_e"]<-min((dados[b[x],"time_s"] + (z_s*(r-1)+z_s)),dados[b[x],"time_e"]) 
			n<-n+1	
			r<-r+1
			}
			x<-x+1
			r<-1
		}
	dados<-rbind(dados[-(dados$ind_rec>1),],new.dados)	
	dados[dados$ind_rec>1,"ind_rec"]<-1
	dados<-dados[order(dados$sp, dados$indiv, dados$station, dados$time_s, decreasing=F),]    								 
	return(dados)
	} else {stop("Time must be inserted as POSIXct class object")}  #Error message in case date is not Date Class}
}#fim da função
