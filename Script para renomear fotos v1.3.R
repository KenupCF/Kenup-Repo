###########################################################
###Selecione tudo e aperte "Ctrl+R" para ativar a fun��o###
###########################################################

######################################################
####Vers�o 1.3########################################
###2015-07-07#########################################
###Modificada por Caio Kenup##########################
###Corre��o: N�o ajusta mais para hor�rio de ver�o####
###1.3 Agora a informa��o do offset continua na foto.# 
######isto �, � possivel recuperar a data errada da ##
######foto a partir da data certa#####################
######################################################

clearwd<-function(){
setwd("C:")
}

renomeador2 <- function(wd,sp,st,lado,offset) {
#input do usuario
setwd(wd)
animal=sp
ponto=st
lado=lado
offset=offset
offset=as.numeric(offset)

#extra��o de horas dos arquivos
datas=system("\"D:\\Cutias PNT\\C�mera Trap\\exiftool.exe\" -s -T -\"ModifyDate\" *.JPG",intern=T)
datas=strtrim(gsub(":","-",datas),19)


if(offset!=0){
datas.numerico=as.POSIXct(datas,format="%Y-%m-%d %H-%M-%S", tz="Etc/GMT-3")  ###O �ltimo argumento dessa fun��o serve para "ignorar" o hor�rio de ver�o
datas.numerico=datas.numerico+(offset*3600)
datas=as.character(datas.numerico,format="%Y-%m-%d %H-%M-%S")
}

file.rename(list.files(pattern="*.JPG"),paste(animal,ponto,strtrim(datas,19),lado,1:length(list.files(pattern="*.JPG")),paste("offset_",offset,sep=""),".JPG",sep=" "))
}

############################################################################
###Para renomear a fotos, simplesmente use "renomeador()" na interface do R#
############################################################################

renomeador2(sp="",		#aqui vai a esp�cie, entre aspas
st="",			#aqui vai a esta��o, entre aspas
lado="",				#lado da camera (Esq ou Dir), entre aspas			
offset=,			#aqui vai o offset
#abaixo vai o caminho da pasta, com barras duplas e entre aspas
wd=""
)
clearwd()