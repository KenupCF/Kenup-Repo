###########################################################
###Selecione tudo e aperte "Ctrl+R" para ativar a função###
###########################################################

######################################################
####Versão 1.3########################################
###2015-07-07#########################################
###Modificada por Caio Kenup##########################
###Correção: Não ajusta mais para horário de verão####
###1.3 Agora a informação do offset continua na foto.# 
######isto é, é possivel recuperar a data errada da ##
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

#extração de horas dos arquivos
datas=system("\"D:\\Cutias PNT\\Câmera Trap\\exiftool.exe\" -s -T -\"ModifyDate\" *.JPG",intern=T)
datas=strtrim(gsub(":","-",datas),19)


if(offset!=0){
datas.numerico=as.POSIXct(datas,format="%Y-%m-%d %H-%M-%S", tz="Etc/GMT-3")  ###O último argumento dessa função serve para "ignorar" o horário de verão
datas.numerico=datas.numerico+(offset*3600)
datas=as.character(datas.numerico,format="%Y-%m-%d %H-%M-%S")
}

file.rename(list.files(pattern="*.JPG"),paste(animal,ponto,strtrim(datas,19),lado,1:length(list.files(pattern="*.JPG")),paste("offset_",offset,sep=""),".JPG",sep=" "))
}

############################################################################
###Para renomear a fotos, simplesmente use "renomeador()" na interface do R#
############################################################################

renomeador2(sp="",		#aqui vai a espécie, entre aspas
st="",			#aqui vai a estação, entre aspas
lado="",				#lado da camera (Esq ou Dir), entre aspas			
offset=,			#aqui vai o offset
#abaixo vai o caminho da pasta, com barras duplas e entre aspas
wd=""
)
clearwd()