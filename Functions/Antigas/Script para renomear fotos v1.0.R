###########################################################
###Selecione tudo e aperte "Ctrl+R" para ativar a função###
###########################################################

renomeador<-function() {
#input do usuario
setwd(choose.dir())
animal=readline("Digite o nome do animal ")
ponto=readline("Digite o nome do ponto ")
offset=readline("Digite a correção do horário (em horas). Se não for necessário, digite 0 ")
offset=as.numeric(offset)

#extração de horas dos arquivos
datas=system("\"D:\\Cutias PNT\\Câmera Trap\\exiftool.exe\" -s -T -\"ModifyDate\" *.JPG",intern=T)
datas=strtrim(gsub(":","-",datas),19)


if(offset!=0){
datas.numerico=as.POSIXct(datas,format="%Y-%m-%d %H-%M-%S")
datas.numerico=datas.numerico+(offset*3600)
datas=as.character(datas.numerico,format="%Y-%m-%d %H-%M-%S")
}

file.rename(list.files(pattern="*.JPG"),paste(animal,ponto,strtrim(datas,19),1:length(list.files(pattern="*.JPG")),".JPG",sep=" "))
}

############################################################################
###Para renomear a fotos, simplesmente use "renomeador()" na interface do R#
############################################################################

renomeador()