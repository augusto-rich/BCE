#Script para puxar as planilhas para dentro do software R
#
##pacotes necessários:
if(!require(dplyr)){ install.packages("dplyr"); require(dplyr)} #manipular dados
#
setwd("D:/Non-Games/BCE/manipulacaoDados") #Definindo diretório de trabalho
#
##função para puxar as diversas planilhas .csv para o ambiente de desenvolvimento
temp = list.files(pattern="*.csv")
temp
myfiles = lapply(temp, read.csv, stringsAsFactors=FALSE, fileEncoding="latin1")
#