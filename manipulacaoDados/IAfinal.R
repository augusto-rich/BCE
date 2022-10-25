#Definindo diretório de trabalho
setwd("D:/Non-Games/BCE/manipulacaoDados")

###

########------------------------- DTGM (DESVIO DA TAXA GERAL DE MORTALIDADE)
###
#Criação da variável de taxa de mortalidade geral por ano
#Dados para criação da TGM
obitos_geral_2000_2017 = read.csv("Obitos_Geral_2000_2017.csv", sep =",")
pop_residente_2000_2017 = read.csv("Pop_Residente_2000_2017.csv", sep =",")
#
TGM_2000_2017 = 1000 * (obitos_geral_2000_2017 / pop_residente_2000_2017)
#
#Cálculo dos desvios das taxas de mortalidade geral por triênio
DTGM_2000_2002 = rowMeans(abs(TGM_2000_2017[,2:4] - rowMeans(TGM_2000_2017[,2:4]))/rowMeans(TGM_2000_2017[,2:4]))
DTGM_2003_2005 = rowMeans(abs(TGM_2000_2017[,5:7] - rowMeans(TGM_2000_2017[,5:7]))/rowMeans(TGM_2000_2017[,5:7]))
DTGM_2006_2008 = rowMeans(abs(TGM_2000_2017[,8:10] - rowMeans(TGM_2000_2017[,8:10]))/rowMeans(TGM_2000_2017[,8:10]))
DTGM_2009_2011 = rowMeans(abs(TGM_2000_2017[,11:13] - rowMeans(TGM_2000_2017[,11:13]))/rowMeans(TGM_2000_2017[,11:13]))
DTGM_2012_2014 = rowMeans(abs(TGM_2000_2017[,14:16] - rowMeans(TGM_2000_2017[,14:16]))/rowMeans(TGM_2000_2017[,14:16]))
DTGM_2015_2017 = rowMeans(abs(TGM_2000_2017[,17:19] - rowMeans(TGM_2000_2017[,17:19]))/rowMeans(TGM_2000_2017[,17:19]))
DTGM_trienios = data.frame(COD_MUN=obitos_geral_2000_2017$Municipio,
                           DTGM_2000_2002=DTGM_2000_2002,
                           DTGM_2003_2005=DTGM_2003_2005,
                           DTGM_2006_2008=DTGM_2006_2008,
                           DTGM_2009_2011=DTGM_2009_2011,
                           DTGM_2012_2014=DTGM_2012_2014,
                           DTGM_2015_2017=DTGM_2015_2017
                           )
sum(is.na(DTGM_trienios)) #Numeros de entradas com NaN (0%). Realmente não deveria ter NaN.

##---
#Definição da adequação (ADEQUADO SE <= 0.10):
adeq_DTGM_trienios = DTGM_trienios
adeq_DTGM_trienios[,2:7] = DTGM_trienios[,2:7] <= 0.10 #TRUE = Adequado


########------------------------- PCMD (PROPORÇÃO DE ÓBITOS INFANTIS COM CAUSA MAL-DEFINIDA)
###
#Criação da variável de proporção de causas mal definidas entre óbitos infantis
#Dados para criação do PCMD 
obitos_infantis_cmd = read.csv("Obitos_Infantis_CMD.csv", sep =",")
obitos_infantis = read.csv("Obitos_Infantis.csv", sep =",")
#
PCMD_2000_2002 = rowSums(obitos_infantis_cmd[,2:4]) / rowSums(obitos_infantis[,2:4])
PCMD_2003_2005 = rowSums(obitos_infantis_cmd[,5:7]) / rowSums(obitos_infantis[,5:7])
PCMD_2006_2008 = rowSums(obitos_infantis_cmd[,8:10]) / rowSums(obitos_infantis[,8:10])
PCMD_2009_2011 = rowSums(obitos_infantis_cmd[,11:13]) / rowSums(obitos_infantis[,11:13])
PCMD_2012_2014 = rowSums(obitos_infantis_cmd[,14:16]) / rowSums(obitos_infantis[,14:16])
PCMD_2015_2017 = rowSums(obitos_infantis_cmd[,17:19]) / rowSums(obitos_infantis[,17:19])
PCMD_trienios = data.frame(COD_MUN=obitos_infantis$Municipio,
                           PCMD_2000_2002=PCMD_2000_2002,
                           PCMD_2003_2005=PCMD_2003_2005,
                           PCMD_2006_2008=PCMD_2006_2008,
                           PCMD_2009_2011=PCMD_2009_2011,
                           PCMD_2012_2014=PCMD_2012_2014,
                           PCMD_2015_2017=PCMD_2015_2017
                           )
sum(is.na(PCMD_trienios)) #Numeros de entradas com NaN (326/5118=6,4%)
#É NaN onde denominador=0. Logo nao teve obito infantil.
#Assim, nao faz sentido calcular esse indicador. Vamos manter como NaN
#O efeito disso é que este município não tera avaliação da adequação.

##---
#Definição da adequação (ADEQUADO SE <= 0.10):
adeq_PCMD_trienios = PCMD_trienios
adeq_PCMD_trienios[,2:7] = adeq_PCMD_trienios[,2:7] <= 0.10 #TRUE = Adequado
###


########------------------------- DTN (DESVIO MÉDIO DA TAXA DE NATALIDADE)
###
#Criação da variável de taxa de natalidade
#Dados para criação da TN
nascidos_vivos = read.csv("Nascidos Vivos.csv", sep =",")
#
taxa_natalidade = 1000 * (nascidos_vivos / pop_residente_2000_2017)
#
#Criação da taxa de desvio das taxas de natalidade por triênio
DTN_2000_2002 = rowMeans(abs(taxa_natalidade[,2:4] - rowMeans(taxa_natalidade[,2:4]))/rowMeans(taxa_natalidade[,2:4]))
DTN_2003_2005 = rowMeans(abs(taxa_natalidade[,5:7] - rowMeans(taxa_natalidade[,5:7]))/rowMeans(taxa_natalidade[,5:7]))
DTN_2006_2008 = rowMeans(abs(taxa_natalidade[,8:10] - rowMeans(taxa_natalidade[,8:10]))/rowMeans(taxa_natalidade[,8:10]))
DTN_2009_2011 = rowMeans(abs(taxa_natalidade[,11:13] - rowMeans(taxa_natalidade[,11:13]))/rowMeans(taxa_natalidade[,11:13]))
DTN_2012_2014 = rowMeans(abs(taxa_natalidade[,14:16] - rowMeans(taxa_natalidade[,14:16]))/rowMeans(taxa_natalidade[,14:16]))
DTN_2015_2017 = rowMeans(abs(taxa_natalidade[,17:19] - rowMeans(taxa_natalidade[,17:19]))/rowMeans(taxa_natalidade[,17:19]))
DTN_trienios = data.frame(COD_MUN=obitos_infantis$Municipio,
                          DTN_2000_2002=DTN_2000_2002,
                          DTN_2003_2005=DTN_2003_2005,
                          DTN_2006_2008=DTN_2006_2008,
                          DTN_2009_2011=DTN_2009_2011,
                          DTN_2012_2014=DTN_2012_2014,
                          DTN_2015_2017=DTN_2015_2017
                          )
sum(is.na(DTN_trienios)) #Numeros de entradas com NaN (0%). Realmente não deveria ter NaN.

##---
#Definição da adequação (ADEQUADO SE <= 0.10):
adeq_DTN_trienios = DTN_trienios
adeq_DTN_trienios[,2:7] = adeq_DTN_trienios[,2:7] <= 0.10 #TRUE = Adequado
###


########------------------------- TGMP (DESVIO MÉDIO DA TAXA GERAL DE MORTALIDADE PADRONIZADA)
###
#Definindo a população padrão:
pop_residente_idade_padrao = as.numeric(colSums(read.csv("Pop_Residente_idade_2010.csv", sep =",")[,-1]))
peso_padrao_idade = as.matrix(pop_residente_idade_padrao/sum(pop_residente_idade_padrao), nrow= 17, ncol=1)

##---
#
#CÁLCULO TAXA GERAL DE MORTALIDADE PADRONIZADA (TGMP) DE 2000-2002
#
#Puxando dados de óbitos gerais por ano
obitos_geral_idade_2000 = read.csv("Obitos_Geral_idade_2000.csv", sep =",")
obitos_geral_idade_2001 = read.csv("Obitos_Geral_idade_2001.csv", sep =",")
obitos_geral_idade_2002 = read.csv("Obitos_Geral_idade_2002.csv", sep =",")
#
#Puxando dados de população residente por ano
pop_residente_idade_2000 = read.csv("Pop_Residente_idade_2000.csv", sep =",")
pop_residente_idade_2001 = read.csv("Pop_Residente_idade_2001.csv", sep =",")
pop_residente_idade_2002 = read.csv("Pop_Residente_idade_2002.csv", sep =",")
#
obitos_geral_idade_2000_2002 = obitos_geral_idade_2000 + obitos_geral_idade_2001 + obitos_geral_idade_2002
pop_residente_idade_2000_2002 = pop_residente_idade_2000 + pop_residente_idade_2001 + pop_residente_idade_2002
taxa_mort_geral_2000_2002 = 1000*rowSums(obitos_geral_idade_2000_2002[,-1])/rowSums(pop_residente_idade_2000_2002[,-1])
taxa_mort_idade_2000_2002 = 1000*as.matrix(obitos_geral_idade_2000_2002[,-1] / pop_residente_idade_2000_2002[,-1], nrow=853, ncol=17, dimnames= FALSE)
TGMP_2000_2002 = as.numeric(taxa_mort_idade_2000_2002 %*% peso_padrao_idade)
plot(taxa_mort_geral_2000_2002,TGMP_2000_2002)
abline(a=0, b=1, col="red", lwd=2)
abline(v=4, h=4, col="blue", lwd=2, lty=2)
###

##---
#
#CÁLCULO TAXA GERAL DE MORTALIDADE PADRONIZADA (TGMP) DE 2003-2005
#Puxando dados de óbitos gerais por ano
obitos_geral_idade_2003 = read.csv("Obitos_Geral_idade_2003.csv", sep =",")
obitos_geral_idade_2004 = read.csv("Obitos_Geral_idade_2004.csv", sep =",")
obitos_geral_idade_2005 = read.csv("Obitos_Geral_idade_2005.csv", sep =",")
#
#Puxando dados de população residente por ano
pop_residente_idade_2003 = read.csv("Pop_Residente_idade_2003.csv", sep =",")
pop_residente_idade_2004 = read.csv("Pop_Residente_idade_2004.csv", sep =",")
pop_residente_idade_2005 = read.csv("Pop_Residente_idade_2005.csv", sep =",")
#
obitos_geral_idade_2003_2005 = obitos_geral_idade_2003 + obitos_geral_idade_2004 + obitos_geral_idade_2005
pop_residente_idade_2003_2005 = pop_residente_idade_2003 + pop_residente_idade_2004 + pop_residente_idade_2005
taxa_mort_geral_2003_2005 = 1000*rowSums(obitos_geral_idade_2003_2005[,-1])/rowSums(pop_residente_idade_2003_2005[,-1])
taxa_mort_idade_2003_2005 = as.matrix(obitos_geral_idade_2003_2005[,-1] / pop_residente_idade_2003_2005[,-1], nrow=853, ncol=17, dimnames= FALSE)
TGMP_2003_2005 = 1000*as.numeric(taxa_mort_idade_2003_2005 %*% peso_padrao_idade)
plot(taxa_mort_geral_2003_2005,TGMP_2003_2005)
abline(a=0, b=1, col="red", lwd=2)
abline(v=4, h=4, col="blue", lwd=2, lty=2)
###

##---
#
#CÁLCULO TAXA GERAL DE MORTALIDADE PADRONIZADA (TGMP) DE 2006-2008
#Puxando dados de óbitos gerais por ano
obitos_geral_idade_2006 = read.csv("Obitos_Geral_idade_2006.csv", sep =",")
obitos_geral_idade_2007 = read.csv("Obitos_Geral_idade_2007.csv", sep =",")
obitos_geral_idade_2008 = read.csv("Obitos_Geral_idade_2008.csv", sep =",")
#
#Puxando dados de população residente por ano
pop_residente_idade_2006 = read.csv("Pop_Residente_idade_2006.csv", sep =",")
pop_residente_idade_2007 = read.csv("Pop_Residente_idade_2007.csv", sep =",")
pop_residente_idade_2008 = read.csv("Pop_Residente_idade_2008.csv", sep =",")
#
obitos_geral_idade_2006_2008 = obitos_geral_idade_2006 + obitos_geral_idade_2007 + obitos_geral_idade_2008
pop_residente_idade_2006_2008 = pop_residente_idade_2006 + pop_residente_idade_2007 + pop_residente_idade_2008
taxa_mort_geral_2006_2008 = rowSums(obitos_geral_idade_2006_2008[,-1])/rowSums(pop_residente_idade_2006_2008[,-1]) * 1000
taxa_mort_idade_2006_2008 = as.matrix(obitos_geral_idade_2006_2008[,-1] / pop_residente_idade_2006_2008[,-1], nrow=853, ncol=17, dimnames= FALSE)
TGMP_2006_2008 = as.numeric(taxa_mort_idade_2006_2008 %*% peso_padrao_idade)*1000
plot(taxa_mort_geral_2006_2008,TGMP_2006_2008)
abline(a=0, b=1, col="red", lwd=2)
abline(v=4, h=4, col="blue", lwd=2, lty=2)
###

##---
#
#CÁLCULO TAXA GERAL DE MORTALIDADE PADRONIZADA (TGMP) DE 2009-2011
#Puxando dados de óbitos gerais por ano
obitos_geral_idade_2009 = read.csv("Obitos_Geral_idade_2009.csv", sep =",")
obitos_geral_idade_2010 = read.csv("Obitos_Geral_idade_2010.csv", sep =",")
obitos_geral_idade_2011 = read.csv("Obitos_Geral_idade_2011.csv", sep =",")
#
#Puxando dados de população residente por ano
pop_residente_idade_2009 = read.csv("Pop_Residente_idade_2009.csv", sep =",")
pop_residente_idade_2010 = read.csv("Pop_Residente_idade_2010.csv", sep =",")
pop_residente_idade_2011 = read.csv("Pop_Residente_idade_2011.csv", sep =",")
#
obitos_geral_idade_2009_2011 = obitos_geral_idade_2009 + obitos_geral_idade_2010 + obitos_geral_idade_2011
pop_residente_idade_2009_2011 = pop_residente_idade_2009 + pop_residente_idade_2010 + pop_residente_idade_2011
taxa_mort_geral_2009_2011 = rowSums(obitos_geral_idade_2009_2011[,-1])/rowSums(pop_residente_idade_2009_2011[,-1]) * 1000
taxa_mort_idade_2009_2011 = as.matrix(obitos_geral_idade_2009_2011[,-1] / pop_residente_idade_2009_2011[,-1], nrow=853, ncol=17, dimnames= FALSE)
TGMP_2009_2011 = as.numeric(taxa_mort_idade_2009_2011 %*% peso_padrao_idade)*1000
plot(taxa_mort_geral_2009_2011,TGMP_2009_2011)
abline(a=0, b=1, col="red", lwd=2)
abline(v=4, h=4, col="blue", lwd=2, lty=2)
###

##---
#
#CÁLCULO TAXA GERAL DE MORTALIDADE PADRONIZADA (TGMP) DE 2012-2014
#Puxando dados de óbitos gerais por ano
obitos_geral_idade_2012 = read.csv("Obitos_Geral_idade_2012.csv", sep =",")
obitos_geral_idade_2013 = read.csv("Obitos_Geral_idade_2013.csv", sep =",")
obitos_geral_idade_2014 = read.csv("Obitos_Geral_idade_2014.csv", sep =",")
#
#Puxando dados de população residente por ano
pop_residente_idade_2012 = read.csv("Pop_Residente_idade_2012.csv", sep =",")
pop_residente_idade_2013 = read.csv("Pop_Residente_idade_2013.csv", sep =",")
pop_residente_idade_2014 = read.csv("Pop_Residente_idade_2014.csv", sep =",")
#
obitos_geral_idade_2012_2014 = obitos_geral_idade_2012 + obitos_geral_idade_2013 + obitos_geral_idade_2014
pop_residente_idade_2012_2014 = pop_residente_idade_2012 + pop_residente_idade_2013 + pop_residente_idade_2014
taxa_mort_geral_2012_2014 = rowSums(obitos_geral_idade_2012_2014[,-1])/rowSums(pop_residente_idade_2012_2014[,-1]) * 1000
taxa_mort_idade_2012_2014 = as.matrix(obitos_geral_idade_2012_2014[,-1] / pop_residente_idade_2012_2014[,-1], nrow=853, ncol=17, dimnames= FALSE)
TGMP_2012_2014 = as.numeric(taxa_mort_idade_2012_2014 %*% peso_padrao_idade)*1000
plot(taxa_mort_geral_2012_2014,TGMP_2012_2014)
abline(a=0, b=1, col="red", lwd=2)
abline(v=4, h=4, col="blue", lwd=2, lty=2)
###

##---
#
#CÁLCULO TAXA GERAL DE MORTALIDADE PADRONIZADA (TGMP) DE 2015-2017
#Puxando dados de óbitos gerais por ano
obitos_geral_idade_2015 = read.csv("Obitos_Geral_idade_2015.csv", sep =",")
obitos_geral_idade_2016 = read.csv("Obitos_Geral_idade_2016.csv", sep =",")
obitos_geral_idade_2017 = read.csv("Obitos_Geral_idade_2017.csv", sep =",")
#
#Puxando dados de população residente por ano
pop_residente_idade_2015 = read.csv("Pop_Residente_idade_2015.csv", sep =",")
pop_residente_idade_2016 = read.csv("Pop_Residente_idade_2016.csv", sep =",")
pop_residente_idade_2017 = read.csv("Pop_Residente_idade_2017.csv", sep =",")
#
obitos_geral_idade_2015_2017 = obitos_geral_idade_2015 + obitos_geral_idade_2016 + obitos_geral_idade_2017
pop_residente_idade_2015_2017 = pop_residente_idade_2015 + pop_residente_idade_2016 + pop_residente_idade_2017
taxa_mort_geral_2015_2017 = rowSums(obitos_geral_idade_2015_2017[,-1])/rowSums(pop_residente_idade_2015_2017[,-1]) * 1000
taxa_mort_idade_2015_2017 = as.matrix(obitos_geral_idade_2015_2017[,-1] / pop_residente_idade_2015_2017[,-1], nrow=853, ncol=17, dimnames= FALSE)
TGMP_2015_2017 = as.numeric(taxa_mort_idade_2015_2017 %*% peso_padrao_idade)*1000
plot(taxa_mort_geral_2015_2017,TGMP_2015_2017)
abline(a=0, b=1, col="red", lwd=2)
abline(v=4, h=4, col="blue", lwd=2, lty=2)
###


##---
#Junção dos TGMP_trienios em um data.frame só
TGMP_trienios = data.frame(COD_MUN=obitos_infantis$Municipio,
                          TGMP_2000_2002=TGMP_2000_2002,
                          TGMP_2003_2005=TGMP_2003_2005,
                          TGMP_2006_2008=TGMP_2006_2008,
                          TGMP_2009_2011=TGMP_2009_2011,
                          TGMP_2012_2014=TGMP_2012_2014,
                          TGMP_2015_2017=TGMP_2015_2017
                          )
sum(is.na(DTN_trienios)) #Numeros de entradas com NaN (0%). Realmente não deveria ter NaN.

##---
#Definição da adequação (ADEQUADO SE >= 4):
adeq_TGMP_trienios = TGMP_trienios
adeq_TGMP_trienios[,2:7] = adeq_TGMP_trienios[,2:7] >= 4 #TRUE = Adequado
###



############################## CRIAÇÃO DAS CATEGORIAS DE ADEQUAÇÃO POR MUNICÍPIO
#Criação das Categorias de classificação dos municípios
##---
#Definição das categorias:

cat_adeq_trienios <- matrix(data=NA, 
                            nrow=nrow(TGMP_trienios), 
                            ncol=ncol(TGMP_trienios))
cat_adeq_trienios[,1] <- TGMP_trienios[,1] #COD_MUN
colnames(cat_adeq_trienios) <- c('COD_MUN', '2000_2002', '2003_2005', 
                                 '2006_2008', '2009_2011', 
                                 '2012_2014', '2015_2017')

#CLASSIFICAÇÃO DOS TGMP #"&"=interceção ("e") e "|"=uniao ("ou" "pelo menos")
for(j in 2:ncol(TGMP_trienios)){
  for(i in 1:nrow(TGMP_trienios)){
    if(!is.na(adeq_PCMD_trienios[i,j])){
      if(adeq_TGMP_trienios[i,j]==TRUE & adeq_DTGM_trienios[i,j]==TRUE & adeq_DTN_trienios[i,j]==TRUE & adeq_PCMD_trienios[i,j]==TRUE){
        cat_adeq_trienios[i,j] <- "Categoria 1 ou Adequada (C1)"}
      if(adeq_TGMP_trienios[i,j]==TRUE & (adeq_DTGM_trienios[i,j]==FALSE | adeq_DTN_trienios[i,j]==FALSE | adeq_PCMD_trienios[i,j]==FALSE)){
        cat_adeq_trienios[i,j] <- "Categoria 2 ou Intermediária (C2)"}
      if(adeq_TGMP_trienios[i,j]==FALSE){
        cat_adeq_trienios[i,j] <- "Categoria 3 ou Inadequada (C3)"}
    }
  }
}
View(cat_adeq_trienios)


############################## CRIAÇÃO DO INDICE DE ADEQUAÇÃO POR MICRORREGIAO
##
#Primeiro, puxar arquivo com  códigos das micrroregioes:
#
##pacotes necessários
if(!require(readxl)){ install.packages("readxl"); require(readxl)} #para ler xlsx
if(!require(dplyr)){ install.packages("dplyr"); require(dplyr)} #para manipulação de dados
if(!require(tidyr)){ install.packages("tidyr"); require(tidyr)} #para manipulação de dados

##-- Carregando codigos subdivisoes IBGE:
cod_IBGE <- read_excel("RELATORIO_DTB_BRASIL_MUNICIPIO.xls") %>%
  filter(Nome_UF=='Minas_Gerais') %>%
  select(c("COD_Municipio_Reduzido", 
           "COD_Microrregiao_Geografica", 
           "Nome_Microrregiao"))

##-- Unindo base dados com os codigos das subdivisoes do IBGE:
cat_adeq_trienios_merge <- merge(x = cod_IBGE, y = cat_adeq_trienios, 
                                 by.x = c("COD_Municipio_Reduzido"), 
                                 by.y = c("COD_MUN"), all.y = TRUE)
nrow(cat_adeq_trienios_merge) #853 municipios
head(cat_adeq_trienios_merge)


##--
#Pra criar o IA, precisaremos da população em cada MUNI em casa TRIENIO
#Para o proximo passo, importante que ela tenha a mesma estrutura da 'cat_adeq_trienios_merge'
pop_residente_trienios <- cat_adeq_trienios_merge
pop_residente_trienios[,4:9] <- NA

pop_residente_trienios[,4] = rowSums(pop_residente_2000_2017[,2:4]) #trienio 20020_2002
pop_residente_trienios[,5] = rowSums(pop_residente_2000_2017[,5:7])
pop_residente_trienios[,6] = rowSums(pop_residente_2000_2017[,8:10])
pop_residente_trienios[,7] = rowSums(pop_residente_2000_2017[,11:13])
pop_residente_trienios[,8] = rowSums(pop_residente_2000_2017[,14:16])
pop_residente_trienios[,9] = rowSums(pop_residente_2000_2017[,17:19])


####----
##
#Vamos transformar a base de dados do formato wide para long
#Isso vai facilitar a calcular o pC1, pC2 e Pc3 necessarios para obter o IA:
#Será feiro anexando matrix de população total e matriz de categorias:
if(!require(reshape2)){ install.packages("reshape2"); require(reshape2)} #transformar formato dos dados

cat_adeq_trienios_merge <- melt(cat_adeq_trienios_merge, #transformar de wide para long
                                id.vars = c("COD_Municipio_Reduzido",
                                            "COD_Microrregiao_Geografica",
                                            "Nome_Microrregiao"), 
                                variable.name = "Trienio")
colnames(cat_adeq_trienios_merge)[5] <- "Categoria_Adequacao"

pop_residente_trienios <- melt(pop_residente_trienios, 
                               id.vars = c("COD_Municipio_Reduzido",
                                           "COD_Microrregiao_Geografica",
                                           "Nome_Microrregiao"), 
                               variable.name = "Trienio")
colnames(pop_residente_trienios)[5] <- "Populacao"

#Nesse tipo de junção, tem-se que tomar cuidado pois a ordem dos municipios
#deve ser a mesma nos dois bancos de dados. Neste caso está, então podemos juntar diretamente.
cat_adeq_trienios_final = data.frame(cat_adeq_trienios_merge, 
                                     Populacao = pop_residente_trienios[,5])
cat_adeq_trienios_final$Categoria_Adequacao <- as.factor(cat_adeq_trienios_final$Categoria_Adequacao)
View(cat_adeq_trienios_final)
#write.csv2(cat_adeq_trienios_final, file="cat_adeq_trienios_final.csv")

####----
##
#Calculando proporções de população por categoria de adequação para cada MICRO
prop_pop_cat <- cat_adeq_trienios_final %>% 
  filter(!is.na(Categoria_Adequacao))%>%
  group_by(COD_Microrregiao_Geografica, Nome_Microrregiao, 
           Trienio, Categoria_Adequacao) %>%
  summarise(Pop = sum(Populacao)) %>%
  ungroup%>%
  group_by(COD_Microrregiao_Geografica, Nome_Microrregiao, Trienio)%>%
  mutate(Tot=sum(Pop), Prop=Pop/Tot) %>%
  complete(Categoria_Adequacao, fill=list(Prop=0))

write.csv2(prop_pop_cat, file="prop_pop_cat.csv")

####----
##
#Calculando o IA para cada MICRO
##
#Vamos transformar a base de dados do formato long para wide
#Isso vai facilitar a calcular o IA em função do pC1, pC2 e pC3

names(prop_pop_cat)
prop_pop_cat <- prop_pop_cat %>% 
  select(COD_Microrregiao_Geografica, Nome_Microrregiao, 
         Trienio, Categoria_Adequacao, Prop)
  
indice_adequacao <- dcast(prop_pop_cat, #trnasformar de long para wide
                          formula = Trienio + Nome_Microrregiao +
                            COD_Microrregiao_Geografica ~ Categoria_Adequacao, 
                          value.var = "Prop") %>%
  mutate(Indice_Adequacao = 100*(`Categoria 1 ou Adequada (C1)` + 
           `Categoria 2 ou Intermediária (C2)`/2 -
           `Categoria 3 ou Inadequada (C3)`))
  

write.csv2(indice_adequacao, file="indice_adequacao.csv")

#TAREFA
#ANEXAR COLUNAS COM CATEGORIAS DE MINAS 1, MINAS 2, MINAS 3 E MINAS 4

indice_adequacao <- indice_adequacao %>%
  mutate(Minas_Categoria = case_when(
      Indice_Adequacao > 70 ~ "Minas 1",
      Indice_Adequacao >= 50.1 & Indice_Adequacao <= 70 ~ "Minas 2",
      Indice_Adequacao >= 20 & Indice_Adequacao <= 50 ~ "Minas 3",
      Indice_Adequacao < 20 ~ "Minas 4"
))


##########---------------- MAPAS
########
##
#---- Part Seven: Maps of posterior results
# Produce a map of the observed TB cases from 2012-2014
if(!require(geobr)){ install.packages("geobr"); require(geobr)}  #brazilian maps
if(!require(crul)){ install.packages("crul"); require(crul)}
if(!require(ggplot2)){ install.packages("ggplot2"); require(ggplot2)}
if(!require(sf)){ install.packages("sf"); require(sf)}
if(!require(dplyr)){ install.packages("dplyr"); require(dplyr)}
if(!require(ggsn)){ install.packages("ggsn"); require(ggsn)} #pra add bússula
if(!require(dichromat)){ install.packages("dichromat"); require(dplyr)}

microMG_map <- read_micro_region(code_micro=31, year=2013, 
                                 simplified=FALSE, showProgress = TRUE) #vem do 'geobr'
names(microMG_map)
nrow(microMG_map)


###BEFORE MAPPING, ENSURING THAT ORDER OF MICRORREGIONS INDEX MATCH IN geobr AND DATA FILE
##-- Unindo base dados com os codigos das subdivisoes do IBGE:
names(microMG_map)
names(indice_adequacao)

microMG_map <- merge(x = microMG_map, y = indice_adequacao, 
                     by.x = c("code_micro"), 
                     by.y = c("COD_Microrregiao_Geografica"), all.y = TRUE)
head(microMG_map)

#
## Map of IA 2000_2002
names(microMG_map)
microMG_map_2000_2002 <- microMG_map %>%
  filter(Trienio=='2000_2002')

ggplot() +
  geom_sf(data=microMG_map_2000_2002, mapping=aes(fill=Indice_Adequacao),
          color=grey(0.5), size = 0.01) + #colour=NA) + # colour=NA to remove borders
  ggtitle('IA 2000_2002')
ggsave('map_IA2000_2002.png',device='png',width=6,height=6)

#IA 2003_2005
microMG_map_2003_2005 <- microMG_map %>%
  filter(Trienio=='2003_2005')

ggplot() +
  geom_sf(data=microMG_map_2003_2005, mapping=aes(fill=Indice_Adequacao),
          color=grey(0.5), size=0.01) +
    ggtitle('IA 2003_2005')
ggsave('map_IA2003_2005.png',device='png',width=6,height=6)

#IA 2006_2008
microMG_map_2006_2008 <- microMG_map %>%
  filter(Trienio=='2006_2008')

ggplot() +
  geom_sf(data=microMG_map_2006_2008, mapping=aes(fill=Indice_Adequacao),
          color=grey(0.5), size=0.01) +
  ggtitle('IA 2006_2008')
ggsave('map_IA2006_2008.png',device='png',width=6,height=6)

#IA 2009_2011
microMG_map_2009_2011 <- microMG_map %>%
  filter(Trienio=='2003_2005')

ggplot() +
  geom_sf(data=microMG_map_2009_2011, mapping=aes(fill=Indice_Adequacao),
          color=grey(0.5), size=0.01) +
  ggtitle('IA 2009_2011')
ggsave('map_IA2009_2011.png',device='png',width=6,height=6)

#IA 2012_2014
microMG_map_2012_2014 <- microMG_map %>%
  filter(Trienio=='2012_2014')

ggplot() +
  geom_sf(data=microMG_map_2012_2014, mapping=aes(fill=Indice_Adequacao),
          color=grey(0.5), size=0.01) +
  ggtitle('IA 2012_2014')
ggsave('map_IA2012_2014.png',device='png',width=6,height=6)

#IA 2015_2017
microMG_map_2015_2017 <- microMG_map %>%
  filter(Trienio=='2015_2017')

ggplot() +
  geom_sf(data=microMG_map_2015_2017, mapping=aes(fill=Indice_Adequacao),
          color=grey(0.5), size=0.01) +
  ggtitle('IA 2015_2017')
ggsave('map_IA2015_2017.png',device='png',width=6,height=6)