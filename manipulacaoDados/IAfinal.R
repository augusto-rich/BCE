#Definindo diretório de trabalho
setwd("D:/Non-Games/BCE/manipulacaoDados") 
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
DTGM_trienios[is.na(DTGM_trienios)] = 0
adeq_DTGM_trienios = DTGM_trienios
adeq_DTGM_trienios = DTGM_trienios[,2:7] <= 0.10 #TRUE = Adequado

###

#Criação da variável de proporção de causas mal definidas entre óbitos infantis
#PCMD_2000_2017 = obitos_infantis_cmd / obitos_infantis
#PCMD_2000_2017[is.na(PCMD_2000_2017)] = 0
#PCMD_trienios = matrix(,nrow = 853, ncol = 7)
#PCMD_trienios[,1] = obitos_geral_idade_2000[,1]
#for (j in 1:853) {
#  for (i in 2:7) {
#    PCMD_trienios[j,i] = sum(obitos_infantis_cmd[j,(i + 2 *(i - 2)):((i + 2 *(i - 2))+2)]) / sum(obitos_infantis[j,(i + 2 *(i - 2)):((i + 2 *(i - 2))+2)])
#  }
#}
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
PCMD_trienios[is.na(PCMD_trienios)] = 0
adeq_PCMD_trienios = PCMD_trienios
adeq_PCMD_trienios = adeq_PCMD_trienios[,2:7] <= 0.10 #TRUE = Adequado
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
DTN_trienios[is.na(DTN_trienios)] = 0
adeq_DTN_trienios = DTN_trienios
adeq_DTN_trienios = adeq_DTN_trienios[,2:7] <= 0.10 #TRUE = Adequado
###

#CÁLCULO TAXA GERAL DE MORTALIDADE PADRONIZADA (TGMP) - DEFININDO A POP. PADRAO
#
#Definindo a população padrão:
pop_residente_idade_padrao = as.numeric(colSums(read.csv("Pop_Residente_idade_2010.csv", sep =",")[,-1]))
peso_padrao_idade = as.matrix(pop_residente_idade_padrao/sum(pop_residente_idade_padrao), nrow= 17, ncol=1)
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
plot(taxa_mort_geral_2000_2002,TGMP_trienio_2000_2002)
abline(a=0, b=1, col="red", lwd=2)
abline(v=4, h=4, col="blue", lwd=2, lty=2)
###

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
plot(taxa_mort_geral_2003_2005,TGMP_trienio_2003_2005)
abline(a=0, b=1, col="red", lwd=2)
abline(v=4, h=4, col="blue", lwd=2, lty=2)
###

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
plot(taxa_mort_geral_2006_2008,TGMP_trienio_2006_2008)
abline(a=0, b=1, col="red", lwd=2)
abline(v=4, h=4, col="blue", lwd=2, lty=2)
###

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
plot(taxa_mort_geral_2009_2011,TGMP_trienio_2009_2011)
abline(a=0, b=1, col="red", lwd=2)
abline(v=4, h=4, col="blue", lwd=2, lty=2)
###

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
plot(taxa_mort_geral_2012_2014,TGMP_trienio_2012_2014)
abline(a=0, b=1, col="red", lwd=2)
abline(v=4, h=4, col="blue", lwd=2, lty=2)
###

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
plot(taxa_mort_geral_2015_2017,TGMP_trienio_2015_2017)
abline(a=0, b=1, col="red", lwd=2)
abline(v=4, h=4, col="blue", lwd=2, lty=2)
###

#Junção dos TGMP_trienios em um data.frame só
TGMP_trienios = data.frame(COD_MUN=obitos_infantis$Municipio,
                          TGMP_2000_2002=TGMP_2000_2002,
                          TGMP_2003_2005=TGMP_2003_2005,
                          TGMP_2006_2008=TGMP_2006_2008,
                          TGMP_2009_2011=TGMP_2009_2011,
                          TGMP_2012_2014=TGMP_2012_2014,
                          TGMP_2015_2017=TGMP_2015_2017
                          )
TGMP_trienios[is.na(TGMP_trienios)] = 0
adeq_TGMP_trienios = TGMP_trienios
adeq_TGMP_trienios = adeq_TGMP_trienios[,2:7] <= 4 #TRUE = Adequado
###

#Criação das Categorias de classificação dos municípios
  ifelse((adeq_DTN_trienios & adeq_DTGM_trienios & adeq_PCMD_trienios & adeq_TGMP_trienios == TRUE)){
    categ1 = data.frame(COD_MUN=obitos_infantis$Municipio,
                        TGMP_2000_2002=adeq_TGMP_trienios$TGMP_2000_2002,
                        TGMP_2003_2005=adeq_TGMP_trienios$TGMP_2003_2005,
                        TGMP_2006_2008=adeq_TGMP_trienios$TGMP_2006_2008
                        )
  }
