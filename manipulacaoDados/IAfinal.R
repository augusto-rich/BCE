#Script para puxar as planilhas para dentro do software R
#
##pacotes necessários:
if(!require(dplyr)){ install.packages("dplyr"); require(dplyr)} #manipular dados
#
setwd("D:/Non-Games/BCE/manipulacaoDados") #Definindo diretório de trabalho
#
##comandos para puxar as diversas planilhas .csv para o ambiente de desenvolvimento
nascidos_vivos = read.csv("Nascidos Vivos.csv", sep =",")
obitos_geral_2000_2017 = read.csv("Obitos_Geral_2000_2017.csv", sep =",")
obitos_geral_idade_2000 = read.csv("Obitos_Geral_idade_2000.csv", sep =",")
obitos_geral_idade_2001 = read.csv("Obitos_Geral_idade_2001.csv", sep =",")
obitos_geral_idade_2002 = read.csv("Obitos_Geral_idade_2002.csv", sep =",")
obitos_geral_idade_2003 = read.csv("Obitos_Geral_idade_2003.csv", sep =",")
obitos_geral_idade_2004 = read.csv("Obitos_Geral_idade_2004.csv", sep =",")
obitos_geral_idade_2005 = read.csv("Obitos_Geral_idade_2005.csv", sep =",")
obitos_geral_idade_2006 = read.csv("Obitos_Geral_idade_2006.csv", sep =",")
obitos_geral_idade_2007 = read.csv("Obitos_Geral_idade_2007.csv", sep =",")
obitos_geral_idade_2008 = read.csv("Obitos_Geral_idade_2008.csv", sep =",")
obitos_geral_idade_2009 = read.csv("Obitos_Geral_idade_2009.csv", sep =",")
obitos_geral_idade_2010 = read.csv("Obitos_Geral_idade_2010.csv", sep =",")
obitos_geral_idade_2011 = read.csv("Obitos_Geral_idade_2011.csv", sep =",")
obitos_geral_idade_2012 = read.csv("Obitos_Geral_idade_2012.csv", sep =",")
obitos_geral_idade_2013 = read.csv("Obitos_Geral_idade_2013.csv", sep =",")
obitos_geral_idade_2014 = read.csv("Obitos_Geral_idade_2014.csv", sep =",")
obitos_geral_idade_2015 = read.csv("Obitos_Geral_idade_2015.csv", sep =",")
obitos_geral_idade_2016 = read.csv("Obitos_Geral_idade_2016.csv", sep =",")
obitos_geral_idade_2017 = read.csv("Obitos_Geral_idade_2017.csv", sep =",")
obitos_infantis_cmd = read.csv("Obitos_Infantis_CMD.csv", sep =",")
obitos_infantis = read.csv("Obitos_Infantis.csv", sep =",")
pop_residente_2000_2017 = read.csv("Pop_Residente_2000_2017.csv", sep =",")
pop_residente_idade_2000 = read.csv("Pop_Residente_idade_2000.csv", sep =",")
pop_residente_idade_2001 = read.csv("Pop_Residente_idade_2001.csv", sep =",")
pop_residente_idade_2002 = read.csv("Pop_Residente_idade_2002.csv", sep =",")
pop_residente_idade_2003 = read.csv("Pop_Residente_idade_2003.csv", sep =",")
pop_residente_idade_2004 = read.csv("Pop_Residente_idade_2004.csv", sep =",")
pop_residente_idade_2005 = read.csv("Pop_Residente_idade_2005.csv", sep =",")
pop_residente_idade_2006 = read.csv("Pop_Residente_idade_2006.csv", sep =",")
pop_residente_idade_2007 = read.csv("Pop_Residente_idade_2007.csv", sep =",")
pop_residente_idade_2008 = read.csv("Pop_Residente_idade_2008.csv", sep =",")
pop_residente_idade_2009 = read.csv("Pop_Residente_idade_2009.csv", sep =",")
pop_residente_idade_2010 = read.csv("Pop_Residente_idade_2010.csv", sep =",")
pop_residente_idade_2011 = read.csv("Pop_Residente_idade_2011.csv", sep =",")
pop_residente_idade_2012 = read.csv("Pop_Residente_idade_2012.csv", sep =",")
pop_residente_idade_2013 = read.csv("Pop_Residente_idade_2013.csv", sep =",")
pop_residente_idade_2014 = read.csv("Pop_Residente_idade_2014.csv", sep =",")
pop_residente_idade_2015 = read.csv("Pop_Residente_idade_2015.csv", sep =",")
pop_residente_idade_2016 = read.csv("Pop_Residente_idade_2016.csv", sep =",")
pop_residente_idade_2017 = read.csv("Pop_Residente_idade_2017.csv", sep =",")
#

#Criação da variável de taxa de mortalidade geral por ano
TGM_2000_2017 = obitos_geral_2000_2017 / pop_residente_2000_2017

#Criação da variável de proporção de causas mal definidas entre óbitos infantis
PCMD_2000_2017 = obitos_infantis_cmd / obitos_infantis
PCMD_2000_2017[is.na(PCMD_2000_2017)] = 0
PCMD_trienios = matrix(,nrow = 853, ncol = 7)
PCMD_trienios[,1] = obitos_geral_idade_2000[,1]
for (j in 1:853) {
  for (i in 2:7) {
    PCMD_trienios[j,i] = sum(obitos_infantis_cmd[j,(i + 2 *(i - 2)):((i + 2 *(i - 2))+2)]) / sum(obitos_infantis[j,(i + 2 *(i - 2)):((i + 2 *(i - 2))+2)])
  }
}
PCMD_trienios[is.na(PCMD_trienios)] = 0
colnames(PCMD_trienios) = c("Municipio", "2000_2002", "2003_2005", "2006_2008", "2009_2011", "2012_2014", "201")

#Criação dos indicadores de adequação da PCMD
adeq_PCMD_trienios = PCMD_trienios
adeq_PCMD_trienios[,2:7] = PCMD_trienios[,2:7] <= 0.10 #1 = Adequado

#Criação da variável de taxa de natalidade
taxa_natalidade = nascidos_vivos / pop_residente_2000_2017

#Criação da taxa de mortalidade infantil
TMI = obitos_infantis / nascidos_vivos

#Criação das taxas de mortalidade por idade por ano
taxa_mort_idade_2000 = obitos_geral_idade_2000 / pop_residente_idade_2000
pop_mg_idade_2000 = c(colSums(pop_residente_idade_2000))
obitos_esperados_ano_2000 = t(pop_mg_idade_2000 * t(taxa_mort_idade_2000))
TGMP_ano_2000 = obitos_esperados_ano_2000 / pop_mg_idade_2000
#
taxa_mort_idade_2001 = obitos_geral_idade_2001 / pop_residente_idade_2001
pop_mg_idade_2001 = c(colSums(pop_residente_idade_2001))
obitos_esperados_ano_2001 = t(pop_mg_idade_2001 * t(taxa_mort_idade_2001))
TGMP_ano_2001 = obitos_esperados_ano_2001 / pop_mg_idade_2001
#
taxa_mort_idade_2002 = obitos_geral_idade_2002 / pop_residente_idade_2002
pop_mg_idade_2002 = c(colSums(pop_residente_idade_2002))
obitos_esperados_ano_2002 = t(pop_mg_idade_2002 * t(taxa_mort_idade_2002))
TGMP_ano_2002 = obitos_esperados_ano_2002 / pop_mg_idade_2002
#
taxa_mort_idade_2003 = obitos_geral_idade_2003 / pop_residente_idade_2003
pop_mg_idade_2003 = c(colSums(pop_residente_idade_2003))
obitos_esperados_ano_2003 = t(pop_mg_idade_2003 * t(taxa_mort_idade_2003))
TGMP_ano_2003 = obitos_esperados_ano_2003 / pop_mg_idade_2003
#
taxa_mort_idade_2004 = obitos_geral_idade_2004 / pop_residente_idade_2004
pop_mg_idade_2004 = c(colSums(pop_residente_idade_2004))
obitos_esperados_ano_2004 = t(pop_mg_idade_2004 * t(taxa_mort_idade_2004))
TGMP_ano_2004 = obitos_esperados_ano_2004 / pop_mg_idade_2004
#
taxa_mort_idade_2005 = obitos_geral_idade_2005 / pop_residente_idade_2005
pop_mg_idade_2005 = c(colSums(pop_residente_idade_2005))
obitos_esperados_ano_2005 = t(pop_mg_idade_2005 * t(taxa_mort_idade_2005))
TGMP_ano_2005 = obitos_esperados_ano_2005 / pop_mg_idade_2005
#
taxa_mort_idade_2006 = obitos_geral_idade_2006 / pop_residente_idade_2006
pop_mg_idade_2006 = c(colSums(pop_residente_idade_2006))
obitos_esperados_ano_2006 = t(pop_mg_idade_2006 * t(taxa_mort_idade_2006))
TGMP_ano_2006 = obitos_esperados_ano_2006 / pop_mg_idade_2006
#
taxa_mort_idade_2007 = obitos_geral_idade_2007 / pop_residente_idade_2007
pop_mg_idade_2007 = c(colSums(pop_residente_idade_2007))
obitos_esperados_ano_2007 = t(pop_mg_idade_2007 * t(taxa_mort_idade_2007))
TGMP_ano_2007 = obitos_esperados_ano_2007 / pop_mg_idade_2007
#
taxa_mort_idade_2008 = obitos_geral_idade_2008 / pop_residente_idade_2008
pop_mg_idade_2008 = c(colSums(pop_residente_idade_2008))
obitos_esperados_ano_2008 = t(pop_mg_idade_2008 * t(taxa_mort_idade_2008))
TGMP_ano_2008 = obitos_esperados_ano_2008 / pop_mg_idade_2008
#
taxa_mort_idade_2009 = obitos_geral_idade_2009 / pop_residente_idade_2009
pop_mg_idade_2009 = c(colSums(pop_residente_idade_2009))
obitos_esperados_ano_2009 = t(pop_mg_idade_2009 * t(taxa_mort_idade_2009))
TGMP_ano_2009 = obitos_esperados_ano_2009 / pop_mg_idade_2009
#
taxa_mort_idade_2010 = obitos_geral_idade_2010 / pop_residente_idade_2010
pop_mg_idade_2010 = c(colSums(pop_residente_idade_2010))
obitos_esperados_ano_2010 = t(pop_mg_idade_2010 * t(taxa_mort_idade_2010))
TGMP_ano_2010 = obitos_esperados_ano_2010 / pop_mg_idade_2010
#
taxa_mort_idade_2011 = obitos_geral_idade_2011 / pop_residente_idade_2011
pop_mg_idade_2011 = c(colSums(pop_residente_idade_2011))
obitos_esperados_ano_2011 = t(pop_mg_idade_2011 * t(taxa_mort_idade_2011))
TGMP_ano_2011 = obitos_esperados_ano_2011 / pop_mg_idade_2011
#
taxa_mort_idade_2012 = obitos_geral_idade_2012 / pop_residente_idade_2012
pop_mg_idade_2012 = c(colSums(pop_residente_idade_2012))
obitos_esperados_ano_2012 = t(pop_mg_idade_2012 * t(taxa_mort_idade_2012))
TGMP_ano_2012 = obitos_esperados_ano_2012 / pop_mg_idade_2012
#
taxa_mort_idade_2013 = obitos_geral_idade_2013 / pop_residente_idade_2013
pop_mg_idade_2013 = c(colSums(pop_residente_idade_2013))
obitos_esperados_ano_2013 = t(pop_mg_idade_2013 * t(taxa_mort_idade_2013))
TGMP_ano_2013 = obitos_esperados_ano_2013 / pop_mg_idade_2013
#
taxa_mort_idade_2014 = obitos_geral_idade_2014 / pop_residente_idade_2014
pop_mg_idade_2014 = c(colSums(pop_residente_idade_2014))
obitos_esperados_ano_2014 = t(pop_mg_idade_2014 * t(taxa_mort_idade_2014))
TGMP_ano_2014 = obitos_esperados_ano_2014 / pop_mg_idade_2014
#
taxa_mort_idade_2015 = obitos_geral_idade_2015 / pop_residente_idade_2015
pop_mg_idade_2015 = c(colSums(pop_residente_idade_2015))
obitos_esperados_ano_2015 = t(pop_mg_idade_2015 * t(taxa_mort_idade_2015))
TGMP_ano_2015 = obitos_esperados_ano_2015 / pop_mg_idade_2015
#
taxa_mort_idade_2016 = obitos_geral_idade_2016 / pop_residente_idade_2016
pop_mg_idade_2016 = c(colSums(pop_residente_idade_2016))
obitos_esperados_ano_2016 = t(pop_mg_idade_2016 * t(taxa_mort_idade_2016))
TGMP_ano_2016 = obitos_esperados_ano_2016 / pop_mg_idade_2016
#
taxa_mort_idade_2017 = obitos_geral_idade_2017 / pop_residente_idade_2017
pop_mg_idade_2017 = c(colSums(pop_residente_idade_2017))
obitos_esperados_ano_2017 = t(pop_mg_idade_2017 * t(taxa_mort_idade_2017))
TGMP_ano_2017 = obitos_esperados_ano_2017 / pop_mg_idade_2017

#Criação das taxas de mortalidade por idade por triênio
obitos_geral_idade_2000_2002 = obitos_geral_idade_2000 + obitos_geral_idade_2001 + obitos_geral_idade_2002
pop_residente_idade_2000_2002 = pop_residente_idade_2000 + pop_residente_idade_2001 + pop_residente_idade_2002
taxa_mort_idade_2000_2002 = matrix(obitos_geral_idade_2000_2002 / pop_residente_idade_2000_2002)
pop_padrao_idade_2000_2002 = as.numeric(colSums(pop_residente_idade_2001)) #Definindo população padrão
str(taxa_mort_idade_2000_2002)
dim(taxa_mort_idade_2000_2002)
class(taxa_mort_idade_2000_2002)

obitos_esperados_ano_2000_2002 = taxa_mort_idade_2000_2002 * pop_padrao_idade_2000_2002
TGMP_trienio_2000_2002 = (obitos_esperados_ano_2000_2002 / sum(pop_padrao_idade_2000_2002)) * 1000
adeq_TGMP_trienio_2000_2002 = TGMP_trienio_2000_2002 < 4
TGMP_teste = matrix(,nrow = 853, ncol = 18)
for (i in 1:18) {
  TGMP_teste[,i] = taxa_mort_idade_2000_2002[,i] * pop_padrao_idade_2000_2002[i]
}

#Criação dos indicadores de adequação da PCMD gerais
adeq_PCMD_2000_2017 = PCMD_2000_2017 <= 0.10 #TRUE = Adequado

# Salvando arquivo como CSV
write.csv(TGMP_ano_2017, "D:/Non-Games/BCE/manipulacaoDados/TGMP_ano_2017.csv")
