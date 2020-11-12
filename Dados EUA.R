#Rotina para coletar algumas séries do FRED
#Feito por: Felipe Simplício Ferreira
#última atualização: 10/11/2020


#Definindo diretórios a serem utilizados

getwd()
setwd("C:/Users/User/Documents")

#Carregando pacotes que serão utilizados
library(fredr)
library(rio)

#Criando função para coleta de séries
coleta_dados_fred = function(series, datainicial="2018-01-01", datafinal = format(Sys.time(), "%Y-%m-%d")){
  #Argumentos: vetor de séries, datainicial que pode ser manualmente alterada e datafinal que automaticamente usa a data de hoje
  #Chave para funcionamento da API do FRED
  fredr_set_key("759fd9f905b9d4025ce26e5ae6e63cb9")
  #Cria estrutura de repetição para percorrer vetor com códigos de séries e depois juntar todas em um único dataframe
    for (i in 1:length(series)){
    dados = fredr(series[i], observation_start = as.Date(datainicial), observation_end = as.Date(datafinal))
    nome_coluna = series[i] #Nomeia cada coluna do dataframe com o código da série
    colnames(dados) = c('data', 'codigo da serie', nome_coluna)
    nome_arquivo = paste("dados", i, sep = "") #Nomeia os vários arquivos intermediários que são criados com cada série
    assign(nome_arquivo, dados)
    
    if(i==1)
      base = dados1[,-2] #Primeira repetição cria o dataframe
    else
      base = merge(base, dados[,-2], by = "data", all = T) #Demais repetições agregam colunas ao dataframe criado
    print(paste(i, length(series), sep = '/')) #Printa o progresso da repetição
  }
  
  base$data = as.Date(base$data, "%d/%m/%Y") #Transforma coluna de data no formato de data
  base = base[order(base$data),] #Ordena o dataframe de acordo com a data
  base[,-1]=apply(base[,-1],2,function(x)as.numeric(gsub(",",".",x))) #Transforma o resto do dataframe em objetos numéricos
  return(base)
}

#Definida a função, podemos criar objetos para guardar os resultados
#Vetor com código para várias séries do FRED
series_mensais = c("ALTSALES", "AWHAETP", "CES0500000003", "CES0500000011", "CIVPART", "CPIAUCSL", "CPILFESL", "CUSR0000SEHA", "CUSR0000SEHC", "EXHOSLUSM495S", "GS10", "HOUST", "HSN1F", "INDPRO", "IPMANSICS", "IPMINE", "IPUTIL", "PAYEMS", "PCEPI", "PCEPILFE", "PERMIT", "RSAFS", "RSXFS", "SPCS20RNSA", "SPCS20RSA", "TTLCONS", "TWEXMMTH", "TWEXMPA", "U6RATE", "UNRATE")
mensais = coleta_dados_fred(series_mensais) #Criando objeto em que ficam guardados as séries

write.csv2(mensais, "01-mensais.csv", row.names = F) #Salvando arquivo csv em padrão brasileiro
export(mensais, "Dados EUA(fonte).xlsx", sheetName  = "mensais")


series_anuais = c("LABSHPUSA156NRUG",	"MEHOINUSA672N")
anuais = coleta_dados_fred(series_anuais)

write.csv2(anuais, "02-anuais.csv", row.names = F)
export(anuais, "Dados EUA(fonte).xlsx", which  = "anuais")


series_diarias = c("DGS10", "DTWEXM", "SP500", "VIXCLS")
diarias = coleta_dados_fred(series_diarias)

write.csv2(diarias, "03-diarias.csv", row.names = F)
export(diarias, "Dados EUA(fonte).xlsx", which  = "diarias")


series_trimestrais = c("A011RE1Q156NBEA", "E318RA3Q086SBEA", "PRS85006092")
trimestrais = coleta_dados_fred(series_trimestrais)

write.csv2(trimestrais, "04-trimestrais.csv", row.names = F)
export(trimestrais, "Dados EUA(fonte).xlsx", which  = "trimestrais")