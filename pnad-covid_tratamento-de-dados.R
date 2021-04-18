# pacotes

library(labelled)
library(srvyr)
library(survey)
library(data.table)

# library(survey)

# dados

dir.dados <- "C:/Users/lefco/Documents/EACH/PUB/PNAD COVID19/Dados"

pnad_covid_jun <- read.csv(file.path(dir.dados, 'PNAD_COVID_062020.csv'))

# transformar variáveis estranhas C em logical

## tentativa anterior (individual)
## pnad_covid_julho$C010[is.na(pnad_covid_julho$C010)] <- 0
## pnad_covid_julho$C010 <- as.logical(pnad_covid_julho$C010)
## class(pnad_covid_julho$C010)

for(var in c("C010", "C0101", "C0102", "C0103", "C0104", "C011A", "C011A1", "C011A2")){
  
  # var <- "C0101"
  
  cat("\n---------------------------\nVariável: ", var, "\n\n")
  cat("Antes da transformação:")
  print(table(pnad_covid_jun[[var]]))
  pnad_covid_jun[[var]] <- as.integer(pnad_covid_jun[[var]])
  pnad_covid_jun[[var]] <- as.logical(pnad_covid_jun[[var]] )
  pnad_covid_jun[[var]] [is.na(pnad_covid_jun[[var]] )] <- F
  
  cat("\nDepois da transformação:")
  print(table(pnad_covid_jun[[var]]))
}

# transformar 99 em NA nas variáveis A001B1 e F006

pnad_covid_jun$A001B1[pnad_covid_jun$A001B1 == 99] <- NA
pnad_covid_jun$F006[pnad_covid_jun$F006 == 99] <- NA
table(is.na(pnad_covid_jun$A001B1))
table(pnad_covid_jun$A001B1)
table(is.na(pnad_covid_jun$F006))
table(pnad_covid_jun$F006)

# value labels

dicionario_pnad_covid <- read.csv(file.path(dir.dados, 'rotulos_valores_05-06.csv'))
names(dicionario_pnad_covid) <- c("variavel", "valor", "rotulo")


# Convertendo o data frame dicionario_pnad_covid em uma lista em que cada elemento corresponde
# às linhas do dicionário de um variável

# . convertendo coluna de valores para números inteiros (células que não contêm números são 
# convertidas em NA)

dicionario_pnad_covid$valor <- as.integer(dicionario_pnad_covid$valor)

# . removendo os NA's:

i <- which(is.na(dicionario_pnad_covid$valor))
rm(i)

# . criando data frame auxiliar apenas com as linhas do dicionário que não contêm NA:

# dic.aux <- dicionario_pnad_covid[-i]

# . criando a lista

dic <- split(dicionario_pnad_covid, f = dicionario_pnad_covid$variavel)

str(dic)

# definindo a função trim

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Convertendo as variáveis categóricas para tipo 'factor'

for(var in names(dic)){
  # var <- "UF"
  print(var)
  # print(table(pnad_covid_junho[[var]]))
  nova.var <- factor(pnad_covid_jun[[var]], levels = dic[[var]]$valor, labels = trim(dic[[var]]$rotulo), exclude = NULL)
  # print(table(nova.var))
  pnad_covid_jun[[var]] <- nova.var
}


# set variable labels

# . se for fácil criar a partir do arquivo excel um data frame contendo os códigos e os rótulos 
# das variáveis, então podemos automatizar essas linhas abaixo também, importanto os rótulos
# de variável direto do arquivo excel (ou csv)

var.labels.aux <- read.csv(file.path(dir.dados, "rotulos_variaveis_05-06.csv"))
var.labels <- split(var.labels.aux$rotulo, f = var.labels.aux$var)
var_label(pnad_covid_jun) <- var.labels


# verificando factors, variable labels e value labels

View(pnad_covid_jun)

# estratificação

pnad_design_jun <- pnad_covid_jun %>%
  as_survey_design(
    ids = UPA,
    strata = Estrato,
    weights = V1032,
    nest = TRUE)

# testando

View(pnad_design_jun[["variables"]])

pnad_design_jun %>%
  group_by(A003) %>%
  summarise(sitdom = survey_total())

pnad_design_jun %>%
  group_by(A004) %>%
  summarise(sitdom = survey_total())

pnad_design_jun %>%
  group_by(C017A) %>%
  summarise(sitdom = survey_total())

pnad_design_julho %>%
  summarise(renda_media = survey_mean(C01012, na.rm = TRUE))

pnad_design_julho %>%
  group_by(A003) %>%
  summarise(renda_media = survey_mean(C01012, na.rm = TRUE))


# criando dicionário - não entendi, não

dicionario <- function(df){
  
  # . Identificando tipos das variáveis presentes:
  classe <- data.frame(cod = names(df), classe = unlist(lapply(df, class)))
  
  # . Identificando labels
  labels = sapply(sapply(df, attributes), "[[", "label")
  labels <- data.frame(cod = names(labels), labels)
  
  # . Identificando níveis (só terá efeito para variáveis factor)
  niveis <- lapply(df, levels)
  
  # .. determinando número de níveis
  num.niveis <- unlist(lapply(niveis, length))
  num.niveis <- data.table(cod = names(num.niveis), num.niveis = num.niveis)
  
  # criando um texto contento todos os níveis da variável
  niveis.2 <- unlist(lapply(niveis, paste, collapse = "; "))
  niveis.2 <- data.table(cod = names(niveis.2), niveis = niveis.2)
  
  # Juntando as informações:
  dic <- merge(labels, classe, by = 'cod', all = T)
  dic <- merge(dic, niveis.2, by = 'cod', all = T)
  dic <- merge(dic, num.niveis, by = "cod", all = T)
  
  return(dic)
}

dicionario_pnad_mai_jun <- dicionario(pnad_design_jun$variables)
View(dicionario_pnad_mai_jun)

# salvar

save(pnad_design_jun, dicionario_pnad_mai_jun, file = 'C:/Users/lefco/Documents/EACH/PUB/PNAD COVID19/Finais/pnad-covid_2020.06.RData')