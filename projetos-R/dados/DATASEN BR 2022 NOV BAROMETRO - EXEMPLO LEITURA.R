########################################################################
#' @descricao Leitura dos dados da pesquisa DataSenado e
# exemplo de cálculo de estimativas pontuais considerando o delineamento amostral 
#' @author DataSenado
#' @date   07/22
#' @Pesquisa Barometro Nov 2022
########################################################################

rm(list = ls())

## Pacotes ####

library(pacman) # Checa se um pacote esta instalado e, em caso afirmativo, faz a carga. Caso contrario, instala e faz a carga em seguida.

pacman::p_load(readxl, tidyverse, survey, purrr)

## diretorios dos arquivos ------------

dir = dirname(rstudioapi::getSourceEditorContext()$path) # diretorio em que o codigo esta salvo

## Microdado ####

df = read.csv2(list.files(dir, ".csv", ignore.case = T,full.names = T), 
               sep = ";")

names(df)

# Mudanca de nomenclatura das variaveis para facilitar entendimento do codigo

df$regiao.wgts      = df$VD_REGIAO
df$sexo.wgts        = df$V02
df$edu.wgts         = df$VD_EDUCACAO
df$raca.wgts        = df$VD_RACA 
df$idade.wgts       = df$VD_IDADE 
df$porte.wgts       = df$VD_PORTE
df$voto.turno1.wgts = df$VD_VOTO.TURNO1
df$voto.turno2.wgts = df$VD_VOTO.TURNO2

## Ponderacao ------------

# Definição das variáveis a serem usadas no raking (calibracao):
var.wgts = c("raca.wgts",
             "sexo.wgts",
             "idade.wgts",
             "porte.wgts",
             "edu.wgts",
             "voto.turno2.wgts",
             "voto.turno1.wgts")


# Definição das variaveis populacionais que servirao de parametro para o raking
# Essas variaveis foram extraidas da PNADC 2021-03

var.pop = c("POP_REGIAO_RACA",
            "POP_REGIAO_SEXO",
            "POP_REGIAO_IDADE",
            "POP_REGIAO_PORTE",
            "POP_REGIAO_EDU",
            "POP_REGIAO_VOTO.TURNO2",
            "POP_REGIAO_VOTO.TURNO1")

# Criacao da lista de referencia com os parametros populacionais a serem usados na funcao rake:

population <- list(0)
for (i in 1:length(var.wgts)) {
  pop = unique(df[, c('regiao.wgts', var.wgts[i], var.pop[i])])
  names(pop) <- c('regiao.wgts', var.wgts[i], 'freq')
  population[[i]] <- pop
  
}

# Definicao das variaveis na amostra que serao usadas no rake, no formato exigido pela funcao

sample <- map(paste0("~", "regiao.wgts +", var.wgts), as.formula)

# Delineamento amostral ------------
data.svy <-
  svydesign(
    id = ~ ID,
    weights = df$W1,
    data = df,
    strata = df$regiao.wgts
  )

# Ponderacao Rake ------------
data.rake <-
  rake(
    data.svy,
    sample.margins = sample,
    population.margins = population,
    control = list(maxit = 62)
  )

# Teste para ver se a ponderacao acima bate com o peso calculado pelo DataSenado disponivel na base

Wrake <-
  data.frame(ID = data.svy$cluster, W2_teste = weights(data.rake))
summary(Wrake$W2_teste)
summary(df$W2)

# Ok se Wrake$W2_teste = df$W2

# Exemplo de estimativa da proporcao:
(tab <- svymean(~ as.factor(P04), data.rake))
round((qt(0.975,df = degf(data.rake))*SE(tab))*100,1)

# Exemplo de estimativa do total populacional:
svytotal( ~ as.factor(P04), data.rake)
svytotal( ~ as.factor(sexo.wgts), data.rake)

# Exemplo de tabela cruzada
a = svyby(~ as.factor(P04), ~ as.factor(sexo.wgts), data.rake, svymean)
t(a)

