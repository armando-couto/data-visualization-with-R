
install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

paths <- c("/Users/armandocouto/workspace/data-visualization-with-R")
p <- 1

##### 1.2 IMPORTANTADO E EXPLORANDO OS DADOS ####
dados <- read.csv(paste(paths[p],"/googleplaystore.csv",sep = ""))
hist(dados$Rating)
table(dados$Rating)
hist(dados$Rating, xlim = c(1, 5))

rating.Histogram <- ggplot(data = dados) + geom_histogram(mapping = aes(x = Rating), na.rm = TRUE, breaks = seq(1,5)) + xlim(c(1,5))
rating.Histogram

## visualizando todos os dados na aba (View())
view(dados) ##SIMULANDO ERRO AO CHAMAR A FUNCAO
View(dados)

## visualizando os primeiros registros
head(dados)

## visualizando os ??ltimos registros
tail(dados)

##### 1.3 EXPLORANDO A BASE DE DADOS ####
## identificando vari??veis quantitativas (discretas e cont??nuas) e qualitativas (nominal e ordinal)
str(dados)

# quantitativas: rating (discretas), reviews (cont??nuas)
# qualitativas: app (nominal), type (ordinal) 

unique(dados$Installs)
unique(dados$Reviews)

## CARREGANDO OS DADOS SEM stringAsFactor
dados <- read.csv(paste(paths[p],"/googleplaystore.csv",sep = ""),stringsAsFactors = FALSE)