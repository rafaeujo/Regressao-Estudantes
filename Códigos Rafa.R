###Codando o relatório
install.packages("knitr")
install.packages("kableExtra")
library(kableExtra)
library(knitr)
library(tidyverse)
library(MASS)
library(dplyr)

#Rodando o Banco

estud <- read.csv("Student_Performance.csv")

estud <- estud %>%
  rename(nota.antiga = Previous.Scores, ind.perf = Performance.Index)

attach(estud)

##Medidas Resumo

resumo.perf <- summary(ind.perf)
resumo.perf
resumo.nota <- summary(nota.antiga)
resumo.nota

sd(ind.perf)
sd(nota.antiga)
  
hist(ind.perf, freq=FALSE)

resumogeral.df <- data.frame(
  variavel = c("Ind Rendimento", "Notas Antigas"),
  min = c(10.00, 40.00),
  Q1 = c(40.00, 54.00),
  mediana = c(55.00, 69.00),
  media = c(55.22, 69.45),
  Q3 = c(71.00, 85.00),
  max = c(100.00, 99.00),
  dp = c(19.21, 17.34)
)

# Gerando a tabela
kable(resumogeral.df, col.names = c("Variável", "Min.", "1º Quartil", "Mediana", "Média", "3º Quartil", "Máx.", "d.p."), 
      caption = "Medidas-resumo: Índice de Rendimento e Notas antigas") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

### Obtendo a densidade

par(new=F, mfrow=c(1,1))
plot(fdp.perf<-density(estud$Performance.Index), xlab="Índice de Peformance", ylab="", main="Gráfico 1: Densidade do Índice de Performance")
polygon(fdp.perf, col="lightblue")

### Obtendo os box-plots

par(new=F, mfrow=c(1,2))
boxplot(estud$Performance.Index, main="Gráfico 2: Boxplot do Ind de Performance",ylab="Ind de Performance", col="lightgreen")
boxplot(estud$Previous.Scores, main="Gráfico 3: Boxplot de Notas antigas",xlab="", ylab="Notas Antigas", col="gray")




#### Obtendo o gráfico de dispersão e a correlação

plot(nota.antiga, ind.perf, pch=16,main="Dispersão ind de performance x notas antigas")

X<-cbind(nota.antiga,ind.perf)
cor(X)

####  Obtendo os ajustes

fit.model<-lm(faturamento~gasto, data = restaurante)
summary(fit.model)

