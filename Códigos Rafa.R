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
plot(fdp.perf<-density(ind.perf), xlab="Índice de Peformance", ylab="", main="Gráfico 1: Densidade do Índice de Performance")
polygon(fdp.perf, col="lightblue")

par(new=F, mfrow=c(1,1))
plot(fdp.perf<-density(nota.antiga), xlab="Índice de Peformance", ylab="", main="Gráfico 1: Densidade do Índice de Performance")
polygon(fdp.perf, col="lightgreen")


### Obtendo os box-plots

par(new=F, mfrow=c(1,2))
boxplot(ind.perf, main="Gráfico 2: Boxplot do Ind de Performance",ylab="Ind de Performance", col="lightgreen")
boxplot(nota.antiga, main="Gráfico 3: Boxplot de Notas antigas",xlab="", ylab="Notas Antigas", col="gray")

#### Obtendo o gráfico de dispersão e a correlação

plot(nota.antiga, ind.perf, pch=16,main="Dispersão ind de performance x notas antigas")

X<-cbind(nota.antiga,ind.perf)
cor(X)

####  Obtendo os ajustes


fit.model<-lm(ind.perf~nota.antiga, data = estud)
summary(fit.model)

ajuste.df <- data.frame(
  ostrem = c("$\alfa$", "$\beta$"),
  Estimativas = c(-15.1817, 1.0138),
  ErroPadrão = c(0.3196, 0.0045),
  tvalor = c(-47.5, 227.1),
  pvalor = c(4,5))
  
 #valores do p-valor ilustrativo nos codigos do markdown estará correto
  
# Gerando a tabela com as colunas corretas
kable(ajuste.df, col.names = c("Variável", "Estimativas", "Erro Padrão", "t-valor", "p-valor"), 
      caption = "Resultados da Análise") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# Intervalos de confian?a para mu
confint(fit.model)

# Intervalos de confiança para mu
conf.int =  predict(fit.model,interval="confidence")
conf.int

#Intervalos de predição
pred.int =  predict(fit.model,interval="prediction")
pred.int

fitted.values = pred.int[,1]

pred.lower = pred.int[,2]
pred.upper = pred.int[,3]
conf.lower = conf.int[,2]
conf.upper = conf.int[,3]   


plot(nota.antiga,ind.perf)
lines(ind.perf,fitted.values, col="red",lwd=2)
lines(ind.perf,conf.lower, lwd=3,col="darkgreen")
lines(ind.perf,conf.upper, lwd=3,col="darkgreen")
lines(ind.perf,pred.lower, lwd=2,col="lightblue")
lines(ind.perf,pred.upper, lwd=2,col="lightblue")

#IP para gasto igual a 12

predict(fit.model, newdata =  data.frame(gasto = 12), interval = "prediction")

#IC para gasto igual a 12
predict(fit.model, newdata =  data.frame(gasto = 12), interval = "confidence")

#Gerando envelopes

par(mfrow=c(1,1))
X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
H <- X%*%solve(t(X)%*%X)%*%t(X)
h <- diag(H)
si <- lm.influence(fit.model)$sigma
r <- resid(fit.model)
tsi <- r/(si*sqrt(1-h))
#
ident <- diag(n)
epsilon <- matrix(0,n,100)
e <- matrix(0,n,100)
e1 <- numeric(n)
e2 <- numeric(n)
#
for(i in 1:100){
  epsilon[,i] <- rnorm(n,0,1)
  e[,i] <- (ident - H)%*%epsilon[,i]
  u <- diag(ident - H)
  e[,i] <- e[,i]/sqrt(u)
  e[,i] <- sort(e[,i]) }
#
for(i in 1:n){
  eo <- sort(e[i,])
  e1[i] <- (eo[2]+eo[3])/2
  e2[i] <- (eo[97]+eo[98])/2 }
#
med <- apply(e,1,mean)
faixa <- range(tsi,e1,e2)
#
par(pty="s")
qqnorm(tsi,xlab="Percentis da N(0,1)",
       ylab="Residuo Studentizado", ylim=faixa, pch=16)
par(new=TRUE)
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1)
par(new=TRUE)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1)
par(new=TRUE)
qqnorm(med,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=2)

#Testando a normalidade

X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
H <- X%*%solve(t(X)%*%X)%*%t(X)
h <- diag(H)
lms <- summary(fit.model)
s <- lms$sigma
r <- resid(lms)
ts <- r/(s*sqrt(1-h))
di <- (1/p)*(h/(1-h))*(ts^2)
si <- lm.influence(fit.model)$sigma
tsi <- r/(si*sqrt(1-h))
a <- max(tsi)
b <- min(tsi)
par(mfrow=c(2,2))
plot(h,xlab="Indice", ylab="Medida h", pch=16, ylim=c(0,1))
cut <- 2*p/n
abline(cut,0,lty=2)
#identify(h, n=1)
#title(sub="(a)")
#
plot(di,xlab="Indice", ylab="Distancia de Cook", pch=16)
#identify(di, n=2)
#title(sub="(b)")
#
plot(tsi,xlab="Indice", ylab="Res?duo Padronizado",
     ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(tsi, n=1)
#title(sub="(c)")
#
plot(fitted(fit.model),tsi,xlab="Valores Ajustados", 
     ylab="Residuo Padronizado", ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#title(sub="(d)")
#identify(fitted(fit.model),tsi, n=1)

