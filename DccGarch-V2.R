###############################################################################
##### Analisando 15 series economicas
##### DCC-GARCH (mais flexivel e com menos parametros para estimar)
###############################################################################
library(corrplot)
library(stochvol)
library(bayesDccGarch)

set.seed(123456)

### Carregando os dados
series <- read.csv("https://raw.githubusercontent.com/ProfNascimento/DCCGARCH/main/index.csv", header = T, sep = ",") ## 2174 o tamanho de cada serie


## PLOT SERIES
par(mfrow=c(3,2), mar=c(3,3,3,.5),mgp=c(2,.6,0))
plot(series$IBOV.index,type="l",ylab="",xlab="",main="IBOV")
plot(logret(series$IBOV.index),type="l",ylab="",xlab="",main="LOG-RET IBOV");abline(h=0,col="red")
plot(series$MERVAL.index,type="l",ylab="",xlab="",main="MERVAL")
plot(logret(series$MERVAL.index),type="l",ylab="",xlab="",main="LOG-RET MERVAL");abline(h=0,col="red")
plot(series$NKY.index,type="l",ylab="",xlab="",main="NKY")
plot(logret(series$NKY.index),type="l",ylab="",xlab="",main="LOG-RET NKY");abline(h=0,col="red")
dev.off()

### Log-Retornos
dados=apply(series[,-1],2,logret,demean=TRUE)*100

### Organizando os dados
dados1=as.data.frame(dados)
names(dados1)=c("IBOV","MERVAL","IPSA","MEXBOL","IGBvl","IBVCI","KOSPI","NKY","HSI",
                "DJI","SPX", "NDX","UKX","DAX","CAC")

## PLOT COR
M<-cor(dados1)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(dados1)
corrplot(M, method="color", 
         p.mat = p.mat, sig.level = 0.05)


# Graficos de algumas autocorrelações
par(mfrow=c(3,2), mar=c(3,3,3,.5),mgp=c(2,.4,0))
acf(logret(series$IBOV.index),main="Retornos IBOV")
acf(logret(series$IBOV.index)^2,main="Retornos^2 IBOV")
acf(logret(series$MERVAL.index),main="Retornos MERVAL")
acf(logret(series$MERVAL.index)^2,main="Retornos^2 MERVAL")
acf(logret(series$NKY.index),main="Retornos NKY")
acf(logret(series$NKY.index)^2,main="Retornos^2 NKY")
dev.off()


### Usando a funcao bayesDccGarch para estimar os parametros
out = bayesDccGarch(dados1, nSim=120000,errorDist = 1) ## SSNorm - erro normal
dados_burnin=as.mcmc(out$MC[30001:120000,])
summary(dados_burnin)

## PLOT MCMC
plot(dados_burnin)

## PLOT VOLATILIT --IBOV--
plotVol(dados1[,1],out$H[,"H_1,1"]) #IBOV
plotVol(dados1[,2],out$H[,"H_2,2"]) #MERVAL
plotVol(dados1[,8],out$H[,"H_8,8"]) #NKY

## PLOT DCC
plot(out$R[-1,"R_10,11"],type="l",ylab="DJI x SPX")
plot(out$R[-1,"R_1,7"],type="l",ylab="IBOV x KOSPI")
plot(out$R[-1,"R_2,13"],type="l",ylab="MEXBOL x UKX")
