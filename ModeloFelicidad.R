library(readxl)
#la base de datos fue sacada de https://www.kaggle.com/venus90210/an-lisis-de-felicidad/data

felicidad <- X2017

head(felicidad)
modelo1<-lm(Happiness.Score~Health..Life.Expectancy.+Freedom+Generosity+Trust..Government.Corruption.+Economy..GDP.per.Capita.,data=felicidad)
summary(modelo1)
fitted(modelo1)
residuals(modelo1)
coer<-coef(modelo1)

# Existe un efecto estadisticamente significativo en la felicidad que dependa de la vida saludable que lleva ?
#h0=b1=0
#h1=b1<>0

#p-value: 0.000132 Hay suficiente evidencia estadistica para rechazar h0 es decir que  que una vida 
 # saludable hace que la persona se seinta mas feliz con sigo misma 

#_____
# Existe un efecto estadisticamente significativo en la felicidad que dependa de la generosidad?
#h0=b3=0
#h1=b3<>0

#p-value:0.242897 Hay suficiente evidencia estadistica para  no rechazar h0 es decir que 
 # la generosidad no afecta en nada la felicidad de la persona 

#_________
#Existe un efecto estadisticamente significativo para la felicidad  entre  el ingreso recibido y la corrupción del gobierno 
#h0=b4=b5=0
#h1=b4<>b5<>0 diferentes 

modelo2<-lm(Happiness.Score~Trust..Government.Corruption.+Economy..GDP.per.Capita.,data=felicidad)
summary(modelo2)

anova(modelo2,modelo1)

#..... es mejor el modelo 2 al 1 por el Pr(>F) ya que es chiquito   pero tambien se puede ver con el numero

#calcular la matriz de varianzas y covarianzas de los betas
vcov(modelo1)

#Elección del mejor modelo
bestsubset <- function(X, y){
  P = ncol(X)
  subsets = expand.grid(rep( list(0:1), P))
  names(subsets)=paste('X',1:P,sep='')
  stat = NULL
  SST = sum((y-mean(y))^2)
  
  fitall = lm(y~X)
  n = nrow(X)
  MSEP = deviance(fitall)/(n-P-1) 
  for(i in 1:nrow(subsets)){
    subs = which(subsets[i,]>0)
    if(length(subs)==0) fit = lm(y~1)
    else {
      subX = X[,which(subsets[i,]>0)]
      fit = lm(y~subX)
    }
    p = length(subs)+1
    SSE = deviance(fit)
    R2 = 1-SSE/SST
    R2a = 1-SSE/SST*(n-1)/(n-p)
    Cp = SSE/MSEP - (n-2*p) 
    AIC = n*log(SSE)-n*log(n)+2*p
    BIC = n*log(SSE)-n*log(n)+log(n)*p
    
    X1 = as.matrix(cbind(1,X[,subs]))
    hatMat = X1%*%solve(t(X1)%*%X1)%*%t(X1)
    eList = fit$residuals
    dList = eList/(1-diag(hatMat))
    PRESS = sum(dList^2)
    
    
    criList = c(length(subs)+1, subsets[i,],  SSE, R2, R2a, Cp, AIC, BIC, PRESS)
    
    stat=rbind(stat,criList)
    
  }
  rownames(stat)=NULL
  colnames(stat)=c('p',names(subsets),'SSE','R2','R2a','Cp','AIC','BIC','PRESS')
  
  
  model = NULL
  model$R2 = which.max(stat[,P+3])
  
  model$R2a = which.max(stat[,P+4])
  
  model$Cp = which.min(stat[,P+5])
  
  model$AIC = which.min(stat[,P+6])
  
  model$BIC = which.min(stat[,P+7])
  
  model$PRESS = which.min(stat[,P+8])
  list(model=model, stat=stat)
}

X=model.matrix(modelo1)[,-1]
y=felicidad$Happiness.Score
bestsubset(X,y)

#VALores ajustados 
data.frame(Estado=felicidad$Country,Observado=felicidad$Happiness.Score,Ajustados=fitted(modelo1))
