set.seed(100)
u1 <- runif(10000,0,1)
Ti <- (-1/0.1)*log(1-u1)
hist(Ti,probability = TRUE,xlim = c(0,100),breaks = 50,main = "Exponencial generada (10,000)")
u2 <- runif(10000,0,1)
xi<-(-10000)*log(1-u2)
hist(xi,probability = TRUE,xlim = c(0,100000),breaks = 100,main = "Exponencial generada (10,000)")
matplot(sort(Ti),xi,type="line",xlim = c(0,90),ylim = c(0,120000))
abline(h=10000,col="red")
Fn <- c(1:10000)
ordenados <- sort(xi)
for (i in 1:10000) {
  cuenta<-0
  for (j in 1:10000) {
    if (ordenados[j]<=ordenados[i]){
      cuenta=cuenta+1
    }
  }
  Fn[i] <- cuenta/10000
}
plot(ecdf(xi),main="Distribucion")
matplot(ordenados,Fn,type="line",xlim = c(0,100000),ylim = c(0,1),xlab = "Xi",ylab = "Fn(xi)",main="Distribución empírica")
xi[100]
u1 <- runif(10000,0,1)
Ti <- (-1/0.1)*log(1-u1)
hist(Ti,probability = TRUE,xlim = c(0,100),breaks = 50,main = "Exponencial generada (10,000)")
u2 <- runif(10000,0,1)
xi<-(-10000)*log(1-u2)
hist(xi,probability = TRUE,xlim = c(0,100000),breaks = 100,main = "Exponencial generada (10,000)")
xi[100]
set.seed(100)
u1 <- runif(10000,0,1)
Ti <- (-1/0.1)*log(1-u1)
hist(Ti,probability = TRUE,xlim = c(0,100),breaks = 50,main = "Exponencial generada (10,000)")
u2 <- runif(10000,0,1)
xi<-(-10000)*log(1-u2)
hist(xi,probability = TRUE,xlim = c(0,100000),breaks = 100,main = "Exponencial generada (10,000)")
xi[100]
rm(list=ls())
set.seed(100)
u1 <- runif(10000,0,1)
Ti <- (-1/0.1)*log(1-u1)
hist(Ti,probability = TRUE,xlim = c(0,100),breaks = 50,main = "Exponencial generada (10,000)")
u2 <- runif(10000,0,1)
xi<-(-10000)*log(1-u2)
hist(xi,probability = TRUE,xlim = c(0,100000),breaks = 100,main = "Exponencial generada (10,000)")
matplot(sort(Ti),xi,type="line",xlim = c(0,90),ylim = c(0,120000))
abline(h=10000,col="red")
Fn <- c(1:10000)
ordenados <- sort(xi)
xi[100]
for (i in 1:10000) {
  cuenta<-0
  for (j in 1:10000) {
    if (ordenados[j]<=ordenados[i]){
      cuenta=cuenta+1
    }
  }
  Fn[i] <- cuenta/10000
}
plot(ecdf(xi),main="Distribucion")
matplot(ordenados,Fn,type="line",xlim = c(0,100000),ylim = c(0,1),xlab = "Xi",ylab = "Fn(xi)",main="Distribución empírica")
xi[10001]
xi[10000]
max(xi)
intervaloinferior <- seq(0,100000,by=5000)
intervaloinferior
intervaloinferior <- seq(0,95000,by=5000)
intervalosuperior <- seq(5000,100000)
intervalosuperior <- seq(5000,100000,by=5000)
intervalosuperior
intervaloinferior
Agrupados <- c(1:20)
for (i in 1:20){
  cuenta<-0
  for (j in 1:10000){
    if(xi[j]>=intervaloinferior[i] & xi[j]<intervaloinferior[i]){
      cuenta=cuenta+1
    }
  }
  Agrupados[i] <- cuenta
}
Agrupados
Agrupados <- c(1:20)
Agrupados
for (i in 1:20){
  cuenta<-0
  for (j in 1:10000){
    if(xi[j]>=intervaloinferior[i] & xi[j]<intervaloinferior[i]){
      cuenta=cuenta+1
    }
    Agrupados[i] <- cuenta
  }
}
Agrupados
Agrupados <- c(1:20)
for (i in 1:20){
  cuenta<-0
  for (j in 1:10000){
    if(xi[j]>=intervaloinferior[i] && xi[j]<intervaloinferior[i]){
      cuenta=cuenta+1
    }
    Agrupados[i] <- cuenta
  }
}
Agrupados
Agrupados <- c(1:20)
for (i in 1:20){
  cuenta<-0
  for (j in 1:10000){
    if((xi[j]>=intervaloinferior[i]) && (xi[j]<intervaloinferior[i])){
      cuenta=cuenta+1
    }
    Agrupados[i] <- cuenta
  }
}
Agrupados
if((xi[1]>=intervaloinferior[1]) && (xi[1]<intervaloinferior[1])){
  cuenta=cuenta+1
}
Agrupados[1] <- cuenta
}
if((xi[1]>=intervaloinferior[1]) && (xi[1]<intervaloinferior[1])){
  cuenta=cuenta+1
}
Agrupados[1] <- cuenta
Agrupados
xi[1]
if((xi[1]>=intervaloinferior[3]) && (xi[1]<intervaloinferior[3])){
  cuenta=cuenta+1
}
Agrupados[1] <- cuenta
Agrupados
if((xi[1]>=intervaloinferior[3]) && (xi[1]<intervaloinferior[3])){
  cuenta=1
  if((xi[1]>=intervaloinferior[3]) && (xi[1]<intervaloinferior[3])){
    cuenta=1
  }
  cuenta
  if((xi[1]>=intervaloinferior[3]) && (xi[1]<intervaloinferior[3])){
    prueba=1
  }
  prueba
  Agrupados <- c(1:20)
  for (i in 1:20){
    cuenta<-0
    for (j in 1:10000){
      if((xi[j]>=intervaloinferior[i]) && (xi[j]<intervaloinferior[i])){
        cuenta=cuenta+1
      }
    }
    Agrupados[i] <- cuenta
  }
  Agrupados
  xi[1]>=intervaloinferior[3]
  (xi[1]>=intervaloinferior[3]) && (xi[1]<intervaloinferior[3])
  Agrupados <- c(1:20)
  Agrupados
  for (i in 1:20){
    cuenta<-0
    for (j in 1:10000){
      if((xi[j]>=intervaloinferior[i]) && (xi[j]<intervalosuperior[i])){
        cuenta=cuenta+1
      }
    }
    Agrupados[i] <- cuenta
  }
  Agrupados
  sum(Agrupados)
  fn <. c(1:20)
  fn < c(1:20)
  fn <-c(1:20)
  fn[i]<- (Agrupados[i]/10000)*(1/(intervalosuperior[i]-intervaloinferior[i]))
  fn <-c(1:20)
  for(I in 1:20){
    fn[i]<- (Agrupados[i]/10000)*(1/(intervalosuperior[i]-intervaloinferior[i]))
  }
  fn
  plot(fn)
  matplot(intervaloinferior,fn,type="line",xlim = c(0,10),ylim = c(0,100000))
  matplot(intervaloinferior,fn,type="line",xlim = c(0,1),ylim = c(0,100000))
  matplot(intervaloinferior,fn,type="line",xlim = c(0,100000),ylim = c(0,100000))
  max(fn)
  matplot(intervaloinferior,fn,type="line",xlim = c(0,20),ylim = c(0,100000))
  min(fn)
  hist(fn)
  Agrupados
  fn[1]
  fn[2]
  fn <-c(1:20)
  for(I in 1:20){
    fn[i]<- (Agrupados[i]/10000)*(1/(intervalosuperior[i]-intervaloinferior[i]))
  }
  fn[2]
  fn <-c(1:20)
  for(I in 1:20){
    fn[i]<- (Agrupados[i]/10000)*(1/(intervalosuperior[i]-intervaloinferior[i]))
  }
  fn
  fn[1]
  fn
  Fn
  fn <-0
  for(I in 1:20){
    fn[i]<- (Agrupados[i]/10000)*(1/(intervalosuperior[i]-intervaloinferior[i]))
  }
  fn <-c(1:20)
  Agrupados[1]
  fn[1]<- (Agrupados[1]/10000)*(1/(intervalosuperior[1]-intervaloinferior[1]))
  fn
  fn <-c(1:20)
  fn
  for(I in 1:20){
    fn[i]<- (Agrupados[i]/10000)*(1/(intervalosuperior[i]-intervaloinferior[i]))
  }
  fn
  fn <-c(1:20)
  for(i in 1:20){
    fn[i]<- (Agrupados[i]/10000)*(1/(intervalosuperior[i]-intervaloinferior[i]))
  }
  fn
  plot(fn)
  hist(fn)
  matplot(intervaloinferior,fn,type="line",xlim = c(0,20),ylim = c(0,100000))
  matplot(intervaloinferior,fn,type="line",xlim = c(0,1),ylim = c(0,100000))
  matplot(intervaloinferior,fn,type="line",xlim = c(0,100000),ylim = c(0,1))
  matplot(intervaloinferior,fn,type="line",xlim = c(0,100000),ylim = c(0,.00001))
  matplot(intervaloinferior,fn,type="line",xlim = c(0,100000),ylim = c(0,.000001))
  matplot(intervaloinferior,fn,type="line",xlim = c(0,100000),ylim = c(0,.00000001))
  matplot(intervaloinferior,fn,type="line",xlim = c(0,100000),ylim = c(0,.0001))
  matplot(intervaloinferior,fn,type="bar",xlim = c(0,100000),ylim = c(0,.0001))
  barplot(intervaloinferior,xlim = c(0,100000),ylim = c(0,.0001))
  barplot(fn)
  barplot(fn,xlim = c(0,100000),ylim = c(0,.0001))
  barplot(fn,xlab = "Intervalos",ylab = "fn")
  barplot(xi)
  max(Ti)
  rm(list=ls())
  set.seed(100)
  u1 <- runif(10000,0,1)
  Ti <- (-1/0.1)*log(1-u1)
  hist(Ti,probability = TRUE,xlim = c(0,100),breaks = 50,main = "Exponencial generada (10,000)")
  hist(Ti,probability = TRUE,xlim = c(0,100),breaks = 50,main = "Fecha (Ti)")
  u2 <- runif(10000,0,1)
  xi<-(-10000)*log(1-u2)
  hist(xi,probability = TRUE,xlim = c(0,100000),breaks = 100,main = "Exponencial generada (10,000)")
  xi[1
     xi[1]
     xi[100]
     matplot(sort(Ti),xi,type="line",xlim = c(0,90),ylim = c(0,120000))
     abline(h=10000,col="red")
     Fn <- c(1:10000)
     ordenados <- sort(xi)
     for (i in 1:10000) {
       cuenta<-0
       for (j in 1:10000) {
         if (ordenados[j]<=ordenados[i]){
           cuenta=cuenta+1
         }
       }
       Fn[i] <- cuenta/10000
     }
     Fn <- c(1:10000)
     ordenados <- sort(xi)
     for (i in 1:10000) {
       cuenta<-0
       for (j in 1:10000) {
         if (ordenados[j]<=ordenados[i]){
           cuenta=cuenta+1
         }
       }
       Fn[i] <- cuenta/10000
     }
     Fn[10000]
     Fn[1]
     Fn[0]
     plot(ecdf(xi),main="Distribucion")
     matplot(ordenados,Fn,type="line",xlim = c(0,100000),ylim = c(0,1),xlab = "Xi",ylab = "Fn(xi)",main="Distribución empírica")
     intervaloinferior <- seq(0,95000,by=5000)
     intervalosuperior <- seq(5000,100000,by=5000)
     Agrupados <- c(1:20)
     for (i in 1:20){
       cuenta<-0
       for (j in 1:10000){
         if((xi[j]>=intervaloinferior[i]) && (xi[j]<intervalosuperior[i])){
           cuenta=cuenta+1
         }
       }
       Agrupados[i] <- cuenta
     }
     Agrupados
     sum(Agrupados)
     fn <-c(1:20)
     for(i in 1:20){
       fn[i]<- (Agrupados[i]/10000)*(1/(intervalosuperior[i]-intervaloinferior[i]))
     }
     barplot(fn,xlab = "Intervalos",ylab = "fn")
     valorlimitado <- c(1:10000)
     valorlimitado <- c(1:10000)
     primersumando<- c(1:10000)
     segundosumando<- c(1:10000)
     valorlimitado <- c(1:10000)
     primersumando<- c(1:10000)
     segundosumando<- c(1:10000)
     for(i in 1:10000){
       for(j in 1:10000){
         if (xi[j]<x[i]){
           primersumando[j] <-(xi[j]*(0.0001)*e^(-0.0001*xi[j]))
         }
         else{
           segundosumando[j] <- xi[i]*((0.0001)*e^(-0.0001*xi[j]))
         }
       }
       valorlimitado[i] <- sum(primersumando)+sum(segumdosumando)
     }
     for(i in 1:10000){
       for(j in 1:10000){
         if (xi[j]<xi[i]){
           primersumando[j] <-(xi[j]*(0.0001)*e^(-0.0001*xi[j]))
         }
         else{
           segundosumando[j] <- xi[i]*((0.0001)*e^(-0.0001*xi[j]))
         }
       }
       valorlimitado[i] <- sum(primersumando)+sum(segumdosumando)
     }
     for(i in 1:10000){
       for(j in 1:10000){
         if (xi[j]<xi[i]){
           primersumando[j] <-(xi[j]*(0.0001)*exp(-0.0001*xi[j]))
         }
         else{
           segundosumando[j] <- xi[i]*((0.0001)*exp(-0.0001*xi[j]))
         }
       }
       valorlimitado[i] <- sum(primersumando)+sum(segumdosumando)
     }
     for(i in 1:10000){
       for(j in 1:10000){
         if (xi[j]<xi[i]){
           primersumando[j] <-(xi[j]*(0.0001)*exp(-0.0001*xi[j]))
         }
         else{
           segundosumando[j] <- xi[i]*((0.0001)*exp(-0.0001*xi[j]))
         }
       }
       valorlimitado[i] <- sum(primersumando)+sum(segundosumando)
     }
     matplot(valorlimitado,type="l")
     matplot(valorlimitado,type="l",xlim = -10,10)
     matplot(valorlimitado,type="l",xlim = c(-10,10))
     matplot(valorlimitado,type="l",xlim = c(-10,1000))
     matplot(valorlimitado,type="l",xlim = c(-10,100000))
     matplot(valorlimitado,type="l",xlim = c(-10,10000))
     matplot(valorlimitado,type="l",xlim = c(-10,1000))
     matplot(valorlimitado,type="l",xlim = c(-10,100))
     matplot(valorlimitado,type="l",xlim = c(-10,10000))
     matplot(valorlimitado,type="l",xlim = c(-10,10000),ylim = c(0,10000))
     matplot(valorlimitado,type="l",xlim = c(-10,10000),ylim = c(0,100000))
     matplot(valorlimitado,type="l",xlim = c(-10,10000),ylim = c(0,1000000))
     matplot(valorlimitado,type="l",xlim = c(-10,10000),ylim = c(0,10000000))
     matplot(valorlimitado,type="l",xlim = c(-10,10000),ylim = c(0,100000000))
     matplot(valorlimitado,type="l",xlim = c(-10,10000),ylim = c(0,10000000))
     max(valorlimitado)
     matplot(valorlimitado,type="l",xlim = c(-10,10000),ylim = c(0,50000000))
     min(valorlimitado)
     matplot(valorlimitado,type="l",xlim = c(-10,10000),ylim = c(5000,50000000))
     matplot(valorlimitado,type="l",xlim = c(-10,10000),ylim = c(5000,10000000))
     matplot(valorlimitado,type="l",xlim = c(-10,10000),ylim = c(5000,1000000))
     mediaenexceso <- c(1:10000)
     mediaenexceso <- c(1:10000)
     for(i in 1:10000){
       for(j in 1:10000){
         if (xi[j]>=xi[i]){
           sumando[j] <-(xi[j]-xi[i])*((0.0001)*exp(-0.0001*xi[j]))/exp(-.0001*xi[j])
         }
       }
       mediaenexceso[i] <- sum(sumando)
     }
     sumando <- c(1:10000)
     for(i in 1:10000){
       for(j in 1:10000){
         if (xi[j]>=xi[i]){
           sumando[j] <-(xi[j]-xi[i])*((0.0001)*exp(-0.0001*xi[j]))/exp(-.0001*xi[j])
         }
       }
       mediaenexceso[i] <- sum(sumando)
     }
     max(mediaenexceso)
     min(mediaenexceso)
     matplot(mediaenexceso,type="l",xlim = c(-10,10000),ylim = c(5000,1000000))
     matplot(mediaenexceso,type="l",xlim = c(-10,10000),ylim = c(5000,10000000))
     matplot(mediaenexceso,type="l",xlim = c(-10,10000),ylim = c(5000,100000000))
     matplot(mediaenexceso,type="l",xlim = c(-10,10000),ylim = c(5000,10000000))
     matplot(mediaenexceso,type="l",xlim = c(-10,100000),ylim = c(5000,10000000))
     matplot(mediaenexceso,type="l",xlim = c(-10,10000),ylim = c(5000,1000000))
     mediaenexceso[1]
     mediaenexceso[10000]
     mediaenexceso[100000]
     mean(mediaenexceso)
     abline(h=mean(mediaenexceso),col="red")
     matplot(mediaenexceso,type="l",xlim = c(-10,1000),ylim = c(5000,1000000))
     matplot(mediaenexceso,type="l",xlim = c(-10,10000),ylim = c(5000,1000000))
     matplot(mediaenexceso,type="l",xlim = c(-10,100000),ylim = c(5000,1000000))
     matplot(mediaenexceso,type="l",xlim = c(-10,10000),ylim = c(5000,1000000))
     matplot(mediaenexceso,type="l",xlim = c(-10,10000),ylim = c(5000,1050000))
     matplot(mediaenexceso,type="l",xlim = c(-10,10000),ylim = c(5000,1500000))
     max(mediaenexceso)
     matplot(sort(Ti),xi,type="line",xlim = c(0,90),ylim = c(0,120000))
     abline(h=10000,col="red")
     mean(xi)
     mediaenexceso[10000]
     