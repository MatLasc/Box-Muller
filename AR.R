f <- function(x){ #repartia principala
  sqrt(2/pi) * exp(-x ^ 2 / 2) 
}

g <- function(x){ #repartitia suport
  exp(-x)  
}

range1 <- seq(-5, 5, by = 0.01)
range2 <- seq(0, 5, by = 0.01)


plot(range1, f(range1), type = 'l',  ylim = c(0, 2))
lines(range2, f(1) / g(1) * g(range2))
lines(-range2, f(1) / g(1)* g(range2))

v <- rep(0, 100000) #vectorul pentru elementele acceptate

for (i in 1 : length(v)){
  repeat{ #testarea unui sample
    x <- -log(runif(1)) 
    y <- runif(1)
    if(y < f(x)/(f(1) / g(1) * g(x))) #conditia de acceptare
    {break;}
  }
  v[i] <- ((-1) ^ (runif(1) < 0.5)) * x
}

hist(v, prob = T, breaks = 50)
curve(dnorm(x,0,1), add = T)

#box muller pentru distributie normala

range = 100000
u = runif(range)
v = runif(range)
x=rep(0,range)
y=rep(0,range)

for (i in 1:range){
  x[i] = sqrt(-2*log(u[i]))*cos(2*pi*v[i]) 
  y[i] = sqrt(-2*log(u[i]))*sin(2*pi*v[i])
}
plot(density(c(x,y)))

