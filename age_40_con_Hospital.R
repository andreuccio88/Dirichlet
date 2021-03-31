#############################################
#############################################
##   Dirichlet CoD - GDP
##
##   andrea.nigri@unifg.it
#############################################
#############################################

library(tidyverse)
library(runjags)
library(ggplot2)
library(tidyverse)
library(readxl)

#  DATA -------------------------------------------------------------------
Dx <- read.csv("USA_m_short_idr.csv", header=T)
head(Dx)

# 1-male;2-female 
# Select Male
Dx_m <- Dx %>% filter(sex=="1")

# Ages selection 0 to 85+ 
Dx_m <- Dx_m[,c(2,6,16)]

# Got long format 
Dx_m <- Dx_m %>% filter(cause!=0)

# Other
Dx_m$Cause <- 3

#1) Circ
Dx_m$Cause[Dx_m$cause==7] <- 1
Dx_m$Cause[Dx_m$cause==8] <- 1
Dx_m$Cause[Dx_m$cause==9] <- 1

#2) Neopl
Dx_m$Cause[Dx_m$cause==2] <- 2

# Age 80
Dx_m$m40 <-Dx_m$m40/1000000
Dx_m <- aggregate(Dx_m, by = list(Year=Dx_m$year, class=Dx_m$Cause), FUN=sum) %>% select(-c(year,cause,Cause))
head(Dx_m)
D <- spread(Dx_m,class,m40)

# Creo proporzioni
D$SUM <- rowSums(D[,-1])
D$CIRC <- D[,2]/D[,5]
D$NEOP <- D[,3]/D[,5]
D$OTHER <- D[,4]/D[,5]
head(D)

D <- D[,c(6,7,8)]

gdp <- read_excel("gdp.xls")
gdp <- t(gdp)
gdp <- gdp[,1]
D$GDP <- log(as.numeric(gdp[41:55]))

# Hosp Bed
hosp <- read_excel("hostp.xls")
hosp <- t(hosp)
hosp <- hosp[,1]
D$h <- log(as.numeric(hosp[2:16]))

n <- 15
y1 <-  D[,1]
y2 <-  D[,2]
y3 <-  D[,3]
cause.y <- as.matrix((data.frame(y1,y2,y3)))
cause.y <-  cause.y/rowSums(cause.y)
mat.x<- D$GDP  
mat.x2 <- D$h


#show there are relationships with mat.
par(mfrow=c(1,3))
for(i in 1:ncol(cause.y)){
  plot(cause.y[,i] ~ mat.x)
  abline(lm(cause.y[,i] ~ mat.x), lwd = 2)
  rsq <- summary(lm(cause.y[,i] ~ mat.x))$r.squared
  txt <- paste0('R2 = ',round(rsq,2))
  mtext(txt, side = 3, line = -2, adj = 0.05)
}

par(mfrow=c(1,3))
for(i in 1:ncol(cause.y)){
  plot(cause.y[,i] ~ mat.x2)
  abline(lm(cause.y[,i] ~ mat.x2), lwd = 2)
  rsq <- summary(lm(cause.y[,i] ~ mat.x2))$r.squared
  txt <- paste0('R2 = ',round(rsq,2))
  mtext(txt, side = 3, line = -2, adj = 0.05)
}



# Model -------------------------------------------------------------------

dirlichet.model = "
model {
#setup priors for each species
for(j in 1:N.spp){
m0[j] ~ dnorm(0, .001) #intercept prior
m1[j] ~ dnorm(0, .001) #      mat.x prior
m2[j] ~ dnorm(0, .001)
}

#implement dirlichet
for(i in 1:N){
y[i,1:N.spp] ~ ddirch(a0[i,1:N.spp])


for(j in 1:N.spp){
log(a0[i,j]) <- m0[j] + m1[j] * mat.x[i]+ m2[j] * mat.x2[i]
}

}} #close model loop.
"

jags.data <- list(y = cause.y,mat.x= mat.x,mat.x2= mat.x2, N = nrow(cause.y), N.spp = ncol(cause.y))
jags.out <- run.jags(dirlichet.model,
                     data=jags.data,
                     adapt = 5000,
                     burnin = 50000,
                     sample = 100000,
                     n.chains=4,
                     monitor=c('m0','m1','m2'))
out <- summary(jags.out)
head(out)

# Seleziono coeff. modello 
coeff <- out[c(1,2,3,4,5,6,7,8,9),4]

coef1 <- out[c(1,4,7),4] #coeff (intercetta e slope) causa 1
coef2 <- out[c(2,5,8),4] #coeff (intercetta e slope) causa 2
coef3 <- out[c(3,6,9),4] #coeff (intercetta e slope) causa 3
pred <- as.matrix(cbind(exp(coef1[1]+coef1[2]*mat.x+coef1[3]*mat.x2),
                        exp(coef2[1]+coef2[2]*mat.x+coef2[3]*mat.x2),
                        exp(coef3[1]+coef3[2]*mat.x+coef3[3]*mat.x2)))

# Dal predict riottengo le proporzioni stimate con il mio modello
pred <- pred / rowSums(pred)

# predict Vs. Obs
lm(pred[,1]~cause.y[,1])
lm(pred[,2]~cause.y[,2])
lm(pred[,3]~cause.y[,3])

head(cause.y)
par(mfrow=c(1,3))
for(i in 1:ncol(cause.y)){
  plot(cause.y[,i] ~ pred[,i])
  abline(lm(cause.y[,i] ~ pred[,i]), lwd = 2)
  rsq <- summary(lm(cause.y[,i] ~ pred[,i]))$r.squared
  txt <- paste0('R2 = ',round(rsq,2))
  mtext(txt, side = 3, line = -2, adj = 0.05)
}


# Plot --------------------------------------------------------------------

Obs <- data.frame(Circ=cause.y[,1],
                  Neop=cause.y[,2],
                  Other=cause.y[,3],
                  GDP=mat.x,
                  h=mat.x2)

Obs$model <- "Obs"


Pred <- data.frame(Circ=pred[,1],
                   Neop=pred[,2],
                   Other=pred[,3],
                   GDP=mat.x,
                   h=mat.x2)

Pred$model <- "Pred"

tot40<-as.data.frame(rbind(Obs,Pred))
tot40$age <- 40


