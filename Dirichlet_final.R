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
library(reshape2)

set.seed(123)

# Dati cause di morte (Circ; Neopl; Altre) USA età 80 anni + GDP log scale 
load("Cause.RData")

n <- 15
y1 <-  D[,1]
y2 <-  D[,2]
y3 <-  D[,3]
cause.y <- as.matrix((data.frame(y1,y2,y3)))
cause.y <-  cause.y/rowSums(cause.y)
mat.x<- D$GDP # GDP log scale 

#show there are relationships with mat.
par(mfrow=c(1,3))
for(i in 1:ncol(cause.y)){
  plot(cause.y[,i] ~ mat.x)
  abline(lm(cause.y[,i] ~ mat.x), lwd = 2)
  rsq <- summary(lm(cause.y[,i] ~ mat.x))$r.squared
  txt <- paste0('R2 = ',round(rsq,2))
  mtext(txt, side = 3, line = -2, adj = 0.05)
}


# Model -------------------------------------------------------------------

dirlichet.model = "
model {
#setup priors for each species
for(j in 1:N.spp){
m0[j] ~ dnorm(0, 1.0E-3) #intercept prior
m1[j] ~ dnorm(0, 1.0E-3) #      mat.x prior
}

#implement dirlichet
for(i in 1:N){
y[i,1:N.spp] ~ ddirch(a0[i,1:N.spp])


for(j in 1:N.spp){
log(a0[i,j]) <- m0[j] + m1[j] * mat.x[i]
}

}} #close model loop.
"

jags.data <- list(y = cause.y,mat.x= mat.x, N = nrow(cause.y), N.spp = ncol(cause.y))
jags.out <- run.jags(dirlichet.model,
                     data=jags.data,
                     adapt = 500,
                     burnin = 50000,
                     sample = 100000,
                     n.chains=4,
                     monitor=c('m0','m1'))
out <- summary(jags.out)
head(out)

# model coeff. selection
coeff <- out[c(1,2,3,4,5,6),4]

coef1 <- out[c(1,4),4] #coeff (inter and slope) caus 1
coef2 <- out[c(2,5),4] #coeff (inter and slope) caus 2
coef3 <- out[c(3,6),4] #coeff (inter and slope) caus 3
pred <- as.matrix(cbind(exp(coef1[1]+coef1[2]*mat.x),exp(coef2[1]+coef2[2]*mat.x),exp(coef3[1]+coef3[2]*mat.x)))

# get proportion
pred <- pred / rowSums(pred)

# predict Vs. Obs
lm(pred[,1]~cause.y[,1])
lm(pred[,2]~cause.y[,2])
lm(pred[,3]~cause.y[,3])


# Plot --------------------------------------------------------------------

Obs <- data.frame(Circ=cause.y[,1],
                  Neop=cause.y[,2],
                  Other=cause.y[,3],
                  GDP=mat.x)
Obs$model <- "Obs"


Pred <- data.frame(Circ=pred[,1],
                   Neop=pred[,2],
                   Other=pred[,3],
                   GDP=mat.x)
Pred$model <- "Pred"

tot80<-as.data.frame(rbind(Obs,Pred))
tot <- melt(tot80,id.vars = c("GDP","model"))

theme_classic()
theme_set(theme_bw(base_size = 15))

tot %>%filter(model=="Obs") %>%  ggplot(aes(GDP,value))+geom_point(size=2)+
  geom_line(size=1.2,data = tot %>%
              filter(model=="Pred"))+facet_wrap(~variable,scales = "free")


