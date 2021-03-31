library(reshape2)

tot40
DB <- rbind(tot40,tot60,tot80)

DB <- as.data.frame(DB)
head(DB)

DB$age <- as.factor(DB$age)
DB$model <- as.factor(DB$model)

DDBB <- melt(DB,id=c("age","GDP","h","model"))


DDBB$variable <- as.factor(DDBB$variable)



DDBB %>%filter(variable=="Circ",age==80,model=="Obs") %>%  ggplot(aes(GDP,value))+geom_point()+
  geom_line(data = DDBB %>%
              filter(variable=="Circ",age==80,model=="Pred"))



###############

DDBB %>%filter(variable==c("Circ","Neop"),age==40,model=="Obs") %>%  ggplot(aes(GDP,value,col=variable))+geom_point()+
  geom_line(data = DDBB %>%
              filter(variable==c("Circ","Neop"),age==40,model=="Pred"))


DDBB %>%filter(variable==c("Circ","Neop"),age==60,model=="Obs") %>%  ggplot(aes(GDP,value,col=variable))+geom_point()+
geom_line(data = DDBB %>%
             filter(variable==c("Circ","Neop"),age==60,model=="Pred"))
  
DDBB %>%filter(variable==c("Circ","Neop"),age==80,model=="Obs") %>%  ggplot(aes(GDP,value,col=variable))+geom_point()+
  geom_line(data = DDBB %>%
              filter(variable==c("Circ","Neop"),age==80,model=="Pred"))



#######################




DDBB %>%filter(variable==c("Circ","Neop"),model=="Obs") %>%  ggplot(aes(GDP,value,col=variable))+geom_point()+
  geom_line(data = DDBB %>%
              filter(variable==c("Circ","Neop"),model=="Pred"))+facet_grid(variable~age,scales = "free")


theme_classic()
theme_set(theme_bw(base_size = 15))

p <- DDBB %>%filter(model=="Obs") %>%  ggplot(aes(GDP,value,col=variable))+geom_point(size=2)+
  geom_line(size=1.2,data = DDBB %>%
              filter(model=="Pred"))+facet_wrap(~age+variable,scales = "free")+

theme(legend.position = "bottom",
      legend.title = element_text(size = 15)) +labs(x = "log(GDP)")+labs(y = "CoD")

p 

ggsave("CoD_plot.png",p, height = 9, width = 12)

  


DDBB %>%filter(variable==c("Circ","Neop"),model=="Obs") %>%  ggplot(aes(h,value,col=variable))+geom_point()+
  geom_line(data = DDBB %>%
              filter(variable==c("Circ","Neop"),model=="Pred"))+facet_grid(variable~age,scales = "free")


theme_classic()
theme_set(theme_bw(base_size = 15))

p <- DDBB %>%filter(model=="Obs") %>%  ggplot(aes(h,value,col=variable))+geom_point(size=2)+
  geom_line(size=1.2,data = DDBB %>%
              filter(model=="Pred"))+facet_wrap(~age+variable,scales = "free")+
  
  theme(legend.position = "bottom",
        legend.title = element_text(size = 15)) +labs(x = "log(H)")+labs(y = "CoD")

p 
  
  