
#########################################
####### PESQUISAS ELEITORAIS ############
#########################################

# Programador: Daniel Morais de Souza
# Última atualização # Junho/2023

#### Leitura dos dados ####

# Pacotes

library(haven)
library(dplyr)
library(data.table)
library(readxl)
library(tidyverse)
library(RODBC)
library(ggplot2)
library(dgof)
library(ggpubr)

# Diretório
setwd("C:\\Users\\daniel.souza\\Desktop\\CAEd\\Assuntos\\Eleições")

# Bases
Base_2016 <- read_excel("C:/Users/Daniel Morais/OneDrive/Desktop/Daniel/Github/Pesquisas-Eleitorais/Base_2016.xlsx")
Base_2020 <- read_excel("C:/Users/Daniel Morais/OneDrive/Desktop/Daniel/Github/Pesquisas-Eleitorais/Base_2020.xlsx")

# Cálculo da diferença para o teste de Wilcox 
Base_2020$D<-Base_2020$Pesquisa - Base_2020$Resultado
Base_2016$D<-Base_2016$Pesquisa - Base_2016$Resultado

Base<-rbind(Base_2020,Base_2016)

for (i in 1:31){
  if (i<16){
    Base$Ano[i]<-"2020"
  } else {
    Base$Ano[i]<-"2016" 
  }
}
Base<-Base[,c(3,4)]

ggplot(Base,aes(y = D, x = Ano, fill = Ano))+
  geom_boxplot()+
  ylab("Diferença percentual")+
  xlab("Ano")+
  geom_boxplot(outlier.color = "red")

ggplot(Base,aes(y = -D, x = Ano, fill = Ano))+
  geom_boxplot()+
  ylab("Diferença percentual")+
  xlab("Ano")+
  geom_boxplot(outlier.color = "red")


# VERIFICAÇÃO DOS PRESSUPOSTOS ####

# NORMALIDADE

### Verificando a Normalidade Através do Histograma
#preenchendo os quatro espaços com 4 histogramas (um para cada variável)
# histogram=function(x){
#   hist(x,prob=T)
#   lines(density(x),col="red")
#   curve(dnorm(x,mean(x), sd(x)),add=T,col="blue")
# }
# histogram(Base_2020$D)
# histogram(Base_2016$D)


### Shapiro

shapiro.test(Base_2020$D)
shapiro.test(Base_2016$D)

### Kolmogorov-Smirnov

ks.test(Base_2020$D,"pnorm",mean(Base_2020$D),sd(Base_2020$D))
ks.test(Base_2016$D,"pnorm",mean(Base_2016$D),sd(Base_2016$D))

ggqqplot(Base_2020$D,title = "2020")
ggqqplot(Base_2016$D,title = "2016")


### Análise 2020 ####

### Análises retirando apenas um par ####

# Itens a bootstrapar --> 2, 3, 4, 8, 9, 11, 12, 13, 15

# Possíveis combinações 
Comb_1<-Base_2020[-2,]
Comb_2<-Base_2020[-3,]
Comb_3<-Base_2020[-4,]
Comb_4<-Base_2020[-8,]
Comb_5<-Base_2020[-9,]
Comb_6<-Base_2020[-11,]
Comb_7<-Base_2020[-12,]
Comb_8<-Base_2020[-13,]
Comb_9<-Base_2020[-15,]


# Teste de Wilcox
wilcox.test(Base_2020$D, mu = 0, alternative = "less") # two sided
wilcox.test(Comb_1$D, mu = 0, alternative = "less")
wilcox.test(Comb_2$D, mu = 0, alternative = "less")
wilcox.test(Comb_3$D, mu = 0, alternative = "less")
wilcox.test(Comb_4$D, mu = 0, alternative = "less")
wilcox.test(Comb_5$D, mu = 0, alternative = "less")
wilcox.test(Comb_6$D, mu = 0, alternative = "less")
wilcox.test(Comb_7$D, mu = 0, alternative = "less")
wilcox.test(Comb_8$D, mu = 0, alternative = "less")
wilcox.test(Comb_9$D, mu = 0, alternative = "less")



# Teste t
t.test(Base_2020$Pesquisa, Base_2020$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_1$Pesquisa, Comb_1$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_2$Pesquisa, Comb_2$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_3$Pesquisa, Comb_3$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_4$Pesquisa, Comb_4$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_5$Pesquisa, Comb_5$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_6$Pesquisa, Comb_6$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_7$Pesquisa, Comb_7$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_8$Pesquisa, Comb_8$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_9$Pesquisa, Comb_9$Resultado, paired=TRUE, alternative = "less")



## 2, 3, 4, 8, 9, 11, 12, 13, 15

da <- data.frame(vals = c(Base_2020$Pesquisa, Base_2020$Resultado),
                 id = rep(c("Pesquisa", "Resultado"), each = 15))
da$id<-as.factor(da$id)

daComb_1<-da[-c(2,17),]
daComb_2<-da[-c(3,18),]
daComb_3<-da[-c(4,19),]
daComb_4<-da[-c(8,23),]
daComb_5<-da[-c(9,24),]
daComb_6<-da[-c(11,26),]
daComb_7<-da[-c(12,27),]
daComb_8<-da[-c(13,28),]
daComb_9<-da[-c(15,30),]



##

# Teste de U de Mann-Whitney

library(coin)

wilcox_test(vals ~ id, data = da, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_1, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_2, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_3, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_4, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_5, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_6, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_7, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_8, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_9, distribution = "exact", alternative = "less")



# Teste de permutação 

library(perm)

permTS(vals ~ id, data = da, alternative = "less")
permTS(vals ~ id, data = daComb_1, alternative = "less")
permTS(vals ~ id, data = daComb_2, alternative = "less")
permTS(vals ~ id, data = daComb_3, alternative = "less")
permTS(vals ~ id, data = daComb_4, alternative = "less")
permTS(vals ~ id, data = daComb_5, alternative = "less")
permTS(vals ~ id, data = daComb_6, alternative = "less")
permTS(vals ~ id, data = daComb_7, alternative = "less")
permTS(vals ~ id, data = daComb_8, alternative = "less")
permTS(vals ~ id, data = daComb_9, alternative = "less")



### Teste de Wilcox tirando 2 ou mais pares ####

Base_2020 <- read_excel("C:/Users/daniel.souza/Desktop/CAEd/Assuntos/Eleições/Base_2020_ENDFIM2.xlsx")
Base_2020$D<-Base_2020$Pesquisa - Base_2020$Resultado

# RETIRANDO 2 OBS

Duas<-combn(9,2)

p_value<-as.numeric(0)

for (i in 1:36){
  p_value[i]<- wilcox.test(Base_2020$D[-(Duas[,i])] , mu = 0, alternative = "less")$p.value
  
}

hist(p_value)

Tres<- combn(9,3)

p_value_3<-as.numeric(0)

for (i in 1:84){
  p_value_3[i]<- wilcox.test(Base_2020$D[-(Tres[,i])] , mu = 0, alternative = "less")$p.value
  
}

hist(p_value_3)

Quatro<-combn(9,4)

p_value_4<-as.numeric(0)

for (i in 1:126){
  p_value_4[i]<- wilcox.test(Base_2020$D[-(Quatro[,i])] , mu = 0, alternative = "less")$p.value
  
}

hist(p_value_4)

p_value_wilcox<-c(p_value,p_value_3,p_value_4)

p_value_wilcox<-as.data.frame(p_value_wilcox)

ggplot(data = p_value_wilcox, aes(x = p_value_wilcox)) +
  geom_histogram(col='black', bins=100)+
  xlab("P-Valor")+
  ylab("Quantidade de combinações")+
  xlim(-0.025, 1)+
  ylim(0,200)



### Teste t pareado tirando dois ou mais pares ####

p_value_t<-as.numeric(0)
p_value_t3<-as.numeric(0)
p_value_t4<-as.numeric(0)

for (i in 1:36){
  p_value_t[i]<- t.test(Base_2020$Pesquisa[-(Duas[,i])], Base_2020$Resultado[-(Duas[,i])], paired=TRUE, alternative = "less")$p.value
}

hist(p_value_t)

for (i in 1:84){
  p_value_t3[i]<- t.test(Base_2020$Pesquisa[-(Tres[,i])], Base_2020$Resultado[-(Tres[,i])], paired=TRUE, alternative = "less")$p.value
}

hist(p_value_t3)

for (i in 1:126){
  p_value_t4[i]<- t.test(Base_2020$Pesquisa[-(Quatro[,i])], Base_2020$Resultado[-(Quatro[,i])], paired=TRUE, alternative = "less")$p.value
}

hist(p_value_t4)

p_value_tpareado<-c(p_value_t,p_value_t3,p_value_t4)

p_value_tpareado<-as.data.frame(p_value_tpareado)

ggplot(data = p_value_tpareado, aes(x = p_value_tpareado)) +
  geom_histogram(col='black', bins=100)+
  xlab("P-Valor")+
  ylab("Quantidade de combinações")+
  xlim(-0.015, 1)+
  ylim(0,200)

### Teste U de Mann-Whitney tirando 2 ou mais pares ####

p_value_u<-as.numeric(0)
p_value_u3<-as.numeric(0)
p_value_u4<-as.numeric(0)

da <- data.frame(vals = c(Base_2020$Pesquisa, Base_2020$Resultado),
                 id = rep(c("Pesquisa", "Resultado"), each = 15))
da$id<-as.factor(da$id)
daComb_1<-da[-c(1,16),]
daComb_2<-da[-c(2,17),]
daComb_3<-da[-c(3,18),]
daComb_4<-da[-c(4,19),]
daComb_5<-da[-c(5,20),]
daComb_6<-da[-c(6,21),]
daComb_7<-da[-c(7,22),]
daComb_8<-da[-c(8,23),]
daComb_9<-da[-c(9,24),]


for (i in 1:36){
  p_value_u[i]<- pvalue(wilcox_test(vals ~ id, data = da[-c(Duas[,i],Duas[,i]+19),], distribution = "exact", alternative = "less"))
}

hist(p_value_u)

for (i in 1:84){
  p_value_u3[i]<- pvalue(wilcox_test(vals ~ id, data = da[-c(Tres[,i],Tres[,i]+19),], distribution = "exact", alternative = "less"))
}

hist(p_value_u3)

for (i in 1:126){
  p_value_u4[i]<- pvalue(wilcox_test(vals ~ id, data = da[-c(Quatro[,i],Quatro[,i]+19),], distribution = "exact", alternative = "less"))
}

hist(p_value_u)

p_value_utotal<-c(p_value_u,p_value_u3,p_value_u4)

p_value_utotal<-as.data.frame(p_value_utotal)

ggplot(data = p_value_utotal, aes(x = p_value_utotal)) +
  geom_histogram(col='black', bins=100)+
  xlab("P-Valor")+
  ylab("Quantidade de combinações")+
  xlim(-0.025, 1)+
  ylim(0,200)

### Teste de permutação tirando 2 ou mais pares ####

p_value_per<-as.numeric(0)
p_value_per3<-as.numeric(0)
p_value_per4<-as.numeric(0)

for (i in 1:36){
  p_value_per[i]<- permTS(vals ~ id, data = da[-c(Duas[,i],Duas[,i]+19),], alternative = "less")$p.value
}

hist(p_value_per)

for (i in 1:84){
  p_value_per3[i]<- permTS(vals ~ id, data = da[-c(Tres[,i],Tres[,i]+19),], alternative = "less")$p.value
}

hist(p_value_per3)

for (i in 1:126){
  p_value_per4[i]<- permTS(vals ~ id, data = da[-c(Quatro[,i],Quatro[,i]+19),], alternative = "less")$p.value
}

hist(p_value_per4)

p_value_pertotal<-c(p_value_per,p_value_per3,p_value_per4)

p_value_pertotal<-as.data.frame(p_value_pertotal)

ggplot(data = p_value_pertotal, aes(x = p_value_pertotal)) +
  geom_histogram(col='black', bins=100)+
  xlab("P-Valor")+
  ylab("Quantidade de combinações")+
  xlim(-0.025, 1)+
  ylim(0,200)

####


p_wilcox<-rbind(wilcox.test(Base_2020$D, mu = 0, alternative = "less")$p.value,
                wilcox.test(Comb_1$D, mu = 0, alternative = "less")$p.value,
                wilcox.test(Comb_2$D, mu = 0, alternative = "less")$p.value,
                wilcox.test(Comb_3$D, mu = 0, alternative = "less")$p.value,
                wilcox.test(Comb_4$D, mu = 0, alternative = "less")$p.value,
                wilcox.test(Comb_5$D, mu = 0, alternative = "less")$p.value,
                wilcox.test(Comb_6$D, mu = 0, alternative = "less")$p.value,
                wilcox.test(Comb_7$D, mu = 0, alternative = "less")$p.value,
                wilcox.test(Comb_8$D, mu = 0, alternative = "less")$p.value,
                wilcox.test(Comb_9$D, mu = 0, alternative = "less")$p.value,
                #                wilcox.test(Comb_10$D, mu = 0, alternative = "less")$p.value,
                p_value_wilcox)

p_t<-rbind(t.test(Base_2020$Pesquisa, Base_2020$Resultado, paired=TRUE, alternative = "less")$p.value,
           t.test(Comb_1$Pesquisa, Comb_1$Resultado, paired=TRUE, alternative = "less")$p.value,
           t.test(Comb_2$Pesquisa, Comb_2$Resultado, paired=TRUE, alternative = "less")$p.value,
           t.test(Comb_3$Pesquisa, Comb_3$Resultado, paired=TRUE, alternative = "less")$p.value,
           t.test(Comb_4$Pesquisa, Comb_4$Resultado, paired=TRUE, alternative = "less")$p.value,
           t.test(Comb_5$Pesquisa, Comb_5$Resultado, paired=TRUE, alternative = "less")$p.value,
           t.test(Comb_6$Pesquisa, Comb_6$Resultado, paired=TRUE, alternative = "less")$p.value,
           t.test(Comb_7$Pesquisa, Comb_7$Resultado, paired=TRUE, alternative = "less")$p.value,
           t.test(Comb_8$Pesquisa, Comb_8$Resultado, paired=TRUE, alternative = "less")$p.value,
           t.test(Comb_9$Pesquisa, Comb_9$Resultado, paired=TRUE, alternative = "less")$p.value,
           #           t.test(Comb_10$Pesquisa, Comb_10$Resultado, paired=TRUE, alternative = "less")$p.value,
           p_value_tpareado)

p_u<-rbind(pvalue(wilcox_test(vals ~ id, data = da, distribution = "exact", alternative = "less")),
           pvalue(wilcox_test(vals ~ id, data = daComb_1, distribution = "exact", alternative = "less")),
           pvalue(wilcox_test(vals ~ id, data = daComb_2, distribution = "exact", alternative = "less")),
           pvalue(wilcox_test(vals ~ id, data = daComb_3, distribution = "exact", alternative = "less")),
           pvalue(wilcox_test(vals ~ id, data = daComb_4, distribution = "exact", alternative = "less")),
           pvalue(wilcox_test(vals ~ id, data = daComb_5, distribution = "exact", alternative = "less")),
           pvalue(wilcox_test(vals ~ id, data = daComb_6, distribution = "exact", alternative = "less")),
           pvalue(wilcox_test(vals ~ id, data = daComb_7, distribution = "exact", alternative = "less")),
           pvalue(wilcox_test(vals ~ id, data = daComb_8, distribution = "exact", alternative = "less")),
           pvalue(wilcox_test(vals ~ id, data = daComb_9, distribution = "exact", alternative = "less")),
           #           pvalue(wilcox_test(vals ~ id, data = daComb_10, distribution = "exact", alternative = "less")),
           p_value_utotal)

p_perm<-rbind(permTS(vals ~ id, data = da, alternative = "less")$p.value,
              permTS(vals ~ id, data = daComb_1, alternative = "less")$p.value,
              permTS(vals ~ id, data = daComb_2, alternative = "less")$p.value,
              permTS(vals ~ id, data = daComb_3, alternative = "less")$p.value,
              permTS(vals ~ id, data = daComb_4, alternative = "less")$p.value,
              permTS(vals ~ id, data = daComb_5, alternative = "less")$p.value,
              permTS(vals ~ id, data = daComb_6, alternative = "less")$p.value,
              permTS(vals ~ id, data = daComb_7, alternative = "less")$p.value,
              permTS(vals ~ id, data = daComb_8, alternative = "less")$p.value,
              permTS(vals ~ id, data = daComb_9, alternative = "less")$p.value,
              #              permTS(vals ~ id, data = daComb_10, alternative = "less")$p.value,
              p_value_pertotal)


p_wilcox <- p_wilcox %>% mutate(SIGNIFICANCIA = case_when(p_wilcox$p_value_wilcox<=0.05 ~"Muito_sig",
                                                          (p_wilcox$p_value_wilcox>0.05 & p_wilcox$p_value_wilcox<=0.10)~"Sig",
                                                          p_wilcox$p_value_wilcox>0.10~"H0"))
table(p_wilcox$SIGNIFICANCIA)
prop.table(table(p_wilcox$SIGNIFICANCIA))

p_t <- p_t %>% mutate(SIGNIFICANCIA = case_when(p_t$p_value_tpareado<=0.05 ~"Muito_sig",
                                                (p_t$p_value_tpareado>0.05 & p_t$p_value_tpareado<=0.10)~"Sig",
                                                p_t$p_value_tpareado>0.10~"H0"))
table(p_t$SIGNIFICANCIA)
prop.table(table(p_t$SIGNIFICANCIA))

p_u <- p_u %>% mutate(SIGNIFICANCIA = case_when(p_u$p_value_utotal<=0.05 ~"Muito_sig",
                                                (p_u$p_value_utotal>0.05 & p_u$p_value_utotal<=0.10)~"Sig",
                                                p_u$p_value_utotal>0.10~"H0"))
table(p_u$SIGNIFICANCIA)
prop.table(table(p_u$SIGNIFICANCIA))


p_perm <- p_perm %>% mutate(SIGNIFICANCIA = case_when(p_perm$p_value_pertotal<=0.05 ~"Muito_sig",
                                                      (p_perm$p_value_pertotal>0.05 & p_perm$p_value_pertotal<=0.10)~"Sig",
                                                      p_perm$p_value_pertotal>0.10~"H0"))
table(p_perm$SIGNIFICANCIA)
prop.table(table(p_perm$SIGNIFICANCIA))

### Análise 2016 ####

# Itens a bootstrapar --> 2, 3, 5, 6, 7, 8, 9, 10, 13, 15 

### Análises retirando apenas um par ####

# Possíveis combinações 
Comb_1<-Base_2016[-2,]
Comb_2<-Base_2016[-3,]
Comb_3<-Base_2016[-5,]
Comb_4<-Base_2016[-6,]
Comb_5<-Base_2016[-7,]
Comb_6<-Base_2016[-8,]
Comb_7<-Base_2016[-9,]
Comb_8<-Base_2016[-10,]
Comb_9<-Base_2016[-13,]
Comb_10<-Base_2016[-15,]

# Teste de Wilcox
wilcox.test(Base_2016$D, mu = 0, alternative = "less") # two sided
wilcox.test(Comb_1$D, mu = 0, alternative = "less")
wilcox.test(Comb_2$D, mu = 0, alternative = "less")
wilcox.test(Comb_3$D, mu = 0, alternative = "less")
wilcox.test(Comb_4$D, mu = 0, alternative = "less")
wilcox.test(Comb_5$D, mu = 0, alternative = "less")
wilcox.test(Comb_6$D, mu = 0, alternative = "less")
wilcox.test(Comb_7$D, mu = 0, alternative = "less")
wilcox.test(Comb_8$D, mu = 0, alternative = "less")
wilcox.test(Comb_9$D, mu = 0, alternative = "less")
wilcox.test(Comb_10$D, mu = 0, alternative = "less")


# Teste t
t.test(Base_2016$Pesquisa, Base_2016$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_1$Pesquisa, Comb_1$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_2$Pesquisa, Comb_2$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_3$Pesquisa, Comb_3$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_4$Pesquisa, Comb_4$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_5$Pesquisa, Comb_5$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_6$Pesquisa, Comb_6$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_7$Pesquisa, Comb_7$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_8$Pesquisa, Comb_8$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_9$Pesquisa, Comb_9$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_10$Pesquisa, Comb_9$Resultado, paired=TRUE, alternative = "less")
###

da <- data.frame(vals = c(Base_2016$Pesquisa, Base_2016$Resultado),
                 id = rep(c("Pesquisa", "Resultado"), each = 16))
da$id<-as.factor(da$id)

daComb_1<-da[-c(2,18),]
daComb_2<-da[-c(3,19),]
daComb_3<-da[-c(5,21),]
daComb_4<-da[-c(6,22),]
daComb_5<-da[-c(7,23),]
daComb_6<-da[-c(8,24),]
daComb_7<-da[-c(9,25),]
daComb_8<-da[-c(10,26),]
daComb_9<-da[-c(13,29),]
daComb_10<-da[-c(15,31),]

###

# Teste de U de Mann-Whitney

library(coin)

wilcox_test(vals ~ id, data = da, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_1, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_2, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_3, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_4, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_5, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_6, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_7, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_8, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_9, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_10, distribution = "exact", alternative = "less")

# Teste de permutação 

library(perm)

permTS(vals ~ id, data = da, alternative = "less")
permTS(vals ~ id, data = daComb_1, alternative = "less")
permTS(vals ~ id, data = daComb_2, alternative = "less")
permTS(vals ~ id, data = daComb_3, alternative = "less")
permTS(vals ~ id, data = daComb_4, alternative = "less")
permTS(vals ~ id, data = daComb_5, alternative = "less")
permTS(vals ~ id, data = daComb_6, alternative = "less")
permTS(vals ~ id, data = daComb_7, alternative = "less")
permTS(vals ~ id, data = daComb_8, alternative = "less")
permTS(vals ~ id, data = daComb_9, alternative = "less")
permTS(vals ~ id, data = daComb_10, alternative = "less")


### Teste de Wilcox tirando 2 ou mais pares ####

Base_2016 <- read_excel("C:/Users/daniel.souza/Desktop/CAEd/Assuntos/Eleições/Base_2016_ENDFIM2.xlsx")
Base_2016$D<-Base_2016$Pesquisa - Base_2016$Resultado

# RETIRANDO 2 OBS

Duas<-combn(10,2)

p_value<-as.numeric(0)

for (i in 1:45){
  p_value[i]<- wilcox.test(Base_2016$D[-(Duas[,i])] , mu = 0, alternative = "less")$p.value
  
}

hist(p_value)

Tres<- combn(10,3)

p_value_3<-as.numeric(0)

for (i in 1:120){
  p_value_3[i]<- wilcox.test(Base_2016$D[-(Tres[,i])] , mu = 0, alternative = "less")$p.value
  
}

hist(p_value_3)

Quatro<-combn(10,4)

p_value_4<-as.numeric(0)

for (i in 1:210){
  p_value_4[i]<- wilcox.test(Base_2016$D[-(Quatro[,i])] , mu = 0, alternative = "less")$p.value
  
}

hist(p_value_4)

p_value_wilcox<-c(p_value,p_value_3,p_value_4)

p_value_wilcox<-as.data.frame(p_value_wilcox)

ggplot(data = p_value_wilcox, aes(x = p_value_wilcox)) +
  geom_histogram(col='black', bins=100)+
  xlab("P-Valor")+
  ylab("Quantidade de combinações")+
  xlim(-0.015, 1)+
  ylim(0,200)

### Teste t pareado tirando dois ou mais pares ####

p_value_t<-as.numeric(0)
p_value_t3<-as.numeric(0)
p_value_t4<-as.numeric(0)

for (i in 1:45){
  p_value_t[i]<- t.test(Base_2016$Pesquisa[-(Duas[,i])], Base_2016$Resultado[-(Duas[,i])], paired=TRUE, alternative = "less")$p.value
}

hist(p_value_t)

for (i in 1:120){
  p_value_t3[i]<- t.test(Base_2016$Pesquisa[-(Tres[,i])], Base_2016$Resultado[-(Tres[,i])], paired=TRUE, alternative = "less")$p.value
}

hist(p_value_t3)

for (i in 1:210){
  p_value_t4[i]<- t.test(Base_2016$Pesquisa[-(Quatro[,i])], Base_2016$Resultado[-(Quatro[,i])], paired=TRUE, alternative = "less")$p.value
}

hist(p_value_t4)

p_value_tpareado<-c(p_value_t,p_value_t3,p_value_t4)

p_value_tpareado<-as.data.frame(p_value_tpareado)

ggplot(data = p_value_tpareado, aes(x = p_value_tpareado)) +
  geom_histogram(col='black', bins=100)+
  xlab("P-Valor")+
  ylab("Quantidade de combinações")+
  xlim(-0.025, 1)+
  ylim(0,200)

### Teste U de Mann-Whitney tirando 2 ou mais pares ####

p_value_u<-as.numeric(0)
p_value_u3<-as.numeric(0)
p_value_u4<-as.numeric(0)

da <- data.frame(vals = c(Base_2016$Pesquisa, Base_2016$Resultado),
                 id = rep(c("Pesquisa", "Resultado"), each = 16))
da$id<-as.factor(da$id)
daComb_1<-da[-c(1,17),]
daComb_2<-da[-c(2,18),]
daComb_3<-da[-c(3,19),]
daComb_4<-da[-c(4,20),]
daComb_5<-da[-c(5,21),]
daComb_6<-da[-c(6,22),]
daComb_7<-da[-c(7,23),]
daComb_8<-da[-c(8,24),]
daComb_9<-da[-c(9,25),]
daComb_10<-da[-c(10,26),]


for (i in 1:45){
  p_value_u[i]<- pvalue(wilcox_test(vals ~ id, data = da[-c(Duas[,i],Duas[,i]+15),], distribution = "exact", alternative = "less"))
}

hist(p_value_u)

for (i in 1:120){
  p_value_u3[i]<- pvalue(wilcox_test(vals ~ id, data = da[-c(Tres[,i],Tres[,i]+15),], distribution = "exact", alternative = "less"))
}

hist(p_value_u3)

for (i in 1:210){
  p_value_u4[i]<- pvalue(wilcox_test(vals ~ id, data = da[-c(Quatro[,i],Quatro[,i]+15),], distribution = "exact", alternative = "less"))
}

hist(p_value_u)

p_value_utotal<-c(p_value_u,p_value_u3,p_value_u4)

p_value_utotal<-as.data.frame(p_value_utotal)

ggplot(data = p_value_utotal, aes(x = p_value_utotal)) +
  geom_histogram(col='black', bins=100)+
  xlab("P-Valor")+
  ylab("Quantidade de combinações")+
  xlim(-0.025, 1)+
  ylim(0,200)

### Teste de permutação tirando 2 ou mais pares ####

p_value_per<-as.numeric(0)
p_value_per3<-as.numeric(0)
p_value_per4<-as.numeric(0)

for (i in 1:45){
  p_value_per[i]<- permTS(vals ~ id, data = da[-c(Duas[,i],Duas[,i]+15),], alternative = "less")$p.value
}

hist(p_value_per)

for (i in 1:120){
  p_value_per3[i]<- permTS(vals ~ id, data = da[-c(Tres[,i],Tres[,i]+15),], alternative = "less")$p.value
}

hist(p_value_per3)

for (i in 1:210){
  p_value_per4[i]<- permTS(vals ~ id, data = da[-c(Quatro[,i],Quatro[,i]+15),], alternative = "less")$p.value
}

hist(p_value_per4)

p_value_pertotal<-c(p_value_per,p_value_per3,p_value_per4)

p_value_pertotal<-as.data.frame(p_value_pertotal)

ggplot(data = p_value_pertotal, aes(x = p_value_pertotal)) +
  geom_histogram(col='black', bins=100)+
  xlab("P-Valor")+
  ylab("Quantidade de combinações")+
  xlim(-0.025, 1)+
  ylim(0,200)

####

p_wilcox<-rbind(wilcox.test(Base_2016$D, mu = 0, alternative = "less")$p.value,
                wilcox.test(Comb_1$D, mu = 0, alternative = "less")$p.value,
                wilcox.test(Comb_2$D, mu = 0, alternative = "less")$p.value,
                wilcox.test(Comb_3$D, mu = 0, alternative = "less")$p.value,
                wilcox.test(Comb_4$D, mu = 0, alternative = "less")$p.value,
                wilcox.test(Comb_5$D, mu = 0, alternative = "less")$p.value,
                wilcox.test(Comb_6$D, mu = 0, alternative = "less")$p.value,
                wilcox.test(Comb_7$D, mu = 0, alternative = "less")$p.value,
                wilcox.test(Comb_8$D, mu = 0, alternative = "less")$p.value,
                wilcox.test(Comb_9$D, mu = 0, alternative = "less")$p.value,
                wilcox.test(Comb_10$D, mu = 0, alternative = "less")$p.value,
                p_value_wilcox)

p_t<-rbind(t.test(Base_2016$Pesquisa, Base_2016$Resultado, paired=TRUE, alternative = "less")$p.value,
           t.test(Comb_1$Pesquisa, Comb_1$Resultado, paired=TRUE, alternative = "less")$p.value,
           t.test(Comb_2$Pesquisa, Comb_2$Resultado, paired=TRUE, alternative = "less")$p.value,
           t.test(Comb_3$Pesquisa, Comb_3$Resultado, paired=TRUE, alternative = "less")$p.value,
           t.test(Comb_4$Pesquisa, Comb_4$Resultado, paired=TRUE, alternative = "less")$p.value,
           t.test(Comb_5$Pesquisa, Comb_5$Resultado, paired=TRUE, alternative = "less")$p.value,
           t.test(Comb_6$Pesquisa, Comb_6$Resultado, paired=TRUE, alternative = "less")$p.value,
           t.test(Comb_7$Pesquisa, Comb_7$Resultado, paired=TRUE, alternative = "less")$p.value,
           t.test(Comb_8$Pesquisa, Comb_8$Resultado, paired=TRUE, alternative = "less")$p.value,
           t.test(Comb_9$Pesquisa, Comb_9$Resultado, paired=TRUE, alternative = "less")$p.value,
           t.test(Comb_10$Pesquisa, Comb_10$Resultado, paired=TRUE, alternative = "less")$p.value,
           p_value_tpareado)

p_u<-rbind(pvalue(wilcox_test(vals ~ id, data = da, distribution = "exact", alternative = "less")),
           pvalue(wilcox_test(vals ~ id, data = daComb_1, distribution = "exact", alternative = "less")),
           pvalue(wilcox_test(vals ~ id, data = daComb_2, distribution = "exact", alternative = "less")),
           pvalue(wilcox_test(vals ~ id, data = daComb_3, distribution = "exact", alternative = "less")),
           pvalue(wilcox_test(vals ~ id, data = daComb_4, distribution = "exact", alternative = "less")),
           pvalue(wilcox_test(vals ~ id, data = daComb_5, distribution = "exact", alternative = "less")),
           pvalue(wilcox_test(vals ~ id, data = daComb_6, distribution = "exact", alternative = "less")),
           pvalue(wilcox_test(vals ~ id, data = daComb_7, distribution = "exact", alternative = "less")),
           pvalue(wilcox_test(vals ~ id, data = daComb_8, distribution = "exact", alternative = "less")),
           pvalue(wilcox_test(vals ~ id, data = daComb_9, distribution = "exact", alternative = "less")),
           pvalue(wilcox_test(vals ~ id, data = daComb_10, distribution = "exact", alternative = "less")),   
           p_value_utotal)

p_perm<-rbind(permTS(vals ~ id, data = da, alternative = "less")$p.value,
              permTS(vals ~ id, data = daComb_1, alternative = "less")$p.value,
              permTS(vals ~ id, data = daComb_2, alternative = "less")$p.value,
              permTS(vals ~ id, data = daComb_3, alternative = "less")$p.value,
              permTS(vals ~ id, data = daComb_4, alternative = "less")$p.value,
              permTS(vals ~ id, data = daComb_5, alternative = "less")$p.value,
              permTS(vals ~ id, data = daComb_6, alternative = "less")$p.value,
              permTS(vals ~ id, data = daComb_7, alternative = "less")$p.value,
              permTS(vals ~ id, data = daComb_8, alternative = "less")$p.value,
              permTS(vals ~ id, data = daComb_9, alternative = "less")$p.value,
              permTS(vals ~ id, data = daComb_10, alternative = "less")$p.value,       
              p_value_pertotal)

p_wilcox <- p_wilcox %>% mutate(SIGNIFICANCIA = case_when(p_wilcox$p_value_wilcox<=0.05 ~"Muito_sig",
                                                          (p_wilcox$p_value_wilcox>0.05 & p_wilcox$p_value_wilcox<=0.10)~"Sig",
                                                          p_wilcox$p_value_wilcox>0.10~"H0"))
table(p_wilcox$SIGNIFICANCIA)
prop.table(table(p_wilcox$SIGNIFICANCIA))

p_t <- p_t %>% mutate(SIGNIFICANCIA = case_when(p_t$p_value_tpareado<=0.05 ~"Muito_sig",
                                                (p_t$p_value_tpareado>0.05 & p_t$p_value_tpareado<=0.10)~"Sig",
                                                p_t$p_value_tpareado>0.10~"H0"))
table(p_t$SIGNIFICANCIA)
prop.table(table(p_t$SIGNIFICANCIA))

p_u <- p_u %>% mutate(SIGNIFICANCIA = case_when(p_u$p_value_utotal<=0.05 ~"Muito_sig",
                                                (p_u$p_value_utotal>0.05 & p_u$p_value_utotal<=0.10)~"Sig",
                                                p_u$p_value_utotal>0.10~"H0"))
table(p_u$SIGNIFICANCIA)
prop.table(table(p_u$SIGNIFICANCIA))


p_perm <- p_perm %>% mutate(SIGNIFICANCIA = case_when(p_perm$p_value_pertotal<=0.05 ~"Muito_sig",
                                                      (p_perm$p_value_pertotal>0.05 & p_perm$p_value_pertotal<=0.10)~"Sig",
                                                      p_perm$p_value_pertotal>0.10~"H0"))
table(p_perm$SIGNIFICANCIA)
prop.table(table(p_perm$SIGNIFICANCIA))



#### Reeleição ####

# Diretório
setwd("C:\\Users\\daniel.souza\\Desktop\\CAEd\\Assuntos\\Eleições")

# Bases
Base_2016 <- read_excel("C:/Users/daniel.souza/Desktop/CAEd/Assuntos/Eleições/Base_2016_reelect.xlsx")
Base_2020 <- read_excel("C:/Users/daniel.souza/Desktop/CAEd/Assuntos/Eleições/Base_2020_reelect.xlsx")

#### Reeleição e pesquisas eleitorais ####

# Cálculo da diferença para o teste de Wilcox 
Base_2020$D<-Base_2020$Pesquisa - Base_2020$Resultado
Base_2016$D<-Base_2016$Pesquisa - Base_2016$Resultado

Base<-rbind(Base_2020,Base_2016)

for (i in 1:25){
  if (i<15){
    Base$Ano[i]<-"2020"
  } else {
    Base$Ano[i]<-"2016" 
  }
}


ggplot(Base,aes(y = D, x = Ano, fill = Ano))+
  geom_boxplot()+
  ylab("Diferença percentual")+
  xlab("Ano")+
  geom_boxplot(outlier.color = "red")

ggplot(Base,aes(y = -D, x = Ano, fill = Ano))+
  geom_boxplot()+
  ylab("Diferença percentual")+
  xlab("Ano")+
  geom_boxplot(outlier.color = "red")

# Possíveis combinações 2020
Comb_1<-Base_2020[-1,]
Comb_2<-Base_2020[-3,]
Comb_3<-Base_2020[-4,]
Comb_4<-Base_2020[-5,]
Comb_5<-Base_2020[-6,]
Comb_6<-Base_2020[-7,]
Comb_7<-Base_2020[-10,]
Comb_8<-Base_2020[-11,]
Comb_9<-Base_2020[-14,]


# Teste de Wilcox
wilcox.test(Base_2020$D, mu = 0, alternative = "less") # two sided
wilcox.test(Comb_1$D, mu = 0, alternative = "less")
wilcox.test(Comb_2$D, mu = 0, alternative = "less")
wilcox.test(Comb_3$D, mu = 0, alternative = "less")
wilcox.test(Comb_4$D, mu = 0, alternative = "less")
wilcox.test(Comb_5$D, mu = 0, alternative = "less")
wilcox.test(Comb_6$D, mu = 0, alternative = "less")
wilcox.test(Comb_7$D, mu = 0, alternative = "less")
wilcox.test(Comb_8$D, mu = 0, alternative = "less")
wilcox.test(Comb_9$D, mu = 0, alternative = "less")


# Teste t
t.test(Base_2020$Pesquisa, Base_2020$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_1$Pesquisa, Comb_1$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_2$Pesquisa, Comb_2$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_3$Pesquisa, Comb_3$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_4$Pesquisa, Comb_4$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_5$Pesquisa, Comb_5$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_6$Pesquisa, Comb_6$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_7$Pesquisa, Comb_7$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_8$Pesquisa, Comb_8$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_9$Pesquisa, Comb_9$Resultado, paired=TRUE, alternative = "less")


##

da <- data.frame(vals = c(Base_2020$Pesquisa, Base_2020$Resultado),
                 id = rep(c("Pesquisa", "Resultado"), each = 14))
da$id<-as.factor(da$id)

daComb_1<-da[-c(1,15),]
daComb_2<-da[-c(3,17),]
daComb_3<-da[-c(4,18),]
daComb_4<-da[-c(5,19),]
daComb_5<-da[-c(6,20),]
daComb_6<-da[-c(7,21),]
daComb_7<-da[-c(10,24),]
daComb_8<-da[-c(11,25),]
daComb_9<-da[-c(14,28),]

##

# Teste de U de Mann-Whitney

library(coin)

wilcox_test(vals ~ id, data = da, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_1, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_2, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_3, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_4, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_5, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_6, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_7, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_8, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_9, distribution = "exact", alternative = "less")


# Teste de permutação 

library(perm)

permTS(vals ~ id, data = da, alternative = "less")
permTS(vals ~ id, data = daComb_1, alternative = "less")
permTS(vals ~ id, data = daComb_2, alternative = "less")
permTS(vals ~ id, data = daComb_3, alternative = "less")
permTS(vals ~ id, data = daComb_4, alternative = "less")
permTS(vals ~ id, data = daComb_5, alternative = "less")
permTS(vals ~ id, data = daComb_6, alternative = "less")
permTS(vals ~ id, data = daComb_7, alternative = "less")
permTS(vals ~ id, data = daComb_8, alternative = "less")
permTS(vals ~ id, data = daComb_9, alternative = "less")




# Possíveis combinações 2016
Comb_1<-Base_2016[-6,]

# Teste de Wilcox
wilcox.test(Base_2016$D, mu = 0, alternative = "less") # two sided
wilcox.test(Comb_1$D, mu = 0, alternative = "less")



# Teste t
t.test(Base_2016$Pesquisa, Base_2016$Resultado, paired=TRUE, alternative = "less")
t.test(Comb_1$Pesquisa, Comb_1$Resultado, paired=TRUE, alternative = "less")


###

da <- data.frame(vals = c(Base_2016$Pesquisa, Base_2016$Resultado),
                 id = rep(c("Pesquisa", "Resultado"), each = 11))
da$id<-as.factor(da$id)


daComb_1<-da[-c(6,17),]


###

# Teste de U de Mann-Whitney

library(coin)

wilcox_test(vals ~ id, data = da, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_1, distribution = "exact", alternative = "less")


# Teste de permutação 

library(perm)

permTS(vals ~ id, data = da, alternative = "less")
permTS(vals ~ id, data = daComb_1, alternative = "less")

#### Reeleição e "plebiscito" ####

# Cálculo da diferença para o teste de Wilcox 
Base_2020$D<-Base_2020$Reeleicao - Base_2020$Contra
Base_2016$D<-Base_2016$Reeleicao - Base_2016$Contra

Base<-rbind(Base_2020,Base_2016)

for (i in 1:25){
  if (i<15){
    Base$Ano[i]<-"2020"
  } else {
    Base$Ano[i]<-"2016" 
  }
}


ggplot(Base,aes(y = D, x = Ano, fill = Ano))+
  geom_boxplot()+
  ylab("Diferença percentual")+
  xlab("Ano")+
  geom_boxplot(outlier.color = "red")

ggplot(Base,aes(y = -D, x = Ano, fill = Ano))+
  geom_boxplot()+
  ylab("Diferença percentual")+
  xlab("Ano")+
  geom_boxplot(outlier.color = "red")

# Possíveis combinações 2020
Comb_1<-Base_2020[-1,]
Comb_2<-Base_2020[-3,]
Comb_3<-Base_2020[-4,]
Comb_4<-Base_2020[-5,]
Comb_5<-Base_2020[-6,]
Comb_6<-Base_2020[-7,]
Comb_7<-Base_2020[-10,]
Comb_8<-Base_2020[-11,]
Comb_9<-Base_2020[-14,]


# Teste de Wilcox
wilcox.test(Base_2020$D, mu = 0, alternative = "less") # two sided
wilcox.test(Comb_1$D, mu = 0, alternative = "less")
wilcox.test(Comb_2$D, mu = 0, alternative = "less")
wilcox.test(Comb_3$D, mu = 0, alternative = "less")
wilcox.test(Comb_4$D, mu = 0, alternative = "less")
wilcox.test(Comb_5$D, mu = 0, alternative = "less")
wilcox.test(Comb_6$D, mu = 0, alternative = "less")
wilcox.test(Comb_7$D, mu = 0, alternative = "less")
wilcox.test(Comb_8$D, mu = 0, alternative = "less")
wilcox.test(Comb_9$D, mu = 0, alternative = "less")


# Teste t
t.test(Base_2020$Reeleicao, Base_2020$Contra, paired=TRUE, alternative = "less")
t.test(Comb_1$Reeleicao, Comb_1$Contra, paired=TRUE, alternative = "less")
t.test(Comb_2$Reeleicao, Comb_2$Contra, paired=TRUE, alternative = "less")
t.test(Comb_3$Reeleicao, Comb_3$Contra, paired=TRUE, alternative = "less")
t.test(Comb_4$Reeleicao, Comb_4$Contra, paired=TRUE, alternative = "less")
t.test(Comb_5$Reeleicao, Comb_5$Contra, paired=TRUE, alternative = "less")
t.test(Comb_6$Reeleicao, Comb_6$Contra, paired=TRUE, alternative = "less")
t.test(Comb_7$Reeleicao, Comb_7$Contra, paired=TRUE, alternative = "less")
t.test(Comb_8$Reeleicao, Comb_8$Contra, paired=TRUE, alternative = "less")
t.test(Comb_9$Reeleicao, Comb_9$Contra, paired=TRUE, alternative = "less")


##

da <- data.frame(vals = c(Base_2020$Reeleicao, Base_2020$Contra),
                 id = rep(c("Reeleicao", "Contra"), each = 14))
da$id<-as.factor(da$id)

daComb_1<-da[-c(1,15),]
daComb_2<-da[-c(3,17),]
daComb_3<-da[-c(4,18),]
daComb_4<-da[-c(5,19),]
daComb_5<-da[-c(6,20),]
daComb_6<-da[-c(7,21),]
daComb_7<-da[-c(10,24),]
daComb_8<-da[-c(11,25),]
daComb_9<-da[-c(14,28),]

##

# Teste de U de Mann-Whitney

library(coin)

wilcox_test(vals ~ id, data = da, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_1, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_2, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_3, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_4, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_5, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_6, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_7, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_8, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_9, distribution = "exact", alternative = "less")


# Teste de permutação 

library(perm)

permTS(vals ~ id, data = da, alternative = "less")
permTS(vals ~ id, data = daComb_1, alternative = "less")
permTS(vals ~ id, data = daComb_2, alternative = "less")
permTS(vals ~ id, data = daComb_3, alternative = "less")
permTS(vals ~ id, data = daComb_4, alternative = "less")
permTS(vals ~ id, data = daComb_5, alternative = "less")
permTS(vals ~ id, data = daComb_6, alternative = "less")
permTS(vals ~ id, data = daComb_7, alternative = "less")
permTS(vals ~ id, data = daComb_8, alternative = "less")
permTS(vals ~ id, data = daComb_9, alternative = "less")




# Possíveis combinações 2016
Comb_1<-Base_2016[-6,]

# Teste de Wilcox
wilcox.test(Base_2016$D, mu = 0, alternative = "less") # two sided
wilcox.test(Comb_1$D, mu = 0, alternative = "less")



# Teste t
t.test(Base_2016$Reeleicao, Base_2016$Contra, paired=TRUE, alternative = "less")
t.test(Comb_1$Reeleicao, Comb_1$Contra, paired=TRUE, alternative = "less")


###

da <- data.frame(vals = c(Base_2016$Reeleicao, Base_2016$Contra),
                 id = rep(c("Reeleicao", "Contra"), each = 11))
da$id<-as.factor(da$id)


daComb_1<-da[-c(6,17),]


###

# Teste de U de Mann-Whitney

library(coin)

wilcox_test(vals ~ id, data = da, distribution = "exact", alternative = "less")
wilcox_test(vals ~ id, data = daComb_1, distribution = "exact", alternative = "less")


# Teste de permutação 

library(perm)

permTS(vals ~ id, data = da, alternative = "less")
permTS(vals ~ id, data = daComb_1, alternative = "less")


#### Exemplo teste de permutação ####

#   http://cursos.leg.ufpr.br/ce089/12_permutacao.html


x <- c(4.1, 8.3, 2.9, 10.8, 9.5)
y <- c(3.7, 5.1, 1.0, 7.7, 8.9)
da <- data.frame(vals = c(x, y),
                 id = rep(c("x", "y"), each = 5))
da

## Compara médias
with(da, tapply(vals, id, mean))

(obsdiff <- with(da, abs(diff(tapply(vals, id, mean)))))

## Teste-t tradicional
t.test(vals ~ id, data = da, var.equal = TRUE)

## Número possível de permutações por grupo
x<-Base_2020$Pesquisa
y<-Base_2020$Resultado
factorial(length(x))
factorial(length(y))

## A permutação dentro de cada grupo não faz sentido, pois as médias não
## serão alteradas

xperm <- gtools::permutations(n = length(x), r = length(x), v = x)
str(xperm)
sort(x)

yperm <- gtools::permutations(n = length(y), r = length(y), v = y)
str(yperm)
sort(y)

## Diferença entre médias para todas as permutações
xydiff <- numeric(nrow(xperm))
for(i in 1:nrow(xperm)) {
  xydiff[i] <- mean(xperm[i, ]) - mean(yperm[i, ])
}
str(xydiff)
summary(xydiff)

## Portanto, a permutação deve ser feita entre os grupos, ou seja,
## alternando todos os valores possíveis entre os dois grupos
xy <- c(x, y)
## Número de permutações
factorial(length(xy))

xyperm <- gtools::permutations(n = length(xy), r = length(xy), v = xy)
str(xyperm)

## Calcula a diferença média para todas as permutações possíveis
library(future.apply)
plan(multicore, workers = 4)
xydiff <- future_apply(matrix(1:nrow(xyperm)), 1, function(i) {
  mean(xyperm[i, 1:5]) - mean(xyperm[i, 6:10])
})
str(xydiff)

summary(xydiff)

hist(xydiff)
abline(v = obsdiff, col = 2)

## P-valor do teste.
2 * sum(xydiff >= obsdiff)/length(xydiff)

## Usando pacotes
library(coin)
oneway_test(vals ~ factor(id), data = da)

oneway_test(vals ~ factor(id), data = da,
            distribution = approximate(nresample = 10000))

library(perm)
permTS(vals ~ id, data = da)


# Mesmo em um caso simples como, esse, onde n = 10, já vimos que o número total de permutações possíveis
# pode ser muito grande, o que faz com que esse processo fique inviável computacionalmente. A ideia então
# é fazer um grande número de permutações aleatórias e fazer o mesmo cálculo. Isso pode ser feito retirando-se
# amostra COM REPOSIÇÃO da amostra conjunta (concatenando os dois grupos) Usando amostras sem reposição

N <- 10000
xydiff <- future_replicate(
  N, diff(tapply(sample(xy), da$id, mean))
)
str(xydiff)
summary(xydiff)

hist(xydiff)
abline(v = obsdiff, col = 2)

## P-valor do teste.
2 * sum(xydiff >= obsdiff)/length(xydiff)


t.test(vals ~ id, data = da, var.equal = TRUE)
t.test(x, y, paired=TRUE)
