# Verifcando quando é o diretório atual de trabalho
getwd()

#Configurando o diretório de trabalho
setwd('C:\\Users\\Anderson\\Documents\\UDACITY\\Nanodegree Data Scientist\\Aulas de R\\Facebook\\Aula_R_Facebook\\')

#Instalando uma biblioteca
#install.packages('ggplot2')
#install.packages("gridExtra")

# Carregando uma biblioteca
library(ggplot2)
library(gridExtra)

#Carregando o DataSet
pf <- read.csv('pseudo_facebook.tsv',sep = '\t')

#Indicando que o DataSet é o pf e que a coluna a ser impressa é a dob_day
ggplot(aes(x = dob_day), data = pf) + 
  #Informando que o BINSIZE é 1
  geom_histogram(binwidth = 1) + 
  #Fazendo que o eixo de X vá do dia 1 ao dia 31
  scale_x_continuous(breaks = 1:31) +
  #Gerando um histograma para cada mês do ano
  facet_wrap(~dob_month)

#Quais são as colunas do DataSet
names(pf)

ggplot(aes(x = friend_count), data = pf) + 
  geom_histogram() +
  facet_wrap(~gender)

# Plotando dois histogramas, um para o sexo masculino e outros para os exo feminino
# Também estão sendo excluídos os registros que têm valor nulo para o sexo
qplot(x = friend_count, data = subset(pf, !is.na(gender)),xlim = c(0,1000),breaks = seq(0,1000,50)) +
  facet_wrap(~gender)

#Para plotar informações de maneira tabular
table(pf$gender)

#Imprimir as estatísticas das colunas
by(pf$friend_count,pf$gender,summary)

qplot(x = tenure/365,data = pf, binwidth= .25, color = I('black'),fill=I('#F79420') ,
  xlab = 'Número de anos usando o Facebook',
  ylab = 'Número de usuários na amostra') +
  scale_x_continuous(breaks = seq(1,7,1),limits = c(0,7))

summary(pf$age)

qplot(x = age,data = pf, binwidth= 1, color = I('black'),fill=I('#5760AB') ,
      xlab = 'Idade',
      ylab = 'Número de usuários na amostra') +
  scale_x_continuous(breaks = seq(10,115,5),limits = c(10,113))


summary(pf$friend_count)
#Escalando a variável por métodos diferentes
summary(log10(pf$friend_count + 1))
summary(sqrt(pf$friend_count))

#Usando a biblioteca GRIDEXTRA para plotar mais de um gráfico na mesma saída
p1 <- qplot(x=friend_count,data=pf)
p2 <- qplot(x=log10(friend_count + 1),data=pf,color = I('black'),fill=I('#F79420'))
p3 <- qplot(x=sqrt(friend_count),data=pf,color = I('black'),fill=I('#5760AB'))
grid.arrange(p1,p2,p3,ncol=3)


ggplot(aes(x = friend_count,y=..count../sum(..count..)), data = subset(pf, !is.na(gender))) + 
  geom_freqpoly(aes(color = gender), binwidth=5) + 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) +
  xlab('Contagem de AmigosLikes') + 
  ylab('Percentual de Amigos')


qplot(x= www_likes,data= subset(pf,!is.na(gender)),geom = 'freqpoly',color= gender)+
  scale_x_continuous() +
  scale_x_log10()


by(pf$www_likes,pf$gender,sum)

#Usando BOXPLOT
qplot(x = gender, y = friend_count, data = subset(pf, !is.na(gender)), geom = 'boxplot') +
      scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))

by(pf$friendships_initiated,pf$gender,summary)  

names(pf)

mobile_check_in <- NA
pf$mobile_check_in <- ifelse(pf$mobile_likes>0,1,0)
pf$mobile_check_in <- factor(pf$mobile_check_in)
summary(pf$mobile_check_in)

#Dois métodos diferentes para obter a frequência relativa da variável mobile_check_in

nrow(subset(pf,pf$mobile_check_in==0)) * 100 / length(pf$mobile_check_in)
nrow(subset(pf,pf$mobile_check_in==1)) * 100 / length(pf$mobile_check_in)

sum(pf$mobile_check_in==1)/length(pf$mobile_check_in)
sum(pf$mobile_check_in==0)/length(pf$mobile_check_in)
