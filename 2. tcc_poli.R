#PRELIMINARES------------------------------------------------------------------------------------------
ls()            #requests all objects in R's brain
rm(list=ls())    #we use two functions at once, rm an ls. rm stands for 

getwd()			    #"get working directory", where R in "currently" loonking...
setwd("C:/Users/Fadini/Documents/2. orientações e defesas/1. em andamento/2023_TCC_Poliana Silva Pereira")
getwd()			    #use getwd to confirm that R is now looking here

detach(data)

#------------------------------------------------------------------------------------------------------
#23.out.2023
#Os dados abaixo sao referentes ao TCC da Poliana Silva Pereira. Ela avaliou o numero de cigarrinha-
#do-milho Dalbulus maidis em funcao da tolerancia do hibrido (baixa, media e alta) e o numero de 
#pulverizacoes (0 a 5) no tempo (0 a 28 dias) em um DIC no campo experimental da Embrapa.

#23.out.2023
#Mas temos uma duvida... essa "tolerancia" e a doenca ou a cigarrinha? Caso seja uma tolerancia a
#doenca, faria sentido avaliar o numero de cigarrinhas em funcao desta "tolerancia"?

#25.out.2023
#Faz sentido avaliar o numero de cigarrinhas em funcao da tolerancia. Apesar da tolerancia ser
#para doenca, estudos tem mostrado que cultivares mais tolerantes a doenca, aparentemente, dificultam
#a alimentacao da cigarrinha devido ao fato da maior espessura da parede celular das plantas.

#25.out.2023
#Devemos investigar o n??mero de cigarrinhas em funcao da tolerancia. Devemos fazer.

------------------------------------------------------------------------------------------------------
#16.dez.2023
#Entrada dos dados -----------------------------------------------------------------------------------
  
#Como importar data do GitHub:

data_git<-paste0("https://raw.githubusercontent.com/",
                 "marcosfadini/",
                 "mip/",
                 "main/",
                 "1.%20cigarrinha_fenologia.csv")

data_git

install_formats()
library("rio")

data <- rio::import(data_git,format = "csv", h=T)

attach(data)
data

#------------------------------------------------------------------------------------------------------
#Entrada dos dados ------------------------------------------------------------------------------------
data<-read.table("1. cigarrinha_fenologia.csv",h=T,sep=";",dec=",") #
attach(data)
data
names(data)
str(data)

#------------------------------------------------------------------------------------------------------
#23.out.2023
#checagens iniciais
plot(n_cigarrinha)               #existe uma grande quantidade de zeros

hist(n_cigarrinha,
     ylim=c(0,2000),
     las=1,					             #orienta????o do texto em y
     bty="l")                    #existe uma grande quantidade de zero = tem que resolver isso...

#------------------------------------------------------------------------------------------------------
#Grafico de dispersao
plot(n_cigarrinha~tempo,
     xlim=c(0,30),				            #determina limites para eixo x
     ylim=c(0,5),				              #determina limetes para eixo y
     xlab="Tempo (dias)",				      #adiciona texto no eixo x
     ylab="Numeor de cigarrinhas",		#adiciona texto no eixo y
     las=1,					                  #orientacao do texto em y
     bty="l")

#------------------------------------------------------------------------------------------------------
#Carregando pacotes
require(ggplot2)                                           #pacote requerido
install.packages("ggpubr")                                 #instalacao do pacote  
library(ggpubr)

#Grafico de linhas por tolerancia no tempo (com jitter) -----------------------------------------------
#23.out.2023
#Mas temos uma duvida... essa "tolerancia" e a doenca ou a cigarrinha? Caso seja uma tolerancia a
#doenca, faria sentido avaliar o numero de cigarrinhas em funcao desta "tolerancia"?

ggline(data, 
       x="tempo", 
       y="n_cigarrinha",
       ylim=c(0,4),
       xlim=c(0,30),
       add=c("mean_se","jitter"),                  #com jitter
       color = "hibrid_tolerancia", 
       xlab="Tempo (dias)",				      #adiciona texto no eixo x
       ylab="Numeor de cigarrinhas",		#adiciona texto no eixo y
       palette = "jco")

#Grafico de linhas por tolerancia no tempo (sem jitter) ----------------------------------------------
#23.out.2023
#Mas temos uma duvida... essa "tolerancia" e a doenca ou a cigarrinha? Caso seja uma tolerancia a
#doenca, faria sentido avaliar o numero de cigarrinhas em funcao desta "tolerancia"?

ggline(data, 
       x="tempo", 
       y="n_cigarrinha",
       ylim=c(0,0.6),
       xlim=c(0,30),
       add=c("mean_se"),                           #sem jitter
       color = "hibrid_tolerancia", 
       xlab="Tempo (dias)",				      #adiciona texto no eixo x
       ylab="Numeor de cigarrinhas",		#adiciona texto no eixo y
       palette = "jco")

#Grafico de linhas por tolerancia na fenologia (sem jitter) ------------------------------------------
#25.out.2023
#Mas temos uma duvida... essa "tolerancia" e a doenca ou a cigarrinha? Caso seja uma tolerancia a
#doenca, faria sentido avaliar o numero de cigarrinhas em funcao desta "tolerancia"?

ggline(data, 
       x="estadio", 
       y="n_cigarrinha",
       ylim=c(0,0.6),
       add=c("mean_se"),                           #sem jitter
       color = "hibrid_tolerancia", 
       xlab="Fenologia da planta",				      #adiciona texto no eixo x
       ylab="Numeor de cigarrinhas",		#adiciona texto no eixo y
       palette = "jco")

#Grafico de linhas por n_pulverizacoes no tempo (com jitter) ------------------------------------------
ggline(data, 
       x="tempo", 
       y="n_cigarrinha",
       ylim=c(0,4),
       xlim=c(0,30),
       add=c("mean_se","jitter"),                  #com jitter
       color = "n_pulverizacoes_caracter",         #n_pulverizacoes como caracter 
       xlab="Tempo (dias)",				      #adiciona texto no eixo x
       ylab="Numero de cigarrinhas",		#adiciona texto no eixo y
       palette = "jco")

#Grafico de linhas por n_pulverizacoes no tempo (sem jitter) ------------------------------------------
ggline(data, 
       x="tempo", 
       y="n_cigarrinha",
       ylim=c(0,0.7),
       xlim=c(0,30),
       add=c("mean_se"),                           #sem jitter
       color = "n_pulverizacoes_caracter",         #n_pulverizacoes como caracter
       xlab="Tempo (dias)",				      #adiciona texto no eixo x
       ylab="Numero de cigarrinhas",		#adiciona texto no eixo y
       palette = "jco")

#------------------------------------------------------------------------------------------------------
#Grafico de barras por n_pulverizacoes por fenologia (com jitter) -------------------------------------
ggbarplot(data, 
       x="estadio", 
       y="n_cigarrinha",
       ylim=c(0,4),
       add=c("mean_se","jitter"),                  #com jitter
       color = "n_pulverizacoes_caracter",         #n_pulverizacoes como caracter 
       xlab="Estagio fenologico",
       ylab="Numero de cigarrinhas",
       palette = "jco")

#Grafico de barras por n_pulverizacoes por fenologia (sem jitter) -------------------------------------
ggbarplot(data, 
          x="estadio", 
          y="n_cigarrinha",
          ylim=c(0,2),
          add=c("mean_se"),                           #sem jitter
          color = "n_pulverizacoes_caracter",         #n_pulverizacoes como caracter 
          xlab="Estagio fenologico",
          ylab="Numero de cigarrinhas",
          palette = "jco")

#Grafico de linhas de n_pulverizacoes por fenologia (sem jitter) --------------------------------------
ggline(data, 
          x="estadio", 
          y="n_cigarrinha",
          ylim=c(0,0.7),
          add=c("mean_se"),                           #sem jitter
          color = "n_pulverizacoes_caracter",         #n_pulverizacoes como caracter 
          xlab="Estagio fenologico",
          ylab="Numero de cigarrinhas",
          palette = "jco")

#------------------------------------------------------------------------------------------------------
#Grafico de barra para n_cigarrinhas em funcao do n_pulberizacoes no tempo 14
data_14<-data[tempo==14,]
data_14

#barra de medias --------------------------------------------------------------------------------------
ggbarplot(data_14,
          x="n_pulverizacoes_caracter", 
          y="n_cigarrinha",
          add =c("mean_se","jitter"),             #com jitter
          color = "n_pulverizacoes_caracter",
          xlab="Numero de pulverizacoes",
          ylab="Numero de cigarrinhas",
          fill="n_pulverizacoes_caracter",
          alpha=0.5)

#barra de medias --------------------------------------------------------------------------------------
ggbarplot(data_14,
          x="n_pulverizacoes_caracter", 
          y="n_cigarrinha",
          ylim=c(0,1),
          add =c("mean_se"),                     #sem jitter
          color = "n_pulverizacoes_caracter",
          fill="n_pulverizacoes_caracter",
          xlab="Numero de pulverizacoes",
          ylab="Numero de cigarrinhas",
          alpha=0.5)

#------------------------------------------------------------------------------------------------------
#Grafico de barra para n_cigarrinhas em funcao do n_pulberizacoes no tempo 28
data_28<-data[tempo==28,]
data_28

#barra de medias --------------------------------------------------------------------------------------
ggbarplot(data_28,
          x="n_pulverizacoes_caracter", 
          y="n_cigarrinha",
          add =c("mean_se","jitter"),             #com jitter
          color = "n_pulverizacoes_caracter",
          xlab="Numero de pulverizacoes",
          ylab="Numero de cigarrinhas",
          fill="n_pulverizacoes_caracter",
          alpha=0.5)

#barra de medias --------------------------------------------------------------------------------------
ggbarplot(data_28,
          x="n_pulverizacoes_caracter", 
          y="n_cigarrinha",
          ylim=c(0,0.5),
          add =c("mean_se"),                     #sem jitter
          color = "n_pulverizacoes_caracter",
          fill="n_pulverizacoes_caracter",
          xlab="Numero de pulverizacoes",
          ylab="Numero de cigarrinhas",
          alpha=0.5)


#########################################################################################################
#23.out.2o23
#Apos fazermos alguns graficos podemos pensar agora em ajustar modelos...
#Observar que existem muitos (muitos mesmo!) zeros para o n_cigarrinha. Tem que usar transformacao ou glm.

summary(model_aov_0<-aov(n_cigarrinha[tempo==0]~n_pulverizacoes_caracter[tempo==0]))      # com 0 + 1 pulverizacao  - ns
summary(model_glm_0<-glm(n_cigarrinha[tempo==0]~n_pulverizacoes_caracter[tempo==0],
                         quasipoisson))                                                   # com 0 + 1 pulverizacao  - ns


summary(model_aov_7<-aov(n_cigarrinha[tempo==7]~n_pulverizacoes_caracter[tempo==7]))      # com 0 + 2 pulverizacoes - ns 
summary(model_glm_7<-glm(n_cigarrinha[tempo==7]~n_pulverizacoes_caracter[tempo==7],
                         quasipoisson))                                                   # com 0 + 2 pulverizacoes - ns

summary(model_aov_14<-aov(n_cigarrinha[tempo==14]~n_pulverizacoes_caracter[tempo==14]))   # com 0 + 3 pulverizacoes - **
summary(model_glm_14<-glm(n_cigarrinha[tempo==14]~n_pulverizacoes_caracter[tempo==14],
                         quasipoisson))                                                   # com 0 + 3 pulverizacoes - **

summary(model_aov_14<-aov(n_cigarrinha[tempo==14]~hibrid_tolerancia[tempo==14]))          # com tolerancia
summary(model_glm_14<-glm(n_cigarrinha[tempo==14]~n_pulverizacoes_caracter[tempo==14],
                          quasipoisson))                                                  # com tolerancia

summary(model_aov_21<-aov(n_cigarrinha[tempo==21]~n_pulverizacoes_caracter[tempo==21]))   # com 0 + 4 pulverizacoes - ***
summary(model_glm_21<-glm(n_cigarrinha[tempo==21]~n_pulverizacoes_caracter[tempo==21],
                          quasipoisson))                                                  # com 0 + 4 pulverizacoes - **

summary(model_aov_28<-aov(n_cigarrinha[tempo==28]~n_pulverizacoes_caracter[tempo==28]))   # com 0 + 5 pulverizacoes - ***
summary(model_glm_28<-glm(n_cigarrinha[tempo==28]~n_pulverizacoes_caracter[tempo==28],
                          quasipoisson))                                                  # com 0 + 5 pulverizacoes - ***

#------------------------------------------------------------------------------------------------------
#26.out.2023
#SK para tolerancia e numero de pulvarizacoes

require(ScottKnott)
#------------------------------------------------------------------------------------------------------
data_0<-data[tempo==0,]

sk1_0<-with(data_0,
            SK(n_cigarrinha~hibrid_tolerancia,                #tolerancia, tempo = 0
               data=data_0,
               which='hibrid_tolerancia'))

sk_0_n_cigarrinha<-summary(sk1_0)

#------------------------------------------------------------------------------------------------------
sk1_0<-with(data_0,
            SK(n_cigarrinha~n_pulverizacoes_caracter,         #pulverizacao, tempo = 0
               data=data_0,
               which='n_pulverizacoes_caracter'))

sk_0_n_cigarrinha<-summary(sk1_0)

#------------------------------------------------------------------------------------------------------
data_7<-data[tempo==7,]

sk1_7<-with(data_7,
            SK(n_cigarrinha~hibrid_tolerancia,                  #tolerancia, tempo = 7
               data=data_7,
               which='hibrid_tolerancia'))

sk_7_n_cigarrinha<-summary(sk1_7)

#------------------------------------------------------------------------------------------------------
sk1_7<-with(data_7,
            SK(n_cigarrinha~n_pulverizacoes_caracter,           #pulverizacao, tempo = 7
               data=data_7,
               which='n_pulverizacoes_caracter'))

sk_7_n_cigarrinha<-summary(sk1_7)

#------------------------------------------------------------------------------------------------------
data_14<-data[tempo==14,]

sk1_14<-with(data_14,
            SK(n_cigarrinha~hibrid_tolerancia,                  #tolerancia, tempo = 14
               data=data_14,
               which='hibrid_tolerancia'))

sk_14_n_cigarrinha<-summary(sk1_14)

#------------------------------------------------------------------------------------------------------
sk1_14<-with(data_14,
            SK(n_cigarrinha~n_pulverizacoes_caracter,            #pulverizacao, tempo = 14
               data=data_14,
               which='n_pulverizacoes_caracter'))

sk_14_n_cigarrinha<-summary(sk1_14)

#------------------------------------------------------------------------------------------------------
data_21<-data[tempo==21,]

sk1_21<-with(data_21,
             SK(n_cigarrinha~hibrid_tolerancia,                   #tolerancia, tempo = 21
                data=data_21,
                which='hibrid_tolerancia'))

sk_21_n_cigarrinha<-summary(sk1_21)

#------------------------------------------------------------------------------------------------------
sk1_21<-with(data_21,
             SK(n_cigarrinha~n_pulverizacoes_caracter,            #pulverizacao, tempo = 21
                data=data_21,
                which='n_pulverizacoes_caracter'))

sk_21_n_cigarrinha<-summary(sk1_21)

#------------------------------------------------------------------------------------------------------
data_28<-data[tempo==28,]

sk1_28<-with(data_28,
             SK(n_cigarrinha~hibrid_tolerancia,                   #tolerancia, tempo = 28
                data=data_28,
                which='hibrid_tolerancia'))

sk_28_n_cigarrinha<-summary(sk1_28)

#------------------------------------------------------------------------------------------------------
sk1_28<-with(data_28,
             SK(n_cigarrinha~n_pulverizacoes_caracter,            #pulverizacao, tempo = 28
                data=data_28,
                which='n_pulverizacoes_caracter'))

sk_28_n_cigarrinha<-summary(sk1_28)

#------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------
#04.dez.2024
#Tendo ajustar modelo glm com distribuicao de Poisson - tem que usar GLMM


model_1 <- glm (n_cigarrinha ~ estadio * planta * hibrid_tolerancia * n_pulverizacoes, family = poisson)
summary (model)

1887.4/2340

model_2 <- update (model_1, ~ . - estadio:planta:hibrid_tolerancia:n_pulverizacoes)

anova(model_1,model_2, test = "Chi")

summary(model_2)

model_3 <- update (model_2, ~ . - planta:hibrid_tolerancia:n_pulverizacoes)

anova(model_2,model_3, test = "Chi")
