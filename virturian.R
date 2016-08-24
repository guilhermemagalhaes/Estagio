rm(list=ls());

source('C:/Users/guilh/Documents/UFMG/Virturian/Bayes_multi.R')
library(mvtnorm)

data = read.table('C:/Users/guilh/Downloads/waveform-+noise.data', sep = ',');

wave_select <- data[,41];

data <- data[,-41];

#Seleciona o tipo de onda. São 3 tipos de onda e a diferença entre elas está
#na coluna 41 da matriz.
#wave0, wave1 e wave2 mostram o tipo de onda de cada trecho de dados.

wave0 <- data[wave_select==0,];
wave1 <- data[wave_select==1,];
wave2 <- data[wave_select==2,];

#Calcula a probabilidade a priori.
p1 <- nrow(wave0)/length(wave_select);
p2 <- nrow(wave1)/length(wave_select);
p3 <- nrow(wave2)/length(wave_select);

##########################################################
#TREINO
##########################################################
#Dados usados para treinar o modelo de classificação.
#Através desses dados os parametros são adquiridos.
#Foram escolhidas 3000 amostras para treino.

s_treino <- sample(length(wave0[,1]),1000)
wave0_treino <- wave0[s_treino,]

#Matriz de covariancia
cv1 <- cov(wave0_treino)

#Média da das colunas da matriz
mc1 <- colMeans(wave0_treino)

s_treino2 <- sample(length(wave1[,1]),1000)
wave1_treino <- wave1[s_treino2,]
cv2 <- cov(wave1_treino)
mc2 <- colMeans(wave1_treino)

s_treino3 <- sample(length(wave2[,1]),1000)
wave2_treino <- wave2[s_treino3,]
cv3 <- cov(wave2_treino)
mc3 <- colMeans(wave2_treino)

##########################################################
#TESTE
##########################################################
#Esses dados são usados para testar o modelo.
#Foram escolhidos 2000 amostras no total para teste.

wave0_teste <- wave0[-s_treino,]
wave1_teste <- wave1[-s_treino2,]
wave2_teste <- wave2[-s_treino3,]

##########################################################
#MATRIZ DE TESTE
##########################################################
#Apenas junta os valores de treino em uma matriz
#para realizar o teste.

m_teste <- rbind(wave0_teste,wave1_teste,wave2_teste)

##########################################################
#REGRA DE BAYES
##########################################################
#Utilizando a regra de Bayes é possível calcular a probabilidade
#de uma amostra estar em uma das 3 classes. Para isso
#utiliza-se a distribuição normal multivariada.

classe1 <- matrix(0,nrow = length(m_teste[,1]))
classe2 <- matrix(0,nrow = length(m_teste[,1]))
classe3 <- matrix(0,nrow = length(m_teste[,1]))

for (i in 1:length(m_teste[,1])){
  classe1[i,] <- dmvnorm(x = m_teste[i,],mean = mc1, sigma = cv1)
  classe2[i,] <- dmvnorm(x = m_teste[i,],mean = mc2, sigma = cv2)
  classe3[i,] <- dmvnorm(x = m_teste[i,],mean = mc3, sigma = cv3)
}

##########################################################
#RESULTADO
##########################################################
#Como diz a regra de Bayes, tem que multiplicar a probabilidade
#a priori com o valor obtido na distribuição normal multivariada.

  result1 <- classe1*p1
  result2 <- classe2*p2
  result3 <- classe3*p3

##########################################################
#CONTAGEM DE ACERTOS
##########################################################
#Os valores em que a probabilidade é maior são escolhidos.
#Por exemplo, a_c1 indica Verdadeiro para todas probabilidades
#em que o result1(wave0) foi o maior.
  
  a_c1 <- (result1 > result2 & result1 > result3)
  a_c2 <- (result2 > result1 & result2 > result3)
  a_c3 <- (result3 > result1 & result3 > result2)

  #Calculo de porcentagem
  acerto1 <- sum(a_c1[1:692] == TRUE)/length(wave0_teste[,1])
  acerto2 <- sum(a_c2[693:1345] == TRUE)/length(wave1_teste[,1])
  acerto3 <- sum(a_c3[1346:2000] == TRUE)/length(wave2_teste[,1])
  
  #Calculo de porcentagem da média
  p_total <- 100*(acerto1+acerto2+acerto3)/3