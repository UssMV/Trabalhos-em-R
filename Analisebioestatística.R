rm(list = ls())


## O objetivo dessa analise e fazer um estudo r�pido e eficiente, atrav�s dos dados
## de doen�as graves de r�pida propaga��o, para este caso a doen�a e a
## Meningoc�cica e pelo diagrama de controle epid�mico, conseguimos monitora e
## controla a propaga��o, para melhor resposta de conten��o no avan�o da doen�a.
## De modo a acionar medidas preventivas nos casos que os limites superiores s�o
## violados.


## OS dados abaixo foram extra�do de Robson M. Rossi - Apostila de Biometria (Curso de
## Estat�stica 2010)



dados =  c(0.45,0.17,0.59,0.51,0.53,0.55,0.82,0.51,0.68,0.77,
          0.41,0.32,0.35,0.40,0.46,0.91,0.53,0.49,0.61,0.59,
          0.39,0.35,0.53,0.44,0.35,0.67,0.67,0.76,0.68,0.61,
          0.35,0.46,0.76,0.76,0.42,0.62,0.67,0.49,0.79,0.61,
          0.28,0.67,0.61,0.44,0.47,0.56,0.58,0.85,0.88,0.47,
          0.45,0.46,0.79,0.46,0.60,0.60,0.80,0.87,0.97,0.95,
          0.52,0.65,0.72,1.26,0.58,0.64,0.84,1.02,1.06,0.90,
          0.37,0.57,0.65,0.62,0.64,0.51,0.98,1.00,0.88,0.65,
          0.355,0.41,0.88,0.68,0.00,0.73,0.76,1.34,0.99,0.74,
          0.34,0.57,0.50,0.62,0.66,0.47,0.71,1.16,0.61,0.32,
          0.52,0.61,0.42,0.51,0.35,0.91,0.91,1.05,0.58,0.22,
          0.62,0.35,0.46,0.71,0.47,0.67,0.71,0.85,0.86,0.68)

max(dados)

length(dados)
library(ggplot2)
library(qcc)


# Gerando dados aleat�rios

# Calculando a m�dia e o desvio padr�o dos dados
media <- mean(dados)
desvio <- sd(dados)

# Definindo os limites de controle e alerta
limites_controle <- c(media - 3*desvio, media + 3*desvio)
limites_alerta <- c(media - 2*desvio, media + 2*desvio)

# Criando o objeto do gr�fico de controle
grafico_controle <- qcc(dados, type = "xbar.one", plot = TRUE)

# Obtendo os limites de controle e alerta do objeto do gr�fico de controle
limites_grafico <- grafico_controle$limits
alertas_grafico <- grafico_controle$limits[1]

# Transformando os dados em um data frame e adicionando um �ndice de grupo
dados_df <- data.frame(
  dados = dados,
  meses = 1:120
)
    
# Criando o gr�fico de controle usando o ggplot2
ggplot(dados_df, aes(x = meses, y = dados)) +
  geom_line() +  # Adicionando uma linha para os dados
  annotate("text", x = 60, y = 1.7, label = " A cada intervalo de 10 no eixo
  X corresponde a um m�s")+
  scale_x_continuous(limits = c(1,120), breaks = seq(0,120,10))+
  scale_y_continuous(limits = c(0,1.8), breaks = seq(0, 2, 0.2))+
  geom_hline(yintercept = limites_controle[1], linetype = "dashed", color = "red")+  # Adicionando a linha do limite inferior de controle
  geom_hline(yintercept = limites_controle[2], linetype = "dashed", color = "red") +  # Adicionando a linha do limite superior de controle
  geom_hline(yintercept = limites_alerta[1], linetype = "dotted", color = "orange") +  # Adicionando a linha do limite inferior de alerta
  geom_hline(yintercept = limites_alerta[2], linetype = "dotted", color = "orange") +  # Adicionando a linha do limite superior de alerta
  geom_hline(yintercept = media, linetype = "dashed", color = "blue") +  # Adicionando a linha da m�dia
   geom_point(data = dados_df[dados_df$dados < limites_alerta[1] | dados_df$dados > limites_alerta[2],], aes(x = meses, y = dados), color = "red", size = 3) +  # Adicionando pontos para os dados fora dos limites de alerta
  labs(x = "Meses", y = "Valores", title = "Gr�fico de Controle")  # Adicionando os r�tulos de eixo e t�tulo


##     No gr�fico de controle acima, temos linhas horizontais representando valores 
## cr�ticos e representativos dos pacientes  como � taxa m�dia(linha azul), limites 
## de alerta(linhas laranjas) e limites de controle(linha vermelha). Na linha preta 
## � a frequ�ncia de distribui��o dos dados sazonalmente em torno da m�dia. Pode-se 
## notar em alguns momentos que os dados v�ria para fora desse intervalo dentro dos
## limites nos meses de abril(2000) � agosto(2000).com esses �ltimos dois 
## meses indicando um poss�vel surto de epidemia em andamento j� que pelos dados 
## esse per�odo est� bem desproporcional, divergindo da m�dia. Acionando um alerta 
## para tomadas de medidas preventivas ser for o caso vacina��o, medica��o,entre
## outros. Sendo bem ilustrado no gr�fico acima com o pico marcado com uma bola
## vermelha quando esses limetes foi cruzado em algum momento.
