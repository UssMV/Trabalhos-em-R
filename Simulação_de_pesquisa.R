rm(list=ls())

# Esse script abaixo, tem como objetivo fazer uma pequena simulação usando 
# o emprego de algumas estatísticas básicas, para uma breve observação a respeito
# da relação  de influência entre os dados do PIB e desemprego simulados com média
# e desvio padrão escolhidos ao acaso em um período de 15 anos usando o coeficiente
# de correlação.



# Gerar dados fictícios para o PIB e o índice de desemprego
set.seed(123)  # Define uma semente para garantir a reprodutibilidade dos resultados
anos <- 1:15
PIB <- rnorm(15, mean = 3.5, sd = 1)  # Dados fictícios para o PIB de uma gerados de 
# distribuição normal 
desemprego <- rnorm(15, mean = 4, sd = 1.2)  # Dados fictícios para o índice de desemprego
#erados de distribuição normal 

# Plotar o crescimento do PIB e o índice de desemprego ao longo do tempo
plot(anos, PIB, type = "l", col = "blue", xlab = "Ano", ylab = "PIB", main = "Crescimento do PIB 
  ao longo do tempo")
lines(anos,desemprego, col ='red')


# Calcular a correlação entre o crescimento do PIB e o índice de desemprego
correlacao <- cor(PIB, desemprego)
print(paste("Correlação:", correlacao))


f <- function(valor){

if (valor == 0)
  {print('PIB e desemprego não são dependentes entre si com correlação igual')}  
if (valor >= 0.1 && valor <= 0.6)
  {print('PiB e desemprego tem boa influência entre si com correlação igual')}
if (valor == 0.6){
  print('PIB e desemprego tem boa dependência entre si com correlação igual')}
if (valor >= 0.7 && valor <= 1){
  print('PiB e desemprego tem forte influência entre si com correlação igual')}
if (valor >= -1 && correlacao <= -0.7){
    print('PiB e desemprego tem forte influência entre si com correlação igual')}
if (valor == -0.6){
    print('PIB e desemprego tem boa dependência entre si com correlação igual')}
if (correlacao >= -0.6 && correlacao <= -0.1){
    print('PIB e desemprego tem pouca influência entre si com correlação igual')}
if (is.numeric(valor)){print(valor)}  
  
}
f(valor = correlacao)


