## RESUMO INICIAL

#-------------------------------------------------------------------------------

## Esse algoritmo foi criado para encontra ra�zes de uma fun��o de 
## forma aproximada, essa forma de resolu��o abaixo  e chamada de 
## m�todo da bisse��o,e aplic�vel para fun��es continuas neste caso
## funciona na seguinte maneira pegamos dois intervalos na qual a raiz 
## esteja definida dentro do dom�nio da fun��o se haver uma troca de 
## sinais nos extremos desse intervalo, ent�o existe um teorema que garante
## a exist�ncia de pelo menos uma raiz real definida para esse intervalo. 
## E assim vai seguindo essa l�gica at� que esse intervalo se torne tao 
## estrito na qual fique t�o perto da raiz.

rm(list=ls())



a<-2                             # a e b serao os pontos do intrvalo extremo para achar raiz
b<-3
f<-function(x){x*log10(x)-1}     # Definindo a funcao para veriguar a raiz 
f(a)
f(b)
media<-(a+b)/2;media
while(f(media)!=0&&b-a>.01)         # Estruturando a condicao a ser atendida
{
  if(f(media)==0)  
  {
    media
  }
  if(f(media)<0)                    # se a imagem da media dos intervalos que sera atendida para
  {;                            # com o sinal de um dos 2 lados e assim seguira at� se 
    a<-media                        # aproximar para tao perto da raiz com o nivel do erro permitido
  }
  else
  {
    b<-media
  }
  {
    media<-(a+b)/2;media
  }
}


print(paste("A raiz entre", as.character(a), "e", as.character(b)," E o pnto medio: ",as.character(media)))

cat("\\Raiz aproximada:",media)
## FIM