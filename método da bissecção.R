## RESUMO INICIAL

#-------------------------------------------------------------------------------

## Esse algoritmo foi criado para encontra raízes de uma função de 
## forma aproximada, essa forma de resolução abaixo  e chamada de 
## método da bisseção,e aplicável para funções continuas neste caso
## funciona na seguinte maneira pegamos dois intervalos na qual a raiz 
## esteja definida dentro do domínio da função se haver uma troca de 
## sinais nos extremos desse intervalo, então existe um teorema que garante
## a existência de pelo menos uma raiz real definida para esse intervalo. 
## E assim vai seguindo essa lógica até que esse intervalo se torne tao 
## estrito na qual fique tão perto da raiz.

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
  {;                            # com o sinal de um dos 2 lados e assim seguira até se 
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