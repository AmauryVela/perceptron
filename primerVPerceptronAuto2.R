#Perceptron
#Angel Amaury Vela Mireles
#15-Octubre-2015
#This code is distributed under the terms and conditions of the MIT Licence.
# Programa el algoritmo del Perceptrón en R y sube aquí el código documentado. Valor 1.5 puntos.

rm(list=ls())
#---------- FUNCIONES ----------
#Funcion para volver a calcular Valores
reCalcularPesos<-function(a,t,px,py,wx,wy,n){
	#Calculamos el error
	error<-t-a
	#Velocidad de aprendizaje por el error, valor que utilizaremos para sacar los nuevos pesos
	ne=n*error
	#Calculamos los pesos nuevos, para esto vamos hacerlo individualmente x y y
	px<-px*ne
	py<-py*ne
	w<-list()
	w[[1]]<-wx+px
   w[[2]]<-wy+py
   return (w)
}
#Funcion para calcular el nuevo Bias
reCalcularBias<-function(a,t,b,n){
	#Calculamos el error
	error<-t-a
	bNuevo<-b+(n*error)
	return(bNuevo)
}
#Funcion para calcular la salida con harlim
harlim <- function(x){
	#Cuando N es menor a 0 la salida es =
	#Cuando N es mayor o igual a 0 la salida es 1
	if(x<0){
		return(0)
	}
	else if (x>=0){
		return(1)
	}
}
#---------- FIN FUNCIONES ----------

#------ INICIO DE PROGRAMA -----------
#Este programa esta basado en el ejemplo visto en clase pensado en la funcion Harlim.
#A continuacion vamos a cargar un csv con los puntos
data <- read.csv("/Users/amauryvela/Downloads/puntosp.csv", header = FALSE, sep = ',')
matrix <- as.matrix(data)


#Bias
bias<-0
#W inicial
w<-list()
w[[1]]<-1
w[[2]]<--0.8
#Velocidad de aprendizaje
n<-1
#Numero de puntos para probar
nP<-3
#Vamos a empezar a calcular los pesos con la funcion Harlim y calcular asi 
#Declareremos una variable que nos dira si los pesos ya funcionan o no, si funcionan terminaran el for, si no seguira andando hasta que los encuentre, o llegue a 100 iteraciones
encontro<-FALSE

for (j in 1:100){
	if(encontro){
		break
	}
	else{
		for ( i in 1:nP){
			#Vamos a calcular el valor de a con la formula
			valA=matrix[i,1]*w[[1]]+matrix[i,2]*w[[2]]+bias		
			#Ahora comparamos el valor de T con el valor de la salida de Harlim
			if (matrix[i,3]!= harlim(valA)){
				encontro<-FALSE
				#Si entra a aqui quiere decir que el valor calculado para harlim es diferente al valor que tenemos en T por lo que sera necesario volver a calcular los valores de W y Bias
				w<-reCalcularPesos(harlim(valA),matrix[i,3],matrix[i,1],matrix[i,2],w[[1]],w[[2]],n)
				bias<-reCalcularBias(harlim(valA),matrix[i,3],bias,n)
			}else{
				encontro<-TRUE
			}
		}
	}#fin else
}

#verificamos si encontro
encontro
#Calculamos y Verificamos el punto nuevo
laSalidaPCuatro<-harlim(matrix[4,1]*w[[1]]+ matrix[4,2]*w[[2]]+bias)
laSalidaPCuatro