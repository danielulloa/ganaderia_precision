# script utilizado para explorar datos de la competencia "IA en Ganadería de Precisión"
# https://metadata.fundacionsadosky.org.ar/competition/14/


library(rstudioapi) # carga una libreria para configurar el path automaticamente
current_path <- getActiveDocumentContext()$path # el path donde está el script
setwd(dirname(current_path )) # cambia el path

# librerias a utilizar
library(seewave)
library(tuneR)
library(signal)
library(rgl)
library(ggplot2)


# obtiene datos https://metadata.fundacionsadosky.org.ar/competition/14/
archivodestino="data.zip" # nombre de archivo a descargar
archivoURL <-
  "https://www.dropbox.com/s/d12x5vknrk9yek1/data.zip?raw=1"

# verifica si existe el archivo en el directorio y extrae
if (!file.exists(archivodestino)) {
  download.file(archivoURL ,archivodestino,method="auto")
  unzip(archivodestino)
  download.file("https://www.dropbox.com/s/z1cwznln0gfpw32/train_labels.csv?raw=1","train_labels.csv",method = "auto")
  }



#generar un vector con la ubicacion relativa de los archivos
file.names <- dir("data", pattern="wav$")
file.names <- paste("data", file.names, sep="/")



# elegir 3 archivos
soundfiles <- file.names[runif(3,0,length(file.names))]
#soundfiles <- file.names[c(1023,765,43,2354)]

# carga un sonido
sound <- readWave(file.names[1])

# grafico de oscilograma y espectograma
par(mfrow=c(1,1), mar=c(5,5,1,1))
oscillo(sound)
flim <- c(0,.65)
spectro(sound,ovlp=75, wl=1024,flim=flim)


# genera graficos exploratorios

col <- "blue"
par(mfrow=c(3,3),mar=c(4,4,1,1))

for(i in 1:length(soundfiles)){
  sound = readWave(soundfiles[i])
  oscillo(sound, colwave=col, cexlab=0.7)
  spec(sound, flim=flim)
  phaseplot(sound, dim=2, col=col)
}

# diagrama de fase de un tono puro, outlier del dataset
sound = readWave(file.names[22])
phaseplot(sound, dim=3, col=col)


# analisis estadistico de los archivos etiquetados para entrenamiento
train <- read.csv("train_labels.csv")

fpromedio <- c()
fkurt <- c()
intdb <- c()
hshannon <- c()

# analisis espectral para determinar frecuencia promedio, entropia de shannon
for(i in 1:nrow(train)){
  sound <- readWave(file.names[train$filename[i]])
  f=sound@samp.rate
  tmp<-meanspec(sound,f=22050,plot=FALSE)
  fpromedio[i] <- specprop(tmp,f=f)$mean
  fkurt[i] <- specprop(tmp,f=f)$kurtosis
  intdb[i] <- meandB(tmp, level ="IL")
  hshannon[i] <- sh(tmp, alpha = "shannon")
}

# concatena los vectores generados al dataframe
train=data.frame(train,fkurt,fpromedio,intdb,hshannon)

# etiqueta los datos

# Masticación (o chew, del inglés): Consiste en el triturado del alimento ingerido.
#   El animal utiliza las piezas dentarias posteriores. 
#   Se da mayormente durante la rumia pero también está presente durante el pastoreo.
# 
# Arranque (o bite, del inglés): Consiste en el corte y rasgado de la pastura. 
#   El animal utiliza las piezas dentarias anteriores. 
#   Se da únicamente en el pastoreo.
#    
# Arranque-masticación (o chew-bite, del inglés): Es un solo movimiento
#   que consiste en la combinación de los dos anteriores.
#   Se da únicamente en el pastoreo.

train$label <- factor(train$label,
                    levels = c(0,1,2),
                    labels = c("bite", "chew", "chew-bite")) 

ggplot(train, aes(train$fpromedio, train$hshannon, color=train$label)) +
  geom_point()+
  xlim(350,2000)

# apilar las componentes de la fft por grupos
# crear una matriz vacia
# a <- matrix(nrow=10,ncol=12)
# numeros = 1:12
# a[1,] <- rbind(numeros)
# a[2,] <- rbind(numeros)
# a[4:6,] <- rbind(numeros)

cuenta1=0
cuenta2=0
cuenta3=0
for(i in 1:nrow(train)){
  if (train$label[i]=='bite'){
    cuenta1=cuenta1+1
  }
  else if (train$label[i]=='chew'){
    cuenta2=cuenta2+1
  }
  else if (train$label[i]=='chew-bite'){
    cuenta3=cuenta3+1
  }
}
f=sound@samp.rate
fmax=0.5*sound@samp.rate
grupo1 <- matrix(nrow = cuenta1, ncol = 8820)
grupo2 <- matrix(nrow = cuenta2, ncol = 8820)
grupo3 <- matrix(nrow = cuenta3, ncol = 8820)


plot(tmp[,2])
cuenta1=0
cuenta2=0
cuenta3=0
vectg1=1:20
for(i in 1:nrow(train)){
sound <- readWave(file.names[train$filename[i]])
f=sound@samp.rate
# x <- sound@left
# sumax <- x^2
# xnorm <- x/(sqrt(sum(sumax)))
# sound@left <- xnorm
tmp <- spec(sound,f=f,plot=FALSE,norm = FALSE)[,2]
sumatmp <- tmp^2
tmpnorm <- tmp/(sqrt(sum(sumatmp)))
tmp <- tmpnorm

if (train$label[i]=='bite'){
  cuenta1=cuenta1+1
  grupo1[cuenta1,] <- tmp
  vectg1[cuenta1] <- sum(tmp^2)
  }
else if (train$label[i]=='chew'){
  cuenta2=cuenta2+1
  grupo2[cuenta2,] <- tmp
}
else if (train$label[i]=='chew-bite'){
  cuenta3=cuenta3+1
  grupo3[cuenta3,] <- tmp
}
}
f <- spec(sound,f=f,plot=FALSE,)[,1]
grupo1df <- data.frame(grupo1df,f)

grupo1df <- as.data.frame(t(grupo1))
grupo2df <- as.data.frame(t(grupo2))
grupo3df <- as.data.frame(t(grupo3))

ggplot(grupo1df,aes(f,y=value, color=variable))+
  geom_line(aes(y=V1,col="V1"))+
  geom_line(aes(y=V2,col="V2"))+
  geom_line(aes(y=V3,col="V3"))+
  geom_line(aes(y=V4,col="V4"))+
  geom_line(aes(y=V5,col="V5"))+
  geom_line(aes(y=V6,col="V6"))+
  geom_line(aes(y=V7,col="V7"))+
  xlim(0,0.65)

ggplot(grupo2df,aes(f,y=value, color=variable))+
  geom_line(aes(y=V1,col="V1"))+
  geom_line(aes(y=V2,col="V2"))+
  geom_line(aes(y=V3,col="V3"))+
  geom_line(aes(y=V4,col="V4"))+
  geom_line(aes(y=V5,col="V5"))+
  geom_line(aes(y=V6,col="V6"))+
  geom_line(aes(y=V7,col="V7"))+
  xlim(0,0.65)

ggplot(grupo3df,aes(f,y=value, color=variable))+
  geom_line(aes(y=V1,col="V1"))+
  geom_line(aes(y=V2,col="V2"))+
  geom_line(aes(y=V3,col="V3"))+
  geom_line(aes(y=V4,col="V4"))+
  geom_line(aes(y=V5,col="V5"))+
  geom_line(aes(y=V6,col="V6"))+
  geom_line(aes(y=V7,col="V7"))+
  xlim(0,0.65)

p <- ggplot(data=grupo1df, aes(x=f))
# loop
for (i in 1:50) {
  # use aes_string with names of the data.frame
  p <- p + geom_line(aes_string(y = names(grupo1df)[i],col=names(grupo1df)[i]))+ xlim(0,1)+ylim(0,0.3)
}
# print the result
print(p)

p2 <- ggplot(data=grupo2df, aes(x=f))
# loop
for (i in 1:50) {
  # use aes_string with names of the data.frame
  p2 <- p2 + geom_line(aes_string(y = names(grupo2df)[i],col=names(grupo2df)[i]))+ xlim(0,1)+ylim(0,0.3)
}
# print the result
print(p2)

p3 <- ggplot(data=grupo3df, aes(x=f))
# loop
for (i in 1:50) {
  # use aes_string with names of the data.frame
  p3 <- p3 + geom_line(aes_string(y = names(grupo3df)[i],col=names(grupo3df)[i]))+ xlim(0,1)+ylim(0,0.3)
}
# print the result
print(p3)

par(mfrow=c(3,1),mar=c(4,4,1,1))

x <- sound@left
plot(x)
sumax <- x^2
xnorm <- x/(sqrt(sum(sumax)))
plot(xnorm)
xnorm
sonido <- xnorm
spec(sonido,f=f,norm = FALSE)