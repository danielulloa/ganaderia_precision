library(rstudioapi) # cargo una libreria para configurar el path automaticamente
current_path <- getActiveDocumentContext()$path # obtengo el path donde está el script
setwd(dirname(current_path )) # cambio el path

library(seewave)
library(tuneR)
library(signal)
library(rgl)


archivodestino="data.zip" # nombre de archivo a descargar
archivoURL <-
  "https://www.dropbox.com/s/d12x5vknrk9yek1/data.zip?raw=1"
if (!file.exists(archivodestino)) {
  download.file(archivoURL ,archivodestino,method="auto")
  unzip(archivodestino)
  }

sound <- readWave("data/0001.wav")

file.names <- dir("data", pattern="wav$")
#soundfiles <- file.names[runif(4,0,1000)]
soundfiles <- file.names[c(1023,765,43,2354)]

oscillo(sound)
spectro(sound,ovlp=75, wl=1024,flim=c(0,0.65))
flim <- c(0,.6)
col <- "blue"
par(mfrow=c(4,3), # 4 * 3 figure plate organisation
    mar=c(4.5,4,1,1), # margins
    lwd=0.5) # line width for all graphics


for(i in 1:length(soundfiles)){
  sound = readWave(paste("data", soundfiles[[i]], sep="/"))
  oscillo(sound, colwave=col, cexlab=0.7)
  spec(sound, flim=flim, col=col)
  phaseplot(sound, dim=2, col=col)
}

sound = readWave(paste("data", file.names[22], sep="/"))
phaseplot(sound, dim=3, col=col)
