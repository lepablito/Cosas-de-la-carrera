library(dplyr)

tablacodigos<-read.csv("C:/Users/pablm/Downloads/datos/countries_codes_and_coordinates.csv", header=TRUE, sep=",")
tabla2015<-read.csv("C:/Users/pablm/Downloads/datos/2015.csv", header=TRUE, sep=",")
tabla2016<-read.csv("C:/Users/pablm/Downloads/datos/2016.csv", header=TRUE, sep=",")
tabla2017<-read.csv("C:/Users/pablm/Downloads/datos/2017.csv", header=TRUE, sep=",")
tabla2018<-read.csv("C:/Users/pablm/Downloads/datos/2018.csv", header=TRUE, sep=",")
tabla2019<-read.csv("C:/Users/pablm/Downloads/datos/2019.csv", header=TRUE, sep=",")
tablaisos<-read.csv("C:/Users/pablm/Downloads/datos/2015renovado.csv", header=TRUE, sep=",")

rank2015<-c(rep(NA,158))
rank2016<-c(rep(NA,158))
rank2017<-c(rep(NA,158))
rank2018<-c(rep(NA,158))
rank2019<-c(rep(NA,158))
score2015<-c(rep(NA,158))
score2016<-c(rep(NA,158))
score2017<-c(rep(NA,158))
score2018<-c(rep(NA,158))
score2019<-c(rep(NA,158))
ISO<-tablaisos$ISO

tabla<-data.frame(Country=tabla2015$Country,Region=tabla2015$Region,rank2015=rank2015,score2015=score2015,rank2016=rank2016,score2016=score2016,rank2017=rank2017,score2017=score2017,rank2018=rank2018,score2018=score2018,rank2019=rank2019,score2019=score2019)
tablanueva<-arrange(tabla, Country)
tablanueva<-cbind(ISO,tablanueva)
tablaaaa<-arrange(tablaisos, rank2017)

for (i in 1:length(tablanueva$Country)){
  for (j in 1:length(tabla2015$Country)){
    if(tablanueva[i,2]==tabla2015[j,1]){
      tablanueva[i,4]<-tabla2015[j,3]
      tablanueva[i,5]<-tabla2015[j,4]
    }
  }
}

for (i in 1:length(tablanueva$Country)){
  for (j in 1:length(tabla2016$Country)){
    if(tablanueva[i,2]==tabla2016[j,1]){
      tablanueva[i,6]<-tabla2016[j,3]
      tablanueva[i,7]<-tabla2016[j,4]
    }
  }
}

for (i in 1:length(tablanueva$Country)){
  for (j in 1:length(tabla2017$Country)){
    if(tablanueva[i,2]==tabla2017[j,1]){
      tablanueva[i,8]<-tabla2017[j,2]
      tablanueva[i,9]<-tabla2017[j,3]
    }
  }
}

for (i in 1:length(tablanueva$Country)){
  for (j in 1:length(tabla2018$Country)){
    if(tablanueva[i,2]==tabla2018[j,2]){
      tablanueva[i,10]<-tabla2018[j,1]
      tablanueva[i,11]<-tabla2018[j,3]
    }
  }
}

for (i in 1:length(tablanueva$Country)){
  for (j in 1:length(tabla2019$Country)){
    if(tablanueva[i,2]==tabla2019[j,2]){
      tablanueva[i,12]<-tabla2019[j,1]
      tablanueva[i,13]<-tabla2019[j,3]
    }
  }
}
tablaisosnueva<-gsub(" ", "", tablaisos[1])
for (i in 1:158){
  tablaisos[i,1]<-gsub(" ", "", tablaisos[i,1])
}
tablanueva<-tablaisos
tabla2015<-arrange(tablaisos, rank2015)
tabla2015<-select(tabla2015,Country, Region,rank2015,score2015)
tabla2016<-arrange(tablaisos, rank2016)
tabla2016<-select(tabla2016,Country, Region,rank2016,score2016)
tabla2017<-arrange(tablaisos, rank2017)
tabla2017<-select(tabla2017,Country, Region,rank2017,score2017)
tabla2018<-arrange(tablaisos, rank2018)
tabla2018<-select(tabla2018,Country, Region,rank2018,score2018)
tabla2019<-arrange(tablaisos, rank2019)
tabla2019<-select(tabla2019,Country, Region,rank2019,score2019)
tablanueva$score2017<-round(tablaisos$score2017, digits=3)

write.csv(tablanueva, file="C:/Users/pablm/Downloads/datos/2015renovado.csv", row.names = F)