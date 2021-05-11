frontera<-c(4,5)
frontera
frontera[c(1,2)]
frontera<-frontera[-c(1,2)]
frontera
frontera<-rbind(c(7,8))
frontera<-rbind(frontera,c(9,10))
c(frontera[1,1],frontera[1,2])
frontera<-frontera[-c(1,2)]
pico<-c(frontera[c(1,2)])
explorado<-c()
explorado<-rbind(explorado,c(1,2))
explorado<-rbind(explorado,c(3,4))
explorado<-rbind(explorado,c(1,5))
contains(c(1,2), explorado)
cbind(1,2)%.%group_by(V1,V2)

lugar<-match(c(1,5), explorado)
explorado<-explorado[-c(lugar[1],lugar[2])]
explorado
#todo NA menos el inicial
disttent<-matrizalg[pos[1],pos[2]]+matrizoriginal[pos[1]+1,pos[2]]
if (disttent<matrizalg[pos[1]+1,pos[2]]){
  matrizalg[pos[1]+1,pos[2]]<-disttent
}
frontera<-rbind(frontera,c(pos[1]+1,pos[2]))

caca<-c(NA,NA,NA,NA)
is.infinite(min(caca, na.rm = TRUE))

